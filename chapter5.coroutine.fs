(*
In this chapter we will see how we can define a mini-language inside F#, which uses F# data structures to represent additional data about computations.
We will see how we can build a coroutine system to represent computations (such as AI algorithms) that must be suspended at each iteration of the simulation and resumed at the next iteration.

This chapter is, possibly, the hardest of the entire book.
While we promise that mastering the following notions is not easy, we also promise that doing so will give you considerable programming powers.
A good strategy may be to read the following section and learn it almost by heart, and then read the next example to understand how these constructs are used.
Also, you may reverse reading order to read first the uses of these constructs (they are more intuitive to use than to define) and then to see how they work.
Finally, you may start with the examples at the end of the chapter and then work your way up to the following description,
since this description is more complex than the examples and through those you may better understand some of the common ideas used.

Either way, give yourself a little time: the prize is worth the effort!
*)

(*
We will build a simulation that is a bit more articulated than those we have seen in the previous chapters.
A cargo freighter is attacked by despearte pirates not too far from the space police station.
A fast police corvette is sent to aid the freighter, but despite its speed its weapons and shield are no match for the pirate armaments, and so the corvette will have to go back to the station often for repairs.

We wish to model the AI of the various ships.
The main idea is that such an AI is a state machine (SM). Let us consider the pirate ship. Its SM states are:
- attack the cargo freighter
- attack the police corvette
The transitions between the states are:
- the police corvette has come into weapons range
- the police corvette has exited weapons range

Our first take might be that of modeling the AI FSMs as discriminated unions, as we have seen in the previous Chapter.
So, we would say something like:
type PirateAI = AttackingCargo | AttackingPolice

attacking a ship, though, requires more information. Are we accelerating, shooting, correcting our course to compensate for the target movements, etc?
This means that we would have to add another SM to model the state of the current attack:
type AttackAI = MovingTowards of Ship | Fighting of Ship | ...

and now the pirate AI would become:
type PirateAI = AttackingCargo of AttackAI | AttackingPolice of AttackAI

The state transition function would then take as input a value of type PirateAI and return another value that represents the updated state.
As behaviors become more and more articulated, and the nesting of sub-behaviors becomes deeper and deeper, our discriminated unions may grow very large.
Also, the state transition function may become very complex to maintain, and adding new nested behaviors could become quite challenging.

Let us move a step back. A very natural way to express AIs is through concurrent sequences of operations.
An AI is essentially a series of processes run at different frequencies, that keep checking the state of the world to initiate certain actions when appropriate.
For example, we may say that the pirate AI is defined as:
- when the police is in range, attack it
- when the police is not in range, attack the cargo

or, by using a more symbolic form, we could write:
police_in_range => attack police
police_not_in_range => attack cargo

where the attack operation is comprised by a series of sequential operations that must be run one after another, with pauses between each action to allow the simulation to progress:
attack ship =
  if too_far
    fire_engines_towards ship
    wait engine_cooloff
  else
    shoot ship
    wait weapons_cooloff

since certain actions may require to be performed during the same step of the simulation to function correctly, we will use the notion of yielding to define when an AI must suspend itself to be resumed at the next simulation step.
For example, in the above piece of code, wait may be defined as:
wait dt =
  t0 = time
  yield
  t = time
  if t - t0 > dt then
    wait (dt - (t-t0))

so that wait will suspend the computation until enough *actual user time* has passed.
These computations that can be suspended and resumed are called "coroutines".

To sum up what we will do:
- we will define a system where:
  - the police tries to rescue the cargo ship, by coming and going for repairs and refueling to a nearby base
  - the pirates attack the cargo and defend from the police
  - the cargo (slowly) flees to the base
- we will use coroutines to define state machines linearly, instead of building complex datatypes
- we will use an F# feature, called "Computation Expressions", to build our custom coroutines into F# itself, seamlessly
*)
namespace Chapter5
  module Coroutines =
    open Microsoft.FSharp
    open Microsoft.FSharp.Core
    open System

    (*
    A coroutines is defined as a function that takes nothing as input and returns one of three things:
    - the final result of the computation if it is finished
    - another coroutine that will execute the remainder of the computation, when the computation is not finished
    *)
    type Coroutine<'a> = Unit -> CoroutineStep<'a>
    and CoroutineStep<'a> =
      Return of 'a
      | Yield of Coroutine<'a>
      | ArrowYield of Coroutine<'a>

    (*
    An F# builder is a class that contains a series of operations on a certain datatype (in our case our coroutine type).
    These operations must have a certain, precise, structure.
    If the operations of a builder are defined correctly, the methods of the builder will be invoked automatically, and implicitly by F#
    instead of explicitly by the user of our library.
    This allows us to build very complex libraries that are incredibly simple to use for the end-user.

    The two, most important operators that we will define are Bind and Return.
    F# will turn statements such as:

    let! x = e1
    e2

    into
    Bind(e1, fun x -> e2)

    and
    return v

    into
    Return(v)

    this translation allows us to chain multiple custom bindings such as:
    Bind(e1, fun x -> Bind(e2, fun y -> ...Return(v) ... ))

    with the far more readable syntax:
    let! x = e1
    let! y = e2
    ...
    return v

    Using F# builder gives us an amazing power: that of redefining (overloading) the let operator.
    Those who used C++ operator overloading extensively know ho much power can be found in operator customization.

    This way what the user sees as a single let binding instead becomes an execution of the Bind operation, which may run a thread or create a list or any other thing.
    *)
    type CoroutineBuilder() =
      (*
      The return operation for a coroutine builds a coroutine that simply contains a result value:
      *)
      member this.Return(x:'a) : Coroutine<'a> =
        fun s -> Return x
      (*
      The binding operator takes as input two parameters:
      - p, which is a coroutine
      - k = fun x -> r, which is a function that takes as input the result of p and returns another coroutine

      Bind is invoked when the user writes:
      let! x = p
      r

      and we must define a new coroutine that represents the execution of p "into" r.
      The binding operator tries to run p; if p returns its result, then this result is passed to k and the resulting coroutine is returned;
      if p returns a suspension, then the result is the binding of the suspension with the rest of the computation. Note that Bind does not run indefinitely,
      since the parameter s is not passed to the second instance of Bind, thereby avoiding the deep recursion that would otherwise happen.

      A long chain of binds is run by passing () to a coroutine many times, until it returns its final result:
      let rec run (c:Coroutine<'a>) : 'a =
        match c () with
        | Result x -> x
        | Yield c' | ArrowYield c' -> c'
      *)
      member this.Bind(p : Coroutine<'a>,
                       k : 'a -> Coroutine<'b>) : Coroutine<'b> =
        fun s ->
          match p s with
          | Return x -> k x s
          | Yield p' -> Yield(this.Bind(p',k))
          | ArrowYield p' -> ArrowYield(this.Bind(p',k))
      (*
      We now see three operators that while not strictly needed (in fact, they are all optional), are still quite useful in practice.

      We can also define a coroutine version of do, called do!; a do! is just a binding which ignores the result of the first coroutine when invoking the second.
      *)
      member this.Combine(p1:Coroutine<'a>,
                          p2:Coroutine<'b>) : Coroutine<'b> =
        this.Bind(p1, fun _ -> p2)
      (*
      The Zero method is defined for builders that wish to support conditionals without both branches.
      For example, if we write:
      if x then
        do! a

      F# turns it into:
      if x then
        do! a
      else
        Zero()
      *)
      member this.Zero() : Coroutine<Unit> = this.Return()
      (*
      Return from allows us to return an entire coroutine, so that instead of having to write:
      let! x = p1
      return x

      we can write directly:
      return! p1
      *)
      member this.ReturnFrom(s:Coroutine<'a>) = s

      (*
      The last two operations we see are used to wrap coroutines.

      Whenever we write a coroutine C, it gets wrapped into
      Run(Delay(fun () -> C))

      The Delay method defines how we wish to extract the coroutine from its delayed version (the fun() -> C).
      Run allows us to define how a Coroutine<'a> is exposed to the rest of the program.
      We might wish that coroutines are run automatically, or explicitly by the programmer; in the first case, Run will have type:
      Coroutine<'a> -> 'a

      in the second case (ours) Run will have type:
      Coroutine<'a> -> 'a
      *)
      member this.Delay s = s()
      member this.Run s = s

    (*
    All coroutine methods we have seen above are defined as members, not static members.
    This means that we need an instance of the Coroutine datatype to invoke its methods.
    This instance is defined once for all methods and is called "co" (we might have chosen any name, but co sounds appropriate).

    We will write:
    co{
      ...
      // a series of let!, do!, return and return!
      ...
    }

    and the F# compiler will turn all the let! into co.Bind, all the return into co.Return, etc.
    The entire transformed expression C will then become:
    co.Run(co.Delay(fun() -> C)
    *)
    let co = CoroutineBuilder()

    (*
    We start by defining the two simplest coroutines: suspensions.
    We will write:
    do! yield_

    or
    do! arrow_yield_

    to suspend a coroutine.
    Notice that there are two suspension mechanisms: one for simple suspensions (yield) and the other for preemptive suspensions (arrow_yield).
    We use arrow_yield to promise that our coroutine is suspending now but it will terminate soon, to signal concurrent operations that they may stop.
    We will see arrow_yield in use in the ( .|| ) operator below.
    *)
    let yield_ : Coroutine<Unit> = fun s -> Yield(fun s -> Return())
    let arrow_yield_ : Coroutine<Unit> = fun s -> ArrowYield(fun s -> Return())

    (*
    Another simple but useful coroutine takes as input a coroutine of type Coroutine<'a> and ignores its result, producing a Coroutine<Unit> as its result.
    Notice that we use co{ ... } to define the entire coroutine, let! to run the input coroutine (ignoring the result) and return to return our own result:
    *)
    let ignore_ (s:Coroutine<'a>) : Coroutine<Unit> =
      co{
        let! _ = s
        return ()
      }

    (*
    We now start defining very powerful operators on coroutines.
    The first operator we define is the OR of two coroutines, which runs two coroutines in parallel (concurrently) and returns the result of the first one to terminate.
    We try running a step of both coroutines: the first one that gives us a Return terminates the computation and returns its result.
    If both coroutines return as result Yield then we keep running them in parallel.
    If one of the two coroutine returns ArrowYield then we stop the other computation and only continue with the one that arrow_yielded.
    *)
    let rec (.||) (s1:Coroutine<'a>) (s2:Coroutine<'b>) : Coroutine<Choice<'a,'b>> =
      fun s ->
        match s1 s,s2 s with
        | Return x, _        -> Return(Choice1Of2 x)
        | _, Return y        -> Return(Choice2Of2 y)
        | ArrowYield k1, _   ->
          co{
            let! res = k1
            return Choice1Of2 res
          } |> Yield
        | _, ArrowYield k2   ->
          co{
            let! res = k2
            return Choice2Of2 res
          } |> Yield
        | Yield k1, Yield k2 -> (.||) k1 k2 |> Yield

    (*
    When two coroutines are run concurrently but we wish to ignore their result, we use the .||> operator for brevity, which just invokes ignore_ over an application of the .|| operator.
    *)
    let (.||>) s1 s2 = ignore_ (s1 .|| s2)

    (*
    The second, powerful operator is very similar to a looping statement such as while, only reversed with respect to the input condition.
    The ( => ) operator keeps running a coroutine until it returns true.
    When the first coroutine returns true, then an arrow_yield is invoked to stop other concurrent operations and the second coroutine is run.
    When the first coroutine returns false, the a simple yield is invoked and the entire guard is run again.

    This construct is also called a guard, since it runs a coroutine only when an actively polled condition becomes true.
    *)
    let rec (=>) (c:Coroutine<bool>) (s:Coroutine<'a>) : Coroutine<'a> =
      co{
        let! x = c
        if x then
          do! arrow_yield_
          let! res = s
          return res
        else
          do! yield_
          return! (=>) c s
      }

    (*
    The repeat operator simply keeps running a coroutine indefinitely.
    *)
    let rec repeat_ (s:Coroutine<Unit>) : Coroutine<Unit> =
      co{
        do! s
        return! repeat_ s
      }

    (*
    We define a wait operation that keeps doing some action until a certain amount of time has elapsed.
    We start by reading the current time t0.
    Then, recursively, we read the current time t; if t - t0 is greater than the desired interval, then we are done.
    If t - t0 is less than the interval, then we perform the action once and then we wait for the remaining interval.
    *)
    let wait_doing (action:float -> Coroutine<Unit>) (interval:float) : Coroutine<Unit> =
      let time : Coroutine<DateTime> = fun _ -> Return(DateTime.Now)
      co{
        let! t0 = time
        let rec wait() =
          co{
              let! t = time
              let dt = (t - t0).TotalSeconds
              if dt < interval then
                do! yield_
                do! action dt
                return! wait()
          }
        do! wait()
      }

    (*
    A simple utility is a partial specialization of the waiting function seen above, which waits while doing nothing.
    *)
    let wait = wait_doing (fun (dt:float) -> co{ return () })
