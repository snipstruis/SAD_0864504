namespace Chapter4
  (*
  Now that we have built the quad tree module we can create the actual simulation.
  This simulation will be very fast, because we will not be computing the force from each asteroid to each other asteroid.
  Instead, we will store asteroids in a quad tree, and compare its asteroids only with the (few) local asteroids plus with the barycenter of the other nodes.

  By using a tree, the resulting computation requires approximately n * log n computations.
  Deriving this result is a bit too complex for the aim of this book, but the idea is that where 200 asteroids required 40000 computations before they will now require
  far less computations, just 460.
  *)
  module LargeAsteroidFieldSimulation =

    (*
    We will reuse the Asteroid definition, the Vec2 definition, even the simulation and asteroid field initialization functions from the previous chapters.
    Reusing code is always good and developers should always strive to reuse code as much as possible, both in the large (modules), in the medium (classes) and in the small (functions).
    *)
    open System
    open System.Threading
    open Chapter2.Math
    open Chapter3.SmallAsteroidFieldSimulation

    (*
    We create a field with 200 asteroids with the create_field function seen in the example of Chapter 3.
    *)
    let f0 = create_field 200

    (*
    We define a Barycenter record that stores the weighted average of a set of asteroids.
    The idea is that if we have two asteroid, but one has a larger mass, then the barycenter is moved towards the larger asteroid.
    *)
    type Barycenter =
      {
        Position : Vec2<m>;
        Mass : float<kg>
      }
      (*
      For reasons of convenience we define a conversion function that turns a barycenter into a (virtual) asteroid.
      *)
      member this.ToAsteroid = { Position = this.Position; Mass = this.Mass; Name = ""; Velocity = Vec2<_>.Zero }
      (*
      We define two further members that allow us to compute the barycenter of two barycenters and the barycenter of a list of asteroids.

      We must always watch out for null masses, beacuse otherwise we run the risk of dividing by zero!
      *)
      static member (+/) (b1:Barycenter, b2:Barycenter) =
        let new_mass = b1.Mass + b2.Mass
        if new_mass < 0.01<_> then
          {
            Position = Vec2<_>.Zero
            Mass     = 0.0<_>
          }
        else
          {
            Position = (b1.Position * b1.Mass + b2.Position * b2.Mass) / new_mass
            Mass     = new_mass
          }
      static member OfAsteroidList (l:Asteroid list) =
        let positions_weighted_sum = l |> Seq.map (fun a -> a.Position * a.Mass) |> Seq.sum
        let masses_sum = l |> Seq.sumBy (fun a -> a.Mass)
        {
          Position = if masses_sum > 0.01<_> then positions_weighted_sum / masses_sum else Vec2<_>.Zero
          Mass     = masses_sum
        }

    (*
    We define an auxiliary function tha computes the local forces between a list of asteroids belonging to a leaf.
    This function also adds (when its mass is not null) the global contribution of the barycenter of the other nodes.
    *)
    let local_forces (others:Barycenter) asteroid_group =
      [
        for a in asteroid_group do
          let forces =
               seq{
                 for a' in asteroid_group do
                   if a' <> a then
                     yield force(a,a')
               }
          let F_local = Seq.sum forces
          let F = 
            if others.Mass < 0.01<_> then
              F_local
            else
              F_local + force(a,others.ToAsteroid)
          let p',v' = clamp(a.Position,a.Velocity)
          yield
            {
              a with
                  Position = p' + dt * v'
                  Velocity = v' + dt * F / a.Mass
            }
      ]

   (*
    For each node of the tree, we traverse it. The barycenter of all the *other* nodes is added when traversing a node.
    *)
    let rec traverse (others:Barycenter, root) =
      match root with
      | QuadTree.Leaf(r,a,b) -> QuadTree.Leaf(r,local_forces others a, b)
      | QuadTree.Node(r,q11,q12,q21,q22,b) ->
        let params = others +/ q12.State +/ q21.State +/ q22.State, q11
        let q11' = traverse params
        let q12' = traverse (others +/ q11.State +/ q21.State +/ q22.State, q12)
        let q21' = traverse (others +/ q12.State +/ q11.State +/ q22.State, q21)
        let q22' = traverse (others +/ q12.State +/ q21.State +/ q11.State, q22)
        QuadTree.Node(r,q11',q12',q21',q22',b)

   (*
    The fast simulation takes as input a list of asteroids and returns as output the updated list.
    The steps are the following:
    - we create an empty tree
    - we insert each asteroid in the empty tree
    - we compute the barycenter at each node of the full tree
    - for each leaf, we compute the local interactions between the asteroids in the leaf plus the global interactions between these asteroids and the barycenters of the surrounding nodes
    *)
    let fast_simulation_step (asteroids:Asteroid list) =
      (*
      We create the empty tree; from testing, it results that having 8 * 8 leaves gives good results.
      *)
      let empty_tree = QuadTree.mk_empty { Min = Vec2<_>.Zero; Size = { X = field_size / 8.0; Y = field_size / 8.0 } }
                                         { Min = Vec2<_>.Zero; Size = { X = field_size; Y = field_size } }
      (*
      Starting from the empty_tree built above we fold over the list of asteroids and for each asteroid we insert the asteroid in the tree.
      *)
      let tree = asteroids |> List.fold (fun t a -> QuadTree.insert (fun (a:Asteroid) -> a.Position) a t) empty_tree
      (*
      We compute the barycenters of the tree nodes and leaves.
      *)
      let tree = tree |> QuadTree.fold (fun a b c d -> (a +/ b) +/ (c +/ d)) Barycenter.OfAsteroidList

      (*
      The final operation consists in traversing the tree, starting with an empty barycenter (null mass) and by converting the resulting tree into a list.
      *)
      (traverse ({ Position = Vec2<_>.Zero; Mass = 0.0<_> }, tree)).ToList


    (*
    We define an auxiliary printing function that only prints the number of simulation steps per second.
    The higher the printed number, the faster the simulation is going.
    Ideally, to maintain a fluid and smooth appearance the simulation should run at least at 30 frames per second, but 60 would be better.
    *)
    let s = Diagnostics.Stopwatch()
    let print_framerate (asteroids:Asteroid list) =
      do Console.Clear()
      let dt = s.Elapsed
      let dt = 1.0 / dt.TotalSeconds
      do Console.WriteLine(dt.ToString("0#.#"))

    (*
    We define a general function that performs a simulation with any step and any printing function, starting with the initial asteroid field.
    *)
    let base_simulation print_scene simulation_step =
      let rec simulation m =
        do print_scene m
        do s.Reset()
        do s.Start()
        let m' = simulation_step m
        do s.Stop()
        do simulation m'
      do simulation f0

    (*
    We define two functions for each simulation: the function that performs the simulation and that prints it on the console and another function that performs the simulation ut only prints the number of steps per seconds.
    The first function will be used to compare the two simulations to see if they both behave similarly and reasonably.
    The second function is the one we will use to see which simulation algorithm gives the best performance.

    All four functions are defined in terms of the base_simulation function.
    *)
    let slow_simulation() = base_simulation print_scene simulation_step
    let slow_simulation_framerate() = base_simulation print_framerate simulation_step

    let fast_simulation() = base_simulation print_scene fast_simulation_step
    let fast_simulation_framerate() = base_simulation print_framerate fast_simulation_step
