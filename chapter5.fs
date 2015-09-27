namespace Chapter5
  module PoliceChase =

    (*
    We import the usual libraries, plus our definition of coroutines.
    *)
    open System
    open System.Threading
    open Chapter2.Math
    open Chapter5.Coroutines

    (*
    We define a unit of measure for the ships integrity, called Life:
    *)
    [<Measure>]
    type Life

    (*
    A Ship is defined in terms of:
    - position, velocity and dry (empty) mass
    - fuel, max fuel capacity, engine thrust and speed of fuel burn when engine is firing
    - current forces the ship is subject to
    - integrity and maximum integrity of the ship
    - damage and range of the ship's weapons
    - current state of the AI
    *)
    type Ship =
      {
        mutable Position      : Vec2<m>
        mutable Velocity      : Vec2<m/s>
        DryMass               : float<kg>
        mutable Fuel          : float<kg>
        MaxFuel               : float<kg>
        Thrust                : float<N/s>
        FuelBurn              : float<kg/s>
        mutable Force         : Vec2<N>
        mutable Integrity     : float<Life>
        MaxIntegrity          : float<Life>
        Damage                : float<Life/s>
        WeaponsRange          : float<m>
        mutable AI            : Coroutine<Unit>
      }
      (*
      We also define a method that computes the current mass of the ship as the sum of the dry mass of the ship and the amount of fuel it contains:
      *)
      member this.Mass = this.DryMass + this.Fuel

    (*
    The police station only has contains its position:
    *)
    type Station =
      {
        Position      : Vec2<m>
      }

    (*
    The state of the simulation is comprised of:
    - the police station
    - the police ship
    - the pirate ship
    - the cargo ship
    *)
    type PoliceChase =
      {
        PoliceStation : Station
        Patrol        : Ship
        Pirate        : Ship
        Cargo         : Ship
      }

    (*
    The simulation uses two constants: the delta time of the simulation and the size of the playing field: 
    *)
    let dt = 180.0<s>
    let field_size = 3.8e7<m>

    (*
    An engine impulse on a ship in a certain direction checks to see if there is enough fuel left in the tank;
    if so, then it adds to the current forces of the ship the engine thrust and removes some fuel form the tank:
    *)
    let impulse (self:Ship) (dir:Vec2<1>) (engine_power:float) =
      if self.Fuel > self.FuelBurn * engine_power * dt then
        do self.Force <- self.Thrust * dir * engine_power * dt
        do self.Fuel <- self.Fuel - self.FuelBurn * engine_power * dt

    (*
    An attack AI of a ship against another ship keeps the ship in range of the target, since it:
    - suspends itself for one simulation step
    - if it is not in weapons range, then it pushes the engines towards the target and it waits for a second before activating the engines again (to avoid using too much fuel)
    The various engine impulses use the dot product between the current ship velocity and the desired velocity to determine if the ship is moving in the right direction,
    to decide if it is better to accelerate or to brake.
    *)
    let attack (self:Ship) (target:Ship) =
      co{
        do! yield_
        let dir = Vec2<_>.Normalize(target.Position - self.Position)
        let dist = (target.Position - self.Position).Length
        if dist > self.WeaponsRange * 0.8 then
          if self.Velocity.Length > 0.01<_> then
            let v_norm = self.Velocity.Normalized
            let dot = Vec2.Dot(dir,v_norm)
            if dot <= 0.0 then
              do impulse self (-self.Velocity.Normalized) 1.0
            elif dot < 0.5 then
              do impulse self (Vec2<_>.Normalize((-(self.Velocity.Normalized - dir * dot)))) 0.3
            else
              do impulse self dir 0.1
            do! wait 1.0
          else
            do impulse self dir 1.0
            do! wait 1.0
        return ()
      }

    (*
    A useful AI coroutine takes a ship to the police station, by using the engines to maneuver close to the station;
    when the ship is close enough to the station, then it waits still for 5 seconds and it gets refueled:
    *)
    let reach_station (self:Ship) (s:PoliceChase) =
      co{
        do! yield_
        let dir = Vec2<_>.Normalize(s.PoliceStation.Position - self.Position)
        if Vec2<_>.Distance(s.PoliceStation.Position, self.Position) <= field_size * 1.0e-1 then
          let zero_velocity =
            co{
              do! yield_
              return self.Velocity <- Vec2<_>.Zero
            }
          do! wait_doing (fun _ -> zero_velocity) 5.0
          do self.Integrity <- self.MaxIntegrity
          do self.Fuel <- self.MaxFuel
        elif self.Velocity.Length > 0.01<_> then
          let dot = Vec2<1>.Dot(self.Velocity.Normalized,dir)
          if dot <= 0.0 then
            do impulse self (-self.Velocity.Normalized) 1.0
          elif dot < 0.5 then
            do impulse self (Vec2<_>.Normalize((-(self.Velocity.Normalized - dir * dot)))) 0.3
          else
            do impulse self dir 0.2
          do! wait 1.0
        else
          do impulse self dir 1.0
          do! wait 1.0
        return ()
      }

    (*
    The police patrol AI continuously repeats the following choice:
    - if the ship is healthy and fueled, then it attacks the pirates
    - if the ship is not healthy and fueled, then it goes back to the station for assistance
    *)
    let patrol_ai (s:PoliceChase) =
      let self = s.Patrol
      let healthy_and_fueled =
        co{
          do! yield_
          return self.Integrity > self.MaxIntegrity * 0.4 && self.Fuel > self.MaxFuel * 0.4
        }
      let need_docking =
        co{
          do! yield_
          let! h = healthy_and_fueled
          return not h
        }
      repeat_ ((healthy_and_fueled => attack self (s.Pirate)) .||>
               (need_docking       => reach_station self s))

    (*
    The pirate AI, similarly to the police AI, decides between two behaviors:
    - if the police is too close, it attacks it
    - if the police is not too close, it attacks the cargo ship
    *)
    let pirate_ai (s:PoliceChase) =
      let self = s.Pirate
      let patrol_near =
        co{
          do! yield_
          return Vec2<_>.Distance(self.Position,s.Patrol.Position) < s.Patrol.WeaponsRange
        }
      let patrol_far =
        co{
          let! n = patrol_near
          return not n
        }
      repeat_ ((patrol_near => (attack (s.Pirate) (s.Patrol))) .||>
               (patrol_far  => (attack (s.Pirate) (s.Cargo))))

    (*
    The cargo ship keeps going towards the police station:
    *)
    let cargo_ai (s:PoliceChase) =
      let self = s.Cargo
      co{
        do! yield_
        do! reach_station self s
      } |> repeat_

    (*
    The initial state positions the various ships and the police station; 
    the police station and the patrol ship start close by, the cargo frigate starts far from the station while the pirate ship starts even further:
    *)
    let s0() =
      let s =
        {
          PoliceStation = { Position = { X = field_size; Y = field_size } * 0.25 }
          Patrol        =
            {
              Position        = { X = field_size; Y = field_size } * 0.25
              Velocity        = Vec2<_>.Zero
              DryMass         = 4.5e4<_>
              Fuel            = 2.2e6<_>
              MaxFuel         = 2.2e6<_>
              FuelBurn        = 2.2e6<_> / (50.0 * 180.0)
              Thrust          = 5.0e6<_> / 180.0
              Force           = Vec2<_>.Zero
              Integrity       = 100.0<_>
              MaxIntegrity    = 100.0<_>
              Damage          = 1.0e-1<_> / 180.0
              WeaponsRange    = field_size * 0.1
              AI              = co{ return () }
            }
          Pirate        =
            {
              Position        = { X = field_size; Y = field_size } * 0.75
              Velocity        = Vec2<_>.Zero
              DryMass         = 3.0e4<_>
              Fuel            = 2.2e6<_>
              MaxFuel         = 2.2e6<_>
              FuelBurn        = 2.2e6<_> / (30.0 * 180.0)
              Thrust          = 5.0e5<_> / 180.0
              Force           = Vec2<_>.Zero
              Integrity       = 75.0<_>
              MaxIntegrity    = 75.0<_>
              Damage          = 2.0e-1<_> / 180.0
              WeaponsRange    = field_size * 0.15
              AI              = co{ return () }
            }
          Cargo        =
            {
              Position        = { X = field_size; Y = field_size  * 0.7 } * 0.7
              Velocity        = Vec2<_>.Zero
              DryMass         = 2.3e6<_>
              Fuel            = 3.5e8<_> * 0.3
              MaxFuel         = 3.5e8<_>
              FuelBurn        = 3.5e6<_> / 180.0
              Thrust          = 3.4e6<_> / 180.0
              Force           = Vec2<_>.Zero
              Integrity       = 300.0<_>
              MaxIntegrity    = 300.0<_>
              Damage          = 1.0e-3<_> / 180.0
              WeaponsRange    = field_size * 0.1
              AI              = co{ return () }
            }
        }
      (*
      Each ship starts with the appropriate AI:
      *)
      do s.Patrol.AI <- patrol_ai s
      do s.Pirate.AI <- pirate_ai s
      do s.Cargo.AI  <- cargo_ai s
      s

    (*
    A coroutine step simply checks:
    - if the coroutine is completed then it returns a fake coroutine that propagates the same result again
    - if the coroutine is suspended then it returns the suspension
    *)
    let co_step =
      function
      | Return x          -> co{ return x }
      | Yield k           -> k
      | ArrowYield k      -> k

    (*
    An update of a ship updates its position, its velocity and zeroes its current force; then it updates the ship's AI.
    Notice that the ship is updated in place, thanks to its mutability.
    This way coroutines may keep a reference to the current state, without having to continuously access the new state.
    This considerably eases state accesses, but it would make it harder to make the application concurrent.
    *)
    let ship_step (s:Ship) =
        do s.Position <- s.Position + s.Velocity * dt
        do s.Velocity <- s.Velocity + dt * s.Force / s.Mass
        do s.Force    <- Vec2<_>.Zero
        do s.AI       <- co_step (s.AI())

    (*
    A step of the entire simulation updates the various ships, and then adds the damage between the various weapons when they are in range:
    *)
    let simulation_step (s:PoliceChase) =
      do ship_step s.Patrol
      do ship_step s.Pirate
      do ship_step s.Cargo
      if Vec2<_>.Distance(s.Patrol.Position, s.Pirate.Position) < s.Patrol.WeaponsRange then
        do s.Pirate.Integrity <- s.Pirate.Integrity - s.Patrol.Damage * dt
      if Vec2<_>.Distance(s.Cargo.Position, s.Pirate.Position) < s.Cargo.WeaponsRange then
        do s.Pirate.Integrity <- s.Pirate.Integrity - s.Cargo.Damage * dt
      if Vec2<_>.Distance(s.Patrol.Position, s.Pirate.Position) < s.Pirate.WeaponsRange then
        do s.Patrol.Integrity <- s.Patrol.Integrity - s.Pirate.Damage * dt
      elif Vec2<_>.Distance(s.Cargo.Position, s.Pirate.Position) < s.Pirate.WeaponsRange then
        do s.Cargo.Integrity <- s.Cargo.Integrity - s.Pirate.Damage * dt

    (*
    The printing and simulation function are essentially the same we have seen in the previous chapters;
    the simulation ends when one of the ships is destroyed:
    *)
    let print(s:PoliceChase) =
      do Console.Clear()
      let set_cursor (v:Vec2<_>) =
        Console.SetCursorPosition((((v.X / field_size) * 79.0) |> int) - 1 |> max 0 |> min 79, ((v.Y / field_size) * 23.0) |> int |> max 0 |> min 23)
      let set_cursor_on_ship (s:Ship) = set_cursor (s.Position)
      let set_cursor_on_station (s:Station) = set_cursor (s.Position)
      do set_cursor_on_station (s.PoliceStation)
      do Console.Write("¤")
      do set_cursor_on_ship (s.Patrol)
      let ship_fuel (s:Ship) =
        (9.0 * s.Fuel / s.MaxFuel).ToString("#.")
      let ship_integrity (s:Ship) =
        (9.0 * s.Integrity / s.MaxIntegrity).ToString("#.")
      do Console.Write((ship_fuel s.Patrol) + "∆" + (ship_integrity s.Patrol))
      do set_cursor_on_ship (s.Pirate)
      do Console.Write((ship_fuel s.Pirate) + "†" + (ship_integrity s.Pirate))
      do set_cursor_on_ship (s.Cargo)
      do Console.Write((ship_fuel s.Cargo) + "•" + (ship_integrity s.Cargo))
      do Console.SetCursorPosition(0,0)
      do Thread.Sleep(10)

    let simulation() =
      let s = s0()
      let rec simulation() =
        do print s
        if s.Patrol.Integrity > 0.0<_> && s.Pirate.Integrity > 0.0<_> && s.Cargo.Integrity > 0.0<_> then
          do simulation (simulation_step s)
      do simulation()
