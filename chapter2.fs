namespace Chapter2
module RocketSimulation =
  open System
  open System.Threading
  open Chapter2.Math

  type PhysicalEntity = {
    Position : Vec2<m>
    Mass     : float<kg>
  }
  type CelestialBody = {
    Body     : PhysicalEntity
    Name     : string
  }
  type Stage = {
    DryMass  : float<kg>
    Fuel     : float<kg>
    FuelBurn : float<kg/s>
    Thrust   : float<N>
  } with member this.Mass = this.DryMass + this.Fuel
  type Rocket = {
    Body     : PhysicalEntity
    BaseMass : float<kg>
    Velocity : Vec2<m/s>
    Stage1   : Option<Stage>
    Stage2   : Option<Stage>
    Stage3   : Option<Stage>
  }
  type ApolloMission = {
    Earth    : CelestialBody
    Moon     : CelestialBody
    Rocket   : Rocket
  }

  let earth_radius = 6.37e6<m>
  let dt = 60.0<s>
  let G = 6.67e-11<m^3 * kg^-1 * s^-2>

  let m0 = {
    Earth= { Name = "E"; Body = { Position = { X = 0.0<m>; Y = 0.0<m> }; Mass = 5.97e24<kg> }}
    Moon = { Name = "M"; Body = { Position = { X = earth_radius * 60.0; Y = 0.0<m> }; Mass = 7.35e22<kg> }}
    Rocket =
      let stage1 = {
        DryMass     = 1.31e5<kg>
        Fuel        = 2.17e6<kg>
        FuelBurn    = 8.25e3<kg/s>
        Thrust      = 3.4e7<N> * 5.0
      }
      let stage2 = {
        DryMass     = 3.6e4<kg>
        Fuel        = 4.4e5<kg>
        FuelBurn    = 1.05e3<kg/s>
        Thrust      = 4.4e6<N> * 5.0
      }
      let stage3 = {
        DryMass     = 1.1e4<kg>
        Fuel        = 1.09e5<kg>
        FuelBurn    = 2.59e2<kg/s>
        Thrust      = 1.0e6<N>
      }
      let base_mass = 4.5e4<kg>
      {
         Body     = { Position = { X = earth_radius; Y = 11.0<m> }; Mass = base_mass + stage1.Mass + stage2.Mass + stage3.Mass }
         BaseMass = base_mass
         Velocity = Vec2<m/s>.Zero
         Stage1   = Some(stage1)
         Stage2   = Some(stage2)
         Stage3   = Some(stage3)
      }
  }

  let simulation_step (m:ApolloMission) =
    let r = m.Rocket

    let F_body (b:CelestialBody) =
      let dir = b.Body.Position - r.Body.Position
      let dist = dir.Length + 1.0<m>
      G * b.Body.Mass * r.Body.Mass * dir / (dist * dist * dist)

    let F_engine,r =
      let stage_step s =
        if Console.KeyAvailable then
          Console.ReadKey() |> ignore
          None
        elif s.Fuel <= s.FuelBurn * dt then
          Console.Beep()
          None
        else
          Some({ s with Fuel = s.Fuel - s.FuelBurn * dt })

      let dir = Vec2<_>.Normalize(m.Moon.Body.Position - r.Body.Position)

      match r.Stage1,r.Stage2,r.Stage3 with
      | Some s,_,_ -> dir * s.Thrust, { r with Stage1 = stage_step s }
      | _,Some s,_ -> dir * s.Thrust, { r with Stage2 = stage_step s }
      | _,_,Some s -> dir * s.Thrust, { r with Stage3 = stage_step s }
      | _ -> Vec2<N>.Zero, r

    let F =
      let F_earth = (F_body m.Earth)
      let F_moon = F_body m.Moon
      F_earth + F_moon + F_engine

    let r =
        let stage_mass =
          function
          | None -> 0.0<_>
          | Some s -> s.DryMass + s.Fuel
        { r with Body =
                   { r.Body
                     with Position = 
                            let p = r.Body.Position + r.Velocity * dt 
                            { p with X = max (p.X) earth_radius };
                          Mass = 
                            r.BaseMass + stage_mass r.Stage1 + stage_mass r.Stage2 + stage_mass r.Stage3 };
                 Velocity = r.Velocity + (F / r.Body.Mass) * dt }
    { m with Rocket = r }


  let print_scene (m:ApolloMission) =
    do Console.Clear()
    for i = 0 to 79 do
      Console.SetCursorPosition(i, 0)
      Console.Write("*")
      Console.SetCursorPosition(i, 23)
      Console.Write("*")
    for j = 0 to 23 do
      Console.SetCursorPosition(0,j)
      Console.Write("*")
      Console.SetCursorPosition(79,j)
      Console.Write("*")
    let set_cursor_on_body b =
      Console.SetCursorPosition(((b.Position.X / 4.0e8<m>) * 78.0 + 1.0) |> int, (b.Position.X / 4.0e8<m> + 11.0) |> int)
    do set_cursor_on_body m.Earth.Body
    do Console.Write(m.Earth.Name)
    do set_cursor_on_body m.Moon.Body
    do Console.Write(m.Moon.Name)
    do set_cursor_on_body m.Rocket.Body
    do Console.Write("R")
    do Thread.Sleep(100)

  let simulation() =
    let rec simulation m =
      do print_scene m
      let m' = simulation_step m
      if Vec2<_>.Distance(m'.Moon.Body.Position,m'.Rocket.Body.Position) > 1.7e6<m> then
        do simulation m'
    do simulation m0
