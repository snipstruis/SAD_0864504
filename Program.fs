
let sim() =
   let bs :Chapter1.ball List = [{X=  5.0; Y= 8.0; DX=  1.0; DY= 0.0;}
                                 {X=  4.0; Y= 5.0; DX=  0.5; DY= 0.0;}
                                 {X=  3.0; Y= 2.0; DX= -0.5; DY= 0.0;}
                                 {X=  1.0; Y= 3.0; DX= -1.5; DY= 0.0;}
                                 {X=  5.0; Y= 6.0; DX=  0.5; DY= 0.0;}]
   do Chapter1.BallSimulation.simulate_ball bs |> ignore
  

[<EntryPoint>]
let main argv = 
  //do sim()
  //do Chapter2.RocketSimulation.simulation()
  //do Chapter3.SmallAsteroidFieldSimulation.simulation()
  //do Chapter4.LargeAsteroidFieldSimulation.fast_simulation()
  do Chapter5.PoliceChase.simulation()
  0
