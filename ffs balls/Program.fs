[<EntryPoint>]
let main argv = 
  //do Chapter1.BallSimulation.simulation()
  let bs :Chapter1.ball List = [{X= 5.0; Y= 8.0; DX= 0.0; DY= 0.0;}]
  do Chapter1.BallSimulation.simulate_ball bs |> ignore
  //do Chapter1.BallSimulation.print_scene (3.0,3.0)  |> ignore
  0