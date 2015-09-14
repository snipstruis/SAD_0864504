[<EntryPoint>]
let main argv = 
  //do Chapter1.BallSimulation.simulation()
  let bs :Chapter1.ball List = [{X=  5.0; Y= 8.0; DX=  1.0; DY= 0.0;}
                                {X=  4.0; Y= 5.0; DX=  0.5; DY= 0.0;}
                                {X=  3.0; Y= 2.0; DX= -0.5; DY= 0.0;}
                                {X=  1.0; Y= 3.0; DX= -1.5; DY= 0.0;}
                                {X=  5.0; Y= 6.0; DX=  0.5; DY= 0.0;}]
  do Chapter1.BallSimulation.simulate_ball bs |> ignore
  //do Chapter1.BallSimulation.print_scene (3.0,3.0)  |> ignore
  0