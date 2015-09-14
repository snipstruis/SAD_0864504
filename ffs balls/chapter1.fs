namespace Chapter1
  type ball = {X:float; Y:float; DX:float; DY:float}
  module BallSimulation =
    open System
    let dt = 0.9
    let g = -0.981
    let max_x = 30.0
    let max_y = 10.0
    
    let simulate_ball_step (b:ball) :ball =
      let dx = 
        if (b.X+b.DX > 15.0) || (b.X+b.DX < -15.0)
        then dt*(-b.DX) else b.DX
      let dy = 
        let foo = b.Y+b.DY+g 
        if (b.Y+b.DY+g > 30.0) || (b.Y+b.DY+g < 0.0)
        then dt*(-b.DY)+g else b.DY+g
      let x = b.X + dx
      let y = b.Y + dy
      { X = x
        Y = y
        DX= dx
        DY= dy
      }

    let print_scene (balls:ball List) =
      let high (x:int) (y:int) :(ball->bool) = function (b:ball) -> (int b.X,int b.Y) = (x,y)
      do Console.Clear()
      for j = 10 downto 0 do
        for i = 0 to 30 do
          if j = 0 || i = 0 || j = 10 || i = 30 then
            Console.Write("*")
          else
            match List.tryFind (high (i-1) (j-1)) balls with
            | None -> Console.Write(" ")
            | _    -> Console.Write("b")
        Console.Write("\n")
      ignore(Console.ReadKey())
      balls

    let rec simulate_ball (bs:ball List) :ball List=
      bs |> List.map simulate_ball_step |> print_scene |> simulate_ball
