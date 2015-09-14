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
      { X = b.X + dx
        Y = b.Y + dy
        DX= dx
        DY= dy
      }

    let print_scene (x,y) =
      do Console.Clear()
      let x,y = int x, int y
      for j = 10 downto 0 do
        for i = 0 to 30 do
          if (y+1) = j && i = (x+1) then
            Console.Write("b")
          elif j = 0 || i = 0 || j = 10 || i = 30 then
            Console.Write("*")
          else
            Console.Write(" ")
        Console.Write("\n")
      ignore(Console.ReadKey())

    let rec simulate_ball (bs:ball List) :ball List=
      let f (b:ball) :ball = 
        let bb = b |> simulate_ball_step
        do print_scene (bb.X,bb.Y)
        bb
      bs |> List.map f |> simulate_ball
