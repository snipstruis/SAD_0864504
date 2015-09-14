namespace Chapter1
    module BallSimulation =
        open System
        let dt = 0.1
        let g = -9.81
        
        let simulation_step (y,v) =
            let y',v' = (y+v*dt, v+g*dt)
            if y' < 0.0 then
                (0.0, -v'*0.7)
            else
                (y',v')
        
        let print_scene (y,v) =
            do Console.Clear()
            let y,v = int y, int v
            for j = 10 downto 0 do
                for i = 0 to 30 do
                    if (y+1) = j && i = 15 then
                        Console.Write("b")
                    elif j = 0 || i = 0 || j = 10 || i = 30 then
                        Console.Write("*")
                    else
                        Console.Write(" ")
                Console.Write("\n")
            ignore(Console.ReadKey())

        let simulation () =
            let rec simulation (y,v) =
                do print_scene (y,v)
                let y',v' = simulation_step (y,v)
                if abs v' > 0.1 then
                    do simulation (y',v')
            do simulation (5.0,-2.0)