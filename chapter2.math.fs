﻿namespace Chapter2
module Math =
  [<Measure>]
  type m
  [<Measure>]
  type kg
  [<Measure>]
  type s
  [<Measure>]
  type N = kg*m/s^2

  type Vec2<[<Measure>] 'a> = { X:float<'a>; Y:float<'a> }
    with
      static member Zero : Vec2<'a> = { X = 0.0<_>; Y = 0.0<_>; }
      static member (+)   (v1:Vec2<'a>, v2:Vec2<'a>) :Vec2<'a> = { X = v1.X+v2.X; Y = v1.Y+v2.Y }
      static member (+)   (v:Vec2<'a>, k:float<'a>) :Vec2<'a> = { X = v.X+k; Y = v.Y+k }
      static member (+)   (k:float<'a>, v:Vec2<'a>) :Vec2<'a> = v+k
      static member (~-)  (v:Vec2<'a>) :Vec2<'a> = { X = -v.X; Y = -v.Y }
      static member (-)   (v1:Vec2<'a>,v2:Vec2<'a>) :Vec2<'a> = v1+(-v2)
      static member (-)   (v:Vec2<'a>,k:float<'a>) :Vec2<'a> = v+(-k)
      static member (-)   (k:float<'a>,v:Vec2<'a>) :Vec2<'a> = k+(-v)
      static member ( * ) (v1:Vec2<'a>,v2:Vec2<'b>):Vec2<'a*'b> = { X = v1.X*v2.X; Y = v1.Y*v2.Y }
      static member ( * ) (v:Vec2<'a>,f:float<'b>):Vec2<'a*'b> = { X = v.X*f; Y = v.Y*f }
      static member ( * ) (f:float<'b>,v:Vec2<'a>):Vec2<'b*'a> = { X = f*v.X; Y = f*v.Y }
      static member (/)   (v:Vec2<'a>,f:float<'b>):Vec2<'a/'b> = v*(1.0/f)
      member this.Length : float<'a> = sqrt((this.X*this.X+this.Y*this.Y))
      static member Distance (v1:Vec2<'a>,v2:Vec2<'a>) = (v1-v2).Length
      static member Normalize (v:Vec2<'a>):Vec2<1> = v/v.Length
      member this.Normalized = this / this.Length
      static member Dot(v1:Vec2<'a>,v2:Vec2<'a>) = v1.X * v2.X + v1.Y * v2.Y

(* WHY DOES VEC2 BREAK WHEN I UNCOMMENT THIS ???
  type Vec3<[<Measure>] 'a> = { X:float<'a>; Y:float<'a>; Z:float<'a> }
    with
      static member Zero : Vec3<'a> = { X = 0.0<_>; Y = 0.0<_>; Z = 0.0<_>; }
      static member (+)   (v1:Vec3<'a>, v2:Vec3<'a>) :Vec3<'a> = { X = v1.X+v2.X; Y = v1.Y+v2.Y; Z=v1.Z+v2.Z }
      static member (+)   (v:Vec3<'a>, k:float<'a>) :Vec3<'a> = { X = v.X+k; Y = v.Y+k; Z = v.Z+k }
      static member (+)   (k:float<'a>, v:Vec3<'a>) :Vec3<'a> = v+k
      static member (~-)  (v:Vec3<'a>) :Vec3<'a> = { X = -v.X; Y = -v.Y; Z = -v.Z }
      static member (-)   (v1:Vec3<'a>,v2:Vec3<'a>) :Vec3<'a> = v1+(-v2)
      static member (-)   (v:Vec3<'a>,k:float<'a>) :Vec3<'a> = v+(-k)
      static member (-)   (k:float<'a>,v:Vec3<'a>) :Vec3<'a> = k+(-v)
      static member ( * ) (v1:Vec3<'a>,v2:Vec3<'b>):Vec3<'a*'b> = { X = v1.X*v2.X; Y = v1.Y*v2.Y; Z = v1.Z*v2.Z }
      static member ( * ) (v:Vec3<'a>,f:float<'b>):Vec3<'a*'b> = { X = v.X*f; Y = v.Y*f; Z=v.Z*f }
      static member ( * ) (f:float<'b>,v:Vec3<'a>):Vec3<'b*'a> = { X = f*v.X; Y = f*v.Y; Z=f*v.Z }
      static member (/)   (v:Vec3<'a>,f:float<'b>):Vec3<'a/'b> = v*(1.0/f)
      member this.Length : float<'a> = sqrt((this.X*this.X+this.Y*this.Y+this.Z*this.Z))
      static member Distance (v1:Vec3<'a>,v2:Vec3<'a>) = (v1-v2).Length
      static member Normalize (v:Vec3<'a>):Vec3<1> = v/v.Length
      member this.Normalized = this / this.Length
      static member Dot(v1:Vec3<'a>,v2:Vec3<'a>) = v1.X*v2.X + v1.Y*v2.Y + v1.Z*v2.Z
*)
