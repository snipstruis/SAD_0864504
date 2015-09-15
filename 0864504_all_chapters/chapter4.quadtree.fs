(*
In this chapter we will create a large asteroid range.

If we try and put a relatively small number of asteroids (about 200) our simulation will become noticeably slower.
And yet a modern PC should have the computing capabilities for a field of 2 million asteroids in real time, let alone 200.
Why is the previous simulation slow?
Let us consider the central portion of the simulation, the simulation-step:
for a in asteroids do
  let forces =
       [
         for a' in asteroids do
           if a' <> a then
             yield force(a,a')
       ]
  ...

In the above snippet, we see that for each asteroid we check all other asteroids, for a grand total of n * n computations where n is the number of asteroids.
For 200 asteroids we are talking about 200 * 200 computations, that is 40000 applications of the gravitational force.
40000 is quite a large number for mathematical computations, and indeed the number of times per second we can afford to perform this computation is not very high.
Also, let us say that we get a computer so fast that its capable of performing 40000 computations in half the time as before: how many asteroids do we support now?
Since we can now perform 80000 computations in the time it took 40000 with out previous computer, then we support x asteroids such that x * x = 80000; x = 282.

So, for a computer that is twice as fast as before, we now get less than half the asteroids more: quite disappointing!

We will now build an algorithmic optimization that takes advantage of a very important fact:
given an asteroid in the asteroid field, the gravitational force coming to it from a cluster of *distant* asteroids can be approximated with great precision with the force coming from the barycenter of the cluster.
If we could partition our asteroid field into many barycenters that represent the average distribution of asteroids in that area and use this data to speed up force computations, we might achieve a much faster simulation.

We will divide our space with a quad tree.
We start with the entire field. This is the root of our tree.
We then divide the field into four sub-fields; these are the children of the root.
We proceed until we reach nodes that are small enough to our liking: those are the leaves and they contain asteorids (our liking is the best performance increase, so we will experiment a bit to find the right size for the leaves).
*)
namespace Chapter4
  (*
  Our first step is to define a quadtree.
  *)
  module QuadTree =
    open Chapter2.Math

    (*
    A range (with unit of measure 'u) is comprised of two vectors: its offset and its size.
    The corners of a range are:
    - Min.X, Min.Y
    - Min.X + Size.X, Min.Y
    - Min.X, Min.Y + Size.Y
    - Min.X + Size.X, Min.Y + Size.Y
    *)
    type Range<[<Measure>] 'u> =
      {
        Min  : Vec2<'u>
        Size : Vec2<'u>
      }
      (*
      A range also has a method for determining it a vector (with unit of measure 'u, same as the range) is inside the range:
      *)
      member this.IsIn v =
        let d = v - this.Min
        v.X >= this.Min.X && v.Y >= this.Min.Y && d.X <= this.Size.X && d.Y <= this.Size.Y


    (*
    A QuadTree stores values of two kinds: 'a and 'b, and uses ranges with unit of measure 'u.
    'a is the type of values contained in the leaves. Each leaf contains a list of values of type 'a.
    'b is the type of values contained in the nodes and leaves. Each node and each leaf has at most one value of type 'b.
    Leaves and nodes all have a range, which determines how large the covered area is.

    In our example, we will define:
    - 'a = Asteroid
    - 'b = Barycenter
    - 'u = meters
    *)
    type QuadTree<'a, 'b, [<Measure>] 'u> =
        Leaf of Range<'u> * List<'a> * Option<'b>
      | Node of Range<'u> * QuadTree<'a,'b,'u> * QuadTree<'a,'b,'u> *  QuadTree<'a,'b,'u> * QuadTree<'a,'b,'u> * Option<'b>
      with
        (*
        We define a few members that make it easier to extract common information from nodes and leaves without pattern matching.
        We define a member that gets the range of a leaf or node, another that gets the current state (assuming, possibly dangerously, that it is not None) and another that returns the list of elements of the tree.
        *)
        member this.Range =
          match this with
          | Leaf(r,_,_) -> r
          | Node(r,_,_,_,_,_) -> r
        member this.State =
          match this with
          | Leaf(_,_,Some s) -> s
          | Node(_,_,_,_,_,Some s) -> s
          | _ -> failwith "Null state"
        member this.ToList =
          match this with
          | Leaf(_,l,_) -> l
          | Node(_,a,b,c,d,_) -> a.ToList @ b.ToList @ c.ToList @ d.ToList

    (*
    Given a minimum desired range and a current range we can build a tree that covers the current range and which has leaves no larger than the minimum desired range.
    If the current range is smaller than the minimum, then we create an empty leaf (no elements and no barycenter).
    If the current range is larger than the minimum, then we create a node which children are computed by creating empty trees in the four subregion of the current range.
    *)
    let rec mk_empty (min_range:Range<_>) (range:Range<_>) =
      if min_range.Size.X < range.Size.X ||
         min_range.Size.Y < range.Size.Y then
         let size' = range.Size / 2.0
         let range11 = { Min = range.Min; Size = size' }
         let range12 = { Min = range.Min + { size' with Y = 0.0<_> }; Size = size' }
         let range21 = { Min = range.Min + { size' with X = 0.0<_> }; Size = size' }
         let range22 = { Min = range.Min + size'; Size = size' }
         Node(range,
              mk_empty min_range range11,
              mk_empty min_range range12,
              mk_empty min_range range21,
              mk_empty min_range range22,
              None)
       else
         Leaf(range,[],None)

    (*
    To insert an element a (of generic type 'a) into a tree we need a bit of information more, that is the position of a so that we can se which ranges it belongs to.
    Rather than getting the position of a as another parameter, we can request a function that given a computes, finds or looks up its position. This function is the first parameter.

    We check the tree to see if we have reached a leaf; in this case, we just add the element a to the list of elements of the leaf.
    If we have reached a node, we see in which of the sub-ranges our element a fits and insert it there.
    *)
    let rec insert position a =
      function
      | Leaf(range,l,s) -> Leaf(range,a :: l,s)
      | Node(r,n11,n12,n21,n22,s) ->
        let n11',n12',n21',n22' =
          if n11.Range.IsIn (position a) then
            insert position a n11, n12, n21, n22
          elif n12.Range.IsIn (position a) then
            n11, insert position a n12, n21, n22
          elif n21.Range.IsIn (position a) then
            n11, n12, insert position a n21, n22
          else
            n11, n12, n21, insert position a n22
        Node(r,n11',n12',n21',n22',s)

    (*
    Up until now we have not manipulated the values of type 'b inside our nodes.
    These values represent the state of folding the tree towards the root; we compute an initial (zero) value from the 'a list inside each leaf, and then we compute the 'b value of each node from the four 'b values of its children.

    The z function computes a leaf 'b. The f function condenses four 'b into one.

    We can use the fold function to compute the sum, average, concatenation, etc. of all the values contained in the leaves up until the root of the tree:
    *)
    let rec fold (f:'b->'b->'b->'b->'b) (z:'a list -> 'b) =
      function
      | Leaf(range,l,_) -> Leaf(range,l,Some(z l))
      | Node(r,n11,n12,n21,n22,_) ->
        let n11,n12,n21,n22 = fold f z n11, fold f z n12, fold f z n21, fold f z n22
        Node(r,n11,n12,n21,n22,Some(f n11.State n12.State n21.State n22.State))
