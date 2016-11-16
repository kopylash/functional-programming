(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------------------

  Coursework 7: Tail recursion and laziness

  ------------------------------------------------
  Name: Vladyslav Kopylash
  Student ID: vlkopy
  ------------------------------------------------


  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as file
  coursework7.fsx in directory coursework7.

  Please do not upload DLL-s. Just include a readme.txt file containing the
  dependencies required (additional DLLs)

  The deadline for completing the above procedure is Friday, November 18, 2016.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)

(*
  Task 1:

  Write a function maxInList : int list -> int that returns the maximum element
  in the given list. Make sure your implementation uses tail recursion.
*)

let maxInList (list : int list) : int =
 let rec worker acc xs =
    match xs with
      | []      -> acc
      | x :: xs -> worker (if x > acc then x else acc) xs
 worker list.Head list

maxInList [1..1000000]

(*
  Task 2:

  Write a function reverse :: 'a list -> 'a list that works like the function
  List.rev. Make sure your implementation uses tail recursion.
*)

let reverse (list : 'a list) : 'a list =
 let rec worker acc xs  =
    match xs with
      | []      -> acc
      | x :: xs -> worker (x::acc) xs
 worker [] list

reverse [1..100000]

(*
  Task 3:

  Below you find the definition of a type Tree of leaf-labeled trees. Write a
  function maxInTree : int Tree -> int that returns the maximum label in the
  given tree. Use continuation-passing style in your implementation.
*)

type 'a Tree =
  | Leaf   of 'a
  | Branch of 'a Tree * 'a Tree

let maxInTree (tree : int Tree) : int = 
  let rec worker tree cont =
    match tree with
      | Leaf a               -> cont a
      | Branch (left, right) -> worker left  (fun leftMax  ->
                                worker right (fun rightMax ->
                                cont (if leftMax > rightMax then leftMax else rightMax)))
  worker tree id

let testTree = Branch(Branch(Branch(Leaf 5, Leaf 22), Leaf 30),Branch(Leaf 10, Leaf 8))

maxInTree testTree

(*
  Task 4:

  Write a function maxInTree' : int Tree -> int that returns the maximum label
  in the given tree, like the function maxInTree from Task 3 does. Use
  continuation-passing style in combination with accumulation in your
  implementation.
*)

let maxInTree' (tree : int Tree) : int = 
  let rec worker acc tree cont =
    match tree with
      | Leaf a               -> cont (if acc > a then acc else a)
      | Branch (left, right) -> worker acc left (fun acc -> 
                                worker acc right cont)
  worker -100000000 tree id

maxInTree' testTree

(*
  Task 5:

  The function streamMap : ('a -> 'b) -> 'a Stream -> 'b Stream from the lecture
  is the stream analog of the function List.map. Write a function streamFilter :
  ('a -> bool) -> 'a Stream -> 'a Stream that is the stream analog of the
  function List.filter.
*)

type 'a Stream =
  | Stream of 'a * Lazy<'a Stream>

let rec streamFilter predicate xs =
  match xs with
    | Stream (x, lxs) -> if predicate x then Stream (x, lazy streamFilter predicate lxs.Value) else streamFilter predicate lxs.Value

//to check
let streamTail xs =
  match xs with
    | Stream (_, lxs) -> lxs.Value

let rec from n = Stream (n, lazy from (n + 1))

let filtered = streamFilter (fun x -> x % 2 = 0) (from 1)

streamTail filtered