(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 3: User defined types

  ------------------------------------
  Name: Vladyslav Kopylash
  TUT Student ID: vlkopy
  ------------------------------------


  Answer the questions below.  You answers to questions 1--7 should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the TUT
  git system using the instructions on the course web page by October 9, 2015.
*)

// 1. Consider expression trees of type ExprTree declared in the lecture.
// Extend the type with if-then-else expression of the form:
// if b then e1 else e2
// where b is a boolean expression and e1 and e2 are expressions.
// An example expression could be:
// if a+3 > b+c && a>0 then c+d else e

type ExprTree = | Const  of int
                | Ident of string
                | Minus  of ExprTree
                | Sum    of ExprTree * ExprTree
                | Diff   of ExprTree * ExprTree
                | Prod   of ExprTree * ExprTree
                | Let    of string * ExprTree * ExprTree
                | IfThenElse   of LogicalExprTree * ExprTree * ExprTree
                | Match  of ExprTree * (int * ExprTree) list 

and LogicalExprTree = | And of LogicalExprTree * LogicalExprTree
                      | Or of LogicalExprTree * LogicalExprTree
                      | LessThan of ExprTree * ExprTree
                      | GreaterThan of ExprTree * ExprTree
                      | Equal of ExprTree * ExprTree
                      | Not of LogicalExprTree

let IfExpression = IfThenElse(And(GreaterThan (Sum(Ident "a", Const 3), Sum(Ident "b", Ident "c")), GreaterThan(Ident "a",Const 0)),
                        Sum(Ident "c", Ident "d"),
                        Ident "e")

// 2. Extend the function eval defined in the lecture to support the
// if-then-else expressions defined in Q1.

let rec eval t clojure =
    match t with
    | Const n        -> n
    | Ident s        -> Map.find s clojure
    | Minus t        -> - (eval t clojure)
    | Sum (t1,t2)    -> eval t1 clojure + eval t2 clojure
    | Diff (t1,t2)   -> eval t1 clojure - eval t2 clojure
    | Prod (t1,t2)   -> eval t1 clojure * eval t2 clojure
    | Let (s,t1,t2)  -> let v1 = eval t1 clojure
                        let clojure1 = Map.add s v1 clojure
                        eval t2 clojure1
    | IfThenElse (condition,t1,t2) -> if logicEval condition clojure then eval t1 clojure else eval t2 clojure
    | Match (t, matchList) -> let m = eval t clojure
                              let matching = List.find (fun x -> (fst x) = m) matchList
                              eval (snd matching) clojure                              

and logicEval logicTree clojure =
    match logicTree with
    | And (lt1, lt2) -> logicEval lt1 clojure && logicEval lt2 clojure
    | Or (lt1, lt2) -> logicEval lt1 clojure || logicEval lt2 clojure
    | LessThan (t1, t2) -> eval t1 clojure < eval t2 clojure
    | GreaterThan (t1, t2) -> eval t1 clojure > eval t2 clojure
    | Equal (t1, t2) -> eval t1 clojure = eval t2 clojure
    | Not (lt1) -> not (logicEval lt1 clojure)

// if a+3 > b+c && a>0 then c+d else e

let m1 = Map.add "a" 7 Map.empty
let m2 = Map.add "b" 5 m1
let m3 = Map.add "c" 4 m2
let m4 = Map.add "d" 1 m3
let clojure = Map.add "e" 13 m4

eval IfExpression clojure


// 3-4: Given the type definition:
type BList = | BEmpty
             | Snoc of BList * int
//
// 3. Make the function filterB: (prop: int -> bool) BList -> BList that will return a list for the elements of which
// the function prop returns true.
//filterB (fun x -> 1<x) (Snoc(Snoc(Snoc(Snoc(BEmpty, 4), -6), 2), 1)) -> Snoc (Snoc (BEmpty,4),2)

let rec filterB (prop: int-> bool) (list: BList) =
    match list with
    | BEmpty                 -> BEmpty
    | Snoc (body, item) ->
               let filteredBody = filterB prop body
               if prop item
               then Snoc (filteredBody, item)
               else filteredBody

filterB (fun x -> 1<x) (Snoc(Snoc(Snoc(Snoc(BEmpty, 4), -6), 2), 1))

// 4. Make the function mapB: (trans: int -> int) BList -> BList that will return a list where the function trans has
// been applied to each element.

let rec mapB (trans: int->int) (list: BList) =
    match list with
    | BEmpty                 -> BEmpty
    | Snoc (body, item) -> Snoc (mapB trans body, 
                                   trans item)

mapB (fun x -> x+1) (Snoc(Snoc(Snoc(Snoc(BEmpty, 4), -6), 2), 1))

// 5-7. Given the type definition
type Tree =
 | Nil
 | Branch2 of Tree * int * Tree
 | Branch3 of Tree * int * Tree * int * Tree
//
// 5. Define the value exampleTree : Tree that represents the following
//    tree:
//
//        2
//       / \
//      *  3 5
//        / | \
//       *  *  *
let exampleTree = Branch2(Nil, 2, Branch3(Nil,3,Nil,5,Nil))

// 6. Define a function sumTree : Tree -> int that computes the sum of
//    all labels in the given tree.
let rec sumTree (tree: Tree) : int = 
    match tree with
     | Nil -> 0
     | Branch2 (b1, label, b2) -> sumTree b1 + label + sumTree b2
     | Branch3 (b1, label1, b2, label2, b3) -> sumTree b1 + label1 + sumTree b2 + label2 + sumTree b3

sumTree exampleTree

// 7. Define a function productTree : Tree -> int that computes the
//    product of all labels in the given tree. If this function
//    encounters a label 0, it shall not look at any further labels, but
//    return 0 right away.

let rec productTree (tree: Tree) : int =
    match tree with
     | Nil -> 1
     | Branch2 (b1, label, b2) when label = 0 -> 0
     | Branch2 (b1, label, b2) -> productTree b1 * label * productTree b2
     | Branch3 (b1, label1, b2, label2, b3) when label1 = 0 || label2 = 0 -> 0
     | Branch3 (b1, label1, b2, label2, b3) -> productTree b1 * label1 * productTree b2 * label2 * productTree b3

productTree exampleTree

// ** Bonus questions **

// 8. Extend the ExprTree type with a pattern match expression
// match p with [p1, ex 1; p2,ex2 ...]
let matchExpression = Match(IfExpression, 
                            [(5, Const 888)
                             (13, Const 999)
                            ])
// 9. Extend the eval function to support match expressions.
eval matchExpression clojure