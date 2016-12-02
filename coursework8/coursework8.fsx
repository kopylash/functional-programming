(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------------------------------------------------

  Coursework 8: Sequences and computation expressions

  ------------------------------------------------------------------------------
  Name: Vladyslav Kopylash
  Student ID: vlkopy
  ------------------------------------------------------------------------------


  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as file
  coursework8.fsx in directory coursework8.

  Please do not upload DLL-s. Just include a readme.txt file containing the
  dependencies required (additional DLLs)

  The deadline for completing the above procedure is Friday, December 2, 2016.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)

(*
  Task 1:

  Define a sequence powers : int seq that contains all powers of 2 in ascending
  order. Use Seq.unfold in your implementation.
*)

let powers : int seq =
  let step k =
    Some (k, k * 2)
  Seq.unfold step 2

powers |> Seq.take 20 |> List.ofSeq

(*
  Task 2:

  Define a sequence primes : int seq that contains all prime numbers in
  ascending order. Use sequence expressions in your implementation. You may want
  to use the function isPrime : int -> bool defined below. This function checks
  whether any given number that is greater or equal 2 is a prime number.
*)

let isPrime n =
  let rec hasDivisorFrom d n =
    if d * d <= n then
      if n % d = 0 then
        true
      else
        hasDivisorFrom (d + 1) n
    else
      false
  not (hasDivisorFrom 2 n)

let primes : int seq =
  let rec worker num =
    seq {
      if isPrime(num) then yield num
      yield! worker (num + 1)
    }
  worker 1

primes |> Seq.take 20 |> List.ofSeq

(*
  Task 3:

  Define a sequence primes' : int seq that again contains all prime numbers in
  ascending order. This time, do not use sequence expressions in your
  implementation, but use an appropriate function from the Seq module. Again,
  you may want to use the function isPrime : int -> bool defined above.
*)

let primes' : int seq = Seq.initInfinite (fun x -> x+1) |> Seq.where isPrime
primes' |> Seq.take 20 |> List.ofSeq

(*
  Task 4:

  Define a function fourthRoot : float -> float option that returns Some x if x
  is the 4th root of the argument, and None if the argument has no 4th root. In
  your implementation, use the squareRoot function from the lecture and
  computation expressions for the option type as defined in the lecture.
*)

type OptionBuilder () =
  member this.Bind   (opt, f) = Option.bind f opt
  member this.Return x        = Some x

let option = new OptionBuilder ()

let fourthRoot (num : float) : float option =
  let squareRoot x = if x >= 0.0 then Some (sqrt x) else None
  option {
    let! srt = squareRoot num
    let! frt = squareRoot srt
    return frt
  }

fourthRoot 256.0

//or do we need to use custom option type?


(*
  Task 5:

  A function from a type 'env to a type 'a can be seen as a computation that
  computes a value of type 'a based on an environment of type 'env. We call such
  a computation a reader computation, since compared to ordinary computations,
  it can read the given environment. Below you find the following:

    • the definition of a builder that lets you express reader computations
      using computation expressions

    • the definition of a reader computation ask : 'env -> 'env that returns the
      environment

    • the definition of a function runReader : ('env -> 'a) -> 'env -> 'a that
      runs a reader computation on a given environment

    • the definition of a type Expr of arithmetic expressions

  Implement a function eval : Expr -> Map<string, int> -> int that evaluates
  an expression using an environment which maps identifiers to values. Use
  computation expressions for reader computations in your implementation. Note
  that partially applying eval to just an expression will yield a function of
  type map <string, int> -> int, which can be considered a reader computation.
  This observation is the key to using computation expressions.
*)

type ReaderBuilder () =
  member this.Bind   (reader, f) = fun env -> f (reader env) env
  member this.Return x           = fun _   -> x

let reader = new ReaderBuilder ()

let ask = id

let runReader = (<|)

type Expr =
  | Const of int
  | Ident of string
  | Neg   of Expr
  | Sum   of Expr * Expr
  | Diff  of Expr * Expr
  | Prod  of Expr * Expr
  | Let   of string * Expr * Expr

let eval (e : Expr) (env : Map<string,int>) : int =
  let rec monadReader expr = 
    reader {
      match expr with
        | Const n        -> return n
        | Ident s        -> let! identifier = Map.find s
                            return identifier
        | Neg t          -> let! e = monadReader t
                            return -e
        | Sum (t1,t2)    -> let! e1 = monadReader t1
                            let! e2 = monadReader t2
                            return (e1 + e2)
        | Diff (t1,t2)   -> let! e1 = monadReader t1
                            let! e2 = monadReader t2
                            return e1 - e2
        | Prod (t1,t2)   -> let! e1 = monadReader t1
                            let! e2 = monadReader t2
                            return e1 * e2
        | Let (s,t1,t2)  -> let! v1 = monadReader t1
                            let! env = Map.add s v1
                            let e = monadReader t2 env
                            return e
    }
  runReader (monadReader e) env



let expr = Sum(Ident "a", Const 3)
let m1 = Map.add "a" 7 Map.empty
let test10 = eval expr m1

// a * (-3 + let x = 5 in x + a)
let expr1 = Prod(Ident "a",
                Sum (Neg (Const 3),
                    Let ("x", Const 5, Sum (Ident "x", Ident "a"))
                    )
               )

let test63 = eval expr1 m1

//test that it returns function without env
let testCE = eval expr1
 


