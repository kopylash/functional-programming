(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 4: Higher order functions, option, list

  ------------------------------------
  Name: Vladyslav Kopylash
  Student ID: vlkopy
  ------------------------------------


  Answer the questions below. You answers to the questions should be
  correct F# code written after the question. This file is an F# script
  file; it should be possible to load the whole file at once. If you
  can't, then you have introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your solution to the repository as file
  coursework4.fsx in directory coursework4.

  The deadline for completing the above procedure is Friday,
  October 21, 2016.

  We will consider the submission to be the latest version of the
  appropriate files in the appropriate directory before the deadline
  of a particular coursework.

*)

// 1. Write a function by pattern matching
// 
//   flattenOption : option<option<'a>> -> option<'a>
//
//   which squashes two layers of possible successes or failures into 1
//   E.g. Some Some 1 -> Some 1
//        Some None
//        None
let flattenOption o = 
    match o with
     | Some a -> a
     | None -> None

flattenOption (Some (Some 1))
flattenOption (Some (None: int option))
flattenOption (None: string option option)

// 2. Can flattenOption by implemented using bind? If so, do it!
let flattenOption2 (o : option<option<'a>>) : option<'a> = Option.bind (fun (a:option<'a>) -> a) o

flattenOption2 (Some (Some 1))
flattenOption2 (Some (None: int option))
flattenOption2 (None: string option option)
// 3. Write a function
//
//    idealist : list<option<'a>> -> list<'a>
//
//    which collects a list of possible successes or failures into a
//    list containing only the successes. Pay close attention to the type.
let idealist (list : 'a option list) : 'a list = List.choose (fun a -> a) list

idealist [Some 4; Some 3; None; Some -1;]
idealist ([] : int option list)
idealist [None; Some "e"; Some "asw"; None; Some "a"; Some "s"]

// 4. Write a function
//
//    conservative : list<option<'a>> -> option<list<'a>>
//
//    that takes a list of possible successes or failures and returns
//    a list of successes if everything succeeded or returns failure
//    if 1 or more elements of the list was a failure. Again, pay
//    close attention to the type.
let conservative (list : 'a option list) : 'a list option = idealist list |> (fun x -> if x.Length = list.Length then Some x else None)

conservative [Some 4; Some 3; Some 4; Some -1;]
conservative [Some 4; Some 3; None; Some -1;]
conservative ([] : int option list)

// 5. Write a function
//
//    chars : list<string> -> list<char>
//
//    This function should use List.collect (bind) and have the
//    following behaviour:
//    ["hello";"world"] -> ['h';'e';'l';'l';'o';'w';'o';'r';'l';'d']
let chars (list : string list) : char list = List.collect (fun l -> List.ofSeq l) list

chars ["Banuelos";"is";"A";"BIG"; "BOSS"]
chars []

// 6. Write a function
//
//    iprint : list<int> -> string
//
//    This function should use List.foldBack and have the following behaviour:
//    [1 .. 5] |-> "1,2,3,4,5,"
let iprint (list : int list) : string = 
    let stringList = List.map (fun x -> x.ToString()) list 
    List.foldBack (fun (acc:string) x -> acc + "," + x.ToString()) stringList ""

[1..5] |> iprint 
