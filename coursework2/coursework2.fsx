(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 2: Operations on lists, recursion

  ------------------------------------
  Name: Vladyslav Kopylash
  TUT Student ID: vlkopy
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the TUT
  git system using the instructions on the course web page by September 30, 2016.
*)

// 1. Make a value sl containing empty list of type string list.
let sl :list<list<string>>= []

// 2. Make a function shuffle: int list -> int list that rearranges the elements of the argument list
// in such a way that the first value goes to first, last to second,
// second to third, last but one to fourth etc.
// E.g.
// shuffle [] -> []
// shuffle [1;2] -> [1;2]
// shuffle [1..4] -> [1;4;2;3]
// [1 2 3 4 5 ] -> [1 5 2 4 3]
// [1 2 3 4 5 6] -> [1 6 2 5 3 4]

let rec shuffle (list: int list) = 
 match list with
  | [] -> []
  | head::tail -> head::shuffle(List.rev tail)

shuffle [1..6]

// 3. Make a function segments: int list -> int list list that splits the list passed
// as an argument into list of lists of nondecreasing segments.
// The segments need to be of maximal possible length (the number of segments
// needs to be minimal)
// E.g.
// segments [] ->  []
// segments [1] -> [[1]]
// segments [3;4;5;5;1;2;3] -> [[3;4;5;5];[1;2;3]]
let rec getSegment (list:int list, t:int list)=
 match list with
  |[] -> ([],[])
  |head::[] -> ([head],[])
  |head::tail when head<=tail.Head -> (head::(fst (getSegment(tail,t))), snd (getSegment(tail,t)))
  |head::tail when head > tail.Head -> ([head], tail)
  |_ -> ([],[])
  
getSegment([3;4;5;5;1;2;3;6;2;3;3], [])
getSegment([3], [])

let rec segmentate (list:int list) :int list list= 
 match (getSegment(list, [])) with 
  |sub,[] -> [sub]
  |sub,tail -> sub::segmentate(tail)

let segments list =
 match list with 
  | [] -> []
  | _ -> segmentate list
  
segments []
segments [1]
segments [3;4;5;5;1;2;3]
segments [3;4;5;5;1;2;3;6;2;3;3]

// 4. Make a function sumSublists : int list list -> int list that will compute the sums of sublists in a list of list of ints.
// Hint: use the function List.fold to compute the sums of lists of ints.

let sumSublists (list: int list list) = List.rev(List.fold (fun acc l -> (List.sum l)::acc) [] list) 

sumSublists [[3; 4; 5; 5]; [1; 2; 3; 6]; [2; 3; 3]]


// 5. Write a function filterSegments : (int list -> bool) -> int list list -> int list list that will filter out lists of ints
// based on some filter function. Write a filter function for even sum, odd sum, even number of elements, odd number of elements.
let evenSum (list : int list) : bool = List.sum list |> (fun x -> x % 2 = 0)
let oddSum (list : int list) : bool = List.sum list |> (fun x -> x % 2 <> 0)
let evenLength (list : int list) : bool = list.Length |> (fun x -> x % 2 = 0)
let oddLength (list : int list) : bool = list.Length |> (fun x -> x % 2 <> 0)

let filterSegments (filter : int list -> bool) (list : int list list) : int list list = List.filter filter list

filterSegments evenSum [[3; 4; 5; 5]; [1; 2; 3; 6]; [2; 3; 3]]
filterSegments oddSum [[3; 4; 5; 5]; [1; 2; 3; 6]; [2; 3; 3]]
filterSegments evenLength [[3; 4; 5; 5]; [1; 2; 3; 6]; [2; 3; 3]]
filterSegments oddLength [[3; 4; 5; 5]; [1; 2; 3; 6]; [2; 3; 3]]

filterSegments evenSum [[3; 4; 5; 5]]