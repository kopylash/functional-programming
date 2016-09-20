(*

  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 1: Basic operations on lists

  ------------------------------------
  Name: Vladyslav Kopylash
  TUT Student ID: vlkopy
  ------------------------------------


  Answer the questions below.  You answers to questions 1--9 should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded.

  To submit the coursework you will be asked to
  
  1) Check out your empty GIT repository
  from the server git.ttu.ee using instructions on page
  https://courses.cs.ttu.ee/pages/ITT8060

  2) Put your solution into a file coursework1/coursework1.fsx
  in the repository. Commit it and push it to the server!

  NB! It is very important to make sure you use the exact name using
  only small caps. Files submitted under wrong name may not get a grade.

 
*)

// 1. Make an empty list of generic type.
let list = []
 // 2. Make an empty list of type 'char list' (or list<char>).
let charList:List<char> = []
// 3. Make a three element list called 'unis' containing pairs of university
// website url (string) and year of establishment (int). The year of
// establishement should be that of the university.
let unis = [("http://kpi.ua" , 1898 ); ("http://ut.ee" , 1632); ("http://ttu.ee" , 1918)]

// 4. Write a function filterOutYoungerThan: int -> string * int -> string * int to filter out news sources
// which are less than some integer years old.  It should use the List.filter
// function from the library.
let filterOutYoungerThan (year:int) (resource:(string*int) list) = 
 let filter tuple = (2016 - (snd tuple)) > year
 List.filter filter resource
  
// 5. Test the function 'filterOutYoungerThan' to filter out universities younger than 100 years in 
// your list 'unis'.
filterOutYoungerThan 100 unis

// 6. Calculuate the average age of your list of universities. The
// function should use pattern matching and recursion.
let getAverageAge unis =
 let rec sumList lst=
  match lst with
   | [] -> 0
   | hd::tl -> hd + sumList tl
 let ages = unis |> List.map(fun(_,age) -> age)
 (sumList ages) / unis.Length
 
getAverageAge unis

// 7. Using the http function write a function
//
//    getSource : (string * int) -> (string * string)
//
//    which takes a pair of a url and a year of establishment of the university and
//    returns a pair of a url and the html source of the page.
open System.IO
open System.Net

let http (url: string) =
  let req = WebRequest.Create(url)
  let resp = req.GetResponse()
  let stream = resp.GetResponseStream()
  let reader = new StreamReader(stream)
  let html = reader.ReadToEnd()
  resp.Close()
  html

let getSource (url,_) = (url, http url)

// 8. Write a function
//
//    getSize : (string * string) -> (string * int)
//    
//    which takes a pair of a url and its html source and returns a
//    pair of the url and the size of the html (length of the string).
let getSize (url:string,html:string) = (url,html.Length)

// 9. Write a function
//
//    getSourceSizes : (string * int) list -> (string * int) list
//
//    It should take a list of pairs of urls and years of
//    establishment and return a list of pairs of urls and
//    corresponding html source sizes

let getSourceSizes unis = unis |> List.map getSource |> List.map getSize
