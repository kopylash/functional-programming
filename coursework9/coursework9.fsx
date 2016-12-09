(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------------------------------------------------

  Coursework 9: Asynchronous and reactive programming

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
  coursework9.fsx in directory coursework9.

  Please do not upload DLL-s. Just include a readme.txt file containing the
  dependencies required (additional DLLs)

  The deadline for completing the above procedure is Friday, December 9, 2016.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)

(*
  Task 1:

  Write a function downloadParallel : string list -> Async<string []> that takes
  a list of URLs and downloads the resources referenced by these URLs in
  parallel. Use the function downloadAsync from the lecture in your
  implementation.
*)

open System.IO
open System.Net

let readToEndAsync (reader : StreamReader) =
  Async.AwaitTask (reader.ReadToEndAsync())

let downloadAsync (url : string) =
  async {
    let  request  = HttpWebRequest.Create(url)
    use! response = request.AsyncGetResponse()
    let  stream   = response.GetResponseStream()
    use  reader   = new StreamReader(stream)
    return! readToEndAsync reader
  }

let downloadParallel (urls : string list) : Async<string []> = urls |> List.map downloadAsync |> Async.Parallel

Async.RunSynchronously (downloadParallel ["http://kpi.ua";"http://ttu.ee";"http://ut.ee"])


(*
  Task 2:

  Write a function downloadSemiParallel : string list -> Async<string []> that
  takes a list of URLs and downloads the resources referenced by these URLs.
  Resources from URLs with the same domain name shall be downloaded
  sequentially, but otherwise, parallelism shall be used. The order of the
  resources in the resulting array can be chosen by you.
*)

let getDomain (uri:string) : string = System.Uri(uri).Host

(*
    getDomain "http://ttu.ee"
*)


let groupBy (projection : string -> string) (list : string list) : string list list = 
  list 
  |> Seq.groupBy (projection)
  |> Seq.map (fun (g,s) -> s |> Seq.toList)
  |> Seq.toList

let groupByDomain (list : string list) : string list list = groupBy getDomain list

//groupByDomain ["http://kpi.ua";"http://kpi.ua/admission";"http://ut.ee";"http://ut.ee/et/teadus";"http://ut.ee/et/ulikoolist"]

let rec downloadSeq (urls : string list) = 
  async {
    if (urls.Length = 0) then
      return []
    else
      let! a = downloadAsync urls.Head
      printfn "%s DOWNLOADED" urls.Head
      let! b = downloadSeq urls.Tail
      return a::b 
  }

let downloadSemiParallel (urls : string list) = 
  async {
    let! l = groupByDomain urls |> List.map downloadSeq |> Async.Parallel
    return l |> List.concat
  }
  
Async.RunSynchronously (downloadSemiParallel ["http://kpi.ua";"http://kpi.ua/admission";"http://ut.ee";"http://ut.ee/et/teadus";"http://ut.ee/et/ulikoolist"])
  
(*
  Task 3:

  Write an event stream additions : IObservable<string> that emits an event
  everytime a file is created in the current directory. Each such event shall
  carry the name of the created file.

  Furthermore, write an event stream removals : IObservable<string> that emits
  an event everytime a file is removed from the current directory. Each such
  event shall carry the name of the removed file.
*)
open System.IO

let watcher = new FileSystemWatcher(__SOURCE_DIRECTORY__)
watcher.EnableRaisingEvents <- true

let additions : System.IObservable<string> = watcher.Created |> Observable.map (fun eventArgs -> eventArgs.Name)

(*  
    let testAdditions() = additions |> Observable.add (printfn "%s")
    testAdditions()
*)

let removals : System.IObservable<string> = watcher.Deleted |> Observable.map (fun eventArgs -> eventArgs.Name)

(*  
    let testRemovals() = removals |> Observable.add (printfn "%s")
    testRemovals()
*)

(*
  Task 4:

  Below you find the definition of a type Change whose values represent changes
  to a directory. Use the event streams from Task 3 to define an event stream
  changes : IObservable<Change> of all file additions and removals in the
  current directory.
*)

type Change =
  | Addition of string
  | Removal  of string

let changes : System.IObservable<Change> = 
  let adds = additions |> Observable.map (fun s -> Addition s)
  let rems = removals |> Observable.map (fun s -> Removal s)
  Observable.merge adds rems

(*  
    let testChanges() = changes |> Observable.add (fun x -> match x with
                                                              | Addition s -> printfn "Addition %s" s
                                                              | Removal s -> printfn "Removal %s" s
                                                  )
    testChanges()
*)

(*
  Task 5:

  Use the event stream changes from Task 4 to define an event stream
  turnover : IObservable<int> that tells at every file addition or removal how
  much the number of files in this directory has increased since the beginning
  (with negative numbers signifying a decrease). For example, if two files are
  added and one file is removed afterwards, there should be three events, that
  carry the numbers 1, 2, and 1, respectively.
*)

let turnover : System.IObservable<int> = changes |> Observable.scan (fun count c -> match c with
                                                                                      | Addition _ -> count + 1
                                                                                      | Removal _ -> count - 1
                                                                    ) 0

(*
    let turnoverTest() = turnover |> Observable.add (printfn "%i")
    turnoverTest()
 *)