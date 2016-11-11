(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------------------

  Coursework 6: Property based testing

  ------------------------------------------------
  Name: Vladyslav Kopylash
  Student ID: vlkopy
  ------------------------------------------------


  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as
  file coursework6.fsx in directory coursework6.

  The file that should be compiled to a dll should go into coursework6.fs.

  Please do not upload DLL-s. Just include a readme.txt file containing the 
  dependencies required (additional DLLs)

  The deadline for completing the above procedure is Friday, November 11, 2016.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)

(*

*)

#r @"..\packages\FsCheck.2.6.2\lib\net45\FsCheck.dll"

//#r "FsCheck"

open FsCheck

(*
    Task 1:

    Define FsCheck properties for the following statements:

      * Taking the lengths of two lists xs and ys and adding them yields the
        same value as concatenating xs and ys and taking the length of the
        result.

      * Reversing two lists xs and ys and concatenating the results yields the
        same value as concatenating ys and xs and reversing the result.
*)

let ``lists length sum is equal to the length of concated list`` (xs : 'a list) (ys : 'a list) : bool = (xs.Length + ys.Length) = List.length (xs @ ys)  
let ``list reversing test`` (xs : 'a list) (ys : 'a list) : bool = (List.rev xs @ List.rev ys) = List.rev (ys @ xs)

Check.Quick ``lists length sum is equal to the length of concated list``
Check.Quick ``list reversing test``

(*
    Task 2:

    A palindrome is a list that is equal to its reverse. Below you find a
    function isPalindrome, which checks whether a given list is a palindrome.

     a) Define an FsCheck property that expresses the above definition of a
        palindrome. Use the operator ==> for defining your property.

     b) Define a variant of your property which makes FsCheck show the
        distribution of the lenghts of the lists on which the property was
        tested.
*)

let rec isPalindrome xs =
  match xs with
    | []        -> true
    | (x :: xs) -> match List.rev xs with
                     | []        -> true
                     | (y :: ys) -> x = y && isPalindrome ys

let palindromeCheck xs = isPalindrome xs ==> (List.rev xs = xs)
Check.Quick palindromeCheck

let ``palindrom check with lists lengths distribution`` xs = isPalindrome xs ==> (List.rev xs = xs) |> Prop.collect (List.length xs)
Check.Quick ``palindrom check with lists lengths distribution``

(*
    Task 3:

    Below you find a function toPalindrome, which converts a given list into a
    palindrome of the same length, more or less by replacing the second half of
    the list with the reverse of the first half.

     a) Define an FsCheck property that expresses the definition of a
        palindrome from the previous task. This time, make sure that FsCheck
        does not generate arbitrary lists from which it uses only the
        palindromes, but generates palindromes directly.

     b) Define a variant of your property which makes FsCheck show the
        distribution of the lenghts of the lists on which the property was
        tested.
*)

let toPalindrome xs =
  let len       = List.length xs
  let suffixLen = len / 2
  let prefixLen = if 2 * suffixLen = len then suffixLen else suffixLen + 1
  let take n xs = Seq.toList (Seq.take n xs)
  take prefixLen xs @ List.rev (take suffixLen xs)

let palindromeGenerator xs = 
  gen { 
    return toPalindrome xs
  }

let ``palindrom check with generators`` xs = Prop.forAll (Arb.fromGen (palindromeGenerator xs)) (fun list -> List.rev list = list)
Check.Quick  ``palindrom check with generators``

let ``palindrom check with generators and lists length distribution`` xs = Prop.forAll (Arb.fromGen (palindromeGenerator xs)) (fun list -> List.rev list = list) |> Prop.collect (List.length xs)
Check.Quick  ``palindrom check with generators and lists length distribution``

(*
    Task 4:

    Copy all the code into coursework6.fs file, use the appropriate [<TestFixture>],
    [<Property>] attributes so that the tests are runnable using FsCheck.NUnit (v. 2.6.2)

*)

//coursework6.fs

(*  Task 5:

    Take the decision tree code from the lecture notes and write unit and property based tests
    to the extent that you consider the code reasonably well tested.

*)
//in coursework6.fs