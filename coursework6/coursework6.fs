module coursework6

#if INTERACTIVE
#r @"..\packages\NUnit.2.6.4\lib\nunit.framework.dll"
#r @"..\packages\FsCheck.2.6.2\lib\net45\FsCheck.dll"
#endif

    open NUnit.Framework    
    open FsCheck.NUnit
    open FsCheck

    let rec isPalindrome xs =
        match xs with
        | []        -> true
        | (x :: xs) -> match List.rev xs with
                            | []        -> true
                            | (y :: ys) -> x = y && isPalindrome ys

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

    [<TestFixture>]
    type ``coursework6 property tests`` () =

        [<Property>]
        member this.``lists length sum is equal to the length of concated list`` (xs : int list) (ys : int list) : bool = (xs.Length + ys.Length) = List.length (xs @ ys)

        [<Property>]
        member this.``list reversing test`` (xs : int list) (ys : int list) : bool = (List.rev xs @ List.rev ys) = List.rev (ys @ xs)

        [<Property>]
        member this.palindromeCheck (xs : int list) = isPalindrome xs ==> (List.rev xs = xs)

        [<Property>]
        member this.``palindrom check with lists lengths distribution`` (xs : int list) = isPalindrome xs ==> (List.rev xs = xs) |> Prop.collect (List.length xs)
    
        [<Property>]
        member this.``palindrom check with generators`` (xs : int list) = Prop.forAll (Arb.fromGen (palindromeGenerator xs)) (fun list -> List.rev list = list)
    
        [<Property>]
        member this.``palindrom check with generators and lists length distribution`` (xs : int list) = Prop.forAll (Arb.fromGen (palindromeGenerator xs)) (fun list -> List.rev list = list) |> Prop.collect (List.length xs)

(*  Task 5:

    Take the decision tree code from the lecture notes and write unit and property based tests
    to the extent that you consider the code reasonably well tested.

*)
    type Client = 
      { Name : string;
        Income : int ;
        YearsInJob : int;
        UsesCreditCard : bool;
        CriminalRecord : bool;  }

    type QueryInfo =
      { Title     : string
        Check     : Client -> bool
        Positive  : Decision
        Negative  : Decision }

    and Decision = 
       | Result of string
       | Query  of QueryInfo

    let rec tree =
       Query  {Title = "More than €40k"
               Check = (fun cl -> cl.Income > 40000)
               Positive = moreThan40
               Negative = lessThan40}
    and moreThan40 =
       Query  {Title = "Has criminal record"
               Check = (fun cl -> cl.CriminalRecord)
               Positive = Result "NO"
               Negative = Result "YES"}
    and lessThan40 =
       Query  {Title = "Years in job"
               Check = (fun cl -> cl.YearsInJob > 1)
               Positive = Result "YES"
               Negative = usesCreditCard}
    and usesCreditCard =
       Query  {Title = "Uses credit card"
               Check = (fun cl -> cl.UsesCreditCard)
               Positive = Result "YES"
               Negative = Result "NO"}

    let clientGenerator =
      gen {
        let! income = Gen.choose (35000,45000)
        let! years = Gen.choose (0,2)
        let! credit = Gen.oneof [ gen { return true }; gen { return false }]
        let! criminal = Gen.oneof [ gen { return true }; gen { return false }]
        return {
                Name = "John Doe"; 
                Income = income; 
                YearsInJob = years 
                UsesCreditCard = credit
                CriminalRecord = criminal 
               }
      }
      
    Gen.sample 0 10 clientGenerator

    let rec testClientTree client tree =
        match tree with
        | Result msg  -> msg
        | Query qinfo -> let result, case = 
                             if qinfo.Check(client) then
                                 "yes", qinfo.Positive
                             else
                                 "no", qinfo.Negative
                         testClientTree client case
       


    [<TestFixture>]
    type ``Loan tree tests`` () =

       [<Test>]
       member this.``rich and criminal`` () = 
           let c = {Name = "John Doe"; Income = 41000 ; YearsInJob = 1 ; 
                          UsesCreditCard = true ; CriminalRecord = true }
           Assert.AreEqual((testClientTree c tree), "NO")

       [<Test>]
       member this.``rich and not criminal`` () = 
           let c = {Name = "John Doe"; Income = 41000 ; YearsInJob = 1 ; 
                          UsesCreditCard = true ; CriminalRecord = false }
           Assert.AreEqual((testClientTree c tree), "YES")

       [<Test>]
       member this.``poor, but with stable job`` () = 
           let c = {Name = "John Doe"; Income = 10000 ; YearsInJob = 10 ; 
                          UsesCreditCard = true ; CriminalRecord = false }
           Assert.AreEqual((testClientTree c tree), "YES")

       [<Test>]
       member this.``poor, without stable job, but uses credit money a lot`` () = 
           let c = {Name = "John Doe"; Income = 10000 ; YearsInJob = 0 ; 
                          UsesCreditCard = true ; CriminalRecord = false }
           Assert.AreEqual((testClientTree c tree), "YES")

       [<Test>]
       member this.``poor, without stable job, doesn't use a credit card`` () = 
           let c = {Name = "John Doe"; Income = 1000 ; YearsInJob = 0 ; 
                          UsesCreditCard = false ; CriminalRecord = false }
           Assert.AreEqual((testClientTree c tree), "NO")

       [<Test>]
       member this.``boundary work experience`` () = 
           let c = {Name = "John Doe"; Income = 1000 ; YearsInJob = 1 ; 
                          UsesCreditCard = false ; CriminalRecord = false }
           Assert.AreEqual((testClientTree c tree), "NO")

       [<Test>]
       member this.``boundary salary`` () =
            let boundarySalaryClient = {Name = "Vasia"; Income = 40000 ; YearsInJob = 12 ; 
                      UsesCreditCard = true ; CriminalRecord = false } 
            Assert.AreEqual((testClientTree boundarySalaryClient tree), "YES")


//Tried to do property based tests with clientGenerator, but Arb.fromGen doesn't yield the right instances from generator,
//so I receive income = -20 etc.
//struggled with it for 2 days and decided to submit only unit tests
//       [<Property(Verbose=true)>]
//       member this.``rich and not criminal`` (cl : Client) = 
//            let prop cl = (cl.Income > 40000 && cl.CriminalRecord = false) ==> (testClientTree cl tree = "YES")
//            Prop.forAll (Arb.fromGen clientGenerator) prop
       
    
