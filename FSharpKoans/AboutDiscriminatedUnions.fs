namespace FSharpKoans
open NUnit.Framework

(*
    A discriminated union is a disjoint set of named cases, where each case
    may have some linked/associated data.  If a discriminated union case has
    associated data, the case name is a function which takes the associated
    data as input and gives a value of the discriminated union type as output.
*)


module ``07: The Good Kind of Discrimination`` = 
    type Subject = // <-- feel free to add your own subjects!
    | Philosophy
    | Linguistics
    | ComputerScience
    | Mathematics
    | Economics
    | Management

    type UndergraduateDegree = 
    | BSc of first:Subject * second:Subject
    | BCom of first:Subject * second:Subject
    | BPharm
    | BA of first:Subject * second:Subject

    type PostgraduateDegree =
    | Honours of Subject
    | Masters of Subject

    [<Test>]
    let ``01 A case isn't the same as a type`` () = 
        let aDegree = BSc (Linguistics, ComputerScience)
        let anotherDegree = BPharm
        let philosopherKing = Masters Philosophy
        aDegree |> should be ofType<UndergraduateDegree> 
        anotherDegree |> should be ofType<UndergraduateDegree> 
        philosopherKing |> should be ofType<PostgraduateDegree> 
   
    [<Test>]
    let ``02 Creating & pattern-matching a discriminated union`` () = 
        let randomOpinion degree =
            match degree with
            | BSc (_, ComputerScience) | BSc (ComputerScience, _) -> "Good choice!"
            | BSc _ -> "!!SCIENCE!!"
            | BPharm -> "Meh, it's OK."
            | BCom(_,Management) | BCom(Management,_)  -> "Money, money, money."
            | BA(_,Philosophy) | BCom(Philosophy,_) -> "A thinker, eh?"
        randomOpinion (BSc (Mathematics, ComputerScience)) |> should equal "Good choice!"
        randomOpinion (BSc(Mathematics,Economics)) |> should equal "!!SCIENCE!!"
        randomOpinion (BCom (Management, Economics)) |> should equal "Money, money, money."
        randomOpinion (BCom (Linguistics, Management)) |> should equal "Money, money, money."
        randomOpinion (BA (Linguistics, Philosophy)) |> should equal "A thinker, eh?"
        randomOpinion BPharm |> should equal "Meh, it's OK."

    [<Test>]
    let ``03 We can create a discriminated union using named fields`` () =
        let someDegree = BSc (second = Subject.Mathematics, first = Subject.ComputerScience)            
        someDegree |> should equal (BSc (ComputerScience, Mathematics))

    [<Test>]
    let ``04 Pattern-matching using named fields`` () =
        let result =
            match BSc (Management, ComputerScience) with
            | BSc (_,Mathematics) | BSc(_,ComputerScience) |  BSc (Mathematics,_) | BSc(ComputerScience,_)  -> "correct" // <-- USE a pattern-match with named fields!
            | _ -> "nope"
        result |> should equal "correct"
        //Would be great to go through this with a tutor
    type EquipmentStatus =
    | Available
    | Broken of daysToRepair:int
    | Rented of renter:string

    [<Test>]
    let ``05 A discriminated union case with associated data is a function`` () =
        Broken |> should be ofType<int->EquipmentStatus>
        Rented |> should be ofType<string->EquipmentStatus>
        //Would be great to go through this with a tutor
    type BinaryTree =
    | Empty
    | Node of string * BinaryTree * BinaryTree

    [<Test>]
    let ``06 A discriminated union can refer to itself (i.e., it can be recursive).`` () =
        let rec depth x =
            match x with
            | Empty -> 0
            | Node (_, a, b) -> 1 + max (depth a) (depth b)
        let f = Empty
        let e = Empty
        let d = Node("Science",e,f)
        let two = Empty
        let c = Node("Computer",d,f)
        let one = Empty
        let b = Node("Love",c,e)
        let zero = Empty
        let a = Node("I",b,d) // <-- you may want to spread this over multiple lines and/or let-bindings ...!
        depth a |> should equal 4

        //I had help here but typed this up myself. //Would be great to go through this with a tutor