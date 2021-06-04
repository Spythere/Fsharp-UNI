// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

//----------------------
//Task 3.4
//
let isInList elementToFind listToCheck = 
    List.fold(fun acc x -> acc || x = elementToFind) false listToCheck

//----------------------
// Task 3.6
// 
let remove lst i =
    let rec remove lst lst' =
        match lst with
        | []   -> lst'
        | h::t -> if List.length lst = i then
                      lst' @ t
                  else
                      remove t (lst' @ [h])
    remove lst []
let myList = ['A'; 'B'; 'C'; 'D'; 'E']
let newList = remove myList 2
//----------------------
// Task 3.7
// 

let avg aList =
    let rec sum = function
        | head :: tail -> head + (sum tail)
        | [] -> 0.
    sum aList / (aList |> List.length |> float)

let res = avg [ 2.; 4.; 6. ]

//----------------------
// Task 3.8
// 
let strings = [ "tomatoes"; "bananas"; "apples" ]
let fullString = String.concat "," strings

//----------------------
// Task 3.10
// 
let values = ["aa"; "x"; "zzz"; "yy"; "eeee"]

// Sort the string list by length in descending (high to low) order.
let result = List.sortBy (fun (x : string) -> -x.Length ) values

//----------------------
// Task 3.11
// 



//----------------------
// Task 3.12
// 

let revlists xs = List.map (fun x -> List.rev x) xs;;

let example5 () =
    let list = [[0;1;1];[3;2];[];[5]]
    let x =revlists [[0;1;1];[3;2];[];[5]]
    printfn "The reverse of %A is %A" list x;;

//----------------------
// Task 3.16
// 


let rec isSorted list =
    match list with
    | [] | [_] -> true
    | h1::(h2::_ as tail) -> h1 <= h2 && isSorted tail

let task_3_16 () = 
    let list = [[5;1;5]];
    printfn "The isSorted of %A is %A" isSorted list;;

//----------------------
// Task 3.20
// 

type 'a Tree = Empty | Branch of 'a * 'a Tree * 'a Tree

let rec foldTreeNaive accFun init = function
    | Empty -> init
    | Branch (x, left, right) ->
        let lacc = foldTreeNaive accFun init left
        let racc = foldTreeNaive accFun init right
        accFun x lacc racc

let counLeaves tree =
    foldTreeNaive (fun abc lc rc ->
        if lc + rc = 0 then 1
        else 1 + lc + rc) 0 tree

let Tree1 = Branch ('x', Branch ('x', Empty, Empty),Branch ('x', Empty, Branch ('x', Empty, Branch ('x', Empty, Empty))))


let result_tree_leaf = counLeaves Tree1

//----------------------
// Task 3.21
// 
type Tree =
    | Tip
    | Node of int * Tree * Tree

let rec sumTree tree =
    match tree with
    | Tip -> 0
    | Node(value, left, right) ->
        value + sumTree(left) + sumTree(right)
let myTree = Node(0, Node(1, Node(2, Tip, Tip), Node(3, Tip, Tip)), Node(4, Tip, Tip))
let resultSumTree = sumTree myTree

[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    printfn "Task 3.4 %s" message
    let x = [1;2;2;3]
    let y = 4
    let z = 2

    isInList y x |> printfn "%A"
    isInList z x |> printfn "%A"
     
    printfn "Task 3.6 %s" message 
    newList |> printfn "%A"
    
    printfn "Task 3.7 %s" message
    printfn "%A" res
    
    printfn "Task 3.8 %s" message
    printfn "%s" fullString
    

    // Print our results.
    printfn "Task 3.10 %s" message
    List.iter(fun x -> printfn "%A" x) result

    // Print all our results.
    printfn "Task 3.12 %s" message
    example5 () |> ignore

    printfn "Task 3.16 %s" message
    task_3_16 () |> ignore

    printfn "Task 3.20 %s" message
    printfn "%A" result_tree_leaf

    printfn "Task 3.21 %s" message
    printfn "%A" resultSumTree

    0 // return an integer exit code