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
    printfn "Task 3.11 %s" message
    example5 () |> ignore
    0 // return an integer exit code