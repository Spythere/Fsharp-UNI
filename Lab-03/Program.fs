// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
module list
open System

//----------------------
//Task 3.1
//

type Lista<'a> = 
|Pusta
|Wezel of 'a*Lista<'a>

let rec lista n =
    if n = 1 then
        Pusta
    else Wezel(n, (lista (n-1)))

//----------------------
//Task 3.2
//

type Lista_2<'a> = 
|Pusta
|Wezel of 'a*Lista_2<'a>

let rec lista_2 n min max =
    if n > max then
        Pusta
    else Wezel(n, (lista_2 (n+min) min max))


//----------------------
//Task 3.3
//

let rec getn n xs =
    match n, xs with
      | 0, (x::_)   -> x
      | _, (_::xs') -> getn (n - 1) xs'
      | _, []       -> invalidArg "n" "n is too large"

/// This invokes the tail recursive helper function
/// An approach like this is common in F#.
let sumListTailRecursive xs = getn 5 xs
let oneThroughTen = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]

//----------------------
//Task 3.5
//
type ElementListy<'a> =
| Istnieje of 'a
| Brak


let findIndex list element =
    let index = List.tryFindIndex (fun v -> v = element) list

    match index with
       | Some(x) -> Istnieje(x)
       | None -> Brak

(* Wywołanie *)
let x = findIndex ["a";"b";"c"] "c"

match x with
| Istnieje(x) -> printfn "Element znajduje się w liście na indeksie %d" x
| Brak -> Console.WriteLine "Ten element nie jest w liście!"

//---------------------- alternate
let ZnajdzIndeks lista element =
    let x = List.tryFindIndex (fun a -> a = element) lista
    if x = None then 
        failwith "Brak elementu "
    else
        x.Value
//----------------------

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
    
    printfn "----------------------"
    printfn "Task 3.1"
    printfn "----------------------"
    
    printfn "Podaj ilosc elementow"
    let ilosc = (int(Console.ReadLine()))
    printfn "%A" (lista ilosc)

    printfn "----------------------"
    printfn "Task 3.2"
    printfn "----------------------"

    printfn "Podaj ilosc krokow"
    let ilosc = (int(Console.ReadLine()))
    printfn "Podaj wartosc min"
    let min = (int(Console.ReadLine()))
    printfn "Podaj wartosc max"
    let max = (int(Console.ReadLine()))
    printfn "%A" (lista_2 ilosc min max)

    printfn "----------------------"
    printfn "Task 3.3"
    printfn "----------------------"

    printfn $"Nth element %d{sumListTailRecursive oneThroughTen}"

    printfn "----------------------"
    printfn "Task 3.4"
    printfn "----------------------"


    let x = [1;2;2;3]
    let y = 4
    let z = 2

    isInList y x |> printfn "%A"
    isInList z x |> printfn "%A"
     
    printfn "----------------------"
    printfn "Task 3.6"
    printfn "----------------------"

    newList |> printfn "%A"
    
    printfn "----------------------"
    printfn "Task 3.7"
    printfn "----------------------"

    printfn "%A" res
    
    printfn "----------------------"
    printfn "Task 3.8"
    printfn "----------------------"

    printfn "%s" fullString
    
    printfn "----------------------"
    printfn "Task 3.10"
    printfn "----------------------"

    List.iter(fun x -> printfn "%A" x) result

    printfn "----------------------"
    printfn "Task 3.12"
    printfn "----------------------"

    example5 () |> ignore

    printfn "----------------------"
    printfn "Task 3.16"
    printfn "----------------------"

    task_3_16 () |> ignore

    printfn "----------------------"
    printfn "Task 3.20"
    printfn "----------------------"

    printfn "%A" result_tree_leaf

    printfn "----------------------"
    printfn "Task 3.21"
    printfn "----------------------"

    printfn "%A" resultSumTree

    0 // return an integer exit code