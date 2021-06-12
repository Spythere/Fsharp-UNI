open System

(* Funkcja z silnią dla ułatwienia zadań z listami *)
let silnia n = 
    let rec obliczSilnie n acc = 
        if(n > 0) then
            obliczSilnie (n-1) (acc*n)
        else 
            acc

    obliczSilnie n 1 


(* Typ drzewa, na którym robione są zadania *)
type Drzewo = 
| Puste
| Wezel of int*Drzewo*Drzewo

(* Typ listy *)
type Lista<'a> = 
| Pusta
| Element of 'a*Lista<'a>

(* 
ZAD 1.

Zdefiniuj nowy typ danych reprezentujący drzewo binarne. Następnie napisz program, 
który wyświetli elementy tego drzewa w kolejności postorder. Zademonstruj jego działanie. 

Postorder: Najpierw wszystkie dzieci (lewe i prawe poddrzewo), potem rodzic (korzeń)
*)

let rec pokazPostOrder = function
| Puste -> ()
| Wezel(x, l, p) -> 

    match l with
       | Wezel(_, _, _) -> pokazPostOrder l
       | Puste -> ()

    match p with
        | Wezel(_, _, _) -> pokazPostOrder p
        | Puste -> ()

    printfn "%d" x

(* 
ZAD 2.

Zdefiniuj nowy typ danych reprezentujący drzewo binarne. Następnie napisz program, 
który wyświetli elementy tego drzewa w kolejności inorder. Zademonstruj jego działanie. 

Inorder: najpierw skrajne lewe poddrzewo, potem korzeń, potem skrajne prawe poddrzewo 
*)


let rec pokazInOrder = function
| Puste -> ()
| Wezel(x, l, p) -> 

    match l with
       | Wezel(_, _, _) -> pokazInOrder l
       | Puste -> ()

    printfn "%d" x

    match p with
        | Wezel(_, _, _) -> pokazInOrder p
        | Puste -> ()

        (*
ZAD 3.

Zdefiniuj nowy typ danych reprezentujący drzewo binarne. Następnie napisz funkcję, 
która określi czy wszystkie węzły drzewa spełniają warunek określony jako parametr. Zademonstruj jej działanie. 
*)

let rec czyWezlySpelniajaWarunek predykat = function
| Puste -> false
| Wezel(x, l, p) when predykat(x) = false -> false
| Wezel(_, l, p) -> 
    czyWezlySpelniajaWarunek predykat l |> ignore 
    czyWezlySpelniajaWarunek predykat p |> ignore
    true

(* 
ZAD 4.

Zdefiniuj nowy typ danych reprezentujący drzewo binarne. Następnie napisz funkcję, która policzy sumę wartości 
w węzłach spełniających warunek  określony w funkcji przekazanej jako parametr. Zademonstruj jej działanie. 
*)

let rec sumaWezlow = function
| Puste -> 0
| Wezel(x, l, p) -> x + sumaWezlow l + sumaWezlow p

let rec sumaWezlowZWarunkiem predykat = function
  | Puste -> 0
  | Wezel(x, l, p) when predykat(x) -> x + (sumaWezlowZWarunkiem predykat l) + (sumaWezlowZWarunkiem predykat p)
  | Wezel(_, l, p) -> (sumaWezlowZWarunkiem predykat l) + (sumaWezlowZWarunkiem predykat p)

(* 
ZAD 5.

Napisz program, który wczytując od użytkownika liczby z klawiatury zapamięta liczby parzyste. 
(Wprowadzanie zakończ, jeżeli użytkownik poda 0). Po zakończeniu wprowadzania danych 
program powinien wyświetlić je w odwrotnej kolejności (od ostatniej wprowadzonej do pierwszej). 
*)

(*
To tylko jak bardzo się boicie, że wpiszecie coś innego niż liczbę i wam wypierdoli program
*)

let bezpiecznePodawanieLiczby () =
    try 
        Some(int(Console.ReadLine()))
    with
        | :? System.FormatException ->  None


let podajLiczbe () = 
    let lista: int list = []

    let rec dodajLiczbe lista =   
        Console.Write "Podaj liczbe: "

        let v = bezpiecznePodawanieLiczby ()

        match v with
        | Some(x) -> 
            if(x <> 0 && x % 2 = 0) then
                    dodajLiczbe (lista @ [x])
                elif (x = 0) then
                    Console.WriteLine "Odwrocona lista wpisanych liczb parzystych: "
                    lista |> List.rev |> List.iter (fun v -> printfn "%d" v)
                else    
                    dodajLiczbe lista
        | None -> printfn "Podano zły format!"; 

    dodajLiczbe lista



(* 
ZAD 6.

Zdefiniuj nowy typ Lista reprezentujący listę łączoną mogącą przechowywać wartości dowolnego typu.. 
Następnie napisz funkcję, która policzy ile wartości dodatnich jest na tej liście. Napisz również kod, 
który demonstruje działanie przygotowanej funkcji. 
*)


let rec tylkoDodatnie = function
| Pusta -> 0
| Element(glowa, ogon) when glowa > 0 -> 1 + tylkoDodatnie ogon
| Element(_, ogon) -> tylkoDodatnie ogon

(* 
ZAD 7.

Napisz funkcję, która obliczy wartość wariancji bez powtórzeń zgodnie ze wzorem n!/(n-k)! 
*)

let wariancjeBezPowtorzen n k = 
    let licznik = silnia n
    let mianownik = silnia (n - k)

    licznik / mianownik


(* 
ZAD 8.

Napisz program, który utworzy listę 100 dwuwymiarowych punktów losowych z przedziału od -20 do 20. 
Następnie określ, odległość euklidesową każdego z tych punktów od początku układu współrzędnych. 
Wykorzystaj funkcje modułu List. 
*)

let okreslOdlegloscPunktow n =
    let gll = new Random()

    List.init n (fun i -> (gll.Next(-20, 21), gll.Next(-20, 21)))
    |> List.map (fun (x, y) -> (x, y, sqrt (((float) x)**2.0 + ((float)y)**2.0)))
    |> List.iter (fun (x, y, v) -> printfn "Odleglosc punktu (%d, %d) od (0,0): %.2f" x y v)

(* 
ZAD 9.

Napisz funkcję, która określi ile razy w danym łańcuchu znaków wystąpiły dowolne cyfry. 
*)

let countDigitInString (str: string) = 
    let occurences = 
        Seq.toList str
        |> List.ofSeq
        |> List.map (fun v -> (((int) v) - 48))
        |> List.filter (fun v -> v >= 0 && v <= 9)
        |> List.fold (fun acc v -> acc + 1) 0 

    printfn "Ilosc wystapien cyfr w '%s': %d" str occurences

(* 
ZAD 10.
 
Napisz funkcję, która pozwoli obliczyć silnię w sposób bezpieczny 
tzn. jeżeli wartość dla której mamy obliczyć silnię jest większa lub równa zero, 
to zwracamy wynik. Jeżeli będzie mniejsza od zera to zwracamy komunikat "Nie mogę policzyć silni z liczby ujemnej". 
Zaimplementuj tę funkcję wykorzystując najlepszy wg. siebie typ danych. 
*)

type BezpiecznyWynik = 
| Poprawny of int
| Błędny
    
    
let bezpiecznaSilnia n =
    if n < 0 then
        Błędny
    else
        Poprawny (silnia n)

        (*
ZAD 11.

Napisz funkcję, która na podstawie określonej liczby rzutów trzema kostkami 
obliczy prawdopodobieństwo wyrzucenia kombinacji 1,2,3. Liczbę powtórzeń podaj jako parametr funkcji
*)

let rec szansaNa123 (k:int) = 
    let gll = new Random()
    let rec szansa i suma = 
        if i = k then
            (float)suma/(float)k
        else
            let (rzut1, rzut2, rzut3) = (gll.Next(6) + 1, gll.Next(6) + 1, gll.Next(6) + 1)
            if (rzut1 = 1 && rzut2 = 2 && rzut3 = 3) 
                || (rzut1 = 1 && rzut2 = 3 && rzut3 = 2) 
                || (rzut1 = 2 && rzut2 = 1 && rzut3 = 3) 
                || (rzut1 = 2 && rzut2 = 3 && rzut3 = 1) 
                || (rzut1 = 3 && rzut2 = 2 && rzut3 = 1)
                || (rzut1 = 3 && rzut2 = 1 && rzut3 = 2) 
            then 
                szansa (i + 1) (suma + 1)
            else
                szansa (i + 1) suma
    szansa 0 0
(*
ZAD 12.

Napisz funkcję, która przyjmie parę liczb i określi, która z nich jest większa 
po podniesieniu do kwadratu np. wywołanie wieksza (-5, 4) powinno zwrócić -5 
*)

let ktoraWieksza (x,y) =
    if (x**2.0 > y**2.0) then
       x
    else
       y


[<EntryPoint>]
let main argv =
   
    
    let drzewo = Wezel(5, Wezel(3,  Wezel(1, Puste, Puste),  Wezel(2, Puste, Puste)), Wezel(8,  Wezel(11, Puste, Puste),  Wezel(20, Puste, Puste)))
    
    printfn "----------"
    printfn "ZADANIE 1"
    printfn "----------"
    (* ZAD 1. demonstracja *)
    pokazPostOrder drzewo

    printfn "----------"
    printfn "ZADANIE 2"
    printfn "----------"
    (* ZAD 2. demonstracja *)
    pokazInOrder drzewo

    printfn "----------"
    printfn "ZADANIE 3"
    printfn "----------"
    (* ZAD 3. demonstracja *)

    let result = sumaWezlowZWarunkiem (fun v -> v > 2) drzewo
    printfn "Suma wezlow: %A" result

    printfn "----------"
    printfn "ZADANIE 4"
    printfn "----------"
    (* ZAD 4. demonstracja *)
    let result = sumaWezlowZWarunkiem (fun v -> v > 5) drzewo
    printfn "Suma wezlow: %A" result

    printfn "----------"
    printfn "ZADANIE 5"
    printfn "----------"
    (* ZAD 5. demonstracja *)
    let result = (podajLiczbe())
    

    printfn "----------"
    printfn "ZADANIE 6"
    printfn "----------"
    (* ZAD 6.*)
    printfn "%A" (tylkoDodatnie (Element(4, Element(-22, Element(3, Pusta)))))

    printfn "----------"
    printfn "ZADANIE 7"
    printfn "----------"
    (* ZAD 7.*)
    printfn "wariancja wynosi %A" (wariancjeBezPowtorzen 6 2)

    printfn "----------"
    printfn "ZADANIE 8"
    printfn "----------"
    (* ZAD 8.*)
    let n = 100
    let wynik = okreslOdlegloscPunktow n

    printfn "----------"
    printfn "ZADANIE 9"
    printfn "----------"
    (* ZAD 9.*)
    let slowo = (string(Console.ReadLine()))
    (countDigitInString slowo)

    printfn "----------"
    printfn "ZADANIE 10"
    printfn "----------"
    (* ZAD 10. Bezpieczna silnia *)
    let n = 5
    let wynik = bezpiecznaSilnia n

    match wynik with
    | Błędny -> printfn "Nie mogę policzyć silni z liczby ujemnej"
    | Poprawny x -> printfn "Silnia z %d to %d" n x
 
    printfn "----------"
    printfn "ZADANIE 11"
    printfn "----------"
    (* ZAD 11. demonstracja *)
    let k = 200
    let wynik = szansaNa123 k

    printfn "Prawdobodobieństwo to %f" wynik

    printfn "----------"
    printfn "ZADANIE 12"
    printfn "----------"
    (* ZAD 12. demonstracja *)
    let a = 2.0
    let b = 4.0
    let wynik = ktoraWieksza (a, b) 

    printfn "Wieksza liczba to %f" wynik
   
    0 // return an integer exit code