
open System

//zad 2.1

let wybor () =
    printfn "Podaj wartosc: "
    let x = int(Console.ReadLine())
    match x with
    | 1 -> printfn "wprowadziles 1"
    | 2 -> printfn "wprowadziles 2"
    | 3 -> printfn "wprowadziles 3"
    | 4 -> printfn "wprowadziles 4"
    | _ -> printfn "wprowadziles inna wartosc"

//zad 2.2

let para () =
    printfn "Podaj dwie wartosci: "
    let x = float(Console.ReadLine())
    let y = float(Console.ReadLine())
    (x, y)  

//zad 2.3

let trojkat a b c =
    let p = (a + b + c) / 2.0
    (sqrt(p * (p - a) * (p - b) * (p - c)), p * 2.0)

//zad 2.4

let email (email:string) =
    let login = (email:string).Split("@").[0]
    let domena = (email:string).Split('@').[1]

    (login, domena)

//zad 2.5

let sufix (login, domena:string) =
    let alias1 = domena.Split('.').[0]
    let alias2 = domena.Split('.').[1]

    printf "Email użytkownika: %A" login

    match alias1 with
    |"pcz" | "PCZ" -> printfn " należy do domeny PCZ"
    |_ -> printfn " nie należy do domeny PCZ" 

//zad 2.6

let Euklides (a1, b1, c1) (a2, b2, c2) = 
   sqrt((a1 - a2)**2.0 + (b1 - b2)**2.0 + (c1 - c2)**2.0)

//zad 2.7

let wnetrze srodek R punkt =
    let d = sqrt((fst srodek - fst punkt)**2.0 + (snd srodek - snd punkt)**2.0)
    match d with
    | k when k  <= R -> printfn "Punkt (%.2f, %.2f) miesci sie w okregu o srodku w punkcie (%.2f, %.2f) i promieniu %.2f" (fst punkt) (snd punkt) (fst srodek) (snd srodek) R
    | _ -> printfn "Punkt (%.2f, %.2f) miesci sie poza okregiem o srodku w punkcie (%.2f, %.2f) i promieniu %.2f" (fst punkt) (snd punkt) (fst srodek) (snd srodek) R

//zad 2.8

 //do pelnej realizacji brakuje upraszczania wyników (podział licznika i mianownika przez NWD)
 //i zabezpieczenia przed dzieleniem przez zero

let dodaj a b = 
    let licznik = fst a * snd b + fst b * snd a  
    let mianownik = snd a * snd b
    licznik, mianownik

let odejmij a b = 
    let licznik = fst a * snd b - fst b * snd a  
    let mianownik = snd a * snd b
    licznik, mianownik

let pomnoz a b = 
    let licznik = fst a * fst b 
    let mianownik = snd a * snd b
    licznik, mianownik

let podziel a b = 
    let licznik = fst a * snd b 
    let mianownik = snd a * fst b
    licznik, mianownik

//zad 2.9

 //do pelnej realizacji brakuje upraszczania wyników (podział licznika i mianownika przez NWD)
 //i zabezpieczenia przed dzieleniem przez zero

type Ulamki = {
    licznik:int;
    mianownik:int;
}

let dodwanie ulamek1 ulamek2 =
    let licznik = ulamek1.licznik * ulamek2.mianownik + ulamek2.licznik * ulamek1.mianownik
    let mianownik = ulamek1.mianownik * ulamek2.mianownik
    {licznik=licznik; mianownik=mianownik}

let odejmowanie ulamek1 ulamek2 =
    let licznik = ulamek1.licznik * ulamek2.mianownik - ulamek2.licznik * ulamek1.mianownik
    let mianownik = ulamek1.mianownik * ulamek2.mianownik
    {licznik=licznik; mianownik=mianownik}

let mnozenie ulamek1 ulamek2 =
    let licznik = ulamek1.licznik * ulamek2.licznik
    let mianownik = ulamek1.mianownik * ulamek2.mianownik
    {licznik=licznik; mianownik=mianownik}

let dzielenie ulamek1 ulamek2 =
    let licznik = ulamek1.licznik * ulamek2.mianownik
    let mianownik = ulamek1.mianownik * ulamek2.licznik
    {licznik=licznik; mianownik=mianownik}

//zad 2.10

type data = {
dzien:int; 
miesiac:int; 
rok:int}

let dzien a = 
    let date1 = seq [string(a.dzien); string(a.miesiac); string(a.rok)]
    let strdate1 = String.concat "-" date1
    let data1 = DateTime.Parse(strdate1)
    let data2 = DateTime.Parse("1-1-1990")
    let spr = (data1 - data2).Days
    match spr % 7 with
    |0 -> "Poniedziałek"
    |1 -> "Wtorek"
    |2 -> "Środa"
    |3 -> "Czwartek"
    |4 -> "Piatek"
    |5 -> "Sobota"
    |_ -> "Niedziela"

//zad 2.11

let dzielenie_zad11 a b =
    if b <> 0.0 then
        (a/b, true)
    else
        (0.0, false)

let wynik_dzielenia wynik =
    match wynik with
    | (x, true) when x>0.0 -> printfn "Wynik to: %A jest dodatni" x
    | (x, true) when x=0.0 -> printfn "Wynik to: %A jest zerem" x
    | (x, true) -> printfn "Wynik to: %A jest ujemny" x
    | (_,false) -> printfn "Nie można dzielić przez 0"

//zad 2.12

type VIN = {
VIN:string
WMI:string
VDS:string
Z:char
VIS:string}

let dekompozycja (str:string) =
    if str.Length <> 17 then failwith "Numer VIN powinien składać się z 17 zaków"
    else 
        {VIN = str; WMI = str.[0..2]; VDS=str.[3..7]; Z=str.[8]; VIS=str.[9..16]}

let dopasuj (a:string) i =
    match a.[i] with
    |'A'|'a'|'J'|'j'|'1' -> 1
    |'B'|'b'|'K'|'k'|'S'|'s'|'2' -> 2
    |'C'|'c'|'L'|'l'|'T'|'t'|'3' -> 3
    |'D'|'d'|'M'|'m'|'U'|'u'|'4' -> 4
    |'E'|'e'|'N'|'n'|'V'|'v'|'5' -> 5
    |'F'|'f'|'W'|'w'|'6' -> 6
    |'G'|'g'|'P'|'p'|'X'|'x'|'7' -> 7
    |'H'|'h'|'Y'|'y'|'8' -> 8
    |'R'|'r'|'R'|'r'|'Z'|'z'|'9' -> 9
    |'0' -> 0
    | _ -> failwith "Niedozwolony znak w nr VIN"

let mod11 liczba =
    match liczba%11 with
    | 0 -> '0'
    | 1 -> '1'
    | 2 -> '2'
    | 3 -> '3'
    | 4 -> '4'
    | 5 -> '5'
    | 6 -> '6'
    | 7 -> '7'
    | 8 -> '8'
    | 9 -> '9'
    | _ -> 'X'


let rec walidacja vin = 
    let rec walidator vin i suma =
        let waga = [|8; 7; 6; 5; 4; 3; 2; 10; 0; 9; 8; 7; 6; 5; 4; 3; 2|]
        if i = 8 then 
            if mod11 suma = vin.Z then printfn "Ten numer jest poprawnym numerem VIN"
            else printfn "Ten numer nie jest numerem VIN"
        else    
            let a = dopasuj vin.VIN i 
            let b = dopasuj vin.VIN (17-i)
            let suma2 = suma + a * waga.[i] + b * waga.[17-i]
            walidator vin (i+1) suma2
    walidator vin 0 0
    
//zad 2.13

let trojkat_obwod (a, b, c) = 
    if a > b && a > c then
        let suma = b + c
        if a >= suma then
            (0.0, false)
        else
        let obwod = a + b + c  
        (obwod, true)
    elif b > a && b > c then
        let suma = a + c
        if b >= suma then
            (0.0, false)
        else
        let obwod = a + b + c  
        (obwod, true)
    elif c > a && c > b then
        let suma = a + b
        if c >= suma then
            (0.0, false)
        else
        let obwod = a + b + c  
        (obwod, true)
    else
        (0.0, false)

let trojkat_pole (a, b, c) = 
    let p = (a+b+c)/2.0
    if a > b && a > c then
        let suma = b + c
        if a >= suma then
            (0.0, false)
        else
        let pole = Math.Sqrt(p*(p-a)*(p-b)*(p-c))
        let obwod = a + b + c  
        (pole, true)
    elif b > a && b > c then
        let suma = a + c
        if b >= suma then
            (0.0, false)
        else
        let pole = Math.Sqrt(p*(p-a)*(p-b)*(p-c))
        (pole, true)
    elif c > a && c > b then
        let suma = a + b
        if c >= suma then
            (0.0, false)
        else
        let pole = Math.Sqrt(p*(p-a)*(p-b)*(p-c))
        (pole, true)
    else
        (0.0, false)

let wynik_trojkata wynik =
    match wynik with
    | (x, true) -> printf "Pole wynosi: %A"x
    | (_,false) -> printf "Nie można policzyć obwodu"

let wynik_trojkata2 wynik =
    match wynik with
    | (x, true) -> printf " a obwód wynosi: %A"x
    | (_,false) -> printf " Nie można policzyć pola"

//zad 2.14



//zad 2.15

type Osoba = {
    imie:string;
    nazwisko:string;
    wiek:int;
}

let printMenu () =
    Console.Clear();
    printfn "1 - Dodaj rekord"
    printfn "2 - Edytuj rekord"
    printfn "3 - Pokaz rekord"
    printfn "4 - koniec"

let wczytaj komunikat = 
    printfn "%s" komunikat
    Console.ReadLine()

let pobierzRekord () = 
    let imie = wczytaj "Podaj imię: "
    let nazwisko = wczytaj "Podaj nazwisko: "
    let wiek = int (wczytaj "Podaj wiek: ")
    {imie = imie; nazwisko = nazwisko; wiek = wiek}

let edytujRekord osoba =

    let nowaOsoba = pobierzRekord ()

    { 
      imie = if String.IsNullOrWhiteSpace nowaOsoba.imie then osoba.imie else nowaOsoba.imie;
      nazwisko = if String.IsNullOrWhiteSpace nowaOsoba.nazwisko then osoba.nazwisko else nowaOsoba.nazwisko;
      wiek = if nowaOsoba.wiek = 0 then osoba.wiek else nowaOsoba.wiek;
    }

let pokazRekord osoba =
    printfn "%s" osoba.imie
    printfn "%s" osoba.nazwisko
    printfn "%d" osoba.wiek
    Console.ReadKey() |> ignore
  
let rec menu osoba kont =
    if kont then
        printMenu ()
        let klawisz = Console.ReadKey ()
        let osoba,kontynuuj = 
            match klawisz.Key with
            | ConsoleKey.D1 -> pobierzRekord (), true
            | ConsoleKey.D2 -> edytujRekord osoba, true
            | ConsoleKey.D3 -> pokazRekord osoba; osoba, true
            | ConsoleKey.D4 -> osoba, false
            | _ -> osoba, true
        menu osoba kontynuuj

[<EntryPoint>]
let main argv =
    
    printfn "Zad 2.1"
    wybor()
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

    printfn "Zad 2.2"
    let para = para()
    match para with 
    | (a,b) when a > b -> printfn "Pierwsza liczba jest większa jak druga"
    | (a,b) when b > a -> printfn "Druga liczba jest większa jak pierwsza"
    | (_,_) -> printfn "Obie liczby są równe"
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()
    
    printfn "Zad 2.3"
    let a = 3.0
    let b = 3.0
    let c = 3.0
    let wynik = trojkat a b c
    printfn "Pole trojkata o bokach %.2f, %.2f i %.2f to %.2f, a jego obwod to %.2f" a b c (fst wynik) (snd wynik) 
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

    printfn "Zad 2.4"
    printfn "Podaj adres email: "
    let adres = Console.ReadLine()
    let para = (email adres)
    printfn "Identyfikator żytkownika to %s" (fst para)
    printfn "Adres domeny to %s" (snd para)
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

    printfn "Zad 2.5"
    printfn "Podaj adres email: "
    //let adress = Console.ReadLine()
    sufix (email adres)
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

    printfn "Zad 2.6"
    let A = (1.0, 1.0, 1.0)
    let B = (2.0, 2.0, 2.0)
    printfn "Odleglosc od siebie punktow %s i %s wynosi %f" (string(A)) (string(B)) (Euklides A B)
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

    printfn "Zad 2.7"
    let srodek = (2.0, 1.0)
    let promien = 1.0
    let punkt = (1.0, 1.0)
    wnetrze srodek promien punkt
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

    printfn "Zad 2.8"
    let ulamekA = (2, 3)
    let ulamekB = (5, 7)
    printfn "Wyniki dodawania ułamków %A" (dodaj ulamekA ulamekB)
    printfn "Wyniki dodawania ułamków %A" (odejmij ulamekA ulamekB)
    printfn "Wyniki mnożenia ułamków %A" (pomnoz ulamekA ulamekB)
    printfn "Wyniki dzielenia ułamków %A" (podziel ulamekA ulamekB)
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

    printfn "Zad 2.9"
    let ulamek1 = {licznik = 2; mianownik = 3}
    let ulamek2 = {licznik = 5; mianownik = 7}
    printfn "Wyniki dodawania ułamków %A" (dodwanie ulamek1 ulamek2)
    printfn "Wyniki dodawania ułamków %A" (odejmowanie ulamek1 ulamek2)
    printfn "Wyniki mnożenia ułamków %A" (mnozenie ulamek1 ulamek2)
    printfn "Wyniki dzielenia ułamków %A" (dzielenie ulamek1 ulamek2)
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

    printfn "Zad 2.10"

    printfn "Sprawdzanie dnia tygodnia:\nPodaj dzień miesiąca:"
    let dd = int(Console.ReadLine())
    printfn "Podaj miesiac:"
    let mm = int(Console.ReadLine())
    printfn "Podaj rok:"
    let rrrr = int(Console.ReadLine())
    let a = {dzien=dd; miesiac=mm; rok=rrrr}
    printfn "%s" (dzien a)
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

    printfn "Zad 2.11"
    printfn "Dzielenie liczb.\nPodaj dzielną: "
    let a = float (Console.ReadLine ())
    printfn "Podaj dzielnik: "
    let b = float (Console.ReadLine ())
    let wynik = dzielenie_zad11 a b
    (wynik_dzielenia wynik)
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

    printfn "Zad 2.12"
    let nrVIN = "2B3CL3CG9BH518087"
    walidacja (dekompozycja nrVIN)
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

    printfn "Zad 2.13"
    printfn "Podaj długości boków trójkąta: "
    let a = float (Console.ReadLine ())
    let b = float (Console.ReadLine ())
    let c = float (Console.ReadLine ())
    let wynik1 = trojkat_pole (a, b, c)
    let wynik2 = trojkat_obwod (a, b, c)
    (wynik_trojkata wynik1)
    (wynik_trojkata2 wynik2)
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

    printfn "Zad 2.14"
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

    printfn "Zad 2.15"
    menu {imie="";nazwisko="";wiek=0} true
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

    0 


