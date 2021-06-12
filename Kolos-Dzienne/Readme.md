# Zadania z kolosa dziennych

### ZADANIE 1
Zdefiniuj nowy typ danych reprezentujący drzewo binarne. Następnie napisz program, 
który wyświetli elementy tego drzewa w kolejności postorder. Zademonstruj jego działanie. 

Postorder: Najpierw wszystkie dzieci (lewe i prawe poddrzewo), potem rodzic (korzeń)

```
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
```


### ZADANIE 2
Zdefiniuj nowy typ danych reprezentujący drzewo binarne. Następnie napisz program, 
który wyświetli elementy tego drzewa w kolejności inorder. Zademonstruj jego działanie. 

Inorder: najpierw skrajne lewe poddrzewo, potem korzeń, potem skrajne prawe poddrzewo 

```
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
```

### ZADANIE 3
Zdefiniuj nowy typ danych reprezentujący drzewo binarne. Następnie napisz funkcję, 
która określi czy wszystkie węzły drzewa spełniają warunek określony jako parametr. Zademonstruj jej działanie. 

```
let rec czyWezlySpelniajaWarunek predykat = function
| Puste -> false
| Wezel(x, l, p) when predykat(x) = false -> false
| Wezel(_, l, p) -> 
    czyWezlySpelniajaWarunek predykat l |> ignore 
    czyWezlySpelniajaWarunek predykat p |> ignore
    true
```

### ZADANIE 4
Zdefiniuj nowy typ danych reprezentujący drzewo binarne. Następnie napisz funkcję, która policzy sumę wartości 
w węzłach spełniających warunek  określony w funkcji przekazanej jako parametr. Zademonstruj jej działanie. 

```
let rec sumaWezlowZWarunkiem predykat = function
  | Puste -> 0
  | Wezel(x, l, p) when predykat(x) -> x + (sumaWezlowZWarunkiem predykat l) + (sumaWezlowZWarunkiem predykat p)
  | Wezel(_, l, p) -> (sumaWezlowZWarunkiem predykat l) + (sumaWezlowZWarunkiem predykat p)
```

### ZADANIE 5

Napisz program, który wczytując od użytkownika liczby z klawiatury zapamięta liczby parzyste. 
(Wprowadzanie zakończ, jeżeli użytkownik poda 0). Po zakończeniu wprowadzania danych 
program powinien wyświetlić je w odwrotnej kolejności (od ostatniej wprowadzonej do pierwszej). 

```
let bezpiecznePodawanieLiczby =
    try 
        Some(int(Console.ReadLine()))
    with
        | :? System.FormatException ->  None


let podajLiczbe () = 
    let lista: int list = []

    let rec dodajLiczbe lista =   
        Console.Write "Podaj liczbe: "

        let v = bezpiecznePodawanieLiczby

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
```

### ZADANIE 6
Zdefiniuj nowy typ Lista reprezentujący listę łączoną mogącą przechowywać wartości dowolnego typu.. 
Następnie napisz funkcję, która policzy ile wartości dodatnich jest na tej liście. Napisz również kod, 
który demonstruje działanie przygotowanej funkcji. 

```
let rec tylkoDodatnie = function
| Pusta -> 0
| Element(glowa, ogon) when glowa > 0 -> 1 + tylkoDodatnie ogon
| Element(_, ogon) -> tylkoDodatnie ogon
```

### ZADANIE 7
Napisz funkcję, która obliczy wartość wariancji bez powtórzeń zgodnie ze wzorem n!/(n-k)! 

```
let wariancjeBezPowtorzen n k = 
    let licznik = silnia n
    let mianownik = silnia (n - k)

    licznik / mianownik
```

### ZADANIE 8
Napisz program, który utworzy listę 100 dwuwymiarowych punktów losowych z przedziału od -20 do 20. 
Następnie określ, odległość euklidesową każdego z tych punktów od początku układu współrzędnych. 
Wykorzystaj funkcje modułu List.

```
let okreslOdlegloscPunktow n =
    let gll = new Random()

    List.init n (fun i -> (gll.Next(-20, 21), gll.Next(-20, 21)))
    |> List.map (fun (x, y) -> (x, y, sqrt (((float) x)**2.0 + ((float)y)**2.0)))
    |> List.iter (fun (x, y, v) -> printfn "Odleglosc punktu (%d, %d) od (0,0): %.2f" x y v)
```

### ZADANIE 9
Napisz funkcję, która określi ile razy w danym łańcuchu znaków wystąpiły dowolne cyfry. 

```
let countDigitInString (str: string) = 
    let occurences = 
        Seq.toList str
        |> List.ofSeq
        |> List.map (fun v -> (((int) v) - 48))
        |> List.filter (fun v -> v >= 0 && v <= 9)
        |> List.fold (fun acc v -> acc + 1) 0 

    printfn "Ilosc wystapien cyfr w '%s': %d" str occurences
```

### ZADANIE 10
Napisz funkcję, która pozwoli obliczyć silnię w sposób bezpieczny 
tzn. jeżeli wartość dla której mamy obliczyć silnię jest większa lub równa zero, 
to zwracamy wynik. Jeżeli będzie mniejsza od zera to zwracamy komunikat "Nie mogę policzyć silni z liczby ujemnej". 
Zaimplementuj tę funkcję wykorzystując najlepszy wg. siebie typ danych. 

```
type BezpiecznyWynik = 
| Poprawny of int
| Błędny
    
    
let bezpiecznaSilnia n =
    if n < 0 then
        Błędny
    else
        Poprawny (silnia n)
```
### ZADANIE 11
Napisz funkcję, która na podstawie określonej liczby rzutów trzema kostkami 
obliczy prawdopodobieństwo wyrzucenia kombinacji 1,2,3. Liczbę powtórzeń podaj jako parametr funkcji

```
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
```

### ZADANIE 12
Napisz funkcję, która przyjmie parę liczb i określi, która z nich jest większa 
po podniesieniu do kwadratu np. wywołanie wieksza (-5, 4) powinno zwrócić -5 

```
let ktoraWieksza (x,y) =
    if (x**2.0 > y**2.0) then
       x
    else
       y
```

### ZADANIE 13
Napisz funkcję rekurencyjną, która określi ile razy w danym łańcuchu znaków wystąpiła każda z cyfr. 
Jeżeli jakaś cyfra nie występuje w tekście jej wartość w wynikach powinna zawierać 0 

```
let liczZnaki slowo =
    let rec liczZnak (slowo:string) (znak:char) i suma = 
        if i=slowo.Length then
            suma
        else
            if (slowo.[i] = znak) then
                liczZnak slowo znak (i+1) (suma+1)
            else
                liczZnak slowo znak (i+1) suma
    

    [for i in 48..57 -> liczZnak slowo ((char)i) 0 0]
```

### ZADANIE 14
Zdefiniuj nowy typ danych, który będzie reprezentował liczbę zespoloną. 
Następnie napisz funkcję pozwalającą dodawać te liczby wg. wzoru: (a1+b1i)+(a2+b2i) = (a1+a2)+(b1+b2)*i

```
type Zespolone = {
    rzeczywista: float;
    urojona: float;
}

let dodaj zespolona1 zespolona2 = 
    let a = zespolona1.rzeczywista + zespolona2.rzeczywista
    let b = zespolona1.urojona + zespolona2.urojona
    {rzeczywista=a; urojona=b}
```
