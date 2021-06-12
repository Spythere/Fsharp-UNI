# Laboratorium 1

### 1.1
Napisz funkcję obliczającą pole koła
```
let pole r = Math.PI*r**2.0

printfn "Zad 1.1"
    let r = 2.0
    let x = pole r
    printfn "Dla promienia %.2f pole wynosi %.2f" r x 
```
### 1.2
Napisz funkcję wyznaczającą wartość pierwiastków równania kwadratowego
```
let pierwiastki a b c =
    printf "Funkcja %.2fx^2 + %.2fx + %.2f " a b c
    let delta = b**2.0 - 4.0*a*c
    if a = 0.0 then
        printf "nie jest funkcja kwadratowa"
    if delta < 0.0 then
        printfn "nie ma pierwiastkow."
    elif delta = 0.0 then
        let x = -b/(2.0*a)
        printfn " ma jeden pierwiastek x = %.2f." x
    else
        let x1 = (-b + delta)/(2.0*a)
        let x2 = (-b - delta)/(2.0*a)
        printfn " ma dwa pierwiastek x1 = %.2f i x2 = %.2f." x1 x2

printfn "\nZad 1.2"
    pierwiastki 1.0 2.0 3.5
```
### 1.3
Napisz funkcję, która sprawdzi czy z trzech podanych wartości rzeczywistych,
da się zbudowaćtrójkąt.
```
let trojkat a b c =
    if a <> 0.0 && b <> 0.0 && c <> 0.0 && a < b + c && b < a + c && c < a + b then
        true
    else 
        false

printfn "\nZad 1.3"
    let a = 1.2
    let b = 3.6
    let c = 4.0
    printf "Z odcinkow o dlugosci %.2f, %.2f i %.2f " a b c
    if trojkat a b c then 
        printfn "można stworzyć trojkat."
    else
        printfn "nie można stworzyć trojkata."
```
### 1.4
Napisz funkcję, która obliczy pole trójkąta, na podstawie długości jego boków.
Jeżeli z podanych wartości nie da się zbudować trójkąta rzuć wyjątek z odpowiednią
wiadomością.
```
let poleTrojkata a b c = 
    if trojkat a b c then 
        let p = (a + b + c) / 2.0
        let P = sqrt(p * (p - a) * (p - b) * (p - c))
        printfn "Pole tego trojkata wynosi %.2f" P
    else 
        printfn "Z odcinkow o dlugosci %.2f, %.2f i %.2f nie można stworzyć trojkata." a b c

printfn "\nZad 1.4"
    poleTrojkata a b c
```
### 1.5
funkcję, która rekurencyjnie oblicza sumę n pierwszych liczb naturalnych
```
let rec suma n =
    if n <= 0 then
        failwith "Nieprawidlowe dane wejsciowe"
    else
        let rec sum n a =
            if n = 1 then
                a
            else sum (n-1) (n+a)
        sum n 1

printfn "\nZad 1.5"
    let n = 7
    printfn "Suma %d pierwszych liczb naturalnych to %d" n (suma n)
```
### 1.6
Napisz funkcję, która rekurencyjnie wyznaczy wartość x”, gdzie x i n są
dowolnymi liczbami naturalnymi.
```
let rec potega x n =
    if n <= 0 then
        failwith "Nieprawidlowe dane wejsciowe."
    else
        let rec pot x n a =
            if n = 1 then
                a
            else pot x (n-1) (x*a)
        pot x n x

printfn "\nZad 1.6"
    let x = 2
    let n = 10
    printfn "%d do potegi %d wynosi %d" x n (potega x n)
```
### 1.7
Napisz funkcję, wyznaczającą wartość n-tego elementu ciągu fibbonaciego
```
let rec fib n =
    if n < 0 then
        failwith "Nieprawidlowe dane wejsciowe."
    else
        let rec nfib n a b =
            if n = 0 then
                a
            elif n = 1 then
                b
            else 
                nfib  (n - 1) b (a + b)
        nfib n 0 1

printfn "\nZad 1.7"
    let n = 10
    printfn "%d element ciagu Fibonacciego to %d" n (fib n)
```
### 1.8
Napisz funkcję, która dla dowolnych liczb całkowitych n i k obliczy wartość
dwumianu Newtona: („_ "m _) zdefiniowanego w sposób rekurencyjny:

```
let rec dwumianNewtona n k =
    if n < 0 || k < 0 || k > n then 
        failwith "Nieprawidlowe dane wejsciowe."
    elif k = 0 || k = n then
        1
    else
        dwumianNewtona (n - 1) (k - 1) + dwumianNewtona (n - 1) k

printfn "\nZad 1.8"
    let n = 4
    let k = 2
    printfn "Dwumian Newtona dla n = %d i k = %d wynosi %d" n k (dwumianNewtona n k)
```
### 1.9
Napisz funkcję, która rekurencyjnie sprawdza, czy dana liczba jest liczbą
pierwszą.
```
let rec czyPierwsza n = 
    if n < 1 then
        failwith "Nieprawidlowe dane wejsciowe."
    else   
        let rec czyPierwszaTR a test=
            if (not test) then 
                false
            elif a <= 1 then 
                true
            else czyPierwszaTR (a - 1) (n % a <> 0 && test)
        czyPierwszaTR (n/2) true

printfn "\nZad 1.9"
    let n = 73
    if (czyPierwsza n) then
        printfn "Liczba %d jest liczbą pierwsza" n
    else 
        printfn "Liczba %d nie jest liczbą pierwsza" n
```
### 1.10
Napisz funkcję, która na podstawie 1000 rzutów kostką do gry określi
prawdopodobieństwo wyrzucenia szóstki
```
let rec szansaNa6 () = 
    let gll = new Random()
    let rec szansa i suma = 
        if i = 1000 then
            (float)suma/1000.0
        else
            let rzut = gll.Next(6) + 1
            if rzut = 6 then 
                szansa (i + 1) (suma + 1)
            else
                szansa (i + 1) suma
    szansa 0 0

printfn "\nZad 1.10"
printfn "Prwadopodobenstwo wyrzucenia 6 przy rzucie kostka wynosi: %.3f" (szansaNa6 ())
```
### 1.11
Napisz funkcję, która na podstawie 1000 rzutów dwiema kostkami do gry
określi prawdopodobieństwo wyrzucenia dwóch szóstek
```
let rec szansaNa66 () = 
    let gll = new Random()
    let rec szansa i suma = 
        if i = 1000 then
            (float)suma/1000.0
        else
            let rzut1 = gll.Next(6) + 1
            let rzut2 = gll.Next(6) + 1
            if rzut1 = 6 && rzut2 = 6 then 
                szansa (i + 1) (suma + 1)
            else
                szansa (i + 1) suma
    szansa 0 0

printfn "\nZad 1.11"
printfn "Prwadopodobenstwo wyrzucenia dwoch 6 przy rzucie dwiema kostkami wynosi: %.3f" (szansaNa66 ())
```
### 1.12
Napisz funkcję określającą największy wspólny dzielnik
```
let rec NWD a b =
    if b<>0 then
        NWD b (a%b)
    else a

printfn "\nZad 1.12"
let a = 641424
let b = 784931
printfn "Największy wspolny dzielnik liczb %d i %d to %d" a b (NWD a b)

```
### 1.13
Oblicz przybliżoną wartość szeregu nieskończonego. Zatrzymaj obliczenia
za kiedy wartość bezwzględna kolejnego elementu w szeregu będzie mniejsza niż ustalona
dokładnośće (np. e = 10 do -7):
```
//Funkcja pomocnicza
let rec silnia n =
    let rec sil n acc =
        if n = 0I then
            acc
        else
            sil (n-1I) (n*acc)
    sil n 1I

let rec wartosciSzeregow () =
    let rec wartosc1 () =    
        let e = 10.0**(-7.0)
        let rec szereg1 i suma = 
            let element = 1.0/(((float)i)**2.0)
            if element < e then
                suma
            else
                szereg1 (i + 1) (suma + element)
        szereg1 1 0.0
    let rec wartosc2 () =        
        let e = 10.0**(-7.0)
        let rec szereg2 i suma = 
            let element = ((-1.0)**(float)i)/(float)(Funkcje.silnia i)
            if element < e && element > -e then
                suma
            else
                szereg2 (i + 1I) ((float)suma + (float)element)
        szereg2 1I 0.0
    let rec wartosc3 () =
        let e = 10.0**(-7.0)
        let rec szereg3 i suma =
            let element = 1.0/(((float)i)*((float)i+1.0))
            if element < e then
                suma
            else
                szereg3 (i + 1) (suma + element)
        szereg3 1 0.0
    let rec wartosc4 () =    
        let e = 10.0**(-7.0)
        let rec szereg4 i suma =
            let element = ((-2.0)**(float)i) / (float)(Funkcje.silnia i)
            if element < e && element > -e then
                suma
            else
                szereg4 (i + 1I) (suma + element)
        szereg4 1I 0.0

    printfn "Wartosc szeregu 1 / i^2 wynosi: %A" (wartosc1 ())
    printfn "Wartosc szeregu (-1)^i / i! wynosi: %A" (wartosc2 ())
    printfn "Wartosc szeregu 1 / (i * (i + 1)) wynosi: %A" (wartosc3 ())
    printfn "Wartosc szeregu (-2.0)^i / i! wynosi: %A" (wartosc4 ())

printfn "\nZad 1.13"
wartosciSzeregow ()
```
### 1.14
Napisz powyższe funkcje z wykorzystaniem rekurencji ogonowej
```
```
### 1.15
Napisz funkcję zamieniającą termperaturę wyrażoną w stopniach Celsjusza
na temperaturę wyrażoną w stopniach Fahrenheita zgodnie z poniższym wzorem:

TFahrenheit = 32 + 9/5 ' TCelsius

Zdefiniuj wymagane jednostki miary oraz odpowiedni konwerter.
```
[<Measure>] type C 
[<Measure>] type F
let zmianaCF:float<F*C^-1> = 1.8<F/C>
let konwersjaCF(C:float<C>) = (32.0<F>) + zmianaCF * C

printfn "\nZad 1.15"
let temperaturaC = 41.0<C>
let temperaturaF = konwersjaCF(temperaturaC)
printfn "Temperatura %.2fC w stopniach Farenhaita to %.2f" temperaturaC temperaturaF
    
```
### 1.16
Napisz funkcję dokonującą odwrotnej transformacji.
```
let zmianaFC:float<C*F^-1> = 5.0/9.0<F/C>
let konwersjaFC(F:float<F>) = (F - 32.0<F>) * zmianaFC

printfn "\nZad 1.16"
let TempF = 120.0<F>
let TempC = konwersjaFC(TempF)
printfn "Temperatura %.2fF w stopniach Celcjusza to %.2f" TempF TempC
```
### 1.17
Napisz funkcję, która sprawdzi czy dane słowo jest palindromem
```
let rec palindrom str = 
    let rec palindromLicznik (str:string) i k = 
        if i = k then 
            if str.[i] = str.[k] then true
            else false
        elif i > k then true
        elif str.[i] = str.[k] then palindromLicznik str (i+1) (k-1)
        else false
    palindromLicznik str 0 (str.Length-1)

printfn "\nZad 1.17"
let wyraz = "anna"
printf "Wyraz %s " wyraz
if not (palindrom wyraz) then printf "nie "
printfn "jest palindromem"

```
### 1.18
Napisz funkcję, która policzy ile razy w podanym tekście wystąpił okreslony znak
```
let rec licznikZnakow tekst znak = 
    let rec licznik (tekst:string) (znak:char) suma i = 
        if i = tekst.Length then suma
        elif tekst.[i] = znak then licznik tekst znak (suma + 1) (i + 1)
        else licznik tekst znak suma (i + 1)
    licznik tekst znak 0 0


printfn "\nZad 1.18
let tekst = "1. Proba zliczenia 5423 z 26436. Zadzwon jak sie skonczy 123456789"
printfn "%s" tekst
printfn "Tekst ma %d znakow '%c'" (licznikZnakow tekst 'a') 'a'
printfn "Tekst ma %d wyrazow" (licznikWyrazow tekst)
printfn "Tekst ma %d cyfr w ciągach" (licznikLiczbWCiagachLiczbowych tekst)
printfn "Najdluzszy ciag ma %d cyfr" (najdluzszyCiagLiczbowy tekst)
```
### 1.19
Napisz funkcję, która określi liczbę wyrazów w podanym tekście
```
let licznikWyrazow (tekst:string) = 
    let slowa = tekst.Split([|' '; '\n'|])
    let ile = slowa.Length
    ile

printfn "\nZad 1.19
let tekst = "1. Proba zliczenia 5423 z 26436. Zadzwon jak sie skonczy 123456789"
printfn "%s" tekst
printfn "Tekst ma %d znakow '%c'" (licznikZnakow tekst 'a') 'a'
printfn "Tekst ma %d wyrazow" (licznikWyrazow tekst)
printfn "Tekst ma %d cyfr w ciągach" (licznikLiczbWCiagachLiczbowych tekst)
printfn "Najdluzszy ciag ma %d cyfr" (najdluzszyCiagLiczbowy tekst)
```
### 1.20
Napisz funkcję, która określi liczbę cyfr występujących pod rząd w tekście.
```
let licznikLiczbWCiagachLiczbowych tekst = 
    let rec licznik (tekst:string) (suma:int) dlugosc i =
        if i = tekst.Length && dlugosc > 1 then suma + dlugosc
        elif i = tekst.Length && dlugosc <= 1 then suma
        else
            if tekst.[i] = '0' || tekst.[i] = '1' || tekst.[i] = '2' || tekst.[i] = '3' 
                || tekst.[i] = '4' || tekst.[i] = '5'|| tekst.[i] = '6' 
                || tekst.[i] = '7' || tekst.[i] = '8' || tekst.[i] = '9' then 
                    licznik tekst suma (dlugosc + 1) (i + 1)
            elif dlugosc > 1 then licznik tekst (suma + dlugosc) 0 (i+1)
            else licznik tekst suma 0 (i+1)
    licznik tekst 0 0 0

let najdluzszyCiagLiczbowy tekst = 
    let rec licznik (tekst:string) (najdluzszy:int) dlugosc i =
        if i = tekst.Length && dlugosc > najdluzszy then dlugosc
        elif i = tekst.Length && dlugosc <= najdluzszy then najdluzszy
        else
            if tekst.[i] = '0' || tekst.[i] = '1' || tekst.[i] = '2' || tekst.[i] = '3' 
                || tekst.[i] = '4' || tekst.[i] = '5'|| tekst.[i] = '6'
                || tekst.[i] = '7' || tekst.[i] = '8' || tekst.[i] = '9' then 
                    if dlugosc < najdluzszy then licznik tekst najdluzszy (dlugosc + 1) (i + 1) 
                    else licznik tekst (dlugosc + 1) (dlugosc + 1) (i + 1)
            else licznik tekst najdluzszy 0 (i + 1)
    licznik tekst 0 0 0


printfn "\nZad 1.20
let tekst = "1. Proba zliczenia 5423 z 26436. Zadzwon jak sie skonczy 123456789"
printfn "%s" tekst
printfn "Tekst ma %d znakow '%c'" (licznikZnakow tekst 'a') 'a'
printfn "Tekst ma %d wyrazow" (licznikWyrazow tekst)
printfn "Tekst ma %d cyfr w ciągach" (licznikLiczbWCiagachLiczbowych tekst)
printfn "Najdluzszy ciag ma %d cyfr" (najdluzszyCiagLiczbowy tekst)
```
### 1.21
Napisz aplikację konsolową, który pozwoli użytkownikowi na wprowadzenie
imienia 1i nazwiska iI wyświetli komunikat "Witaj <imie i nazwisko>"
```


```
### 1.22
Napisz aplikację konsolową, który pozwoli użytkownikowi na wprowadzenie
roku. W odpowiedzi program powinien wyświetlić informacje czy jest to rok
przestępnyczy nie.
```

```
### 1.23
Napisz aplikację konsolową, która na podstawie podanych użytkownika3
liczb rzeczywistych wyświetli na ekranie komunikat, czy da się z nich utworzyćtrójkąt
równoboczny, równoramienny, prostokątny, dowolny inny lub że z podanych wartości nie
da się Uworzyć trójkąta.
```
let prostokatny a b c =
    if a**2.0 + b**2.0 = c**2.0 || b**2.0 + c**2.0 = a**2.0 || c**2.0 + a**2.0 = b**2.0 
    then true else false

let rownoboczny a b c = 
    if  a = b && a = c then true else false

let rownoramienny a b c = 
    if  a = b || b = c || c = a then true else false

    printfn "\nZad 1.23"
    printfn "Sprwadzenie rodzaju trojkata\nPodaj trzy dlugosci bokow: "
    let a = float(Console.ReadLine())
    let b = float(Console.ReadLine())
    let c = float(Console.ReadLine())
    if trojkat a b c then
        if rownoboczny a b c then 
            printfn "Trojkat o bokach %.2f %.2f %.2f jest rownoboczny" a b c
        elif rownoramienny a b c then 
            printf "Trojkat o bokach %.2f %.2f %.2f jest rownoramienny" a b c
            if prostokatny a b c then 
                printfn "m trojkatem prostokatnym"
            else printfn ""
        elif prostokatny a b c then 
            printfn "Trojkat o bokach %.2f %.2f %.2f jest prostokatny" a b c
        else 
            printfn "Trojkat o bokach %.2f %.2f %.2f jest trojkatem dowolnym" a b c
    else 
        printfn "Z dlugosci %.2f %.2f %.2f odcinkow nie na sie utworzyc trojkata" a b c
```
### 1.24
Napisz aplikację konsolową, która przyjmie od użytkownikaciąg 11 cyfr.
Sprawdź, czy cyfry te tworzą poprawny numer PESEL, oraz na jego podstawie wyświetl
informacje o« dacie urodzenia danej osoby oraz czy jest to kobieta czy mężczyzna.
```
let dataPesel (pesel:string) = 
    let rok = int(pesel.[0..1])
    let miesiac = int(pesel.[2..3])
    let dzien = int(pesel.[4..5])
    if  dzien = 29 && miesiac % 20 = 2 && rok % 4 = 0 && rok <> 0 || int(pesel.[0..5]) = 229 then true 
    elif dzien >= 1 && dzien <= 28 && miesiac % 20 = 2 then true
    elif dzien >= 1 && dzien <= 30 && (miesiac % 20 = 4 || miesiac % 20 = 6 || miesiac % 20 = 9 
        || miesiac % 20 = 11) then true
    elif dzien >= 1 && dzien <= 31 && (miesiac % 20 = 1 || miesiac % 20 = 3 || miesiac % 20 = 5 
        || miesiac % 20 = 7 || miesiac % 20 = 8 || miesiac % 20 = 10 || miesiac % 20 = 12) then true
    else false 

let sumaKontrolna (pesel:string) =
    let sumaWazona = 
        int(pesel.[0]) * 1 + int(pesel.[1]) * 3 + int(pesel.[2]) * 7 
        + int(pesel.[3]) * 9 + int(pesel.[4]) * 1 + int(pesel.[5]) * 3 
        + int(pesel.[6]) * 7 + int(pesel.[7]) * 9 + int(pesel.[8]) * 1 + int(pesel.[9]) * 3
    if sumaWazona = int(pesel.[10]) then true else false

let dlugoscPesel (pesel:string) = 
    if pesel.Length = 11 then true else false

let czyPesel (pesel:string) =
    if pesel.Length = 11 then
        if (dataPesel pesel) && (sumaKontrolna pesel) then true else false
    else false


printfn "\nZad 1.24"
printfn "Podaj liczbe aby sprawdzic czy jest to PESEL"
let pesel = Console.ReadLine()
if (czyPesel pesel) then printfn "Liczba %s to PESEL" pesel else printfn "Liczba %s to nie PESEL" pesel
```
### 1.25
Napisz aplikację konsolową, która przyjmie od użytkownika dowolnytekst,
1 wyświetli go zakodowanego zgodnie z szyfrem Cezara
```
let szyfruj (tekst:string) kod =
    let zaszyfrowane = String.init tekst.Length ( fun i -> sprintf"%c" ( if int(tekst.[i]) - kod > 127 then char(int(tekst.[i]) + kod - 128) else char(int(tekst.[i]) + kod) ) )
    zaszyfrowane

    printfn "\nZad 1.25"
    printfn "Wpisz tekst do zakodowania"
    let tekst = Console.ReadLine()
    printfn "Podaj kod szyfru (0 - 26)"
    let kod = Console.ReadLine()
    if int(kod) < int("0") && int(kod) > int("26") then printfn "Nieprawidlowy kod" else
        let KOD = int(kod)
        printfn "Zaszyfrowany tekst to"
        let zaszyfrowanyTekst = szyfruj tekst KOD
        printfn "%s" (zaszyfrowanyTekst)
```
### 1.26
Napisz aplikację konsolową, która przyjmie od użytkownika dowolny tekst
zakodowany zgodnie z szyfrem Cezara, i odkoduje go.
```
let deszyfruj (tekst:string) kod =
    let odszyfrowane = String.init tekst.Length ( fun i -> sprintf"%c" ( if int(tekst.[i]) - kod < 0 then char(int(tekst.[i]) - kod + 128) else char(int(tekst.[i]) - kod) ) )
    odszyfrowane

    printfn "\nZad 1.26"
    printfn "Wpisz zakodowany tekst"
    let tekst = Console.ReadLine()
    printfn "Podaj kod szyfru (0 - 26)"
    let kod = Console.ReadLine()
    if int(kod) < int("0") && int(kod) > int("26") then printfn "Nieprawidlowy kod" else
        let KOD = int(kod)
        printfn "Odszyfrowany tekst to"
        printfn "%s" (deszyfruj tekst KOD)

```
### 1.27
Napisz aplikację konsolową, która wczytuje liczbę całkowitą określającą
liczbę minut od północy. Program powinien wyświetlać w odpowiedzi konkretną godzinę,
która będzie odpowiadaćtej liczbie. Jeżeli podana liczba godzin będzie przekraczała liczbę
godzin wciągu doby na ekranie powinien pojawić się stosowny komunikat

```

```
### 1.28
Napisz aplikację konsolową, która wczytuje liczbę całkowitą określającą
liczbę minut do startu. Program w odpowiedzi powinien wyświetlać komunikaty: "do
startu pozostało ... minut". Jeżeli odliczanie dojdzie do zera powinien wyświetlić się
odpowiedni komunikat. Odpowiednią funkcję proszę umieścić w osobnym pliku.
```
let rec odliczanie (czas:int) =
    if czas < 0 then 
        printfn "Nieprawidlawa wartosc"
    elif czas = 0 then 
        printfn "START" 
    else 
        printfn "Do startu pozostalo %d minut" czas
        odliczanie (czas - 1)

printfn "\nZad 1.28"
printfn "Podaj ile minut do starty rakiety"
let czas = int(Console.ReadLine())
Funkcje.odliczanie czas

```

### 1.29
Napisz aplikację konsolową, która rozwiązuje problem Collatza. Program
powinien wczytywać od użytkownikaliczbę całkowitą dodatnią. Na jej podstawie
obliczamy kolejne wartości:

TU OBRAZEK

gdzie Co jest wartością wprowadzoną przez użytkownika. Przypuszczasię, że od nie ważne
od jakiej liczby zaczniemy, to osiągniemy wartość O.
Napisz program, który wyświetli kolejne wyrazy tego ciągu wraz z ich numerem. Np.
jeżeli zaczniemy od wartości 5 na ekranie powinniśmy zobaczyć:
1.5
2.16
3.8
4.4
5.3
6.1
```
let rec Collatz liczba =
    let rec collatz liczba i =
        if liczba < 1 then printfn "Nieprawidlawa wartosc"
        elif liczba > (2147483647 - 1) / 3 then printfn "Obliczenia niemozliwe z powodu ograniczen typu int"
        elif liczba = 1 then printfn "%d. %d\n" i liczba
        elif liczba % 2 = 0 then 
            printf "%d. %d\n" i liczba
            collatz (int(liczba/2)) (i + 1)
        else 
            printf "%d. %d\n" i liczba
            collatz (3 * liczba + 1) (i + 1)
    collatz liczba 0

printfn "\nZad 1.29"
printfn "Podaj pierwszy element ciagu Collatza"
let wartosc = int(Console.ReadLine())
Funkcje.Collatz wartosc
```
### 1.30
Napisz aplikację konsolową, która pozwoli na wczytywanie z klawiatury
liczb całkowitych. Oblicz średnią tych liczb. Użytkownik powinien wprowadzać wartości
dopóki nie poda wartości ujemnej. Odpowiednią funkcję proszę umieścić w osobnym
pliku.
```
let rec srednia () =   
    let rec Srednia n suma srednia =
        let liczba = int(Console.ReadLine())
        if liczba < 0 then srednia
        else
            Srednia (n + 1) (suma + liczba) (float(suma)/float(n))
    Srednia 1 0 0.0

printfn "\nZad 1.30"
printfn "Podaj wartosci do obliczenia sredniej:"
let srednia = Funkcje.srednia () 
printfn "Srednia z tych wartosci wynosi %.3f:" srednia


```
