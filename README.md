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
                     
# Laboratorium 2

### 2.1
Dany jpest program w C++:
OBRAZEK DO WGLADU PRZY ZADANIU
Napisz równoważny mu program w F+
```
let wybor () =
    printfn "Podaj wartosc: "
    let x = int(Console.ReadLine())
    match x with
    | 1 -> printfn "wprowadziles 1"
    | 2 -> printfn "wprowadziles 2"
    | 3 -> printfn "wprowadziles 3"
    | 4 -> printfn "wprowadziles 4"
    | _ -> printfn "wprowadziles inna wartosc"



printfn "Zad 2.1"
    wybor()
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

```
### 2.2
Napisz funkcję, która będzie wczytywała od użytkownika dwie liczby, i
zwracała je w formie pary. Następnie wykorzystaj dopasowanie wzorców do krotki i
wyświetl na ekranie informacje czy pierwszaliczba z pary jest większa niż druga, czy
druga jest większa czy obie są równe.
```
let para () =
    printfn "Podaj dwie wartosci: "
    let x = float(Console.ReadLine())
    let y = float(Console.ReadLine())
    (x, y)  


printfn "Zad 2.2"
    let para = para()
    match para with 
    | (a,b) when a > b -> printfn "Pierwsza liczba jest większa jak druga"
    | (a,b) when b > a -> printfn "Druga liczba jest większa jak pierwsza"
    | (_,_) -> printfn "Obie liczby są równe"
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()
```
### 2.3
Napisz funkcję, która jako parametry będzie przyjmowała długości trzech
boków trójkąta oraz zwracała jego pole i obwód. Rezultat zamodeluj za pomocą pary.
Wyświetl wynik na ekranie z komunikatem w stylu "pole trójkąta to: ```, a obwód to: .
```

let trojkat a b c =
    let p = (a + b + c) / 2.0
    (sqrt(p * (p - a) * (p - b) * (p - c)), p * 2.0)


printfn "Zad 2.3"
    let a = 3.0
    let b = 3.0
    let c = 3.0
    let wynik = trojkat a b c
    printfn "Pole trojkata o bokach %.2f, %.2f i %.2f to %.2f, a jego obwod to %.2f" a b c (fst wynik) (snd wynik) 
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()
```
### 2.4
Napisz funkcję, która będzie przyjmowała jako parametr łańcuch znaków
określających adres email i wydzielała z niego identyfikator użytkownika oraz adres
domenowyserwera
```
let email (email:string) =
    let login = (email:string).Split("@").[0]
    let domena = (email:string).Split('@').[1]

    (login, domena)


printfn "Zad 2.4"
    printfn "Podaj adres email: "
    let adres = Console.ReadLine()
    let para = (email adres)
    printfn "Identyfikator żytkownika to %s" (fst para)
    printfn "Adres domeny to %s" (snd para)
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()
```
### 2.5
Wykorzystaj funkcję napisaną w poprzednim zadaniu do podziału adresu
na części, a następnie wyświetl informacje czy podany adres należy do domeny PCz, czy
nie. Komunikat powinien mieć formę Email użytkownika (nazwa użytkownika) należy do
domeny PCz”", lub że nie należy. Wykorzystaj dopasowanie wzorca.
```
let sufix (login, domena:string) =
    let alias1 = domena.Split('.').[0]
    let alias2 = domena.Split('.').[1]

    printf "Email użytkownika: %A" login

    match alias1 with
    |"pcz" | "PCZ" -> printfn " należy do domeny PCZ"
    |_ -> printfn " nie należy do domeny PCZ" 

printfn "Zad 2.5"
    printfn "Podaj adres email: "
    //let adress = Console.ReadLine()
    sufix (email adres)
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()
```
### 2.6
Napisz funkcję obliczającą odległość Euklidesową pomiędzy dwoma punktami
w przestrzeni 3D. Punkty zamodeluj za pomocą krotki.
```
let Euklides (a1, b1, c1) (a2, b2, c2) = 
   sqrt((a1 - a2)**2.0 + (b1 - b2)**2.0 + (c1 - c2)**2.0)


printfn "Zad 2.6"
    let A = (1.0, 1.0, 1.0)
    let B = (2.0, 2.0, 2.0)
    printfn "Odleglosc od siebie punktow %s i %s wynosi %f" (string(A)) (string(B)) (Euklides A B)
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()
```
### 2.7
Napisz funkcję, która sprawdzi czy podany punkt znajduje się wewnątrz
okręgu. Jako parametry powinna przjmować środek okręgu, jego promień oraz punkt,
który chcemy sprawdzić.
```
let wnetrze srodek R punkt =
    let d = sqrt((fst srodek - fst punkt)**2.0 + (snd srodek - snd punkt)**2.0)
    match d with
    | k when k  <= R -> printfn "Punkt (%.2f, %.2f) miesci sie w okregu o srodku w punkcie (%.2f, %.2f) i promieniu %.2f" (fst punkt) (snd punkt) (fst srodek) (snd srodek) R
    | _ -> printfn "Punkt (%.2f, %.2f) miesci sie poza okregiem o srodku w punkcie (%.2f, %.2f) i promieniu %.2f" (fst punkt) (snd punkt) (fst srodek) (snd srodek) R

printfn "Zad 2.7"
    let srodek = (2.0, 1.0)
    let promien = 1.0
    let punkt = (1.0, 1.0)
    wnetrze srodek promien punkt
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

```
### 2.8
Napisz aplikację, w której zdefiniujesz nowy typ danych opisujący ułamki
zwykłe. Napisz funkcje umożliwiające wykonanie podstawowych operacji arytmetycznych:
dodawanie, odejmowanie, mnożenie i dzielenie. Wykorzystaj krotki.
```
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

```
### 2.9
Napisz tę samą aplikację co powyżej, ale z wykorzystaniem rekordów.
```
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

```
### 2.10
Napisz program, w którym zadeklarujesz nowy typ opisujący datę (dzień,
miesiąc, rok), a następnie wykorzystasz go do określenia, jaki jest dzień tygodnia w którym
ta data wypada np. jeżeli użytkownik poda 1.03.2021 program powinien odpowiedzieć
poniedziałek. Jako punkt wyjścia możemy przyjąć, że 1.01.1990 wypadł również w
poniedziałek.
```
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

```
### 2.11
Napisz funkcję realizującą bezpiecznie dzielenie. Funkcja powinna przyjmować
dwie liczby, które chcemy podzielić. Jeżeli jest to możliwe to funkcja powinna
zwracać wynik dzielenia, a jeżeli nie komunikat Ńie można dzielić przez 0". Jaki będzie
najlepszy typ.do reprezentowania rezultatu takiej funkcji.
```
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
```
### 2.12
Zdefiniuj odpowiedni typ danych do przechowywaniainformacji zakodowanych
w numerze VIN (Vehicle Identification Number), a następnie napisz program
pozwalający dekodować ten numer. Informacje o VIN możesz zaczerpnąć np. z Wikipedii
https://pl.wikipedia.org/wiki/Vehicle_Identification_Number
```
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

printfn "Zad 2.12"
    let nrVIN = "2B3CL3CG9BH518087"
    walidacja (dekompozycja nrVIN)
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()
```
### 2.13
Napisz funkcję, która jako parametry będzie przyjmowała długości trzech
boków trójkąta oraz zwracała jego pole i obwód. Rezultat zamodeluj za pomocą pary.
Wyświetl wynik na ekranie z komunikatem w stylu "pole trójkąta to: ```, a obwód to: ```".
Jeżeli z podanych boków nie da się stworzyć trójkąta, to zwróć komunikat Ź podanych
wartości nie da się stworzyć trójkąta"
```
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
```
### 2.14
Napisz program, który będzie wczytywał od użytkownika współczynniki
a, b, c równania kwadratowego ax” + bx + c = O rozpatrz wszystkie możliwe przypadki.
Zamodeluj je za pomocą unii z dyskryminatorem. Po obliczeniach wyświetl odpowiednie
Komunikaty.
```
```
### 2.15
Napisz program, który będzie pozwalał przechowywaćnastępujące informacje
o osobie: imie, nazwisko, wiek. Przygotuj odpowiedni typ danych. Napisz program,
który będzie pozwalał na wprowadzenie informacji o osobie, modyfikowanie ich oraz
wyświetlenie. Po uruchomieniu programu użytkownik powinien zobaczyć menu:
WYBIERZ OPCJE
| - utworzenie rekordu
2 - modyfikacja rekordu
3 - pokaz rekord
4 - zakończenie programu
Po wybraniu odpowiedniej opcji użytkownik powinien mieć możliwość wprowadzenia
danych (program powinien wyświetlać informacje o jakie dane prosi użytkownika), lub
ich zobaczenia. Podczas modyfikowania danych jeżeli użytkownik wprowadziłańcuch
pusty (w przypadku imienia lub nazwiska) lub O (w przypadku wieku) program powininen
zachowywać wcześniejsze dane. Po zakończeniu wprowadzania lub przeglądania danych
oraz w przypadku, gdy użytkownik wybierze inną opcję niż od 1 do 4 program powinien
wracać do menu głównego. Wykorzystaj dopasowanie wzorców.
```
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


printfn "Zad 2.15"
    menu {imie="";nazwisko="";wiek=0} true
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

```
### 2.16
Zmodyfikuj poprzednie zadanie, tak aby wyświetlając dane wprowadzonej
osoby pojawiała się również informacja czy jest ona pełnoletnia, czy nie. Wykorzystaj
dopasowanie wzorca do rekordu.
```
```          
  
# Laboratorium 3  

### 3.1


Napisz funkcję, która generuje listę zbudowaną z n pierwszych liczb naturalnych. 
Sygnatura funkcji powinna przyjmować następującą postać:
nPierwszych: n:ini->Lista<int>

```
type Lista<'a> = 
|Pusta
|Wezel of 'a*Lista<'a>

let rec lista n =
    if n = 1 then
        Pusta
    else Wezel(n, (lista (n-1)))

//wywolanie
 printfn "Podaj ilosc elementow"
    let ilosc = (int(Console.ReadLine()))
    printfn "%A" (lista ilosc)
```

### 3.2

Napisz funkcję, która generuje listę liczb całkowitych z określonego przedziału min, max. 
Funkcja powinna również pozwalać określić krok z jakim wartości
będą generowane. Pierwszą wartością na liście powinna być min, a ostatnia powinna być
mniejsza lub równa max




```

type Lista_2<'a> = 
|Pusta
|Wezel of 'a*Lista_2<'a>

let rec lista_2 n min max =
    if n > max then
        Pusta
    else Wezel(n, (lista_2 (n+min) min max))

//wywolanie
 printfn "Podaj ilosc krokow"
    let ilosc = (int(Console.ReadLine()))
    printfn "Podaj wartosc min"
    let min = (int(Console.ReadLine()))
    printfn "Podaj wartosc max"
    let max = (int(Console.ReadLine()))
    printfn "%A" (lista_2 ilosc min max)


```

### 3.3

Napisz funkcję, która zwróci n-ty elementlisty.

```

let rec getn n xs =
    match n, xs with
      | 0, (x::_)   -> x
      | _, (_::xs') -> getn (n - 1) xs'
      | _, []       -> invalidArg "n" "n is too large"

/// This invokes the tail recursive helper function
/// An approach like this is common in F#.
let sumListTailRecursive xs = getn 5 xs
let oneThroughTen = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]

//wywolanie
printfn $"Nth element %d{sumListTailRecursive oneThroughTen}"

```


### 3.4

Napisz funkcję, która określi czy dany element znajduje się na liście.

```

let isInList elementToFind listToCheck = 
    List.fold(fun acc x -> acc || x = elementToFind) false listToCheck

//wywolanie
let x = [1;2;2;3]
    let y = 4
    let z = 2

    isInList y x |> printfn "%A"
    isInList z x |> printfn "%A"



```



### 3.5

Napisz funkcję, która określi indeks podanego elementu. Jeżeli element nie
znajduje się na liście zwróć odpowiednią wartość (możesz wykorzystać unie z dyskryminatorem).

```

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



```


### 3.6

Napisz funkcję, która usuwaz listy element na podanej pozycji.

```

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

//wywolanie
 newList |> printfn "%A"

```

### 3.7

Napisz funkcję pozwalającą obliczyć średnią wartości naliście.

```

let avg aList =
    let rec sum = function
        | head :: tail -> head + (sum tail)
        | [] -> 0.
    sum aList / (aList |> List.length |> float)

let res = avg [ 2.; 4.; 6. ]

//wywolanie

printfn "%A" res

```

### 3.8

Napisz funkcję, która pozwoli połączyć tablicę stringów w jeden łańcuch
znaków. Funkcja powinna przyjmować parametr separator, który określa znak lub znaki
jakiminależy rozdzielić poszczególne łańcuchy.

```

let strings = [ "tomatoes"; "bananas"; "apples" ]
let fullString = String.concat "," strings

//wywolanie
 printfn "%s" fullString

```

### 3.10

Napisz funkcję, która będzie przyjmowałalistę stringów oraz wyszukiwała
najdłuższy 1 najkrótszy wyraz

```

let values = ["aa"; "x"; "zzz"; "yy"; "eeee"]

// Sort the string list by length in descending (high to low) order.
let result = List.sortBy (fun (x : string) -> -x.Length ) values

//wywolanie
 List.iter(fun x -> printfn "%A" x) result

```


### 3.12 

Napisz funkcję, która będzie odwracała kolejność elementów na liście.

```

let revlists xs = List.map (fun x -> List.rev x) xs;;

let example5 () =
    let list = [[0;1;1];[3;2];[];[5]]
    let x =revlists [[0;1;1];[3;2];[];[5]]
    printfn "The reverse of %A is %A" list x;;

//wywolanie
example5 () |> ignore

```

### 3.16

Napisz funkcję, która sprawdzi, czy lista elementów jest posortowana. Kierunek sortowania (malejący lub rosnący) powinien być zdefiniowany jako typ wyliczeniowy
i przekazywany do funkcji.

```


let rec isSorted list =
    match list with
    | [] | [_] -> true
    | h1::(h2::_ as tail) -> h1 <= h2 && isSorted tail

let task_3_16 () = 
    let list = [[5;1;5]];
    printfn "The isSorted of %A is %A" isSorted list;;

//wywolanie
task_3_16 () |> ignore

```

### 3.20

Napisz funkcję, która będzie zliczała liczbę elementów na drzewie binarym

```

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

//wywolanie
printfn "%A" result_tree_leaf

```


### 3.21

Napisz funkcję, która oblicza sumę wartości przechowywanych w drzewie binarnym

```

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

//wywolanie
 printfn "%A" resultSumTree

``` 
 
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
