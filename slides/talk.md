# Plan prezentacji

- Na początku przedstawię cele mojej pracy, oraz przybliżę efekty algebraiczne które są głównym zagadnieniem w niej poruszanym.

- w drugiej części omówię rachunek który zaprojektowałem oraz mikrojęzyk który zbudowałem na jego bazie, ilustrując go kilkoma przykładami.

- Na koniec podsumuję i sformułuję wnioski.

# Efekty algebraiczne

- kompozycję ... np. mutowalny stan, niedeterminizm, porażki
- definicję ... aby wyrazić specyfikę problemu, np. wyspecjalizowane efekty opisujące dostęp do bazy danych
- rozdzielają one interfejs od implementacji, co pozwala na różną interpretację tych samych programów np. program niedeterministyczny może zostać wykonany z nawrotami tak aby znaleźć pierwszy wynik, listę wszystkich wyników, lub też, korzystając z generatora liczb pseudolosowych sprawdzić jedno możliwe wykonanie.

W użyciu:
- pisząc program czy funkcję, programista może użyć abstrakcyjnych operacji, które są interfejsem wobec którego program jest stworzony
- ...
Gdy już mamy program osadzony w wyrażeniu obsługującym, możemy zacząć jego wykonanie.

Semantyka dynamiczna jest moim zdaniem najciekawszą, ale zarazem najtrudniejszą częścią rachunku z efektami algebraicznymi.
Omówię teraz intuicyjnie semantykę operacyjną wywołania operacji.
# Obsługa
W trakcie wykonania ...
- normalny tok ... ale kontekst jest zachowany
- odnalezione ...
- ma ono dostęp do:
  * wartości ...
  * oraz funkcji ... czyli do kontynuacji, ograniczonej przez wyrażenie obsługujące
- wyrażenie ... może także być zwrócone jako funkcja

# Przykład 1
Pierwszy przykład obrazuje redukcję wyrażenia wywołującego operację Magic, która jest obsługiwana poprzez wznowienie wykonania z wartością 42.
A na koniec wynik zagnieżdżonego wyrażenia zmniejszany jest o 1.
Kolorem pomarańczowym oznaczony jest redeks.
1. - Najpierw wykonujemy zagnieżdżone odejmowanie.
   - Redukuje się ono do 1.
2. - Następnie wywoływana jest operacja Magic, jest ona oznaczona na pomarańczowo, wraz z wyrażeniem ją obsługującym.     
     Kolorem czerwonym oznaczony jest zapamiętany kontekst -- ograniczona kontynuacja.
     W wyrażeniu obsługującym `v:r` jest nazwą zmiennej dla wznowienia.
   - Po redukcji zewnętrznym wyrażeniem jest obsługujące ciało, natomiast za `v:r` został podstawiony kontekst, opakowany w to samo wyrażenie obsługujące, reifikowany jako funkcja.
3. - Teraz wykonujemy beta redukcję wznowienia i 42.
   - Otrzymujemy dodawanie osadzone w tym samym wyrażeniu obsługującym.
4. Dodajemy 42 i 1.
5. Jako, że wyrażenie zagnieżdżone zredukowało się do wartości wywołujemy klauzulę `return`
6. Na koniec odejmujemy 1 od 43 i otrzymujemy 42


# Motywacja
Efekty algebraiczne są stosunkowo nowym zagadnieniem, aktywnie rozwijanym.
Jak widzieliśmy na przykładzie, podczas wywołania operacji dzieje naprawdę wiele.
Nic dziwnego zatem, że ich implementacja ...
Powstało już kilka języków z efektami algebraicznymi, za pomocą których można eksperymentować.
Jednakże dla osoby zainteresowanej precyzyjnym opisem semantyki i zachowania programów z efektami algebraicznymi pozostają tylko opisy rachunków w artykułach naukowych oraz zgadywanie jak będą się one zachowywać.
Brakuje modelowej implementacji rachunku, która pozwoliłaby na zbadanie i zaznajomienie się z działaniem programów z efektami przez pryzmat semantyki operacyjnej, pozwalając na obserwację redukcji krok po kroku, jak w przykładzie pierwszym.

# Cel
Co prowadzi nas do celu mojej pracy, czyli zaprojektowania oraz implementacji:
- rachunku oraz jego semantyki dynamicznej w formacie operacyjnym
- semantyki statycznej, czyli systemu typów
- a także, maszyny abstrakcyjnej która opisuje obliczenia na niższym poziomie.
Oczywiście wszystkie elementy muszą być gotowe do uruchomienia i badania programów z efektami algebraicznymi.

# Rachunek
Rachunek, który zaimplementowałem udostępnia programiście:
- wyrażenia liczbowe z podstawowymi operacjami - dodawanie, odejmowanie, mnożenie oraz porównywanie liczb
- wyrażenia logiczne i warunkowe
- homogeniczne listy (dla dowolnego typu) ze standardowymi operacjami (hd, tl, cons, nil)
- funkcje anonimowe oraz rekurencyjne

Efekty algebraiczne są realizowane przez:
- Abstrakcyjne operacje, które
  * nie muszą być zdefiniowane a priori
  * przyjmują jeden argument i zwracają być może jedną wartość
- Wyrażenia je obsługujące, które
  * mają dostęp do wznowienia reifikowanego jako funkcja
  * realizują semantykę głębokiej obsługi, czyli w funkcji wznowienia przechwycony kontekst jest zagnieżdżony w tym samym wyrażeniu obsługującym
  * potrafią obsłużyć wiele operacji naraz
  * mają klauzulę `return` która jest wywoływana gdy zagnieżdżone wyrażenie zredukuje się do wartości.
- oraz wyrażenia 'podnoszące' które pozwalają operacji przeskoczyć nad najbliższym w trakcie wykonania wyrażeniem obsługującym

# Przykład 2
Drugi przykład pokazuje działanie głębokiej obsługi.
Przedstawiony program odczytuje stan początkowy, nadpisuje stan przez 29, po czym znów odczytuje stan.
Jako wynik zwraca sumę obu odczytów.
Obsługa takiego wyrażenia odbywa się poprzez stworzenie funkcji przekazującej stan a następnie zaaplikowania do niej stanu początkowego czyli 13.
Wyrażenia obsługujące obie operacje zwracają funkcje modyfikujące stan, natomiast klauzula `return` zwraca funkcję która zjada stan i zwraca ostateczny wynik.
- wykonanie zaczyna się od wywołania operacji Get, w wyniku tego wywołania powstaje funkcja oczekująca na stan, której ciało zaaplikuje go zarówno do wznowienia jak i jego wyniku.
  Kluczowe dla działania tego wyrażenia obsługującego jest opakowanie wznowienia w to wyrażenie, co zapewni, że wynik wznowienia będzie funkcją.
- Następnie wykonywane są dwie aplikacje:
  + najpierw najbardziej zewnętrzna, otrzymanej funkcji do stanu początkowego
  + później najbardziej wewnętrzna, wznowienia do stanu początkowego.
  Zauważmy, że w ten sposób pod pierwsze wywołanie Get podstawiliśmy 13, natomiast reszta wyrażenia jest opakowana w ten sam handler.
- Teraz wywoływana jest operacja Set. Podobnie jak wcześniej tworzona jest funkcja oczekująca na stary stan, który zostanie zignorowany.
  Jej ciało zawiera dwie aplikacje: Wznowienia do pustej wartości 0 oraz wyniku wznowienia do nowego stanu, czyli 29.
- Znów wykonujemy dwie redukcje, otrzymując sytuację w której Set zostało zastąpione przez pustą wartość, natomiast stan został zamieniony na nowy.
- 


System typów dla rachunku jest przedstawiony w stylu Curry'ego, natomiast
- jego implementacja tworzy funkcję odtwarzającą typ wyrażenia oraz efekty z których ono korzysta.
- System inferuje typy proste dla wyrażeń, a typowanie efektów korzysta z tzw. rzędów.
- Niestety system nie wspiera polimorfizmu
- ale znajduje najogólniejszy prosty typ wyrażenia który może zawierać nieukonkretnione zmienne.

# Przykład 3
Przykład 3 obrazuje działanie inferencji typów
Przedstawiony program jest odroczonym obliczeniem z przykładu 2.
Jako, że jest to funkcja otrzymuje on typ funkcyjny.
Typem argumentu jest nieukonkretniona zmienna, jako, że jest on ignorowany.
Typem wynikowym, jest liczba.
Jako, że ciało tej funkcji korzysta z dwóch operacji Get i Set, jest to odzwierciedlone w 

# Przykład
Przy ostatnim przykładzie chciałbym pokazać działanie modelu na żywo.
Przedstawiony program korzysta z wyrażenia lift, które sprawia, że operacja Tock zostanie obsłużona przez zewnętrzny handler.
Po uruchomieniu modułu, możemy zobaczyć wyrażenie oraz jego typ.
Możemy także użyć maszyny abstrakcyjnej by zredukować program do wartości końcowej, lub zobaczyć przekształcenia po kolei.