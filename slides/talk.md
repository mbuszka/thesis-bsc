# Plan prezentacji

Plan prezentacji jest następujący:
- We wstępie najpierw omówię __efekty algebraiczne__ - główny temat mojej pracy
- a następnie przedstawię __motywację__ ku jej powstaniu oraz __cele__ które sobie postawiłem.


- w drugiej części omówię rachunek który zaprojektowałem, jego implementację oraz mikrojęzyk który zbudowałem na jego bazie, ilustrując go kilkoma przykładami.

- Na koniec podsumuję i sformułuję wnioski.

# Efekty algebraiczne

__Efekty algebraiczne__ to __technika__ strukturyzacji, modelowania i udostępnienia programiście efektów obliczeniowych.
Na wysokim poziomie:
- kompozycję ... np. mutowalny stan, niedeterminizm, porażki
- definicję ... aby wyrazić specyfikę problemu, np. wyspecjalizowane efekty opisujące dostęp do bazy danych
- rozdzielają one interfejs od implementacji, co pozwala na różną interpretację tych samych programów np. program niedeterministyczny może zostać wykonany z nawrotami tak aby znaleźć pierwszy wynik, listę wszystkich wyników, lub też, korzystając z generatora liczb pseudolosowych sprawdzić jedno możliwe wykonanie.

W użyciu:
- pisząc program czy funkcję, programista może użyć abstrakcyjnych operacji, które są interfejsem wobec którego program jest stworzony
- ...
Gdy już mamy program osadzony w wyrażeniu obsługującym, możemy zacząć jego wykonanie.
# Obsługa
W trakcie wykonania ...
- normalny tok ... ale kontekst jest zachowany
- odnalezione ...
- ma ono dostęp do:
  * wartości ...
  * funkcji ... czyli do kontynuacji, ograniczonej przez wyrażenie obsługujące
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
Jak widzieliśmy na przykładzie, podczas wywołania operacji wiele się dzieje.
Nic dziwnego zatem, że ich implementacja ...
Powstało już kilka języków z efektami algebraicznymi, za pomocą których można eksperymentować.
Jednakże dla osoby zainteresowanej precyzyjnym opisem semantyki i zachowania programów z efektami algebraicznymi pozostają tylko opisy rachunków w artykułach naukowych oraz zgadywanie jak będą się one zachowywać.
Brakuje modelowej implementacji rachunku, która pozwoliłaby na zbadanie i zaznajomienie się z działaniem programów z efektami przez pryzmat formalnej semantyki.
Pozwalając na obserwację redukcji krok po kroku, jak w przykładzie pierwszym.

# Cel
Co prowadzi nas do celu mojej pracy, czyli zaprojektowania oraz implementacji:
- rachunku oraz jego semantyki dynamicznej
- semantyki statycznej, czyli systemu typów
- a także, maszyny abstrakcyjnej która opisuje obliczenia na niższym poziomie.
Oczywiście wszystkie elementy muszą być gotowe do uruchomienia i badania programów z efektami algebraicznymi.

# Rachunek