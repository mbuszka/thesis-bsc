# Plan prezentacji
Na początek przedstawię kontekst mojej pracy, wyjaśniając niektóre słowa z tytułu, a także motywacje ku jej powstaniu.

Następnie sformułuję cele które sobie postawiłem, a później opiszę wyniki które osiągnąłem.

Na koniec podsumuję.


# Formalizacja języków programowania
Dlaczego formalizować?

Formalizacja wymusza dokładne zastanowienie się nad każdym elementem języka i zdefiniowanie jego zachowania.

Trzeba rozważyć czy nie duplikuje funkcji innego elementu.

Jednocześnie, sprawdzenie wzajemnego oddziaływania wszystkich elementów, zapewnia spójność ich działania.

Te cechy języka przekładają się na łatwiejsze utrzymanie i rozwój programów w nim napisanych.

Narzędziem do formalizacji języków są rachunki formalne.


# Efekty obliczeniowe
Efekty obliczeniowe, z angielskiego side effects to główne zagadnienie poruszane w mojej pracy.

Zjawisko z którym programiści zmagają się od początków istnienia języków programowania.

Jest to źródło wielu błędów, frustracji i nieporozumień, gdyż odbiera im podstawowe narzędzie do wnioskowania, czyli możliwość podzielenia problemu na mniejsze części i zrozumienia ich oddzielnie, a następnie połączenia ich działania.

Wynika to bezpośrednio z bardzo ogólnej definicji czym są efekty obliczeniowe, czyli interakcji z otoczeniem.

Niejawne zależności między fragmentami większego systemu na ogół można przypisać do dwóch kategorii.

Pierwszą z nich jest modyfikacja jakiegoś stanu lub jego sprawdzenie.

Przykładami są tutaj problemy wejścia-wyjścia, komunikacji z bazą danych, generacją liczb pseudolosowych, czy też wykorzystania zmiennych.

Dodatkową komplikacją przy efektach związanych ze stanem jest zachowanie programów współbieżnych, czy też działających asynchronicznie.

Drugą kategorię stanowią efekty zmieniające przepływ sterowania programu, na przykład wyjątki.


# Efekty obliczeniowe - podejścia konwencjonalne
Konwencjonalne podejścia do traktowania efektów obliczeniowych przez systemy typów pozostawiają wiele do życzenia.

Pierwsze z nich, najprostsze i najpopularniejsze to oczywiście ignorowanie problemu.

Systemy typów języków zarówno imperatywnych takich jak C, C++, obiektowych jak Java oraz funkcyjnych takich jak OCaml i Scala nie dają programistom żadnej wskazówki na temat efektów które mogą zostać spowodowane przez funkcję czy metodę.

Ten brak informacji sprawia, często objawia się w postaci subtelnych, ale poważnych błędów wynikających z efektów ubocznych.

Ignorowanie efektów uniemożliwia także wnioskowanie równościowe, czyli zamianę wywołania funkcji na jej wynik.

Dodatkowo, takie systemy typów pozwalają na niejawne zależności między modułami.

Drugie podejście, znane z języka Haskell, polega na podzieleniu języka na dwie części, pierwszą czystą, a drugą na wywołującą efekty.

Funkcje wywołujące efekty zwracają specjalny typ, a programiści mają do dyspozycji operacje pozwalające je łączyć, a także dedykowaną składnię do pisania programów z efektami.

Dwie składnie i specjalne typy stanowią duży problem dla nowych programistów i utrudniają naukę języka.

Jednocześnie łączenie różnych efektów obliczeniowych jest dość skomplikowane i wymaga zaawansowanych technik, takich jak transformatory monad.

Na koniec, takie rozwiązanie sprawia, że dodanie efektów do czystej funkcji jest bardzo frustrujące dla programisty.


# Efekty algebraiczne - możliwości
Jedną z odpowiedzi na problemy klasycznych rozwiązań są tak zwane efekty algebraiczne.

Zapewniają one jednolitą składnię dla programów zarówno z jak i bez efektów.

Pozwalają na stworzenie systemu typów który informuje programistę jakie efekty mogą zostać wywołane przez dany program, a także może zagwarantować, że dana funkcja nie wywołuje żadnych efektów obliczeniowych.

Przykładowo, dla podanego programu algorytm wykryje, że korzysta on z dwóch operacji, get i put, które obie przyjmują i zwracają liczbę

Efekty algebraiczne umożliwiają także bezproblemowe łączenie różnych efektów i mogą zostać użyte do modelowania obu wymienionych wcześniej kategorii.

Dodatkowo umożliwiają one nadanie różnych interpretacji jednemu programowi, a także opis problemu z dziedziny za pomocą zbioru operacji.


# Cel
Wobec tego postanowiłem stworzyć przykładowy rachunek zawierający konstrukcje pozwalające na pracę z efektami algebraicznymi.

Oprócz nich rachunek powinien zawierać także zwykłe konstrukcje językowe umożliwiające wygodne budowanie programów.

Aby rachunek był użyteczny zaimplementowałem także jego model.

Zależało mi na tym aby był on wykonywalny i pozwalał na inferencję typu i efektów wyrażenia oraz obserwację jego wykonania krok po kroku.

# Rachunek
Rachunek umożliwia programiście korzystanie z efektów algebraicznych za pomocą trzech konstrukcji językowych.

Pierwsza z nich to abstrakcyjne operacje.

Nie muszą być one zdefiniowane przed użyciem, a z punktu widzenia używającego ich wyrażenia przyjmują jeden argument i zwracają jedną wartość.

Zbiór operacji użytych w danym wyrażeniu tworzy interfejs, którego implementacji dostarczają wyrażenia obsługujące, z angielskiego handler.

Mają one dostęp do wznowienia przerwanego obliczenia i realizują semantykę głębokiej obsługi, a więc będą także obsługiwać operacje użyte we wznowieniu.

Umożliwiają obsługę wielu operacji na raz, a także zmianę wartości końcowej dzięki klauzuli return.

Trzecim składnikiem są wyrażenia podnoszące, z angielskiego lift które pozwalają na przeskoczenie najbliższego w trakcie wykonania wyrażenia obsługującego daną operację.

W rachunku znajdują się także typy bazowe takie jak liczby, wartości boolowskie i listy, oraz operacje na nich.

Dla tego rachunku stworzyłem także model, korzystając z biblioteki PLT Redex.

# Model
Model implementuje składnię abstrakcyjną rachunku, ale dodatkowo zbudowałem parser który pozwala pisać programy w przyjaznej składni podobnej do MLa.

Implementacja semantyki statycznej, czyli systemu typów, pozwala na algorytmiczną inferencję typu a także efektów wyrażenia, natomiast relacja redukcji definiująca semantykę dynamiczną umożliwia zarówno pełną ewaluację wyrażeń, jak i obserwację każdego przekształcenia.

Dodatkowo rozszerzyłem rachunek o maszynę abstrakcyjną, a model o jej implementację w postaci deterministycznego systemu przejść.

Operuje ona na jawnym środowisku, a przepływ sterowania realizuje za pomocą stosu i meta-stosu.

Zgodność maszyny abstrakcyjnej z relacją redukcji sprawdziłem za pomocą testów.

Dodatkowo w trakcie implementacji modelu i rozwoju rachunku korzystałem z automatycznego generowania kontrprzykładów przez bibliotekę Redex, co pozwoliło mi wykryć i poprawić błędy.



# Podsumowanie
Podsumowując, w mojej pracy stworzyłem formalny rachunek, oraz niewielki język programowania na nim bazujący.

Rachunek ten bada zachowanie efektów algebraicznych, czyli nowego i obiecującego rozwiązania problemów związanych z efektami obliczeniowymi.

Wykonywalny model tego rachunku pozwala na zrozumienie działania efektów algebraicznych dzięki obrazowaniu wykonania krok po kroku oraz inferencji typu i efektów wyrażenia.



Dalsza praca nad tym rachunkiem mogłaby objąć rozszerzenie go o polimorfizm, ale już teraz model pokazuje potencjał rozwiązania jakim są efekty algebraiczne.







# Efekty algebraiczne - wyzwania
Efekty algebraiczne są nowym, aktywnie rozwijanym zagadnieniem.

W literaturze można znaleźć wiele pomysłów i rozwiązań zarówno w zakresie semantyki statycznej jak i dynamicznej.

Różne połączenia tych elementów potrafią dać bardzo różne zachowania programów.

Wynika to ze skomplikowanej semantyki i wzajemnych oddziaływań elementów rachunku.

Istnieje wielu języków, oraz formalnych rachunków na bazie których są one zbudowane.

Nie pozwalają one jednak zaobserwować i dokładnie zrozumieć jak wybory semantyczne przekładają się na zachowanie danego rachunku jako, że ich formalizacje nie są interaktywne.