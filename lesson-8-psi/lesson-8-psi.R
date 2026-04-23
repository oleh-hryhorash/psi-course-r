
# Wymagane pakiety ----
library(tm)           # Przetwarzanie tekstu
library(SnowballC)    # Stemming
library(cluster)      # Klastrowanie
library(wordcloud)    # Chmury słów
library(factoextra)   # Wizualizacje klastrów
library(RColorBrewer) # Kolory
library(ggplot2)      # Wykresy
library(dplyr)        # Przetwarzanie danych
library(ggrepel)      # Dodawania etykiet w wykresach
library(DT)           # Interaktywne tabele



# Dane tekstowe ----

# Ustaw Working Directory!
# Załaduj dokumenty z folderu
docs <- DirSource("textfolder")
# W razie potrzeby dostosuj ścieżkę
# np.: docs <- DirSource("C:/User/Documents/textfolder")


# Utwórz korpus dokumentów tekstowych
corpus <- VCorpus(docs)


### Gdy tekst znajduje się w jednym pliku csv:
### data <- read.csv("file.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
### corpus <- VCorpus(VectorSource(data$text))


# Korpus
inspect(corpus)


# Korpus - zawartość przykładowego elementu
corpus[[1]]
corpus[[1]][[1]][7:9]
corpus[[1]][2]




# 1. Przetwarzanie i oczyszczanie tekstu ----
# (Text Preprocessing and Text Cleaning)


# Normalizacja i usunięcie zbędnych znaków ----


# Zapewnienie kodowania w całym korpusie
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))



# Funkcja do zamiany znaków na spację
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))


# Usuń zbędne znaki lub pozostałości url, html itp.

# symbol @
corpus <- tm_map(corpus, toSpace, "@")

# symbol @ ze słowem (zazw. nazwa użytkownika)
corpus <- tm_map(corpus, toSpace, "@\\w+")

# linia pionowa
corpus <- tm_map(corpus, toSpace, "\\|")

# tabulatory
corpus <- tm_map(corpus, toSpace, "[ \t]{2,}")

# CAŁY adres URL:
corpus <- tm_map(corpus, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")

# http i https
corpus <- tm_map(corpus, toSpace, "http\\w*")

# tylko ukośnik odwrotny (np. po http)
corpus <- tm_map(corpus, toSpace, "/")

# pozostałość po re-tweecie
corpus <- tm_map(corpus, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)")

# inne pozostałości
corpus <- tm_map(corpus, toSpace, "www")
corpus <- tm_map(corpus, toSpace, "~")
corpus <- tm_map(corpus, toSpace, "â€“")


# Sprawdzenie
corpus[[1]][[1]][7:9]










# Decyzja dotycząca korpusu ----
# Należy w tym momencie rozważyć, 
# który obiekt użyć do dalszej analizy:
#
# - corpus (oryginalny, bez stemmingu)
# - corpus_stemmed (po stemmingu)
# - corpus_completed (uzupełnione rdzenie)





# Tokenizacja ----


# Macierze częstości TDM i DTM ----


# a) Funkcja TermDocumentMatrix() ----
# tokeny = wiersze, dokumenty = kolumny
tdm <- TermDocumentMatrix(corpus)
tdm
inspect(tdm)


tdm_m <- as.matrix(tdm)

tdm_m[1:5, 1:5]
# Można zapisać TDM w pliku .csv
# write.csv(tdm_m, file="TDM.csv")


# b) Funkcja DocumentTermMatrix() ----
# dokumenty = wiersze, tokeny = kolumny
dtm <- DocumentTermMatrix(corpus)
dtm
inspect(dtm)

dtm_m <- as.matrix(dtm)

dtm_m[1:5, 1:5]
# Można zapisać DTM w pliku .csv
# write.csv(dtm_m, file="DTM.csv")



# 2. Zliczanie częstości słów ----
# (Word Frequency Count)

# Można zliczyć same częstości słów w macierzach
# dla TDM i DTM da to identyczny rezultat
v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)
head(tdm_df, 10)

v2 <- sort(colSums(dtm_m), decreasing = TRUE)
dtm_df <- data.frame(word = names(v2), freq = v2)
head(dtm_df, 10)



# 3. Eksploracyjna analiza danych ----
# (Exploratory Data Analysis, EDA)


# Chmura słów (globalna)
wordcloud(words = tdm_df$word, freq = tdm_df$freq, min.freq = 7, 
          colors = brewer.pal(8, "Dark2"))


# Wyświetl top 10
print(head(tdm_df, 10))




# 4. Inżynieria cech w modelu Bag of Words: ----
# Reprezentacja słów i dokumentów w przestrzeni wektorowej ----
# (Feature Engineering in vector-space BoW model)


# - podejście surowych częstości słów
# (częstość słowa = liczba wystąpień w dokumencie)
# (Raw Word Counts)



# Użyj utworzonej wcześniej macierzy DTM
dtm

inspect(dtm)

dtm_m[1:5, 1:5]




# UCZENIE MASZYNOWE NIENADZOROWANE ----
# (Unsupervised Machine Learning)



# Klastrowanie k-średnich (k-means) ----


# Dobór liczby klastrów
# Metoda sylwetki (silhouette)
fviz_nbclust(t(dtm_m), kmeans, method = "silhouette") +
  labs(title = "Dobór liczby klastrów", subtitle = "Metoda sylwetki")

# Wykonaj klastrowanie kmeans
# (sprawdź wyniki dla k = 3,4,5)
set.seed(123) # ziarno losowe dla replikacji wyników




# c) Ustaw liczbę klastrów k = 4 ----
k <- 4 # ustaw liczbę klastrów


klastrowanie <- kmeans(dtm_m, centers = k, nstart = 25)


# Wizualizacja klastrów
fviz_cluster(list(data = dtm_m, cluster = klastrowanie$cluster),
             geom = "point",
             main = "Wizualizacja klastrów dokumentów")



# Interaktywna tabela z przypisaniem dokumentów i top 5 słów
# Dla każdego klastra: liczba dokumentów oraz top 5 słów
cluster_info <- lapply(1:k, function(i) {
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- sort(colSums(cluster_docs), decreasing = TRUE)
  top_words <- paste(names(word_freq)[1:5], collapse = ", ")
  data.frame(
    Klaster = i,
    Liczba_dokumentów = length(cluster_docs_idx),
    Top_5_słów = top_words,
    stringsAsFactors = FALSE
  )
})

# Połącz wszystko w ramkę danych
cluster_info_df <- do.call(rbind, cluster_info)

# Nazwy dokumentów z korpusu
document_names <- names(corpus)

# Tabela przypisania dokumentów do klastrów
documents_clusters <- data.frame(
  Dokument = document_names,
  Klaster = klastrowanie$cluster,
  stringsAsFactors = FALSE
)

# Dołączamy dane z podsumowania (JOIN po klastrze)
documents_clusters_z_info <- left_join(documents_clusters, cluster_info_df, by = "Klaster")

# Interaktywna tabela z pełnym podsumowaniem
datatable(documents_clusters_z_info,
          caption = "Dokumenty, klastry, najczęstsze słowa i liczność klastrów",
          rownames = FALSE,
          options = list(pageLength = 10))




# Chmury słów dla każdego klastra
for (i in 1:k) {
  # znajdź indeksy dokumentów w danym klastrze
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  
  # nazwy plików odpowiadające dokumentom w tym klastrze
  doc_names <- names(klastrowanie$cluster)[cluster_docs_idx]
  
  # generuj chmurę słów dla klastra
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- colSums(cluster_docs)
  wordcloud(names(word_freq), freq = word_freq, 
            max.words = 15, colors = brewer.pal(8, "Dark2"))
  title(paste("Chmura słów - Klaster", i))
}




# c) Przypisanie dokumentów do klastrów ----
document_names <- names(corpus)  # Nazwy dokumentów z korpusu
clusters <- klastrowanie$cluster  # Przypisanie dokumentów do klastrów

# Ramka danych: dokumenty i ich klastry
documents_clusters <- data.frame(Dokument = document_names,
                                 Klaster = as.factor(clusters))

# Podgląd
print(documents_clusters)


# c) Wizualizacja przypisania dokumentów do klastrów ----
ggplot(documents_clusters, aes(x = reorder(Dokument, Klaster), fill = Klaster)) +
  geom_bar(stat = "count", width = 0.7) +
  coord_flip() +
  labs(title = "Przypisanie dokumentów do klastrów",
       x = "Dokument",
       y = "Liczba wystąpień (powinna wynosić 1)",
       fill = "Klaster") +
  theme_minimal(base_size = 13)





# Odpowied� 1:
# Tak, na podstawie chmur s��w mo�na wnioskowa� o tre�ci dokument�w,
# poniewa� najwi�ksze s�owa wskazuj� na dominuj�ce tematy w klastrach.
# Analiza najcz�stszych s��w pozwala okre�li�, o czym s� dokumenty,
# cho� jest to przybli�ona interpretacja.

# Odpowied� 2:
# Na podstawie chmur s��w mo�na wyr�ni� og�lne obszary tematyczne,
# takie jak: technologia, biznes/ekonomia oraz spo�ecze�stwo/polityka.
# Liczba obszar�w tematycznych odpowiada liczbie klastr�w (np. 3 lub 4),
# jednak ich interpretacja zale�y od dominuj�cych s��w w ka�dym klastrze.


