### This script loads the Posts, Users, and Comments datasets and implements five 
### analytical tasks using four approaches: SQL with sqldf, base R, dplyr, 
### and data.table. For each task, the script provides alternative solutions, 
### along with commented-out code for result validation and performance benchmarking,
### as well as the obtained benchmark results.


# -----------------------------------------------------------------------------#
library("sqldf")
library("dplyr")
library("data.table")
library("stringi")
Posts <- read.csv("data/Posts.csv.gz")
Users <- read.csv("data/Users.csv.gz")
Comments <- read.csv("data/Comments.csv.gz")
# -----------------------------------------------------------------------------#



# -----------------------------------------------------------------------------#
# Zadanie 1
# -----------------------------------------------------------------------------#

sql_1 <- function(Users){
    sqldf("SELECT Location, SUM(UpVotes) as TotalUpVotes
          FROM Users
          WHERE Location != ''
          GROUP BY Location
          ORDER BY TotalUpVotes DESC
          LIMIT 10")
  # Kolejne operacje:
  # wybieramy kolumnę Location, sumujemy wartości w kolumnie UpVotes dla każdej lokalizacji (grupujemy po kolumnie Location), zwracamy je w kolumnie TotalUpVotes
  # dane pobieramy z tabeli Users
  # pomijamy wiersze, w których wartość Location jest pusta
  # sortujemy malejąco po sumach w kolumnie TotalUpVotes
  # wybieramy pierwsze 10 wierszy - 10 lokalizacji w największą liczbą UpVotes
}

# Wynik zapytania:
# Tabela z dwiema kolumnami: Location i TotalUpVotes,
# zawiera 10 lokalizacji (niepustych) z największą sumaryczną liczbą głosów UpVotes

base_1 <- function(Users){
    x <- aggregate(Users["UpVotes"], by = list(Location = Users$Location), FUN = sum)    # grupujemy wybrane kolumny Location i UpVotes z tabeli Users po wartościach z kolumny Location i sumujemy wartości UpVotes dla każdej lokalizacji
    names(x)[2] <- "TotalUpVotes"    # nadajemy nazwę TotalUpVotes kolumnie z sumami
    x <- x[x$Location != '',]    # pomijamy wiersze z pustym polem Location
    x <- x[order(x$TotalUpVotes, decreasing=TRUE),]    # sortujemy malejąco po wartościach w kolumnie TotalUpVotes
    x <- x[1:10,]    # wybieramy pierwsze 10 wierszy
    row.names(x) <- NULL    # ustawiamy od nowa numery wierszy (od 1 do 10) - dla przejrzystości wyniku
    x
}

dplyr_1 <- function(Users){
    Users %>%
      group_by(Location) %>%    # grupowanie po kolumnie Location
      filter(Location != '') %>%    # pomijanie wierszy z pustym polem Location
      summarise(TotalUpVotes = sum(UpVotes)) %>%    # wyliczanie sumy wartości w kolumnie UpVotes dla każdej lokalizacji - umieszczone w kolumnie TotalUpVotes
      arrange(desc(TotalUpVotes)) %>%    # sortowanie malejące po kolumnie TotalUpVotes
      head(10)    # wybór pierwszych 10 wierszy
}

table_1 <- function(Users){
    Users_dt <- as.data.table(Users)
    Users_dt[Location != '', .(TotalUpVotes = sum(UpVotes)), by = Location][order(-TotalUpVotes)][1:10]
    # Najpierw konwertujemy obiekt data.frame Users na obiekt o typie używanym przez pakiet data.table
    # Filtrujemy dane - wybieramy wiersze, gdzie Location != '', grupujemy po Location i wyliczamy sumę UpVotes dla każdej lokalizacji, 
    # sortujemy malejąco po TotalUpVotes, zwracamy pierwsze 10 wierszy
}

# compare::compare(sql_1(Users), base_1(Users), allowAll = TRUE)
# compare::compare(sql_1(Users), dplyr_1(Users), allowAll = TRUE)
# compare::compare(sql_1(Users), table_1(Users), allowAll = TRUE)

# microbenchmark::microbenchmark(sql_1 = sql_1(Users))
# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# sql_1 247.1495 249.0218 256.5868 250.4031 253.2242 481.2509   100
# 
# microbenchmark::microbenchmark(base_1 = base_1(Users))
# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# base_1 204.9823 209.4973 222.5383 212.6746 223.0464 308.4682   100

# microbenchmark::microbenchmark(dplyr_1 = dplyr_1(Users))
# Unit: milliseconds
# expr     min       lq     mean   median       uq      max neval
# dplyr_1 72.8233 77.01725 104.2548 121.5229 127.3304 155.2162   100
# 
# microbenchmark::microbenchmark(table_1 = table_1(Users))
# Unit: milliseconds
# expr     min      lq     mean  median      uq     max neval
# table_1 18.5689 19.4868 24.51816 20.1447 20.7975 90.8567   100


# -----------------------------------------------------------------------------#
# Zadanie 2
# -----------------------------------------------------------------------------#

sql_2 <- function(Posts){
    sqldf("SELECT STRFTIME('%Y', CreationDate) AS Year, STRFTIME('%m', CreationDate) AS Month,
          COUNT(*) AS PostsNumber, MAX(Score) AS MaxScore
          FROM Posts
          WHERE PostTypeId IN (1, 2)
          GROUP BY Year, Month
          HAVING PostsNumber > 1000")
    # Kolejne operacje:
    # Pobieramy dane z tabeli Posts i zwracamy kolumny: Year, Month, PostsNumber, MaxScore
    # W kolumnach Year i Month umieszczamy wyodrębniony rok i miesiąc z kolumny CreationDate
    # Usuwamy wszystkie wiersze, dla których wartość PostTypeId nie jest równa 1 lub 2
    # Grupujemy wyniki po kolumnach Year i Month i dla każdego roku i miesiąca zliczamy liczbę wpisów - te dane są w kolumnie PostsNumber
    # W kolumnie MaxScore znajdują się najwyższe wartości z kolumny Score dla każdego roku i miesiąca
    # Wybieramy tylko te wiersze, w których PostsNumber jest większe niż 1000
}

# Wynik zapytania:
# Tabela z 4 kolumnami: Year, Month, PostsNumber, MaxScore,
# pokazuje dla danego roku i miesiąca ilość postów (których PostTypeId jest równy 1 lub 2) i maksymalny wynik (wartość Score), 
# ale tylko dla tych lat i miesięcy, w których liczba postów był większa niż 1000

base_2 <- function(Posts){
    x <- subset(Posts, Posts$PostTypeId %in% c(1, 2), c("CreationDate", "PostTypeId", "Score"))    # wybieramy z tabeli Posts wiersze, w których wartość PostTypeId jest równa 1 lub 2 oraz kolumny: CreationDate, PostTypeId, Score
    x$Year <- stri_sub(x$CreationDate, 1, 4)
    x$Month <- stri_sub(x$CreationDate, 6, 7)    # dodajemy do utworzonej tabeli kolumny Year i Month z wyodrębnionymi odpowiednio rokiem i miesiącem z kolumny CreationDate
    df1 <- aggregate(x["PostTypeId"], by = list(Year = x$Year, Month = x$Month), FUN = length)    # tworzymy kolejną tabelę, którą grupujemy po kolumnach Year i Month i zliczamy liczbę wierszy dla każdej pary - rok, miesiąc
    df2 <- aggregate(x["Score"], by = list(Year = x$Year, Month = x$Month), FUN = max)    # tworzymy następną tabelę, którą też grupujemy po Year i Month i dla każdej pary liczymy maksymalną wartość z kolumny Score
    result <- merge(df1, df2, by = c(1, 2))    # łączymy dwie powyższe tabele kolumnowo
    names(result)[3:4] <- c("PostsNumber", "MaxScore")    # ustawiamy odpowiednie nazwy dla kolumn wynikowej tabeli
    result <- result[result$PostsNumber > 1000,]    # wybieramy te wiersze wyniku, które mają PostsNumber > 1000
    row.names(result) <- NULL    # resetujemy etykiety wierszy
    result
}

dplyr_2 <- function(Posts){
    Posts %>%
      filter(PostTypeId %in% c(1, 2)) %>%    # filtrowanie danych - wybieramy tylko te wiersze, gdzie PostTypeId ma wartość 1 lub 2
      mutate(Year = stri_sub(CreationDate, 1, 4), Month = stri_sub(CreationDate, 6, 7)) %>%    # dodajemy kolumny Year i Month zawierające wyodrębnione odpowiednio rok i miesiąc z kolumny CreationDate
      group_by(Year, Month) %>%    # grupujemy po kolumnach Year i Month
      summarize(PostsNumber = n(), MaxScore = max(Score)) %>%    # dla każdej pary rok-miesiąc: w kolumnie PostsNumber zliczamy liczbę wierszy oraz w kolumnie MaxScore - maksymalną wartość z kolumny Score
      filter(PostsNumber > 1000)    # zostawiamy tylko te wiersze, w których wartość PostsNumber > 1000
}

table_2 <- function(Posts){
    Posts_dt <- as.data.table(Posts)
    Posts_dt <- mutate(Posts_dt, Year = stri_sub(CreationDate, 1, 4), Month = stri_sub(CreationDate, 6, 7))    # dodajemy kolumny Year i Month jak wyżej
    Posts_dt[PostTypeId %in% c(1, 2), .(PostsNumber = .N, MaxScore = max(Score)), by = .(Year, Month)][PostsNumber > 1000]
    # wybieramy te wiersze, gdzie PostTypeId jest równe 1 lub 2, grupujemy po Year i Month i dla każdej takiej pary zliczamy liczbę wierszy i maksymalną wartość Score - umieszczamy wyniki w kolumnach PostsNumber i MaxScore, zostawiamy tylko te wiersze, gdzie PostsNumber > 1000
}

# compare::compare(sql_2(Posts), base_2(Posts), allowAll = TRUE)
# compare::compare(sql_2(Posts), dplyr_2(Posts), allowAll = TRUE)
# compare::compare(sql_2(Posts), table_2(Posts), allowAll = TRUE)

# microbenchmark::microbenchmark(sql_2 = sql_2(Posts))
# Unit: seconds
# expr      min       lq     mean  median       uq      max neval
# sql_2 1.556683 1.572559 1.603789 1.58691 1.623268 1.881183   100

# microbenchmark::microbenchmark(base_2 = base_2(Posts))
# Unit: milliseconds
# expr     min       lq    mean   median       uq      max neval
# base_2 196.161 199.1676 229.972 219.4741 254.0764 455.7668   100

# microbenchmark::microbenchmark(dplyr_2 = dplyr_2(Posts))
# Unit: milliseconds
# expr     min       lq     mean  median      uq      max neval
# dplyr_2 75.0337 79.47535 93.49962 82.1989 93.6159 161.6495   100

# microbenchmark::microbenchmark(table_2 = table_2(Posts))
# Unit: milliseconds
# expr     min       lq     mean  median       uq      max neval
# table_2 36.3752 38.38555 53.11147 41.2726 45.13185 282.1095   100


# -----------------------------------------------------------------------------#
# Zadanie 3
# -----------------------------------------------------------------------------#

sql_3 <- function(Posts, Users){
    Questions <- sqldf("SELECT OwnerUserId, SUM(ViewCount) as TotalViews
                        FROM Posts 
                        WHERE PostTypeId = 1
                        GROUP BY OwnerUserId")
  
    sqldf("SELECT Id, DisplayName, TotalViews
          FROM Questions
          JOIN Users
          ON Users.Id = Questions.OwnerUserId
          ORDER BY TotalViews DESC
          LIMIT 10")
    # Tworzymy tabelę Questions składającą się z kolumn (dane pobieramy z tabeli Posts): OwnerUserId i TotalViews, zawierającą te wiersze, dla których PostTypeId = 1
    # W kolumnie TotalViews sumujemy wartości z kolumny ViewCount, grupujemy po kolumnie OwnerUserId
    # Następnie tworzymy wynikową tabelę przez połączenie tabel Questions i Users na podstawie kolumny Id i wybranie z nich kolumn Id, DisplayName i TotalViews
    # Sortujemy wyniki malejąco po wartościach w TotalViews i wybieramy pierwszych 10 wierszy
}

# Wynik zapytania:
# Tabela z 3 kolumnami: Id, DisplayName, TotalViews,
# pokazuje 10 użytkowników (ich Id i nazwę) z największą liczbą wyświetleń swoich postów (tylko tych, których PostTypeId = 1)

base_3 <- function(Posts, Users){
    df1 <- Posts[Posts$PostTypeId == 1,]    # wybieramy z tabeli Posts wiersze, gdzie PostTypeId = 1, tworzymy w ten sposób nową tabelę df1
    df1 <- aggregate(df1["ViewCount"], by = list(Id = df1$OwnerUserId), FUN = sum)    # grupujemy po kolumnie OwnerUserId (zmieniamy jej nazwę na Id) i sumujemy wartości z kolumny ViewCount
    names(df1)[2] <- "TotalViews"    # zmieniamy nazwę kolumny z "ViewCount" na "TotalViews"
    df2 <- merge(df1, Users, by = "Id")    # łączymy otrzymaną tabelę z tabelą Users na podtawie kolumny Id, zapisujemy wynik jako tabelę df2
    df2 <- df2[order(df2$TotalViews, decreasing=TRUE), c("Id", "DisplayName", "TotalViews")]    # sortujemy wyniki po kolumnie TotalViews i wybieramy kolumny "Id", "DisplayName" i "TotalViews"
    df2 <- df2[1:10,]    # wybieramy pierwsze 10 wierszy
    row.names(df2) <- NULL    # resetujemy etykiety wierszy wynikowej tabeli
    df2
}

dplyr_3 <- function(Posts, Users){
    Posts %>%
      filter(PostTypeId == 1) %>%    # wybieramy z tabeli Posts wiersze, gdzie PostTypeId = 1
      group_by(OwnerUserId) %>%    # grupujemy po kolumnie OwnerUserId
      summarize(TotalViews = sum(ViewCount)) %>%    # dla każdego OwnerUserId sumujemy wartości z kolumny ViewCount - sumy umieszczamy w kolumnie TotalViews
      inner_join(Users, by = join_by("OwnerUserId" == "Id")) %>%    # łączymy otrzymaną tabelę z tabelą Users na podtawie kolumny Id
      select(Id = OwnerUserId, DisplayName, TotalViews) %>%    # wybieramy kolumny Id, DisplayName, TotalViews
      arrange(desc(TotalViews)) %>%    # sortujemy wyniki malejąco po kolumnie TotalViews
      head(10)    # wybieramy pierwsze 10 wierszy
}

table_3 <- function(Posts, Users){
    Posts_dt <- as.data.table(Posts)
    Users_dt <- as.data.table(Users)
    Posts_dt <- Posts_dt[PostTypeId == 1, .(TotalViews = sum(ViewCount)), by = OwnerUserId]    # wybieramy z tabeli Posts_dt wiersze, gdzie PostTypeId = 1, grupujemy po kolumnie OwnerUserId i sumujemy wartości z kolumny ViewCount - sumy umieszczamy w kolumnie TotalViews
    Posts_dt <- Posts_dt[Users_dt, on = c("OwnerUserId" = "Id")][, c("OwnerUserId", "DisplayName", "TotalViews")][order(-TotalViews)][1:10]    # łączymy otrzymaną tabelę z tabelą Users na podtawie kolumny OwnerUserId, wybieramy kolumny: "OwnerUserId", "DisplayName", "TotalViews", sortujemy malejąco po TotalViews i wybieramy pierwsze 10 wierszy
    setnames(Posts_dt, "OwnerUserId", "Id")    # zmieniamy nazwę kolumny "OwnerUserId" w wynikowej tabeli na "Id"
    Posts_dt
}

# compare::compare(sql_3(Posts, Users), base_3(Posts, Users), allowAll = TRUE)
# compare::compare(sql_3(Posts, Users), dplyr_3(Posts, Users), allowAll = TRUE)
# compare::compare(sql_3(Posts, Users), table_3(Posts, Users), allowAll = TRUE)

# microbenchmark::microbenchmark(sql_3 = sql_3(Posts, Users), times = 25)
# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# sql_3 1.747289 1.756791 1.795905 1.770307 1.808975 2.082283    25

# microbenchmark::microbenchmark(base_3 = base_3(Posts, Users))
# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# base_3 432.9124 445.0514 474.0845 461.5163 504.4873 544.7429   100

# microbenchmark::microbenchmark(dplyr_3 = dplyr_3(Posts, Users))
# Unit: milliseconds
# expr     min       lq     mean median       uq      max neval
# dplyr_3 83.6022 87.49575 112.7718 91.983 145.6495 183.6029   100

# microbenchmark::microbenchmark(table_3 = table_3(Posts, Users))
# Unit: milliseconds
# expr     min       lq     mean   median       uq     max neval
# table_3 38.7922 42.12955 65.60958 44.81495 100.0361 311.824   100


# -----------------------------------------------------------------------------#
# Zadanie  4
# -----------------------------------------------------------------------------#

sql_4 <- function(Posts, Users){
    sqldf("SELECT DisplayName, QuestionsNumber, AnswersNumber, Location, Reputation, UpVotes, DownVotes
          FROM (
            SELECT *
              FROM (
                SELECT COUNT(*) as AnswersNumber, OwnerUserId
                FROM Posts
                WHERE PostTypeId = 2
                GROUP BY OwnerUserId
              ) AS Answers
            JOIN
            (
              SELECT COUNT(*) as QuestionsNumber, OwnerUserId
              FROM Posts
              WHERE PostTypeId = 1
              GROUP BY OwnerUserId
            ) AS Questions
            ON Answers.OwnerUserId = Questions.OwnerUserId
            WHERE AnswersNumber > QuestionsNumber
            ORDER BY AnswersNumber DESC
            LIMIT 5
          ) AS PostsCounts
          JOIN Users
          ON PostsCounts.OwnerUserId = Users.Id")
    # Tabelę PostsCounts - powstałą przez połączenie tabeli Answers (tworzymy ją przez wybranie z tabeli Posts kolumny OwnerUserId, wybieramy wiersze, dla których PostTypeId = 2 i grupujemy po kolumnie OwnerUserId i dla każdego OwnerUserId zliczamy liczbę wierszy - w kolumnie AnswersNumber)
    # i tabeli Questions (którą tworzymy analogicznie do Answers, inny warunek: PostTypeId = 1)
    # na podstawie kolumny OwnerUserId i następnie wybranie wierszy, dla których zachodzi AnswersNumber > QuestionsNumber, posortowanie malejąco po kolumnie AnswersNumber i wybranie pierwszych 5 wierszy
    # - łączymy z tabelą Users na podstawie odpowiadających sobie kolumn OwnerUserId (z PostsCounts) i Id (z Users)
    # Wybieramy z otrzymanej tabeli kolumny: DisplayName, QuestionsNumber, AnswersNumber, Location, Reputation, UpVotes, DownVotes
}

# Wynik zapytania:
# Tabela z kolumnami: DisplayName, QuestionsNumber, AnswersNumber, Location, Reputation, UpVotes, DownVotes,
# pokazuje 5 użytkowników (ich nazwy) z największą liczbą postów typu odpowiedź oraz liczbę ich postów typu pytanie (ale tylko tych użytkowników, dla których liczba odpowiedzi jest większa niż liczba pytań),
# poza tym pokazuje lokalizację każdego z nich, reputację, liczbę UpVotes i DownVotes

base_4 <- function(Posts, Users){
    Answers <- Posts[Posts$PostTypeId == 2, c("OwnerUserId", "AnswerCount")]
    Answers <- aggregate(Answers["AnswerCount"], by = list(OwnerUserId = Answers$OwnerUserId), FUN = length)
    names(Answers)[2] <- "AnswersNumber"    # tworzymy tabelę Answers z tabeli Posts - warunek PostTypeId = 2, grupowanie po OwnerUserId, zliczenie liczby wierszy dla każdego OwnerUserId - zapisane w kolumnie AnswersNumber
    Questions <- Posts[Posts$PostTypeId == 1, c("OwnerUserId", "AnswerCount")]
    Questions <- aggregate(Questions["AnswerCount"], by = list(OwnerUserId = Questions$OwnerUserId), FUN = length)
    names(Questions)[2] <- "QuestionsNumber"    # analogicznie do Answers, inny warunek: PostTypeId = 1
    PostsCounts <- merge(Answers, Questions, by = "OwnerUserId")    # tworzymy tabelę PostsCounts - łączymy tabele Answers i Questions po OwnerUserId
    PostsCounts <- PostsCounts[PostsCounts$AnswersNumber > PostsCounts$QuestionsNumber,]    # wybór wierszy, gdzie AnswersNumber > QuestionsNumber
    PostsCounts <- PostsCounts[order(PostsCounts$AnswersNumber, decreasing=TRUE),]    # sortowanie malejące po AnswersNumber
    PostsCounts <- PostsCounts[1:5,]    # wybór pierwszych 5 wierszy
    names(PostsCounts)[1] <- "Id"    # zmiana nazwy pierwszej kolumny
    result <- merge(PostsCounts, Users, by = "Id")    # tworzymy wynikową tabelę - łączymy PostsCounts i Users po kolumnie Id
    result[c("DisplayName", "QuestionsNumber", "AnswersNumber", "Location", "Reputation", "UpVotes", "DownVotes")]    # wybór odpowiednich kolumn z wyniku
}

dplyr_4 <- function(Posts, Users){
    Answers <- Posts %>%    # tworzymy tabelę Answers - dane pobieramy z Posts
      filter(PostTypeId == 2) %>%    # wybór wierszy zgodnie z warunkiem
      group_by(OwnerUserId) %>%    # grupowanie po OwnerUserId
      summarize(AnswersNumber = n())    # dla każdego OwnerUserId zliczamy liczbę wierszy - w kolumnie AnswersNumber
    Questions <- Posts %>%
      filter(PostTypeId == 1) %>%
      group_by(OwnerUserId) %>%
      summarize(QuestionsNumber = n())    # analogicznie dla Questions, inny warunek na wybór wierszy z Posts
    PostsCounts <- inner_join(Answers, Questions, by = "OwnerUserId") %>%    # łączymy tabele Answers i Questions po OwnerUserId
      filter(AnswersNumber > QuestionsNumber) %>%    # wybór wierszy zgodnych z warunkiem
      arrange(desc(AnswersNumber)) %>%    # sortowanie malejące po kolumnie AnswersNumber
      head(5)    # wybór pierwszych 5 wierszy
    result <- inner_join(PostsCounts, Users, by = join_by("OwnerUserId" == "Id")) %>%    # złaczamy tabele PostsCounts i Users po kolumnach OwnerUserId (z PostsCounts) i Id (z Users)
      select(DisplayName, QuestionsNumber, AnswersNumber, Location, Reputation, UpVotes, DownVotes)    # wybór odpowiednich kolumn wyniku
}

table_4 <- function(Posts, Users){
  Posts_dt <- as.data.table(Posts)
  Users_dt <- as.data.table(Users)
  Answers <- Posts_dt[PostTypeId == 2, .(AnswersNumber = .N), by = OwnerUserId]    # tworzymy tabelę Answers - wybieramy kolumnę OwnerUserId, wiersze zgodnie z warunkiem, grupujemy po OwnerUserId i zliczamy liczbę wierszy dla każdej wartości (kolumna AnswersNumber)
  Questions <- Posts_dt[PostTypeId == 1, .(QuestionsNumber = .N), by = OwnerUserId]    # analogicznie dla Questions, inny warunek na wiersze
  PostsCounts <- Answers[Questions, on = "OwnerUserId"]    # łączymy tabele Answers i Questions po kolumnie OwnerUserId
  PostsCounts <- PostsCounts[AnswersNumber > QuestionsNumber][order(-AnswersNumber)][1:5]    # wybieramy odpowiednie wiersze zgodnie z warunkiem, sortujemy malejąco po AnswersNumber, wybieramy pierwsze 5 wierszy
  result <- merge(PostsCounts, Users_dt, by.x = "OwnerUserId", by.y = "Id")    # łączymy tabele PostsCounts i Users_dt po odpowiadających sobie kolumnach z id użytkowników
  result[, c("DisplayName", "QuestionsNumber", "AnswersNumber", "Location", "Reputation", "UpVotes", "DownVotes")]    # wybór odpowiednich kolumn wyniku
}

# compare::compare(sql_4(Posts, Users), base_4(Posts, Users), allowAll = TRUE)
# compare::compare(sql_4(Posts, Users), dplyr_4(Posts, Users), allowAll = TRUE)
# compare::compare(sql_4(Posts, Users), table_4(Posts, Users), allowAll = TRUE)

# microbenchmark::microbenchmark(sql_4 = sql_4(Posts, Users), times = 25)
# Unit: seconds
# expr      min       lq     mean   median      uq      max neval
# sql_4 1.729227 1.751844 1.800919 1.821605 1.82903 1.928211    25

# microbenchmark::microbenchmark(base_4 = base_4(Posts, Users))
# Unit: milliseconds
# expr      min       lq     mean  median       uq      max neval
# base_4 539.4664 555.9956 584.8501 578.227 608.6974 833.7149   100

# microbenchmark::microbenchmark(dplyr_4 = dplyr_4(Posts, Users))
# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# dplyr_4 326.4833 365.4775 400.3113 383.2996 425.2091 669.2124   100

# microbenchmark::microbenchmark(table_4 = table_4(Posts, Users))
# Unit: milliseconds
# expr     min       lq     mean   median       uq      max neval
# table_4 28.5205 31.58025 57.15442 35.03325 94.24645 274.1893   100


# -----------------------------------------------------------------------------#
# Zadanie 5
# -----------------------------------------------------------------------------#

sql_5 <- function(Posts, Comments, Users){
  CmtTotScr <- sqldf("SELECT PostId, SUM(Score) AS CommentsTotalScore
                      FROM Comments
                      GROUP BY PostId")
  PostsBestComments <- sqldf("SELECT Posts.OwnerUserId, Posts.Title, Posts.CommentCount, Posts.ViewCount, CmtTotScr.CommentsTotalScore
                              FROM CmtTotScr
                              JOIN Posts ON Posts.Id = CmtTotScr.PostId
                              WHERE Posts.PostTypeId=1")
  sqldf("SELECT Title, CommentCount, ViewCount, CommentsTotalScore, DisplayName, Reputation, Location
        FROM PostsBestComments
        JOIN Users ON PostsBestComments.OwnerUserId = Users.Id
        ORDER BY CommentsTotalScore DESC
        LIMIT 10")
  # Tabelę PostsBestComments - powstałą przez złączenie tabeli CmtTotScr (utworzona z tabeli Comments - wybranie kolumny PostId, pogrupowanie po niej i zsumowanie wartości Score dla każdego PostId)
  # i tabeli Posts po kolumnach z id postów, wybór wierszy, dla których PostTypeId = 1 i wybór kolumn: "OwnerUserId", "Title", "CommentCount", "ViewCount", "CommentsTotalScore"
  # - łączymy z tabelą Users po kolumnach z id użytkowników, sortujemy malejąco po CommentsTotalScore, wybieramy pierwsze 10 wierszy i kolumny: "Title", "CommentCount", "ViewCount", "CommentsTotalScore", "DisplayName", "Reputation", "Location"
}

# Wynik zapytania:
# Tabela z kolumnami: Title, CommentCount, ViewCount, CommentsTotalScore, DisplayName, Reputation, Location,
# pokazuje 10 tytułów postów z największą sumą wartości Score, liczbę komentarzy do każdego z nich, liczbę wyświetleń, nazwę użytkownika, którego jest to post, jego reputację i lokalizację

base_5 <- function(Posts, Comments, Users){
    CmtTotScr <- aggregate(Comments["Score"], by = list(PostId = Comments$PostId), FUN = sum)    # grupowanie po PostId z tabeli Comments, zsumowanie dla każdego PostId liczby wierszy
    names(CmtTotScr) <- c("Id", "CommentsTotalScore")    # zmiana nazw kolumn tabeli CmtTotScr
    PostsBestComments <- merge(CmtTotScr, Posts, by = "Id")    # złączenie tabel CmtTotScr i Posts po kolumnie Id
    PostsBestComments <- PostsBestComments[PostsBestComments$PostTypeId == 1, c("OwnerUserId", "Title", "CommentCount", "ViewCount", "CommentsTotalScore")]    # wybór wierszy, dla których PostTypeId = 1 i wybór odpowiednich kolumn
    names(PostsBestComments)[1] <- "Id"    # zmiana nazwy pierwszej kolumny w tabeli PostsBestComments
    result <- merge(PostsBestComments, Users, by = "Id")    # złączenie tabel PostsBestComments i Users po kolumnie Id
    result <- result[order(result$CommentsTotalScore, decreasing = TRUE), c("Title", "CommentCount", "ViewCount", "CommentsTotalScore", "DisplayName", "Reputation", "Location")]    # sortowanie malejące po CommentsTotalScore, wybór odpowiednich kolumn
    result <- result[1:10,]    # wybór pierwszych 10 wierszy
    row.names(result) <- NULL    # zresetowanie etykiet wierszy
    result
}

dplyr_5 <- function(Posts, Comments, Users){
    CmtTotScr <- Comments %>%    # tworzymy tabelę CmtTotScr z tabeli Comments
      group_by(PostId) %>%    # grupujemy po kolumnie PostId
      summarize(CommentsTotalScore = sum(Score))    # dla każdego PostId sumujemy wartości Score w kolumnie CommentsTotalScore
    PostsBestComments <- inner_join(CmtTotScr, Posts, by = join_by("PostId" == "Id")) %>%    # tworzymy tabelę PostsBestComments - łączymy tabele CmtTotScr i Posts po kolumnie z id postów
      filter(PostTypeId == 1) %>%    # wybór wierszy, dla których PostTypeId = 1
      select(OwnerUserId, Title, CommentCount, ViewCount, CommentsTotalScore)    # wybór odpowiednich kolumn
    result <- inner_join(PostsBestComments, Users, by = join_by("OwnerUserId" == "Id")) %>%    # tabela result - łączymy tabele PostsBestComments i Users po kolumnie z id użytkowników
      select(Title, CommentCount, ViewCount, CommentsTotalScore, DisplayName, Reputation, Location) %>%    # wybór odpowiednich kolumn z result
      arrange(desc(CommentsTotalScore)) %>%    # sortowanie malejące po CommentsTotalScore
      head(10)    # wybór pierwszych 10 wierszy wyniku
}

table_5 <- function(Posts, Comments, Users){
  Posts_dt <- as.data.table(Posts)
  Users_dt <- as.data.table(Users)
  Comments_dt <- as.data.table(Comments)
  CmtTotScr <- Comments_dt[,.(CommentsTotalScore = sum(Score)), by = PostId]    # tworzymy tabelę CmtTotScr z tabeli Comments - grupujemy po kolumnie PostId, dla każdego PostId sumujemy wartości Score w kolumnie CommentsTotalScore
  PostsBestComments <- CmtTotScr[Posts_dt, on = c("PostId" = "Id")]    # tworzymy tabelę PostsBestComments - łączymy tabele CmtTotScr i Posts_dt po kolumnie z id postów
  PostsBestComments <- PostsBestComments[PostTypeId == 1, c("OwnerUserId", "Title", "CommentCount", "ViewCount", "CommentsTotalScore")]    # wybór wierszy, dla których PostTypeId = 1 i wybór odpowiednich kolumn
  result <- merge(PostsBestComments, Users_dt, by.x = "OwnerUserId", by.y = "Id")    # tabela result - łączymy tabele PostsBestComments i Users_dt po kolumnie z id użytkowników
  result[, c("Title", "CommentCount", "ViewCount", "CommentsTotalScore", "DisplayName", "Reputation", "Location")][order(-CommentsTotalScore)][1:10]    # wybór odpowiednich kolumn wyniku, posortowanie malejące po CommentsTotalScore, wybór pierwszych 10 wierszy wyniku
}

# compare::compare(sql_5(Posts, Comments, Users), base_5(Posts, Comments, Users), allowAll = TRUE)
# compare::compare(sql_5(Posts, Comments, Users), dplyr_5(Posts, Comments, Users), allowAll = TRUE)
# compare::compare(sql_5(Posts, Comments, Users), table_5(Posts, Comments, Users), allowAll = TRUE)

# microbenchmark::microbenchmark(sql_5 = sql_5(Posts, Comments, Users), times = 25)
# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# sql_5 2.166068 2.193878 2.232295 2.225482 2.274107 2.330074    25

# microbenchmark::microbenchmark(base_5 = base_5(Posts, Comments, Users))
# nit: seconds
# expr      min       lq     mean   median       uq      max neval
# base_5 1.530941 1.583058 1.634269 1.616275 1.671143 1.938626   100

# microbenchmark::microbenchmark(dplyr_5 = dplyr_5(Posts, Comments, Users))
# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# dplyr_5 275.5933 350.7658 410.8829 395.0866 445.4556 711.6068   100

# microbenchmark::microbenchmark(table_5 = table_5(Posts, Comments, Users))
# Unit: milliseconds
# expr     min       lq     mean  median       uq      max neval
# table_5 81.0135 93.14045 140.2031 145.268 159.8073 308.6779   100
