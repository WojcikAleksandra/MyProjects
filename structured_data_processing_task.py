"""
This script performs data analysis using Python libraries pandas and NumPy
together with an SQLite database. Data are loaded from compressed CSV files,
stored in a local SQLite database, and analyzed using SQL queries.

The same analytical results are then reproduced using pandas and NumPy
operations and compared with the SQL results to verify correctness.
"""

# 1.) Przygotowanie danych

import pandas as pd
import numpy as np

import os

Posts = pd.read_csv(os.path.expanduser("~/Posts.csv.gz"), compression="gzip")
# print(Posts.head())

Users = pd.read_csv(os.path.expanduser("~/Users.csv.gz"), compression = 'gzip')
# print(Users.head())

Comments = pd.read_csv(os.path.expanduser("~/Comments.csv.gz"), compression = 'gzip')
# print(Comments.head())

import sqlite3

baza = os.path.expanduser('~/pdu_pd5.db')
conn = sqlite3.connect(baza)
# Comments.to_sql("Comments", conn)
# Posts.to_sql("Posts", conn)
# Users.to_sql("Users", conn)


# 2.) Wyniki zapytań SQL

sql_1 = pd.read_sql_query("""SELECT Location, SUM(UpVotes) as TotalUpVotes
FROM Users
WHERE Location != ''
GROUP BY Location
ORDER BY TotalUpVotes DESC
LIMIT 10
""", conn)
# print(sql_1)
# Wynikowa tabela pokazuje 10 lokalizacji (niepusta nazwa lokalizacji) z największą sumaryczną liczbą UpVotes (dane z tabeli Users) podaną w kolumnie TotalUpVotes, posortowane są malejąco po wartościach w kolumnie TotalUpVotes

sql_2 = pd.read_sql_query("""SELECT STRFTIME('%Y', CreationDate) AS Year, STRFTIME('%m', CreationDate) AS Month,
COUNT(*) AS PostsNumber, MAX(Score) AS MaxScore
FROM Posts
WHERE PostTypeId IN (1, 2)
GROUP BY Year, Month
HAVING PostsNumber > 1000""", conn)
# print(sql_2)
# Wynikowa tabela pokazuje liczbę postów (kolumna PostsNumber), których PostTypeId jest równe 1 lub 2, oraz największą wartość Score (kolumna MaxScore) tych postów dla każdej pary rok-miesiąc, dla której PostsNumber jest większe niż 1000. Dane pobrane z tabeli Posts.

sql_3 = pd.read_sql_query("""SELECT Id, DisplayName, TotalViews
FROM (
SELECT OwnerUserId, SUM(ViewCount) as TotalViews
FROM Posts
WHERE PostTypeId = 1
GROUP BY OwnerUserId
) AS Questions
JOIN Users
ON Users.Id = Questions.OwnerUserId
ORDER BY TotalViews DESC
LIMIT 10
""", conn)
# print(sql_3)
# Wynikowa tabela pokazuje 10 użytkowników (ich numer Id i nazwę DisplayName) z największą sumaryczną liczbą wyświetleń swoich postów, których PostTypeId jest równe 1 (podaną w kolumnie TotalViews). Dane w tabeli posortowane są malejąco po wartościach TotalViews i pochodzą w tabel Posts oraz Users.

sql_4 = pd.read_sql_query("""SELECT DisplayName, QuestionsNumber, AnswersNumber, Location, Reputation, UpVotes, DownVotes
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
ON PostsCounts.OwnerUserId = Users.Id""", conn)
# print(sql_4)
# Wynikowa tabela zawiera informacje o użytkownikach (ich nazwę DisplayName, liczbę postów typu pytanie (PostTypeId = 1), liczbę postów typu odpowiedź (PostTypeId = 2), lokalizację, reputację, liczbę głosów za (UpVotes) i przeciw (DownVotes)),
# dla których liczba postów typu odpowiedź jest większa od liczby postów typu pytanie i spośród nich wybranych jest pięciu, którzy mają największą liczbę postów typu odpowiedź (dane posortowane są malejąco po liczbie odpowiedzi). Dane pochodzą z tabel Posts i Users.

# sql_5:

CmtTotScr = pd.read_sql_query("""SELECT PostId, SUM(Score) AS CommentsTotalScore
FROM Comments
GROUP BY PostId""", conn)

# CmtTotScr.to_sql("CmtTotScr", conn)

PostsBestComments = pd.read_sql_query("""SELECT Posts.OwnerUserId, Posts.Title, Posts.CommentCount, Posts.ViewCount,
CmtTotScr.CommentsTotalScore
FROM CmtTotScr
JOIN Posts ON Posts.Id = CmtTotScr.PostId
WHERE Posts.PostTypeId=1""", conn)

# PostsBestComments.to_sql("PostsBestComments", conn)

sql_5 = pd.read_sql_query("""SELECT Title, CommentCount, ViewCount, CommentsTotalScore, DisplayName, Reputation, Location
FROM PostsBestComments
JOIN Users ON PostsBestComments.OwnerUserId = Users.Id
ORDER BY CommentsTotalScore DESC
LIMIT 10""", conn)
# print(sql_5)
# Wynikowa tabela zawiera informacje o 10 postach typu pytanie (PostTypeId = 1) - ich tytuł, liczbę komentarzy, liczbę wyświetleń, łączny wynik komentarzy (CommentsTotalScore), nazwę użytkownika, który jest autorem postu oraz jego reputację i lokalizację,
# z największą wartością CommentsTotalScore. Dane posortowane są malejąco po CommentsTotalScore. Pochodzą z tabel Comments, Posts i Users.


# 3.) Wyniki zapytań SQL odtworzone przy użyciu metod pakietu Pandas.

# zad. 1

try:
    pandas_1 = Users[Users['Location'] != ''].groupby('Location')['UpVotes'].sum()
    pandas_1 = pandas_1.to_frame()
    pandas_1 = pandas_1.rename(columns={'UpVotes': 'TotalUpVotes'})
    pandas_1 = pandas_1.sort_values(by='TotalUpVotes', ascending=False).head(10).reset_index()
    print(pandas_1.equals(sql_1))
except Exception as e:
    print("Zad. 1: niepoprawny wynik.")
    print(e)

# zad. 2

try:
     pandas_2 = Posts
     pandas_2['CreationDate'] = pd.to_datetime(pandas_2['CreationDate'])
     pandas_2['Year'] = pandas_2['CreationDate'].dt.strftime('%Y')
     pandas_2['Month'] = pandas_2['CreationDate'].dt.strftime('%m')
     pandas_2 = pandas_2[pandas_2['PostTypeId'].isin([1, 2])].groupby(['Year', 'Month']).agg(PostsNumber = ('Year', 'size'), MaxScore = ('Score', 'max'))
     pandas_2 = pandas_2[pandas_2['PostsNumber'] > 1000].reset_index()
     print(pandas_2.equals(sql_2))
except Exception as e:
    print("Zad. 2: niepoprawny wynik.")
    print(e)

# zad. 3

try:
    Questions = Posts[Posts['PostTypeId'] == 1].groupby('OwnerUserId')['ViewCount'].sum()
    Questions = Questions.to_frame()
    Questions = Questions.rename(columns={'ViewCount': 'TotalViews'})
    pandas_3 = pd.merge(Questions, Users[['Id', 'DisplayName']], left_on='OwnerUserId', right_on='Id', how='inner')
    pandas_3 = pandas_3.sort_values(by='TotalViews', ascending=False).head(10).reset_index()
    pandas_3 = pandas_3[['Id', 'DisplayName', 'TotalViews']]
    print(pandas_3.equals(sql_3))
except Exception as e:
    print("Zad. 3: niepoprawny wynik.")
    print(e)

# zad. 4

try:
    Answers = Posts[Posts['PostTypeId'] == 2].groupby('OwnerUserId')['AnswerCount'].size()
    Answers = Answers.to_frame()
    Answers = Answers.rename(columns={'AnswerCount': 'AnswersNumber'})
    Questions = Posts[Posts['PostTypeId'] == 1].groupby('OwnerUserId')['AnswerCount'].size()
    Questions = Questions.to_frame()
    Questions = Questions.rename(columns={'AnswerCount': 'QuestionsNumber'})
    PostsCounts = pd.merge(Answers, Questions, on='OwnerUserId', how='inner')
    PostsCounts = PostsCounts[PostsCounts['AnswersNumber'] > PostsCounts['QuestionsNumber']].sort_values(by='AnswersNumber', ascending=False).head(5).reset_index()
    pandas_4 = pd.merge(PostsCounts, Users, left_on='OwnerUserId', right_on='Id', how='inner')
    pandas_4 = pandas_4[['DisplayName', 'QuestionsNumber', 'AnswersNumber', 'Location', 'Reputation', 'UpVotes', 'DownVotes']]
    print (pandas_4.equals(sql_4) )
except Exception as e:
    print("Zad. 4: niepoprawny wynik.")
    print(e)

# zad. 5

try:
    CmtTotScr_pd = Comments[['PostId', 'Score']].groupby('PostId')['Score'].sum()
    CmtTotScr_pd = CmtTotScr_pd.to_frame()
    CmtTotScr_pd = CmtTotScr_pd.rename(columns={'Score': 'CommentsTotalScore'})
    PostsBestComments_pd = pd.merge(Posts, CmtTotScr_pd, left_on='Id', right_on='PostId', how='inner')
    PostsBestComments_pd = PostsBestComments_pd[PostsBestComments_pd['PostTypeId'] == 1]
    PostsBestComments_pd = PostsBestComments_pd[['OwnerUserId', 'Title', 'CommentCount', 'ViewCount', 'CommentsTotalScore']]
    pandas_5 = pd.merge(Users, PostsBestComments_pd, left_on='Id', right_on='OwnerUserId', how='inner')
    pandas_5 = pandas_5[['Title', 'CommentCount', 'ViewCount', 'CommentsTotalScore', 'DisplayName', 'Reputation', 'Location']].sort_values(by='CommentsTotalScore', ascending=False).head(10).reset_index()
    pandas_5 = pandas_5.drop(pandas_5.columns[0], axis=1)
    print(pandas_5.equals(sql_5))
except Exception as e:
    print("Zad. 5: niepoprawny wynik.")
    print(e)

conn.close()