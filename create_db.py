#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu May 21 14:07:24 2026

@author: jeppelund
"""

import pandas as pd

from langchain_core.documents import Document

from langchain_ollama import OllamaEmbeddings

from langchain_chroma import Chroma

import shutil
import os
import gender_guesser.detector as gender


MAX_TEXT_LENGTH = 700
BATCH_SIZE = 10
TEST_MODE = True
TEST_ROWS_PER_COMPANY = 500


# -----------------------------------
# LOAD DATA
# -----------------------------------

elgiganten = pd.read_parquet(
    "/Users/jeppelund/python/Elgigant og power reviews/elgiganten.parquet"
)

power = pd.read_parquet(
    "/Users/jeppelund/python/Elgigant og power reviews/power.parquet"
)


# -----------------------------------
# VIRKSOMHEDSKOLONNE
# -----------------------------------

elgiganten["virksomhed"] = "Elgiganten"

power["virksomhed"] = "Power"


# -----------------------------------
# MERGE DATAFRAMES
# -----------------------------------

df = pd.concat(
    [elgiganten, power],
    ignore_index=True
)

if TEST_MODE:
    df = (
        df
        .groupby("virksomhed", group_keys=False)
        .sample(n=TEST_ROWS_PER_COMPANY, random_state=42)
        .reset_index(drop=True)
    )


print(df.head())

print(df["virksomhed"].value_counts())


# -----------------------------------
# FEATURE ENGINEERING
# -----------------------------------

df["rating"] = pd.to_numeric(
    df["rating"],
    errors="coerce"
) / 10


locations = [
    "Aalborg",
    "Aarhus",
    "Amager",
    "Ballerup",
    "Brøndby",
    "Esbjerg",
    "Fredericia",
    "Frederiksberg",
    "Glostrup",
    "Herning",
    "Hillerød",
    "Hjørring",
    "Horsens",
    "Kolding",
    "København",
    "Lyngby",
    "Næstved",
    "Odense",
    "Randers",
    "Roskilde",
    "Silkeborg",
    "Slagelse",
    "Taastrup",
    "Vejle",
    "Viborg",
]


def find_location(row):

    text = (
        str(row["title"]) + " " +
        str(row["content"])
    ).lower()

    for location in locations:
        if location.lower() in text:
            return location

    return "ukendt"


df["location"] = df.apply(
    find_location,
    axis=1
)


koen_detector = gender.Detector()


def gaet_koen(navn):

    navn = str(navn).strip()

    if navn == "" or navn.lower() in ["nan", "no name", "kunde", "customer"]:
        return "unknown"

    foerste_navn = navn.split()[0]
    foerste_navn = foerste_navn.split("-")[0]
    koen = koen_detector.get_gender(foerste_navn)

    if koen in ["mostly_male"]:
        return "male"

    if koen in ["mostly_female"]:
        return "female"

    return koen


df["gender"] = df["name"].apply(
    gaet_koen
)


def contains_words(row, words):

    text = (
        str(row["title"]) + " " +
        str(row["content"])
    ).lower()

    for word in words:
        if word in text:
            return True

    return False


df["delivery_issue"] = df.apply(
    lambda row: contains_words(
        row,
        [
            "levering",
            "leveret",
            "pakke",
            "fragt",
            "forsinket",
            "sendt",
            "postnord",
            "gls",
            "bring",
            "dao",
        ]
    ),
    axis=1
)

df["customer_service_issue"] = df.apply(
    lambda row: contains_words(
        row,
        [
            "kundeservice",
            "service",
            "support",
            "hjælp",
            "medarbejder",
            "ansat",
            "ekspedient",
            "telefon",
            "mail",
            "chat",
        ]
    ),
    axis=1
)

df["complaint_issue"] = df.apply(
    lambda row: contains_words(
        row,
        [
            "reklamation",
            "klage",
            "defekt",
            "fejl",
            "garanti",
            "reparation",
            "reparere",
            "ombytning",
        ]
    ),
    axis=1
)

df["price_issue"] = df.apply(
    lambda row: contains_words(
        row,
        [
            "pris",
            "billig",
            "dyr",
            "tilbud",
            "rabat",
            "price match",
            "prismatch",
            "faktureres",
            "kr",
        ]
    ),
    axis=1
)

df["return_issue"] = df.apply(
    lambda row: contains_words(
        row,
        [
            "retur",
            "returnere",
            "tilbage",
            "fortryd",
            "refusion",
            "pengene tilbage",
        ]
    ),
    axis=1
)

df["store_issue"] = df.apply(
    lambda row: contains_words(
        row,
        [
            "butik",
            "varehus",
            "kasse",
            "kø",
            "ventetid",
            "lager",
            "afhentning",
            "udlevering",
        ]
    ),
    axis=1
)


def sentiment_group(rating):

    if rating >= 4:
        return "positiv"
    if rating >= 3:
        return "neutral"
    return "negativ"


def find_problem_types(row):

    problem_types = []

    if row["delivery_issue"]:
        problem_types.append("levering")
    if row["customer_service_issue"]:
        problem_types.append("kundeservice")
    if row["complaint_issue"]:
        problem_types.append("reklamation")
    if row["price_issue"]:
        problem_types.append("pris")
    if row["return_issue"]:
        problem_types.append("retur")
    if row["store_issue"]:
        problem_types.append("butik")

    if len(problem_types) == 0:
        return "andet"

    return ", ".join(problem_types)


df["sentiment_group"] = df["rating"].apply(sentiment_group)
df["problem_types"] = df.apply(find_problem_types, axis=1)
df["review_length"] = df["content"].astype(str).str.len()
df["published_date"] = pd.to_datetime(df["published"], errors="coerce")
df["year"] = df["published_date"].dt.year.fillna(0).astype(int)
df["month"] = df["published_date"].dt.month.fillna(0).astype(int)


print(
    df[
        [
            "virksomhed",
            "rating",
            "sentiment_group",
            "location",
            "gender",
            "problem_types",
        ]
    ].head(20)
)

print(df["location"].value_counts().head(20))
print(df["gender"].value_counts())
print(df["sentiment_group"].value_counts())
print(df["problem_types"].value_counts().head(20))


# -----------------------------------
# EMBEDDINGS
# -----------------------------------

embeddings = OllamaEmbeddings(
    model="mxbai-embed-large"
)


# -----------------------------------
# DATABASE LOCATION
# -----------------------------------

db_location = "/Users/jeppelund/python/Elgigant og power reviews/electronics_ai_db"
features_location = "/Users/jeppelund/python/Elgigant og power reviews/electronics_features.parquet"


df.to_parquet(features_location, index=False)

print(f"Feature dataframe gemt her: {features_location}")


# slet gammel db
if os.path.exists(db_location):

    shutil.rmtree(db_location)


os.makedirs(db_location, exist_ok=True)


# -----------------------------------
# CREATE DOCUMENTS
# -----------------------------------

documents = []

for i, row in df.iterrows():

    text = (
        "Virksomhed: " + str(row["virksomhed"]) + "\n" +
        "Location: " + str(row["location"]) + "\n" +
        "Rating: " + str(row["rating"]) + "\n" +
        "Sentiment: " + str(row["sentiment_group"]) + "\n" +
        "Køn: " + str(row["gender"]) + "\n" +
        "Temaer: " + str(row["problem_types"]) + "\n" +
        str(row["title"]) + " " +
        str(row["content"])
    )

    # forkort reviews, ellers kan Ollama embedding få for langt input
    text = text[:MAX_TEXT_LENGTH]

    rating = row["rating"]
    if pd.isna(rating):
        rating = 0

    reviewcount = row["reviewCount"]
    if pd.isna(reviewcount):
        reviewcount = 0

    document = Document(

        page_content=text,

        metadata={

            "rating": float(rating),
            "date": str(row["published"]),
            "username": str(row["name"]),
            "reviewcount": int(reviewcount),
            "virksomhed": row["virksomhed"],
            "location": row["location"],
            "gender": row["gender"],
            "sentiment_group": row["sentiment_group"],
            "problem_types": row["problem_types"],
            "delivery_issue": bool(row["delivery_issue"]),
            "customer_service_issue": bool(row["customer_service_issue"]),
            "complaint_issue": bool(row["complaint_issue"]),
            "price_issue": bool(row["price_issue"]),
            "return_issue": bool(row["return_issue"]),
            "store_issue": bool(row["store_issue"]),
            "review_length": int(row["review_length"]),
            "year": int(row["year"]),
            "month": int(row["month"])

        },

        id=str(i)

    )

    documents.append(document)


# -----------------------------------
# CREATE VECTOR DATABASE
# -----------------------------------

vector_store = Chroma(
    persist_directory=db_location,
    embedding_function=embeddings,
    collection_name="electronics_reviews"
)


# Tilføj i små batches, så Ollama ikke får for meget tekst på én gang

for start in range(0, len(documents), BATCH_SIZE):

    batch = documents[start:start + BATCH_SIZE]
    ids = [doc.id for doc in batch]

    vector_store.add_documents(
        documents=batch,
        ids=ids
    )

    print(
        f"Tilføjet {min(start + BATCH_SIZE, len(documents))} af {len(documents)} reviews"
    )


print("Database oprettet!")
