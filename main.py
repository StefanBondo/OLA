#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu May 21 14:16:29 2026

@author: jeppelund
"""

from langchain_ollama.llms import OllamaLLM

from langchain_core.prompts import ChatPromptTemplate

import vector
import pandas as pd
import os
os.environ["MPLCONFIGDIR"] = "/Users/jeppelund/python/Elgigant og power reviews/.matplotlib_cache"
os.makedirs(os.environ["MPLCONFIGDIR"], exist_ok=True)
import matplotlib.pyplot as plt
from collections import Counter, defaultdict


FEATURES_LOCATION = "/Users/jeppelund/python/Elgigant og power reviews/electronics_features.parquet"
PLOTS_DIR = "/Users/jeppelund/python/Elgigant og power reviews/plots"


# -----------------------------------
# MODEL
# -----------------------------------

model = OllamaLLM(
    model="llama3.2"
)


# -----------------------------------
# PROMPT
# -----------------------------------

template = """

Du er ekspert i kundeanmeldelser.

Brug KUN anmeldelserne og statistikblokken nedenfor.

Vigtige regler:
- Du må ikke opfinde tal, procenter, antal reviews, gennemsnit eller fordelinger.
- Hvis et tal ikke står i statistikblokken, må du ikke nævne det.
- Hvis anmeldelserne ikke giver grundlag for et svar, skal du sige det tydeligt.
- Skeln mellem statistik for hele datasættet og de få anmeldelser, der er hentet med søgning.

Svar på dansk.

Forklar:
- overordnet tendens
- forskelle mellem virksomheder
- service
- levering
- reklamationer
- kundeservice
- pris
- konkrete problemer eller styrker
- brug kun statistik som støtte, hvis spørgsmålet lægger op til det
- ved almindelige spørgsmål skal du svare som en normal tekstbaseret analyse af anmeldelserne

Statistik for de hentede anmeldelser:
{stats}

Anmeldelser:
{reviews}

Spørgsmål:
{question}

Svar:
"""


prompt = ChatPromptTemplate.from_template(
    template
)

chain = prompt | model


# -----------------------------------
# INTERPRETATION PROMPT
# -----------------------------------

interpretation_template = """

Du er en dansk analyseassistent.

Forklar hvad statistikken betyder i almindeligt dansk.

Vigtige regler:
- Brug KUN tallene i statistikblokken.
- Opfind ikke nye tal, procenter eller konklusioner.
- Hvis forskellen er lille, så sig at forskellen er lille.
- Hvis datagrundlaget er lille, så nævn det.
- Hvis flere tema-procenter tilsammen overstiger 100%, så forklar at én anmeldelse kan nævne flere temaer.
- Svar med 5-8 sætninger som sammenhængende brødtekst.

Statistik:
{stats}

Spørgsmål:
{question}

Fortolkning:
"""


interpretation_prompt = ChatPromptTemplate.from_template(
    interpretation_template
)

interpretation_chain = interpretation_prompt | model


# -----------------------------------
# DATAFRAME STATS
# -----------------------------------

def load_feature_df():

    try:
        return pd.read_parquet(FEATURES_LOCATION)
    except FileNotFoundError:
        return None


def is_stats_question(question):

    q = question.lower()

    clear_stats_words = [
        "statistik",
        "statistisk",
        "beskrivende",
        "gennemsnit",
        "fordeling",
        "antal",
        "procent",
        "%",
        "hvor mange",
        "median",
        "laveste rating",
        "højeste rating",
        "hojeste rating",
        "gennemsnitlig rating",
        "ratingfordeling",
        "sentimentfordeling",
        "mænd",
        "maend",
        "mand",
        "kvinder",
        "kvinde",
        "køn",
        "koen",
    ]

    for word in clear_stats_words:
        if word in q:
            return True

    return False


def filter_company(df, question):

    q = question.lower()

    if "elgiganten" in q:
        return df[df["virksomhed"] == "Elgiganten"], "Elgiganten"

    if "power" in q:
        return df[df["virksomhed"] == "Power"], "Power"

    return df, "Elgiganten og Power"


def filter_location(df, question):

    if "location" not in df.columns:
        return df, None

    q = question.lower()

    known_locations = [
        location
        for location in df["location"].dropna().unique()
        if location != "ukendt"
    ]

    known_locations = sorted(
        known_locations,
        key=lambda location: len(str(location)),
        reverse=True
    )

    for location in known_locations:
        if str(location).lower() in q:
            return df[df["location"] == location], location

    return df, None


def pct(count, total):

    if total == 0:
        return 0

    return count / total * 100


def is_gender_question(question):

    q = question.lower()

    gender_words = [
        "mænd",
        "maend",
        "mand",
        "kvinder",
        "kvinde",
        "køn",
        "koen",
    ]

    for word in gender_words:
        if word in q:
            return True

    return False


def answer_with_simple_counts(question):

    q = question.lower()

    if "hvor mange" not in q and "antal" not in q:
        return None

    df = load_feature_df()

    if df is None:
        return (
            "Jeg kan ikke tælle endnu, fordi "
            "electronics_features.parquet ikke findes. Kør create_db.py først."
        )

    data, company_label = filter_company(df, question)
    data, location_label = filter_location(data, question)

    if location_label is not None:
        label = f"{company_label} i {location_label}"
    else:
        label = company_label

    if "kvinder" in q or "kvinde" in q:
        count = len(data[data["gender"] == "female"])
        return f"Der er {count} anmeldelser fra kvinder i {label}."

    if "mænd" in q or "maend" in q or "mand" in q:
        count = len(data[data["gender"] == "male"])
        return f"Der er {count} anmeldelser fra mænd i {label}."

    if "unknown" in q or "ukendt" in q:
        count = len(data[data["gender"] == "unknown"])
        return f"Der er {count} anmeldelser med ukendt køn i {label}."

    if "anmeldelser" in q or "reviews" in q:
        return f"Der er {len(data)} anmeldelser i {label}."

    return None


def is_plot_question(question):

    q = question.lower()

    plot_words = [
        "graf",
        "plot",
        "visualiser",
        "visualisering",
        "diagram",
        "bar chart",
        "søjle",
    ]

    for word in plot_words:
        if word in q:
            return True

    return False


def make_plot(question):

    if not is_plot_question(question):
        return None

    df = load_feature_df()

    if df is None:
        return (
            "Jeg kan ikke lave graf endnu, fordi "
            "electronics_features.parquet ikke findes. Kør create_db.py først."
        )

    data, company_label = filter_company(df, question)
    data, location_label = filter_location(data, question)

    if location_label is not None:
        label = f"{company_label} i {location_label}"
    else:
        label = company_label

    if len(data) == 0:
        return f"Der findes ingen data for {label}."

    q = question.lower()
    os.makedirs(PLOTS_DIR, exist_ok=True)

    plt.figure(figsize=(9, 5))

    if "køn" in q or "koen" in q or "mænd" in q or "kvinder" in q:
        plot_data = (
            data[data["gender"].isin(["male", "female"])]
            .groupby("gender")["rating"]
            .mean()
            .sort_values(ascending=False)
        )
        title = f"Gennemsnitlig rating efter køn - {label}"
        filename = "graf_koen_rating.png"
        ylabel = "Gennemsnitlig rating"

    elif "sentiment" in q or "positive" in q or "negative" in q:
        plot_data = data["sentiment_group"].value_counts()
        title = f"Sentimentfordeling - {label}"
        filename = "graf_sentiment.png"
        ylabel = "Antal anmeldelser"

    elif "tema" in q or "problem" in q or "levering" in q or "kundeservice" in q:
        issue_columns = {
            "Levering": "delivery_issue",
            "Kundeservice": "customer_service_issue",
            "Reklamation": "complaint_issue",
            "Pris": "price_issue",
            "Retur": "return_issue",
            "Butik": "store_issue",
        }
        values = {}
        for label_name, column in issue_columns.items():
            if column in data.columns:
                values[label_name] = int(data[column].sum())
        plot_data = pd.Series(values).sort_values(ascending=False)
        title = f"Temaer i anmeldelser - {label}"
        filename = "graf_temaer.png"
        ylabel = "Antal anmeldelser"

    elif "location" in q or "lokation" in q or "by" in q:
        plot_data = data["location"].value_counts().head(10)
        title = f"Hyppigste locations - {label}"
        filename = "graf_locations.png"
        ylabel = "Antal anmeldelser"

    else:
        plot_data = (
            data
            .dropna(subset=["rating"])
            .groupby("virksomhed")["rating"]
            .mean()
            .sort_values(ascending=False)
        )
        title = f"Gennemsnitlig rating - {label}"
        filename = "graf_rating.png"
        ylabel = "Gennemsnitlig rating"

    if len(plot_data) == 0:
        plt.close()
        return f"Der er ikke nok data til at lave grafen for {label}."

    ax = plot_data.plot(kind="bar")
    ax.set_title(title)
    ax.set_xlabel("")
    ax.set_ylabel(ylabel)
    plt.xticks(rotation=30, ha="right")
    plt.tight_layout()

    plot_path = os.path.join(PLOTS_DIR, filename)
    plt.savefig(plot_path, dpi=150)
    plt.show()

    return f"Grafen er lavet og gemt her: {plot_path}"


def answer_with_dataframe_facts(question):

    q = question.lower()

    fact_words = [
        "højest rating",
        "hojest rating",
        "højeste rating",
        "hojeste rating",
        "lavest rating",
        "laveste rating",
        "hvad er",
        "hvor ligger",
    ]

    if not any(word in q for word in fact_words):
        return None

    df = load_feature_df()

    if df is None:
        return (
            "Jeg kan ikke slå det op endnu, fordi "
            "electronics_features.parquet ikke findes. Kør create_db.py først."
        )

    data, company_label = filter_company(df, question)
    data, location_label = filter_location(data, question)
    rating_data = data.dropna(subset=["rating"])

    if len(rating_data) == 0:
        return "Der findes ikke rating-data for det udsnit."

    if (
        "højest rating" in q
        or "hojest rating" in q
        or "højeste rating" in q
        or "hojeste rating" in q
        or "hvor ligger" in q
    ):
        highest = rating_data["rating"].max()
        rows = rating_data[rating_data["rating"] == highest]

        if "location" in rows.columns:
            location_counts = rows["location"].value_counts()
            locations = [
                location
                for location in location_counts.index.tolist()
                if location != "ukendt"
            ]
        else:
            locations = []

        if len(locations) > 0:
            location_text = ", ".join(locations[:10])
        else:
            location_text = "ukendt location"

        return (
            f"Den højeste rating for {company_label} i feature-datasættet er "
            f"{highest:.1f}. Den findes i {len(rows)} anmeldelser. "
            f"Registrerede locations med denne rating: {location_text}."
        )

    if "lavest rating" in q or "laveste rating" in q:
        lowest = rating_data["rating"].min()
        rows = rating_data[rating_data["rating"] == lowest]

        return (
            f"Den laveste rating for {company_label} i feature-datasættet er "
            f"{lowest:.1f}. Den findes i {len(rows)} anmeldelser."
        )

    if "hvad er" in q and "rating" in q:
        avg_rating = rating_data["rating"].mean()
        count = len(rating_data)

        if location_label is not None:
            label = f"{company_label} i {location_label}"
        else:
            label = company_label

        return (
            f"{label} har en gennemsnitlig rating på {avg_rating:.2f} "
            f"baseret på {count} anmeldelser i feature-datasættet."
        )

    return None


def answer_with_dataframe_stats(question):

    if not is_stats_question(question):
        return None

    df = load_feature_df()

    if df is None:
        return (
            "Jeg kan ikke lave statistik endnu, fordi "
            "electronics_features.parquet ikke findes. Kør create_db.py først."
        )

    data, company_label = filter_company(df, question)
    data, location_label = filter_location(data, question)

    if location_label is not None:
        label = f"{company_label} i {location_label}"
    else:
        label = company_label

    if len(data) == 0:
        return f"Der findes ingen anmeldelser for {label} i feature-datasættet."

    rating_data = data.dropna(subset=["rating"])
    total = len(data)

    lines = []
    lines.append(f"Beskrivende statistik for {label}:")
    lines.append(f"Antal anmeldelser i datasættet: {total}")

    if len(rating_data) > 0:
        lines.append(f"Gennemsnitlig rating: {rating_data['rating'].mean():.2f}")
        lines.append(f"Median rating: {rating_data['rating'].median():.2f}")
        lines.append(f"Laveste rating: {rating_data['rating'].min():.1f}")
        lines.append(f"Højeste rating: {rating_data['rating'].max():.1f}")

    if "virksomhed" in data.columns:
        lines.append("")
        lines.append("Fordeling pr. virksomhed:")
        for virksomhed, count in data["virksomhed"].value_counts().items():
            lines.append(f"{virksomhed}: {count} anmeldelser ({pct(count, total):.1f}%)")

    if "sentiment_group" in data.columns:
        lines.append("")
        lines.append("Sentimentfordeling:")
        for sentiment, count in data["sentiment_group"].value_counts().items():
            lines.append(f"{sentiment}: {count} anmeldelser ({pct(count, total):.1f}%)")

    issue_columns = {
        "Levering": "delivery_issue",
        "Kundeservice": "customer_service_issue",
        "Reklamation": "complaint_issue",
        "Pris": "price_issue",
        "Retur": "return_issue",
        "Butik": "store_issue",
    }

    lines.append("")
    lines.append("Temaer fundet med simple keyword-regler:")
    for label_name, column in issue_columns.items():
        if column in data.columns:
            count = int(data[column].sum())
            lines.append(f"{label_name}: {count} anmeldelser ({pct(count, total):.1f}%)")

    if "location" in data.columns:
        location_counts = data["location"].value_counts().head(10)
        lines.append("")
        lines.append("Hyppigste locations:")
        for location, count in location_counts.items():
            lines.append(f"{location}: {count} anmeldelser ({pct(count, total):.1f}%)")

    if "review_length" in data.columns:
        lines.append("")
        lines.append(f"Gennemsnitlig review-længde: {data['review_length'].mean():.0f} tegn")

    if "gender" in data.columns and is_gender_question(question):
        gender_data = data[
            data["gender"].isin(["male", "female"])
        ]

        lines.append("")
        lines.append("Kønsfordeling og rating:")

        if len(gender_data) == 0:
            lines.append(
                "Der er ikke nok anmeldelser med genkendt mand/kvinde-navn til at sammenligne køn."
            )
        else:
            gender_summary = (
                gender_data
                .groupby("gender")
                .agg(
                    antal=("rating", "count"),
                    gennemsnit_rating=("rating", "mean"),
                    positive=("sentiment_group", lambda x: (x == "positiv").sum()),
                    negative=("sentiment_group", lambda x: (x == "negativ").sum()),
                )
                .reset_index()
            )

            for _, row in gender_summary.iterrows():
                gender_name = row["gender"]
                antal = int(row["antal"])
                positive = int(row["positive"])
                negative = int(row["negative"])

                if gender_name == "male":
                    gender_label = "Mænd"
                else:
                    gender_label = "Kvinder"

                lines.append(
                    f"{gender_label}: {antal} anmeldelser, "
                    f"gennemsnitlig rating {row['gennemsnit_rating']:.2f}, "
                    f"{positive} positive ({pct(positive, antal):.1f}%), "
                    f"{negative} negative ({pct(negative, antal):.1f}%)"
                )

            best_gender = gender_summary.sort_values(
                "gennemsnit_rating",
                ascending=False
            ).iloc[0]

            if best_gender["gender"] == "male":
                best_label = "mænd"
            else:
                best_label = "kvinder"

            lines.append(
                f"Højeste gennemsnitlige rating i denne gruppe findes hos {best_label}."
            )

    lines.append("")
    lines.append(
        "Tallene er beregnet direkte i pandas på feature-datasættet, ikke gættet af modellen."
    )

    return "\n".join(lines)


# -----------------------------------
# FORMAT STATS
# -----------------------------------

def format_stats(docs):

    if len(docs) == 0:
        return "Der blev ikke hentet nogen anmeldelser."

    ratings_by_company = defaultdict(list)
    sentiment_counter = Counter()
    company_counter = Counter()
    location_counter = Counter()
    problem_counter = Counter()
    issue_counter = Counter()

    for doc in docs:

        metadata = doc.metadata
        virksomhed = metadata.get("virksomhed", "ukendt")
        rating = metadata.get("rating")
        sentiment = metadata.get("sentiment_group", "ukendt")
        location = metadata.get("location", "ukendt")
        problem_types = metadata.get("problem_types", "andet")
        gender = metadata.get("gender", "unknown")

        company_counter[virksomhed] += 1
        sentiment_counter[sentiment] += 1
        location_counter[location] += 1

        if isinstance(rating, int) or isinstance(rating, float):
            ratings_by_company[virksomhed].append(rating)

        for problem_type in str(problem_types).split(","):
            problem_type = problem_type.strip()
            if problem_type:
                problem_counter[problem_type] += 1

        issue_fields = {
            "levering": "delivery_issue",
            "kundeservice": "customer_service_issue",
            "reklamation": "complaint_issue",
            "pris": "price_issue",
            "retur": "return_issue",
            "butik": "store_issue",
        }

        for label, field in issue_fields.items():
            if metadata.get(field) is True:
                issue_counter[label] += 1

    lines = []
    lines.append(f"Antal hentede anmeldelser: {len(docs)}")

    lines.append("Antal anmeldelser pr. virksomhed:")
    for virksomhed, count in company_counter.items():
        lines.append(f"{virksomhed}: {count}")

    lines.append("Gennemsnitlig rating pr. virksomhed:")
    for virksomhed, ratings in ratings_by_company.items():
        avg_rating = sum(ratings) / len(ratings)
        lines.append(f"{virksomhed}: {avg_rating:.2f}")

    lines.append("Sentimentfordeling:")
    for sentiment, count in sentiment_counter.items():
        lines.append(f"{sentiment}: {count}")

    lines.append("Hyppigste temaer:")
    for problem_type, count in problem_counter.most_common(8):
        lines.append(f"{problem_type}: {count}")

    lines.append("Hyppigste locations:")
    for location, count in location_counter.most_common(8):
        lines.append(f"{location}: {count}")

    return "\n".join(lines)


# -----------------------------------
# FORMAT REVIEWS
# -----------------------------------

def format_reviews(docs):

    dele = []

    for doc in docs:

        virksomhed = doc.metadata.get(
            "virksomhed",
            "ukendt"
        )

        rating = doc.metadata.get(
            "rating",
            ""
        )

        location = doc.metadata.get(
            "location",
            "ukendt"
        )

        sentiment_group = doc.metadata.get(
            "sentiment_group",
            ""
        )

        problem_types = doc.metadata.get(
            "problem_types",
            ""
        )

        gender = doc.metadata.get(
            "gender",
            "unknown"
        )

        dele.append(

            f"""
Virksomhed: {virksomhed}

Location: {location}

Rating: {rating}

Sentiment: {sentiment_group}

Køn: {gender}

Temaer: {problem_types}

Review:
{doc.page_content}
"""
        )

    return "\n\n---\n\n".join(dele)


# -----------------------------------
# CHAT LOOP
# -----------------------------------

while True:

    print("\n-------------------------")

    question = input(
        "Stil spørgsmål (q to quit): "
    )

    if question == "q":
        break

    plot_answer = make_plot(
        question
    )

    if plot_answer is not None:
        print("\n")
        print(plot_answer)
        print()
        continue

    simple_answer = answer_with_simple_counts(
        question
    )

    if simple_answer is not None:
        print("\n")
        print(simple_answer)
        print()
        continue

    fact_answer = answer_with_dataframe_facts(
        question
    )

    if fact_answer is not None:
        print("\n")
        print(fact_answer)

        interpretation = interpretation_chain.invoke({
            "stats": fact_answer,
            "question": question
        })

        print("\nFortolkning:\n")
        print(interpretation.strip())
        print()
        continue

    stats_answer = answer_with_dataframe_stats(
        question
    )

    if stats_answer is not None:
        print("\n")
        print(stats_answer)

        interpretation = interpretation_chain.invoke({
            "stats": stats_answer,
            "question": question
        })

        print("\nFortolkning:\n")
        print(interpretation.strip())
        print()
        continue


    reviews = vector.hent_anmeldelser(
        question
    )

    reviews_tekst = format_reviews(
        reviews
    )

    stats_tekst = format_stats(
        reviews
    )

    result = chain.invoke({

        "reviews": reviews_tekst,

        "stats": stats_tekst,

        "question": question

    })

    print("\n")

    print(result)

    print()
