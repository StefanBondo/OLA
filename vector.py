#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu May 21 14:16:28 2026

@author: jeppelund
"""

from langchain_chroma import Chroma

from langchain_ollama import OllamaEmbeddings


# -----------------------------------
# LOAD EMBEDDINGS
# -----------------------------------

embeddings = OllamaEmbeddings(
    model="mxbai-embed-large"
)


# -----------------------------------
# LOAD DATABASE
# -----------------------------------

db_location = "/Users/jeppelund/python/Elgigant og power reviews/electronics_ai_db"

vector_store = Chroma(

    persist_directory=db_location,

    embedding_function=embeddings,

    collection_name="electronics_reviews"

)


# -----------------------------------
# RETRIEVER
# -----------------------------------

retriever = vector_store.as_retriever(

    search_kwargs={"k": 20}

)


# -----------------------------------
# SEARCH FUNCTION
# -----------------------------------

def hent_anmeldelser(query):

    docs = retriever.invoke(query)

    return docs
