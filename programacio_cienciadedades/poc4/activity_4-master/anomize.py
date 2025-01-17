#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jan 16 14:39:02 2025

@author: albert
"""
import faker


def generate_name(val):
    """
    Generates a fake name using the Faker library.

    Parameters:
    val (any): Input value (not used in the function but required for apply method compatibility)

    Returns:
    str: Generated fake name
    """
    fake = faker.Faker()
    fake.seed_instance()
    return (fake.name())


def name_surname(df):
    """
    Anonymizes the 'biker' column in the dataframe by replacing names with fake names.
    Filters out entries where the 'time' column has the value "00:00:00".

    Parameters:
    df (DataFrame): Input dataframe with a 'biker' and 'time' column

    Returns:
    DataFrame: Modified dataframe with anonymized names and filtered entries
    """
    #  Anonymizes the 'biker' column using the funcion generate_name
    df["biker"] = df["biker"].apply(lambda x: generate_name(x))
    print("Despres de anominitzar les dades les 5 primeres entrades del dataset son: \n")
    print(df.head(5))
    
    # Filter out entries with 'time' value "00:00:00"
    df = df[df["time"] != "00:00:00"]
    print("\n El data set te " + str(len(df)) + " entrades \n")
    print("Despres de filtrar les dades les 5 primeres entrades del dataset son: \n")
    print(df.head(5))

    # Display data for biker with dorsal number 1000
    print("\n Les dades del cicliste amb el dorsal 1000 son: ")
    print(df[df["dorsal"] == 1000])
    return (df)
