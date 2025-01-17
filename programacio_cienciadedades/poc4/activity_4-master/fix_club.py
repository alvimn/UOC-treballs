#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jan 16 20:20:01 2025

@author: albert
"""
import re


def clean_club(name):
    """
    Cleans the club name by removing specific substrings and patterns.
    
    Parameters:
    name (str): The original club name.
    
    Returns:
    str: The cleaned club name.
    """
     # Convert name to uppercase
    name = name.upper()

    # List of substrings to remove from the club name
    to_delete = ["PEÑA CICLISTA", "PENYA CICLISTA",
                 "AGRUPACIÓN CICLISTA", "AGRUPACION CICLISTA",
                 "AGRUPACIÓ CICLISTA", "AGRUPACIO CICLISTA",
                 "CLUB CICLISTA", "CLUB"]
    
    # Remove specified substrings
    for i in to_delete:
        name = name.replace(i, "")

    # List of patterns to remove from the start of the club name
    to_delete = ['C.C. ', 'C.C ', 'CC ', 'C.D. ',
                 'C.D ', 'CD ', 'A.C. ', 'A.C ',
                 'AC ', 'A.D. ', 'A.D ', 'AD ',
                 'A.E. ', 'A.E ', 'AE ', 'E.C. ',
                 'E.C ', 'EC ', 'S.C. ', 'S.C ',
                 'SC ', 'S.D. ', 'S.D ', 'SD ']
                   
    # Remove specified patterns from the start of the name
    for i in to_delete:
        name = re.sub("^" + i, "", name)

    # List of patterns to remove from the end of the club name
    to_delete = [' T.T.', ' T.T', ' TT',
                 ' T.E.', ' T.E', ' TE',
                 ' C.C.', ' C.C', ' CC',
                 ' C.D.', ' C.D', ' CD',
                 ' A.D.', ' A.D', ' AD',
                 ' A.C.', ' A.C', ' AC']
                 
    # Remove specified patterns from the end of the name
    for i in to_delete:
        name = re.sub(i + "$", "", name)

     # Remove leading and trailing whitespace
    name = name.strip()
    return name


def club_depuration(df):
     """
    Cleans the 'club' column in the dataframe and adds a new 'club_clean' column with cleaned names.
    Prints the first 15 entries of the cleaned dataframe and the most common club names.
    
    Parameters:
    df (DataFrame): Input dataframe with a 'club' column.
    
    Returns:
    DataFrame: Modified dataframe with an additional 'club_clean' column.
    """
    # Apply clean_club function to each entry in the 'club' column
    df["club_clean"] = df["club"].apply(lambda x: clean_club(x))

    print("Despres de netegar els clubs les dades les 15 primeres entrades del dataset son: \n")
    print(df.head(15))

    # Count the occurrences of each unique club name in 'club_clean'
    df_gruped = df["club_clean"].value_counts(sort=True)

    print("Despres de agrupar els tems les dades del dataset son: \n")
    print(df_gruped.head(15))

    return df

