#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jan 16 07:24:05 2025

@author: albert
"""
import pandas as pd


def datapull():
     """
    Loads the dataset from a CSV file, prints basic details of the dataset,
    and returns the DataFrame.

    Returns:
    DataFrame: Loaded dataset as a pandas DataFrame.
    """
    # Load the dataset from the specified CSV file
    df = pd.read_csv("data/dataset.csv", sep=";")

    # Print the first 5 entries of the dataset
    print("Les 5 primeres entrades del dataset son: \n")
    print(df.head(5))

    # Print the total number of entries in the dataset
    print("\n El data set te " + str(len(df)) + " entrades \n")
    
    # Print the column names of the dataset
    print("Les columnes del data set son:\n")
    print(list(df.columns))

    return (df)
