#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jan 16 07:24:05 2025

@author: albert
"""
import pandas as pd


def datapull():
    df = pd.read_csv("data/dataset.csv", sep=";")
    print("Les 5 primeres entrades del dataset son: \n")
    print(df.head(5))
    print("\n El data set te " + str(len(df)) + " entrades \n")
    print("Les columnes del data set son:\n")
    print(list(df.columns))
    return (df)
