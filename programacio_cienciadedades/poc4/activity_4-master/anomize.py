#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jan 16 14:39:02 2025

@author: albert
"""
import faker


def generate_name(val):
    fake = faker.Faker()
    fake.seed_instance()
    return (fake.name())


def name_surname(df):
    df["biker"] = df["biker"].apply(lambda x: generate_name(x))
    print("Despres de anominitzar les dades les 5 primeres entrades del dataset son: \n")
    print(df.head(5))
    df = df[df["time"] != "00:00:00"]
    print("\n El data set te " + str(len(df)) + " entrades \n")
    print("Despres de filtrar les dades les 5 primeres entrades del dataset son: \n")
    print(df.head(5))
    print("\n Les dades del cicliste amb el dorsal 1000 son: ")
    print(df[df["dorsal"] == 1000])
    return (df)
