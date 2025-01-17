#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jan 16 20:20:01 2025

@author: albert
"""
import re


def clean_club(name):
    name = name.upper()
    to_delete = ["PEÑA CICLISTA", "PENYA CICLISTA",
                 "AGRUPACIÓN CICLISTA", "AGRUPACION CICLISTA",
                 "AGRUPACIÓ CICLISTA", "AGRUPACIO CICLISTA",
                 "CLUB CICLISTA", "CLUB"]
    for i in to_delete:
        name = name.replace(i, "")
    to_delete = ['C.C. ', 'C.C ', 'CC ', 'C.D. ',
                 'C.D ', 'CD ', 'A.C. ', 'A.C ',
                 'AC ', 'A.D. ', 'A.D ', 'AD ',
                 'A.E. ', 'A.E ', 'AE ', 'E.C. ',
                 'E.C ', 'EC ', 'S.C. ', 'S.C ',
                 'SC ', 'S.D. ', 'S.D ', 'SD ']
    for i in to_delete:
        name = re.sub("^" + i, "", name)
    to_delete = [' T.T.', ' T.T', ' TT',
                 ' T.E.', ' T.E', ' TE',
                 ' C.C.', ' C.C', ' CC',
                 ' C.D.', ' C.D', ' CD',
                 ' A.D.', ' A.D', ' AD',
                 ' A.C.', ' A.C', ' AC']
    for i in to_delete:
        name = re.sub(i + "$", "", name)
    name = name.strip()
    return name


def club_depuration(df):
    df["club_clean"] = df["club"].apply(lambda x: clean_club(x))
    print("Despres de netegar els clubs les dades les 15 primeres entrades del dataset son: \n")
    print(df.head(15))
    df_gruped = df["club_clean"].value_counts(sort=True)
    print("Despres de agrupar els tems les dades del dataset son: \n")
    print(df_gruped.head(15))

    return df

