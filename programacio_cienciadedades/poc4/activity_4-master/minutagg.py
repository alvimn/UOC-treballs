#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jan 16 19:00:41 2025

@author: albert
"""
import math
import matplotlib.pyplot as plt


def minutes_002040(time):
    list_times = time.split(":")
    list_times[2] = "00"
    if int(list_times[1]) < 20:
        list_times[1] = "00"
    elif int(list_times[1]) >= 40:
        list_times[1] = "40"
    else:
        list_times[1] = "20"
    return (":".join(list_times))


def gruptimes(df):
    df["time_grouped"] = df["time"].apply(lambda x: minutes_002040(x))
    print("Despres de agrupar els tems les dades les 15 primeres entrades del dataset son: \n")
    print(df.head(15))
    df_gruped = df["time_grouped"].value_counts(sort=False).sort_index()
    print("Despres de agrupar els tems les dades del dataset son: \n")
    print(df_gruped)
    df_gruped.plot(kind='bar')
    plt.show()
    plt.savefig("img/histograma.png")
    return
