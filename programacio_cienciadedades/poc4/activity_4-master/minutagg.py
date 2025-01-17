#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jan 16 19:00:41 2025

@author: albert
"""
import math
import matplotlib.pyplot as plt


def minutes_002040(time):
    """
    This function takes a time string in the format HH:MM:SS and groups the minutes into 00, 20, or 40.

    Parameters:
    time (str): Time string in the format HH:MM:SS

    Returns:
    str: Modified time string with minutes grouped into 00, 20, or 40
    """
    list_times = time.split(":") # Splits between hours minuts seconds
    list_times[2] = "00" # Set seconds to 00

    # Group minutes into 00, 20, or 40
    if int(list_times[1]) < 20:
        list_times[1] = "00"
    elif int(list_times[1]) >= 40:
        list_times[1] = "40"
    else:
        list_times[1] = "20"
    return (":".join(list_times))


def gruptimes(df):
    """
    This function groups the times in the dataframe into 00, 20, or 40-minute intervals and plots a histogram.

    Parameters:
    df (DataFrame): Input dataframe with a 'time' column

    Returns:
    df : Modified df with extra column 'time_grouped' with time grouped in intervals
    """
    # Apply the minutes_002040 function to the 'time' column
    df["time_grouped"] = df["time"].apply(lambda x: minutes_002040(x))
    print("Despres de agrupar els temps les dades les 15 primeres entrades del dataset son: \n")
    print(df.head(15))

    # Count the occurrences of each grouped time
    df_gruped = df["time_grouped"].value_counts(sort=False).sort_index()
    print("Despres de agrupar els tems les dades del dataset son: \n")
    print(df_gruped)

    # Plot the grouped time counts as a bar chart
    df_gruped.plot(kind='bar')
    plt.show()  # Display the plot
    plt.savefig("img/histograma.png")   # Save the plot as an image file
    return df
