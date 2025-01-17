# -*- coding: utf-8 -*-
"""
Created on Fri Jan 17 07:46:12 2025

@author: alber
"""


def analisis(df,team):
     """
    Analyzes the data for a specific team, prints information about the team's members,
    the member with the best time, and their position relative to the entire dataset.

    Parameters:
    df (DataFrame): Input dataframe with a 'club_clean' and 'time' column.
    team (str): The name of the team to analyze.

    Returns:
    None
    """
    # Filter the dataframe to only include rows where 'club_clean' matches the team
    df_team=df[df["club_clean"] == team]
 
    # Check if the team exists in the dataframe
    if len(df_team) == 0:
        print("\n L' equip " + team +" no existeix")
        return

    # Print the members of the team
    print("\n Els corredord de l'equip " + team + " son:")
    print(df_team)

     # Print the member with the best time in the team
    print("\n La persona amb el millor temps de l'equip es:")    
    print(df_team[df_team['time']==df_team['time'].min()])

    # Get the index of the member with the best time
    best_team_index = df_team[df_team['time']==df_team['time'].min()].index
    
    # Sort the dataframe by time and reset the index
    df_bytime = df.sort_values("time").reset_index()
    
    # Get the position of the member with the best time in the entire dataset
    position = df_bytime[df_bytime["index"] == best_team_index[0]].index
    
    # Print the position of the member relative to the entire dataset
    print("\n La possicio respecte el total d'aquesta persona es: \n")
    print(position[0])
    print("\n Aquesta possicio el posen el " + str(position[0] * 100 / len(df_bytime)) + "% més alt")
