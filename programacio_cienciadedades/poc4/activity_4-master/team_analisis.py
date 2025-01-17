# -*- coding: utf-8 -*-
"""
Created on Fri Jan 17 07:46:12 2025

@author: alber
"""


def analisis(df,team):
    df_team=df[df["club_clean"] == team]
    if len(df_team) == 0:
        print("\n L' equip " + team +" no existeix")
        return
    print("\n Els corredord de l'equip " + team + " son:")
    print(df_team)
    print("\n La persona amb el millor temps de l'equip es:")
    print(df_team[df_team['time']==df_team['time'].min()])
    best_team_index = df_team[df_team['time']==df_team['time'].min()].index
    df_bytime = df.sort_values("time").reset_index()
    position = df_bytime[df_bytime["index"] == best_team_index[0]].index
    print("\n La possicio respecte el total d'aquesta persona es: \n")
    print(position[0])
    print("\n Aquesta possicio el posen el " + str(position[0]*100/len(df_bytime)) +"% més alt")
    
    
    
    
team = "UCSC"