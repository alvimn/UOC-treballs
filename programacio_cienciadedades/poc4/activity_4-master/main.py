#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jan 16 07:23:00 2025

@author: albert
"""

# Import necessary modules
import setup
import anomize
import minutagg
import fix_club
import team_analisis

# Load the dataset using the datapull function from setup module
df = setup.datapull()

# Anonymize the 'biker' names in the dataset
df = anomize.name_surname(df)

# Group times in the dataset into intervals and plot a histogram
df = minutagg.gruptimes(df)

# Clean and depurate club names in the dataset
df = fix_club.club_depuration(df)

# Analyze the team data for the "UCSC" team
team_analisis.analisis(df, "UCSC")
