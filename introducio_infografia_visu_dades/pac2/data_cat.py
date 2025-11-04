# -*- coding: utf-8 -*-
"""
Created on Wed Oct 29 20:51:51 2025

@author: alber
"""

import pandas as pd


df = pd.read_csv("estat_nama_10_gdp_en.csv")
df = df.fillna(0)
# Clean numeric column (remove commas, convert to float)
df["OBS_VALUE"] = pd.to_numeric(df["OBS_VALUE"]).astype(int)

# Convert TIME_PERIOD to datetime (we’ll set day/month to first)
df["period"] = pd.to_datetime(df["TIME_PERIOD"].astype(str) + "-01-01")

# Pivot: geo → columns, period → index
pivot = (
    df.pivot_table(
        index="period",
        columns="geo",
        values="OBS_VALUE",
        aggfunc="sum",
        fill_value=0
    )
    .reset_index()
    .sort_values("period")
)

pivot.to_csv("data_cat2.csv")