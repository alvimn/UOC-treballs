# -*- coding: utf-8 -*-
"""
Created on Mon Jan  5 19:54:27 2026

@author: alber
"""

import pandas as pd
import matplotlib.pyplot as plt
from cycler import cycler

plotly_colors = [
    "#636EFA", "#EF553B", "#00CC96", "#AB63FA", "#FFA15A",
    "#19D3F3", "#FF6692", "#B6E880", "#FF97FF", "#FECB52",
]

plt.rcParams["axes.prop_cycle"] = cycler(color=plotly_colors)


df = pd.read_csv("Contractació_pública_a_Catalunya__publicacions_a_la_Plataforma_de_serveis_de_contractació_pública_20260105.csv")
df_len = df.shape
df_filter = df[df["import_adjudicacio_sense_iva"].notnull()&df["pressupost_licitacio_sense_iva"].notnull()]
df_no_duplicates = df_filter.drop_duplicates("codi_expedient")
df_no_duplicates["new_cpv"] = df_filter["codi_cpv"].astype(str).str[:2]
df_no_duplicates["any"] = (
    pd.to_datetime(df_no_duplicates["data_adjudicacio_contracte"], errors="coerce")
      .dt.year
      .astype("Int64")
)
df_no_duplicates = df_no_duplicates[(df_no_duplicates["any"]>=2015)&(df_no_duplicates["any"]<=2025)]
distibution_cpv = df_no_duplicates.groupby(["new_cpv","any"], as_index=False)["pressupost_licitacio_sense_iva"].sum()
top_10 = df_no_duplicates.groupby(["new_cpv"], as_index=False)["pressupost_licitacio_sense_iva"].sum()

legend = pd.read_csv("Sin título 1.csv")
legend["new_cpv"] = legend["new_cpv"].astype(str).str[:2]
top_10 = top_10.merge(legend, on="new_cpv", how="left").sort_values("pressupost_licitacio_sense_iva", ascending=False).head(10)
top_10_list = top_10["desc_cpv"].tolist()
distibution_cpv = distibution_cpv.merge(legend, on="new_cpv", how="left")
wide = (distibution_cpv.pivot_table(index=["desc_cpv"],
                       columns="any",
                       values="pressupost_licitacio_sense_iva",
                       aggfunc="sum")
          .reset_index())
wide.to_csv("Progresion_by_year_divided_by_cpv_spendigs.csv")

df_no_duplicates["import_adjudicacio_sense_iva"] = pd.to_numeric(df_no_duplicates["import_adjudicacio_sense_iva"], errors="coerce").astype(float)
df_no_duplicates["diferencia"] = df_no_duplicates["import_adjudicacio_sense_iva"] - df_no_duplicates["pressupost_licitacio_sense_iva"]
diff_in_price_by_CPV = df_no_duplicates.groupby("new_cpv")["import_adjudicacio_sense_iva"].agg(["mean", "std", "var", "median"]).reset_index()
diff_in_price_by_CPV = diff_in_price_by_CPV.merge(legend, on="new_cpv", how="left")
diff_in_price_by_CPV = diff_in_price_by_CPV[diff_in_price_by_CPV["desc_cpv"].isin(top_10_list)]
diff_in_price_by_CPV.to_csv("Table_mean_median_std_var.csv")
differencia = df_no_duplicates[["diferencia","new_cpv"]]
differencia = differencia.merge(legend, on="new_cpv", how="left")
differencia = differencia[differencia["desc_cpv"].isin(top_10_list)]
differencia.to_csv("diff_pres_adj.csv")
differencia["diferencia"] = pd.to_numeric(differencia["diferencia"], errors="coerce")
differencia = differencia.dropna(subset=["diferencia", "desc_cpv"])

# order categories by median
order = (differencia.groupby("desc_cpv")["diferencia"]
         .median()
         .sort_values()
         .index)

data = [differencia.loc[differencia["desc_cpv"] == c, "diferencia"].values for c in order]

fig, ax = plt.subplots(figsize=(14, max(6, 0.55 * len(order))))

bp = ax.boxplot(
    data,
    labels=order,
    vert=False,
    patch_artist=True,
    showfliers=True,
    whis=1.5,
    medianprops=dict(linewidth=2),
    boxprops=dict(linewidth=1),
    whiskerprops=dict(linewidth=1),
    capprops=dict(linewidth=1),
    flierprops=dict(marker="o", markersize=3, alpha=0.35, markeredgewidth=0.6),
)

# Apply colors per category
for i, (box, med) in enumerate(zip(bp["boxes"], bp["medians"])):
    c = plotly_colors[i % len(plotly_colors)]
    box.set_facecolor(c)
    box.set_alpha(0.25)          # softer fill
    box.set_edgecolor(c)
    med.set_color(c)             # colored median

# Make whiskers/caps a neutral gray (cleaner)
for w in bp["whiskers"]:
    w.set_color("#888888")
for cap in bp["caps"]:
    cap.set_color("#888888")

# Color y tick labels to match boxes
for i, tick in enumerate(ax.get_yticklabels()):
    tick.set_color(plotly_colors[i % len(plotly_colors)])

ax.axvline(0, linewidth=1, linestyle="--", color="#666666", alpha=0.7)
ax.grid(axis="x", alpha=0.25)
ax.set_title("Distribució de 'diferencia' per categoria (top 10 CPV)")
ax.set_xlabel("diferencia")
ax.set_ylabel("")

# Remove top/right spines for a cleaner look
ax.spines["top"].set_visible(False)
ax.spines["right"].set_visible(False)

plt.tight_layout()
plt.show()