# -*- coding: utf-8 -*-
"""
Created on Sun Jan 11 22:50:57 2026

@author: alber
"""

import pandas as pd
import numpy as np


def parse_amount_cell(x):
    """
    Dataset field 'import_adjudicacio_sense_iva' is Text and may contain '||' separated numbers.
    Returns float (sum of parts). NaN if not parsable.
    """
    if pd.isna(x):
        return np.nan
    s = str(x).strip()
    if not s:
        return np.nan
    parts = [p.strip() for p in s.split("||")]
    vals = pd.to_numeric(parts, errors="coerce")
    if np.all(pd.isna(vals)):
        return np.nan
    return float(np.nansum(vals))

def split_list_cell(x):
    if pd.isna(x):
        return []
    s = str(x).strip()
    if not s:
        return []
    return [p.strip() for p in s.split("||")]

def make_offer_bucket(n):

    if pd.isna(n):
        return "NA"
    n = int(n)
    if n <= 0:
        return "0"
    if n == 1:
        return "1"
    if n == 2:
        return "2"
    if 3 <= n <= 4:
        return "3-4"
    if 5 <= n <= 9:
        return "5-9"
    return "10+"

df = pd.read_csv("Contractació_pública_a_Catalunya__publicacions_a_la_Plataforma_de_serveis_de_contractació_pública_20260105.csv")

df_filter = df[
    df["import_adjudicacio_sense_iva"].notnull()
    & df["pressupost_licitacio_sense_iva"].notnull()
].copy()

# Year
df_filter["any"] = (
    pd.to_datetime(df_filter["data_adjudicacio_contracte"], errors="coerce")
      .dt.year
      .astype("Int64")
)

# Keep years
df_filter = df_filter[(df_filter["any"] >= 2015) & (df_filter["any"] <= 2025)].copy()

# CPV group
df_filter["new_cpv"] = df_filter["codi_cpv"].astype(str).str[:2]

# Parse numeric fields
df_filter["pressupost_num"] = pd.to_numeric(df_filter["pressupost_licitacio_sense_iva"], errors="coerce")
df_filter["import_adj_num"] = df_filter["import_adjudicacio_sense_iva"].apply(parse_amount_cell)

# Difference (absolute and %)
df_filter["diff_abs"] = df_filter["import_adj_num"] - df_filter["pressupost_num"]
df_filter["diff_pct"] = df_filter["diff_abs"] / df_filter["pressupost_num"]

df_no_duplicates = df_filter.drop_duplicates("codi_expedient").copy()

# Legend (CPV descriptions)
legend = pd.read_csv("Sin título 1.csv")
legend["new_cpv"] = legend["new_cpv"].astype(str).str[:2]

# Top 10 CPV by total budget 
top_10 = (df_no_duplicates.groupby("new_cpv", as_index=False)["pressupost_num"].sum()
          .merge(legend, on="new_cpv", how="left")
          .sort_values("pressupost_num", ascending=False)
          .head(10))

top_10_list = top_10["desc_cpv"].tolist()

# Attach descriptions to filter dataset
df_filter = df_filter.merge(legend, on="new_cpv", how="left")

# Keep only top 10 CPV desc
df_top = df_filter[df_filter["desc_cpv"].isin(top_10_list)].copy()

distibution_cpv = df_no_duplicates.groupby(["new_cpv","any"], as_index=False)["pressupost_num"].sum()
distibution_cpv = distibution_cpv.merge(legend, on="new_cpv", how="left")
wide = (distibution_cpv.pivot_table(index=["desc_cpv"],
                       columns="any",
                       values="pressupost_num",
                       aggfunc="sum")
          .reset_index())
wide.to_csv("Progresion_by_year_divided_by_cpv_spendigs.csv")


# ------------------------
# (5) Competition: offers vs difference
# ------------------------
df_top["ofertes_rebudes_num"] = pd.to_numeric(df_top["ofertes_rebudes"], errors="coerce")
df_top["offer_bucket"] = df_top["ofertes_rebudes_num"].apply(make_offer_bucket)

# Raw points for Flourish scatter (one row per lot/record)
scatter_cols = [
    "any", "new_cpv", "desc_cpv",
    "ofertes_rebudes_num", "offer_bucket",
    "pressupost_num", "import_adj_num",
    "diff_abs", "diff_pct",
    "procediment", "tipus_contracte",
    "nom_ambit", "codi_nuts", "lloc_execucio",
    "denominacio_adjudicatari", "identificacio_adjudicatari",
]
scatter_df = df_top[scatter_cols].copy()

# Export 
scatter_df.to_csv("flourish_scatter_offers_vs_diff.csv", index=False, encoding="utf-8-sig")

#Aggregated stats by CPV + offer bucket 
agg_df = (df_top.dropna(subset=["diff_pct", "ofertes_rebudes_num"])
          .groupby(["desc_cpv", "offer_bucket"], as_index=False)
          .agg(
              n=("diff_pct", "size"),
              diff_pct_mean=("diff_pct", "mean"),
              diff_pct_median=("diff_pct", "median"),
              diff_pct_p25=("diff_pct", lambda s: s.quantile(0.25)),
              diff_pct_p75=("diff_pct", lambda s: s.quantile(0.75)),
              budget_sum=("pressupost_num", "sum"),
              awarded_sum=("import_adj_num", "sum"),
          ))

agg_df.to_csv("flourish_offers_bucket_stats.csv", index=False, encoding="utf-8-sig")

# ------------------------
# (6) Supplier concentration (Flourish bar chart + treemap-ready table)
# ------------------------
# Build an exploded supplier table so each supplier gets an amount
# We will:
# - split supplier ids/names by "||"
# - split amounts by "||" when possible
# - if amounts don't match suppliers, distribute total equally among suppliers

sup = df_top[[
    "any", "desc_cpv",
    "identificacio_adjudicatari", "denominacio_adjudicatari",
    "import_adjudicacio_sense_iva"
]].copy()

sup["sup_ids"] = sup["identificacio_adjudicatari"].apply(split_list_cell)
sup["sup_names"] = sup["denominacio_adjudicatari"].apply(split_list_cell)

sup["amt_parts"] = sup["import_adjudicacio_sense_iva"].apply(split_list_cell)
sup["amt_parts_num"] = sup["amt_parts"].apply(lambda parts: pd.to_numeric(parts, errors="coerce").tolist() if parts else [])

def allocate_amounts(row):
    ids_ = row["sup_ids"]
    names_ = row["sup_names"]
    amts_ = row["amt_parts_num"]

    k = max(len(ids_), len(names_))
    if k == 0:
        return []

    # normalize lengths
    if len(ids_) < k:   ids_ = ids_ + [None] * (k - len(ids_))
    if len(names_) < k: names_ = names_ + [None] * (k - len(names_))

    total = parse_amount_cell(row["import_adjudicacio_sense_iva"])
    if pd.isna(total) or total == 0:
        alloc = [np.nan] * k
    else:
        # If we have per-supplier amounts matching k, use them
        if len(amts_) == k and not all(pd.isna(amts_)):
            alloc = amts_
        # If only one amount but multiple suppliers, split equally
        elif len(amts_) == 1 and k > 1 and not pd.isna(amts_[0]):
            alloc = [float(amts_[0]) / k] * k
        # Otherwise split total equally
        else:
            alloc = [float(total) / k] * k

    out = []
    for i in range(k):
        out.append({
            "any": row["any"],
            "desc_cpv": row["desc_cpv"],
            "supplier_id": ids_[i],
            "supplier_name": names_[i],
            "amount_awarded": alloc[i],
        })
    return out

rows = []
for _, r in sup.iterrows():
    rows.extend(allocate_amounts(r))

sup_exploded = pd.DataFrame(rows)

# Clean
sup_exploded["amount_awarded"] = pd.to_numeric(sup_exploded["amount_awarded"], errors="coerce")
sup_exploded = sup_exploded.dropna(subset=["amount_awarded"])
sup_exploded = sup_exploded[(sup_exploded["amount_awarded"] > 0)].copy()

# ---- A) Overall supplier concentration table (top N + shares)
supplier_totals = (sup_exploded.groupby(["supplier_id", "supplier_name"], as_index=False)["amount_awarded"].sum()
                   .sort_values("amount_awarded", ascending=False))

total_awarded = supplier_totals["amount_awarded"].sum()
supplier_totals["share"] = supplier_totals["amount_awarded"] / total_awarded
supplier_totals["cum_share"] = supplier_totals["share"].cumsum()
supplier_totals["rank"] = np.arange(1, len(supplier_totals) + 1)

# Limit for Flourish (top 200 suppliers)
supplier_totals.head(200).to_csv("flourish_supplier_concentration_top200.csv", index=False, encoding="utf-8-sig")

# ---- B) Supplier x CPV table (treemap / stacked bars)
supplier_cpv = (sup_exploded.groupby(["supplier_name", "desc_cpv"], as_index=False)["amount_awarded"].sum()
                .sort_values("amount_awarded", ascending=False))

# Optional: keep only top suppliers to avoid massive treemap
top_suppliers = set(supplier_totals.head(100)["supplier_name"])
supplier_cpv_top = supplier_cpv[supplier_cpv["supplier_name"].isin(top_suppliers)].copy()

supplier_cpv_top.to_csv("flourish_supplier_cpv_treemap_top100sup.csv", index=False, encoding="utf-8-sig")
