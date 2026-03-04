import pandas as pd
import numpy as np
import os 
import json 


BLAST_FILE = "data/raw/blastSample.xlsx"
OUTPUT_DIR = "data/processed"
os.makedirs(OUTPUT_DIR, exist_ok=True)
os.makedirs(os.path.join(OUTPUT_DIR, "players"), exist_ok=True)


def load_blast_data(filepath):
    df = pd.read_excel(filepath)
    df = df[["Player", "Handedness", "Plane Score", "Connection Score", "Rotation Score", "Bat Speed (mph)", "Early Connection (deg)"]].copy()
    df = df.dropna(subset=["Player"])
    print(f"  Found {len(df)} swings across {df['Player'].nunique()} players")
    return df

df = load_blast_data(BLAST_FILE)
print(df.head())

def aggregate_players(df):
    players = df.groupby("Player").agg(
        SwingCount = ("Bat Speed (mph)", "count"), 
        BatSpeed = ("Bat Speed (mph)", "mean"), 
        EarlyConnection = ("Early Connection (deg)", "mean"),
        EarlyConnectionVar = ("Early Connection (deg)", "std"),
        ConnectionScore = ("Connection Score", "mean"),
        RotationScore = ("Rotation Score", "mean"),
        PlaneScore = ("Plane Score", "mean"),
    ).reset_index()
    
    players["EC Variance"] = (players["EarlyConnectionVar"] * 2).round(1)
    players["BatSpeed"] = players["BatSpeed"].round(1)
    players["EarlyConnectionAngle"]  = players["EarlyConnection"].round(1)
    players["ConnectionScore"] = players["ConnectionScore"].round(1)
    players["RotationScore"] = players["RotationScore"].round(1)
    players["PlaneScore"] = players["PlaneScore"].round(1)

    players = players.drop(columns=["EarlyConnectionVar"])
    return players

players = aggregate_players(df)
print(players)


def compute_benchmarks(players):
    avg_bat_speed = players["BatSpeed"].mean()
    sd_bat_speed = players["BatSpeed"].std()

    benchmarks = {
        "avg_bat_speed":  round(avg_bat_speed, 2),
        "high_bat_speed": round(avg_bat_speed + 0.75 * sd_bat_speed, 2),  # elite threshold
        "low_bat_speed":  round(avg_bat_speed - 0.75 * sd_bat_speed, 2),  # concern threshold
    }

   # print(f"\nBenchmarks:")
  #  print(f"  Avg bat speed:  {benchmarks['avg_bat_speed']} mph")
  #  print(f"  Elite threshold: {benchmarks['high_bat_speed']} mph")
  #  print(f"  Concern threshold: {benchmarks['low_bat_speed']} mph")

    return benchmarks

benchmarks = compute_benchmarks(players)


def assign_bucket(row, bm):
    ec  = row["EarlyConnectionAngle"]
    ecv = row["EC Variance"]
    bs  = row["BatSpeed"]

    # --- REQUIRES HITTING DATA (not yet available) ---
    # woba   = row["wOBA"]
    # k_pct  = row["K_pct"]
    # gb_pct = row["GB_pct"]
    # fb_pct = row["FB_pct"]

    # Elite — needs wOBA
    # if woba > bm["high_wOBA"]:
    #     return "challenge"

    # Red flags
    if ec >= 95 or ec <= 85 or ecv > 9:
        return "launch pos lv1"

    # These two need K% and GB% — uncomment when hitting data is ready
    # if bs < bm["low_bat_speed"] and k_pct < bm["low_K_pct"] and gb_pct > bm["high_GB_pct"]:
    #     return "bat speed lv1"
    # if gb_pct > bm["high_GB_pct"] or fb_pct > bm["high_FB_pct"]:
    #     return "swing decision lv1"
    # if bs < bm["low_bat_speed"] and k_pct > bm["high_K_pct"]:
    #     return "confidence"

    if bs < bm["low_bat_speed"]:
        return "bat speed lv1"

    # Yellow flags
    if ecv > 7:
        return "launch pos lv2"
    if bs < bm["avg_bat_speed"]:
        return "bat speed lv2"

    # if gb_pct > bm["LG_avg_GB_pct"] or fb_pct > bm["LG_avg_FB_pct"]:
    #     return "swing decision lv2"

    # Refining
    if ecv > 5:
        return "launch pos lv3"
    if bs < bm["high_bat_speed"]:
        return "bat speed lv3"

    # if gb_pct > bm["low_GB_pct"] or fb_pct > bm["low_FB_pct"]:
    #     return "swing decision lv3"

    return "undefined"

players["bucket"] = players.apply(assign_bucket, axis=1, bm=benchmarks)
print(players[["Player", "bucket"]])