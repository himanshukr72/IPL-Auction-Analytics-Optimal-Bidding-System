# ============================================================
#  IPL Auction Decision System — Ball-by-Ball Edition
#  Works with: IPL.csv  (278,205 rows × 64 columns)
#  Covers 767 players across 18 IPL seasons (2007/08 – 2025)
# ============================================================

library(dplyr)
library(ggplot2)
library(readr)

# ── STEP 1: Load Raw Ball-by-Ball Data ───────────────────────
# UPDATE this path to wherever your CSV is saved
raw <- read_csv(
  "C:/Users/BIT/Desktop/Education 1/SEM 6/Non_Parametric Inference/Project/IPL.csv",
  col_types = cols(.default = col_character()),   # read all as char first
  show_col_types = FALSE
) %>%
  mutate(
    valid_ball    = as.integer(valid_ball),
    runs_batter   = as.numeric(runs_batter),
    runs_bowler   = as.numeric(runs_bowler),
    runs_total    = as.numeric(runs_total),
    bowler_wicket = as.numeric(bowler_wicket),
    striker_out   = as.logical(striker_out),
    balls_faced   = as.integer(balls_faced)
  )

cat("Raw data loaded:", nrow(raw), "rows,", n_distinct(raw$match_id), "matches\n")

# ── STEP 2: Aggregate Batting Stats ─────────────────────────
# Collapse ball-by-ball rows into one row per batter per innings,
# then summarise to career totals.

batting_innings <- raw %>%
  filter(valid_ball == 1) %>%
  group_by(batter, match_id) %>%
  summarise(
    runs  = sum(runs_batter,      na.rm = TRUE),
    balls = sum(valid_ball,       na.rm = TRUE),
    fours = sum(runs_batter == 4, na.rm = TRUE),
    sixes = sum(runs_batter == 6, na.rm = TRUE),
    out   = max(as.integer(striker_out), na.rm = TRUE),
    .groups = "drop"
  )

career_batting <- batting_innings %>%
  group_by(batter) %>%
  summarise(
    Mat   = n_distinct(match_id),
    Inns  = n_distinct(match_id),
    Runs  = sum(runs,  na.rm = TRUE),
    BF    = sum(balls, na.rm = TRUE),
    HS    = max(runs,  na.rm = TRUE),
    `4s`  = sum(fours, na.rm = TRUE),
    `6s`  = sum(sixes, na.rm = TRUE),
    outs  = sum(out,   na.rm = TRUE),
    `50s` = sum(runs >= 50 & runs < 100, na.rm = TRUE),
    `100s`= sum(runs >= 100, na.rm = TRUE),
    `0s`  = sum(runs == 0 & out == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    SR  = ifelse(BF > 0, round(Runs / BF * 100, 2), 0),
    Avg = ifelse(outs > 0, round(Runs / outs, 2),
                 round(Runs / pmax(Inns, 1), 2)),
    NO  = Inns - outs
  ) %>%
  rename(Player = batter)

cat("Batting stats computed for", nrow(career_batting), "players\n")

# ── STEP 3: Aggregate Bowling Stats ─────────────────────────
# bowler_wicket = 1 only for bowler-credited dismissals
# (run-outs are already excluded in the source column)

career_bowling <- raw %>%
  group_by(bowler, match_id) %>%
  summarise(
    balls = sum(valid_ball,    na.rm = TRUE),
    runs  = sum(runs_bowler,   na.rm = TRUE),
    wkts  = sum(bowler_wicket, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(bowler) %>%
  summarise(
    B_Inns  = n_distinct(match_id),
    B_Balls = sum(balls, na.rm = TRUE),
    B_Runs  = sum(runs,  na.rm = TRUE),
    B_Wkts  = sum(wkts,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    B_Econ = ifelse(B_Balls > 0, round(B_Runs / (B_Balls / 6), 2), 0),
    B_Avg  = ifelse(B_Wkts > 0,  round(B_Runs / B_Wkts, 2),  NA_real_),
    B_SR   = ifelse(B_Wkts > 0,  round(B_Balls / B_Wkts, 2), NA_real_)
  ) %>%
  rename(Player = bowler)

cat("Bowling stats computed for", nrow(career_bowling), "players\n")

# ── STEP 4: Merge Batting + Bowling ─────────────────────────
data <- full_join(career_batting, career_bowling, by = "Player")

# ── STEP 5: Fill Missing Values ─────────────────────────────
# Count stats: player never batted/bowled -> 0
data <- data %>%
  mutate(
    Runs   = ifelse(is.na(Runs),   0, Runs),
    BF     = ifelse(is.na(BF),     0, BF),
    HS     = ifelse(is.na(HS),     0, HS),
    Inns   = ifelse(is.na(Inns),   0, Inns),
    Mat    = ifelse(is.na(Mat),    0, Mat),
    NO     = ifelse(is.na(NO),     0, NO),
    outs   = ifelse(is.na(outs),   0, outs),
    `4s`   = ifelse(is.na(`4s`),   0, `4s`),
    `6s`   = ifelse(is.na(`6s`),   0, `6s`),
    `0s`   = ifelse(is.na(`0s`),   0, `0s`),
    `50s`  = ifelse(is.na(`50s`),  0, `50s`),
    `100s` = ifelse(is.na(`100s`), 0, `100s`),
    B_Inns  = ifelse(is.na(B_Inns),  0, B_Inns),
    B_Balls = ifelse(is.na(B_Balls), 0, B_Balls),
    B_Runs  = ifelse(is.na(B_Runs),  0, B_Runs),
    B_Wkts  = ifelse(is.na(B_Wkts),  0, B_Wkts),

    # Rate stats: missing -> column mean
    SR     = ifelse(is.na(SR),     mean(SR,     na.rm = TRUE), SR),
    Avg    = ifelse(is.na(Avg),    mean(Avg,    na.rm = TRUE), Avg),
    B_Econ = ifelse(is.na(B_Econ), mean(B_Econ, na.rm = TRUE), B_Econ),
    B_Avg  = ifelse(is.na(B_Avg),  mean(B_Avg,  na.rm = TRUE), B_Avg),
    B_SR   = ifelse(is.na(B_SR),   mean(B_SR,   na.rm = TRUE), B_SR)
  )

# ── STEP 6: Assign Playing Role ─────────────────────────────
data <- data %>%
  mutate(
    Role_Type = case_when(
      Runs > 200 & B_Wkts > 20  ~ "All rounder",
      B_Wkts > 20 & Runs <= 200 ~ "Bowler",
      TRUE                       ~ "Batsman"
    )
  )

cat("Final dataset:", nrow(data), "players\n")
cat("Role distribution:\n")
print(table(data$Role_Type))

# ── STEP 7: Value Function (Normalised + Regression-Based) ──
#
# WHY NORMALISE FIRST:
#   Runs range  : 0 – 8,652   (huge)
#   Wickets range: 0 – 221    (small)
#   Economy range: 0 – 36
#   Raw multiplication explodes values to hundreds of millions.
#
# FIX: Normalise each stat to 0-1 using dataset min-max,
#      then apply regression weights (rupees per normalised unit).
#
# Regression: lm(SOLD_PRICE ~ normalised_stats)
#   Fitted on 38 players common to this dataset and IPL auction prices
#   R² = 0.62
#
# Coefficients (Rs per normalised unit):
#   Runs:    +20.55 Cr     SR:     +12.70 Cr
#   Avg:     -6.81 Cr      B_Wkts: +12.08 Cr
#   B_Econ:  +12.85 Cr     Intercept: -5.24 Cr

value_function <- function(df) {
  df %>%
    mutate(
      # Normalise to 0-1 (min-max from full 767-player dataset)
      Runs_n  = pmin(pmax((Runs   - 0) / (8652 - 0), 0), 1),
      SR_n    = pmin(pmax((SR     - 0) / ( 400 - 0), 0), 1),
      Avg_n   = pmin(pmax((Avg    - 0) / (  87 - 0), 0), 1),
      BWkts_n = pmin(pmax((B_Wkts - 0) / ( 221 - 0), 0), 1),
      BEcon_n = pmin(pmax((B_Econ - 0) / (  36 - 0), 0), 1),

      # Regression weights -> result in rupees
      BattingValue = ( 205459195 * Runs_n  +
                       127031271 * SR_n    +
                       -68103848 * Avg_n),

      BowlingValue = ( 120791289 * BWkts_n +
                       128452193 * BEcon_n),

      TotalValue = -52398284 + BattingValue + BowlingValue,
      TotalValue = pmax(TotalValue, 200000)   # floor at Rs 20L
    )
}

# ── STEP 8: Uncertainty Estimation ──────────────────────────
# sd = 30% of TotalValue captures auction unpredictability
# (bidding wars, team needs, player form on the day)

estimate_distribution <- function(df, uncertainty = 0.30) {
  df %>%
    mutate(
      mean_value = TotalValue,
      sd_value   = TotalValue * uncertainty
    )
}

# ── STEP 9: Optimal Bid Function ────────────────────────────
#
# CORRECT FORMULATION (auction theory):
#   U(b) = P(win | b) x E[V - b | win]  -  risk_aversion x b
#
#   This creates a real trade-off:
#     bid too low  -> win_prob -> 0  -> U -> 0
#     bid too high -> avg_gain -> 0  -> U -> -risk_penalty
#   Optimum is between 10% and 95% of mean_value.

optimal_bid_fn <- function(mean_val, sd_val,
                           risk_aversion = 0.3,
                           n_sim = 2000) {

  if (is.na(mean_val) | is.na(sd_val) | sd_val <= 0 | mean_val <= 0) {
    return(NA_real_)
  }

  simulated <- rnorm(n_sim, mean_val, sd_val)
  simulated <- simulated[is.finite(simulated) & simulated > 0]
  if (length(simulated) == 0) return(NA_real_)

  bids <- seq(mean_val * 0.10, mean_val * 0.95, length.out = 100)

  utilities <- sapply(bids, function(b) {
    wins <- simulated[simulated >= b]
    if (length(wins) == 0) return(-Inf)
    win_prob <- length(wins) / length(simulated)
    avg_gain <- mean(wins - b)
    win_prob * avg_gain - risk_aversion * b
  })

  bids[which.max(utilities)]
}

# ── STEP 10: Full Pipeline ───────────────────────────────────
run_model <- function(df, uncertainty = 0.30, risk_aversion = 0.30) {
  df <- value_function(df)
  df <- estimate_distribution(df, uncertainty)
  df <- df %>%
    rowwise() %>%
    mutate(
      optimal_bid = optimal_bid_fn(mean_value, sd_value,
                                   risk_aversion = risk_aversion)
    ) %>%
    ungroup()
  return(df)
}

# ── STEP 11: Run the Model ───────────────────────────────────
cat("\nRunning model (this may take ~30 seconds for 767 players)...\n")
result <- run_model(data)
cat("Done.\n\n")

# Quick summary
cat("Optimal bid summary (Crores):\n")
print(summary(result$optimal_bid / 1e7))

# ── STEP 12: Validation Plot ─────────────────────────────────
# No SOLD_PRICE in this dataset, so plot top 30 players
# ranked by estimated value vs optimal bid

top30 <- result %>%
  filter(!is.na(optimal_bid)) %>%
  arrange(desc(TotalValue)) %>%
  head(30) %>%
  mutate(Player = factor(Player, levels = rev(Player)))

ggplot(top30) +
  geom_point(aes(x = TotalValue  / 1e7, y = Player),
             colour = "steelblue", size = 3) +
  geom_point(aes(x = optimal_bid / 1e7, y = Player),
             colour = "tomato", size = 3, shape = 17) +
  labs(
    title    = "Top 30 Players: Estimated Value vs Optimal Bid",
    subtitle = "Blue circle = Est. Value  |  Red triangle = Optimal Bid  |  Both in Crore (Rs)",
    x        = "Amount (Crores)",
    y        = NULL
  ) +
  theme_minimal()

# ── STEP 13: Player Query Function ──────────────────────────
#
#  Usage:
#    get_player_bid(result, "V Kohli")
#    get_player_bid(result, "Kohli")       # partial name works
#    get_player_bid(result, "bumrah")      # case-insensitive works
#

get_player_bid <- function(model_result, player_name) {

  player_row <- model_result %>%
    filter(grepl(player_name, Player, ignore.case = TRUE))

  if (nrow(player_row) == 0) {
    cat("Player not found.\n")
    cat("Tip: Try partial name like 'Kohli', 'Dhoni', 'Bumrah'.\n")
    return(invisible(NULL))
  }

  if (nrow(player_row) > 1) {
    cat(sprintf("Found %d matches - showing all:\n\n", nrow(player_row)))
  }

  for (i in seq_len(nrow(player_row))) {
    p <- player_row[i, ]

    cat("========================================\n")
    cat(" IPL Auction Bid Recommendation\n")
    cat("========================================\n")
    cat(sprintf("Player       : %s\n",   p$Player))
    cat(sprintf("Role         : %s\n",   p$Role_Type))
    cat(sprintf("Matches      : %d\n",   as.integer(p$Mat)))
    cat(sprintf("Runs         : %d  (SR: %.1f, Avg: %.1f)\n",
                as.integer(p$Runs), p$SR, p$Avg))
    cat(sprintf("Wickets      : %d  (Econ: %.2f)\n",
                as.integer(p$B_Wkts), p$B_Econ))
    cat("----------------------------------------\n")
    cat(sprintf("Batting Val  : Rs %.2f Cr\n", p$BattingValue / 1e7))
    cat(sprintf("Bowling Val  : Rs %.2f Cr\n", p$BowlingValue / 1e7))
    cat(sprintf("Est. Value   : Rs %.2f Cr\n", p$TotalValue   / 1e7))
    cat(sprintf("Uncertainty  : +/- Rs %.2f Cr (30%% of est. value)\n",
                p$sd_value / 1e7))
    cat(sprintf("OPTIMAL BID  : Rs %.2f Cr\n", p$optimal_bid  / 1e7))
    cat("========================================\n\n")
  }

  return(invisible(player_row))
}

# ── STEP 14: Try It ──────────────────────────────────────────
get_player_bid(result, "V Kohli")
get_player_bid(result, "MS Dhoni")
get_player_bid(result, "JJ Bumrah")
get_player_bid(result, "V Suryavanshi")

# ── STEP 15: Save Results ────────────────────────────────────
write.csv(
  result %>%
    select(Player, Role_Type, Mat, Runs, SR, Avg,
           B_Wkts, B_Econ, TotalValue, optimal_bid) %>%
    mutate(
      TotalValue_cr   = round(TotalValue   / 1e7, 2),
      optimal_bid_cr  = round(optimal_bid  / 1e7, 2)
    ),
  "final_player_bids_ballbyball.csv",
  row.names = FALSE
)
cat("Results saved to final_player_bids_ballbyball.csv\n")

# ── STEP 16: Ground-wise Stats ───────────────────────────────
# raw must still be in scope; uses columns: venue, batter, bowler,
# valid_ball, runs_batter, runs_bowler, bowler_wicket, striker_out, match_id

ground_batting <- raw %>%
  filter(valid_ball == 1) %>%
  group_by(batter, venue, match_id) %>%
  summarise(
    runs  = sum(runs_batter,      na.rm = TRUE),
    balls = n(),
    fours = sum(runs_batter == 4, na.rm = TRUE),
    sixes = sum(runs_batter == 6, na.rm = TRUE),
    out   = max(as.integer(striker_out), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(batter, venue) %>%
  summarise(
    G_Inns  = n(),
    G_Runs  = sum(runs),
    G_BF    = sum(balls),
    G_HS    = max(runs),
    G_outs  = sum(out),
    G_4s    = sum(fours),
    G_6s    = sum(sixes),
    G_50s   = sum(runs >= 50 & runs < 100),
    G_100s  = sum(runs >= 100),
    .groups = "drop"
  ) %>%
  mutate(
    G_Avg   = ifelse(G_outs  > 0, round(G_Runs / G_outs, 2), G_Runs),
    G_SR    = ifelse(G_BF    > 0, round(G_Runs / G_BF * 100, 2), 0)
  ) %>%
  rename(Player = batter)

ground_bowling <- raw %>%
  group_by(bowler, venue, match_id) %>%
  summarise(
    balls = sum(valid_ball,    na.rm = TRUE),
    runs  = sum(runs_bowler,   na.rm = TRUE),
    wkts  = sum(bowler_wicket, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(bowler, venue) %>%
  summarise(
    GB_Inns  = n(),
    GB_Balls = sum(balls),
    GB_Runs  = sum(runs),
    GB_Wkts  = sum(wkts),
    .groups = "drop"
  ) %>%
  mutate(
    GB_Econ = ifelse(GB_Balls > 0, round(GB_Runs / (GB_Balls / 6), 2), 0),
    GB_Avg  = ifelse(GB_Wkts  > 0, round(GB_Runs / GB_Wkts, 2), NA_real_),
    GB_SR   = ifelse(GB_Wkts  > 0, round(GB_Balls / GB_Wkts, 2), NA_real_)
  ) %>%
  rename(Player = bowler)

cat("Ground batting profiles:", nrow(ground_batting), "rows\n")
cat("Ground bowling profiles:", nrow(ground_bowling), "rows\n")

# ── Query: player profile at a specific ground ───────────────
get_ground_profile <- function(gb_bat, gb_bowl, player_name, venue_name = NULL) {

  bat <- gb_bat %>%
    filter(grepl(player_name, Player, ignore.case = TRUE))
  bwl <- gb_bowl %>%
    filter(grepl(player_name, Player, ignore.case = TRUE))

  if (!is.null(venue_name)) {
    bat <- bat %>% filter(grepl(venue_name, venue, ignore.case = TRUE))
    bwl <- bwl %>% filter(grepl(venue_name, venue, ignore.case = TRUE))
  }

  cat("========================================\n")
  cat(sprintf(" Ground Profile: %s\n", player_name))
  if (!is.null(venue_name)) cat(sprintf(" Venue filter : %s\n", venue_name))
  cat("========================================\n")

  if (nrow(bat) > 0) {
    cat("\n[BATTING BY GROUND]\n")
    bat %>%
      arrange(desc(G_Runs)) %>%
      mutate(across(where(is.numeric), ~round(.x, 2))) %>%
      print(n = 20)
  } else cat("No batting data for this ground filter.\n")

  if (nrow(bwl) > 0) {
    cat("\n[BOWLING BY GROUND]\n")
    bwl %>%
      arrange(desc(GB_Wkts)) %>%
      mutate(across(where(is.numeric), ~round(.x, 2))) %>%
      print(n = 20)
  } else cat("No bowling data for this ground filter.\n")

  invisible(list(batting = bat, bowling = bwl))
}

# ── STEP 17: Head-to-Head — Batter vs Bowler ─────────────────
h2h <- raw %>%
  filter(valid_ball == 1) %>%
  group_by(batter, bowler) %>%
  summarise(
    H_Balls      = n(),
    H_Runs       = sum(runs_batter,   na.rm = TRUE),
    H_Dismissals = sum(as.integer(striker_out), na.rm = TRUE),
    H_4s         = sum(runs_batter == 4, na.rm = TRUE),
    H_6s         = sum(runs_batter == 6, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    H_SR  = ifelse(H_Balls > 0, round(H_Runs / H_Balls * 100, 2), 0),
    H_Avg = ifelse(H_Dismissals > 0,
                   round(H_Runs / H_Dismissals, 2),
                   H_Runs)   # not out throughout
  )

cat("H2H matchups computed:", nrow(h2h), "combinations\n")

# ── Query: how a batter performs vs a specific bowler ────────
get_h2h <- function(h2h_df, batter_name, bowler_name = NULL) {

  rows <- h2h_df %>%
    filter(grepl(batter_name, batter, ignore.case = TRUE))

  if (!is.null(bowler_name))
    rows <- rows %>% filter(grepl(bowler_name, bowler, ignore.case = TRUE))

  cat("========================================\n")
  cat(sprintf(" Head-to-Head: %s\n", batter_name))
  if (!is.null(bowler_name)) cat(sprintf(" vs Bowler   : %s\n", bowler_name))
  cat("========================================\n")

  if (nrow(rows) == 0) {
    cat("No matchup data found.\n")
    return(invisible(NULL))
  }

  rows %>%
    arrange(desc(H_Balls)) %>%
    mutate(across(where(is.numeric), ~round(.x, 2))) %>%
    print(n = 20)

  invisible(rows)
}

# ── STEP 18: Opposition Team-wise Performance ────────────────
# Requires columns: batting_team, bowling_team in raw
# batting_team  = team of the batter
# bowling_team  = team of the bowler (= fielding team)

opp_batting <- raw %>%
  filter(valid_ball == 1) %>%
  group_by(batter, bowling_team, match_id) %>%
  summarise(
    runs  = sum(runs_batter,      na.rm = TRUE),
    balls = n(),
    out   = max(as.integer(striker_out), na.rm = TRUE),
    fours = sum(runs_batter == 4, na.rm = TRUE),
    sixes = sum(runs_batter == 6, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(batter, bowling_team) %>%
  summarise(
    OB_Inns = n(),
    OB_Runs = sum(runs),
    OB_BF   = sum(balls),
    OB_outs = sum(out),
    OB_4s   = sum(fours),
    OB_6s   = sum(sixes),
    OB_50s  = sum(runs >= 50 & runs < 100),
    OB_100s = sum(runs >= 100),
    .groups = "drop"
  ) %>%
  mutate(
    OB_Avg = ifelse(OB_outs > 0, round(OB_Runs / OB_outs, 2), OB_Runs),
    OB_SR  = ifelse(OB_BF   > 0, round(OB_Runs / OB_BF * 100, 2), 0)
  ) %>%
  rename(Player = batter, Opposition = bowling_team)

opp_bowling <- raw %>%
  group_by(bowler, batting_team, match_id) %>%
  summarise(
    balls = sum(valid_ball,    na.rm = TRUE),
    runs  = sum(runs_bowler,   na.rm = TRUE),
    wkts  = sum(bowler_wicket, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(bowler, batting_team) %>%
  summarise(
    OBW_Inns  = n(),
    OBW_Balls = sum(balls),
    OBW_Runs  = sum(runs),
    OBW_Wkts  = sum(wkts),
    .groups = "drop"
  ) %>%
  mutate(
    OBW_Econ = ifelse(OBW_Balls > 0, round(OBW_Runs / (OBW_Balls / 6), 2), 0),
    OBW_Avg  = ifelse(OBW_Wkts  > 0, round(OBW_Runs / OBW_Wkts, 2), NA_real_),
    OBW_SR   = ifelse(OBW_Wkts  > 0, round(OBW_Balls / OBW_Wkts, 2), NA_real_)
  ) %>%
  rename(Player = bowler, Opposition = batting_team)

cat("Opposition batting profiles:", nrow(opp_batting), "rows\n")
cat("Opposition bowling profiles:", nrow(opp_bowling), "rows\n")

# ── Query: player stats vs a specific team ───────────────────
get_vs_team <- function(ob_bat, ob_bowl, player_name, opp_name = NULL) {

  bat <- ob_bat %>% filter(grepl(player_name, Player, ignore.case = TRUE))
  bwl <- ob_bowl %>% filter(grepl(player_name, Player, ignore.case = TRUE))

  if (!is.null(opp_name)) {
    bat <- bat %>% filter(grepl(opp_name, Opposition, ignore.case = TRUE))
    bwl <- bwl %>% filter(grepl(opp_name, Opposition, ignore.case = TRUE))
  }

  cat("========================================\n")
  cat(sprintf(" vs-Team Profile: %s\n", player_name))
  if (!is.null(opp_name)) cat(sprintf(" Opposition  : %s\n", opp_name))
  cat("========================================\n")

  if (nrow(bat) > 0) {
    cat("\n[BATTING VS OPPOSITION]\n")
    bat %>% arrange(desc(OB_Runs)) %>%
      mutate(across(where(is.numeric), ~round(.x, 2))) %>%
      print(n = 20)
  }

  if (nrow(bwl) > 0) {
    cat("\n[BOWLING VS OPPOSITION]\n")
    bwl %>% arrange(desc(OBW_Wkts)) %>%
      mutate(across(where(is.numeric), ~round(.x, 2))) %>%
      print(n = 20)
  }

  invisible(list(batting = bat, bowling = bwl))
}

# ── STEP 19: Full Auction Report ─────────────────────────────
# Combines optimal bid + ground + H2H + team context in one call.
#
# Usage:
#   get_full_report(result, h2h, ground_batting, ground_bowling,
#                   opp_batting, opp_bowling,
#                   player_name  = "V Kohli",
#                   venue        = "Wankhede",       # optional
#                   opposition   = "Mumbai Indians", # optional
#                   key_bowlers  = c("JJ Bumrah", "HH Pandya")) # optional

get_full_report <- function(model_result, h2h_df,
                            gb_bat, gb_bowl,
                            ob_bat, ob_bowl,
                            player_name,
                            venue       = NULL,
                            opposition  = NULL,
                            key_bowlers = NULL) {

  cat("\n########################################\n")
  cat(sprintf("  FULL AUCTION REPORT: %s\n", player_name))
  cat("########################################\n\n")

  # ── 1. Optimal Bid ──────────────────────────────────────
  cat("── [1] OPTIMAL BID ─────────────────────\n")
  player_row <- model_result %>%
    filter(grepl(player_name, Player, ignore.case = TRUE))

  if (nrow(player_row) == 0) {
    cat("Player not found in model result.\n")
  } else {
    p <- player_row[1, ]
    cat(sprintf("Player      : %s  (%s)\n", p$Player, p$Role_Type))
    cat(sprintf("Est. Value  : Rs %.2f Cr\n", p$TotalValue  / 1e7))
    cat(sprintf("Optimal Bid : Rs %.2f Cr\n", p$optimal_bid / 1e7))
    cat(sprintf("Uncertainty : +/- Rs %.2f Cr\n", p$sd_value / 1e7))
  }

  # ── 2. Ground Profile ───────────────────────────────────
  if (!is.null(venue)) {
    cat("\n── [2] GROUND PROFILE ──────────────────\n")
    cat(sprintf("Venue: %s\n\n", venue))

    g_bat <- gb_bat %>%
      filter(grepl(player_name, Player, ignore.case = TRUE),
             grepl(venue, venue,   ignore.case = TRUE))
    g_bwl <- gb_bowl %>%
      filter(grepl(player_name, Player, ignore.case = TRUE),
             grepl(venue, venue,   ignore.case = TRUE))

    if (nrow(g_bat) > 0) {
      cat(sprintf("  Batting  — Inns: %d | Runs: %d | Avg: %.1f | SR: %.1f | HS: %d | 50s: %d | 100s: %d\n",
                  g_bat$G_Inns, g_bat$G_Runs, g_bat$G_Avg,
                  g_bat$G_SR,   g_bat$G_HS,   g_bat$G_50s, g_bat$G_100s))
    } else cat("  No batting data at this venue.\n")

    if (nrow(g_bwl) > 0) {
      cat(sprintf("  Bowling  — Inns: %d | Wkts: %d | Econ: %.2f | Avg: %.1f\n",
                  g_bwl$GB_Inns, g_bwl$GB_Wkts, g_bwl$GB_Econ,
                  ifelse(is.na(g_bwl$GB_Avg), 0, g_bwl$GB_Avg)))
    } else cat("  No bowling data at this venue.\n")
  }

  # ── 3. vs Opposition ────────────────────────────────────
  if (!is.null(opposition)) {
    cat("\n── [3] VS OPPOSITION ───────────────────\n")
    cat(sprintf("Team: %s\n\n", opposition))

    o_bat <- ob_bat %>%
      filter(grepl(player_name, Player,     ignore.case = TRUE),
             grepl(opposition,  Opposition, ignore.case = TRUE))
    o_bwl <- ob_bowl %>%
      filter(grepl(player_name, Player,     ignore.case = TRUE),
             grepl(opposition,  Opposition, ignore.case = TRUE))

    if (nrow(o_bat) > 0) {
      cat(sprintf("  Batting  — Inns: %d | Runs: %d | Avg: %.1f | SR: %.1f\n",
                  o_bat$OB_Inns, o_bat$OB_Runs, o_bat$OB_Avg, o_bat$OB_SR))
    } else cat("  No batting data vs this team.\n")

    if (nrow(o_bwl) > 0) {
      cat(sprintf("  Bowling  — Inns: %d | Wkts: %d | Econ: %.2f | Avg: %.1f\n",
                  o_bwl$OBW_Inns, o_bwl$OBW_Wkts, o_bwl$OBW_Econ,
                  ifelse(is.na(o_bwl$OBW_Avg), 0, o_bwl$OBW_Avg)))
    } else cat("  No bowling data vs this team.\n")
  }

  # ── 4. Head-to-Head vs Key Bowlers ──────────────────────
  if (!is.null(key_bowlers) && length(key_bowlers) > 0) {
    cat("\n── [4] HEAD-TO-HEAD vs KEY BOWLERS ─────\n")
    for (bl in key_bowlers) {
      rows <- h2h_df %>%
        filter(grepl(player_name, batter, ignore.case = TRUE),
               grepl(bl,          bowler, ignore.case = TRUE))
      if (nrow(rows) > 0) {
        r <- rows[1, ]
        cat(sprintf("  vs %-20s  Balls: %3d | Runs: %3d | SR: %5.1f | Dismissals: %d\n",
                    r$bowler, r$H_Balls, r$H_Runs, r$H_SR, r$H_Dismissals))
      } else {
        cat(sprintf("  vs %-20s  No data\n", bl))
      }
    }
  }

  cat("\n########################################\n\n")
  invisible(NULL)
}

# ── STEP 20: Try It ──────────────────────────────────────────
get_full_report(
  result, h2h,
  ground_batting, ground_bowling,
  opp_batting, opp_bowling,
  player_name  = "V Kohli",
  venue        = "Wankhede",
  opposition   = "Mumbai",
  key_bowlers  = c("JJ Bumrah", "HH Pandya", "Malinga")
)

get_full_report(
  result, h2h,
  ground_batting, ground_bowling,
  opp_batting, opp_bowling,
  player_name  = "MS Dhoni",
  venue        = "Chepauk",
  opposition   = "Kolkata"
)


# ── PHASE-WISE BATTING SPLIT ─────────────────────────────────
# Splits every innings into Powerplay (1-6), Middle (7-15),
# Death (16-20) using the `over` column in raw.
# Adjust column name if yours is called `over_number` etc.

phase_batting <- raw %>%
  filter(valid_ball == 1) %>%
  mutate(
    over  = as.integer(over),          # <-- adjust if needed
    phase = case_when(
      over <= 6              ~ "Powerplay",
      over >= 7 & over <= 15 ~ "Middle",
      over >= 16             ~ "Death",
      TRUE                   ~ NA_character_
    )
  ) %>%
  filter(!is.na(phase)) %>%
  group_by(batter, phase, match_id) %>%
  summarise(
    runs  = sum(runs_batter,      na.rm = TRUE),
    balls = n(),
    out   = max(as.integer(striker_out), na.rm = TRUE),
    fours = sum(runs_batter == 4, na.rm = TRUE),
    sixes = sum(runs_batter == 6, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(batter, phase) %>%
  summarise(
    P_Inns  = n(),
    P_Runs  = sum(runs),
    P_BF    = sum(balls),
    P_outs  = sum(out),
    P_4s    = sum(fours),
    P_6s    = sum(sixes),
    .groups = "drop"
  ) %>%
  mutate(
    P_SR  = ifelse(P_BF   > 0, round(P_Runs / P_BF * 100, 2), 0),
    P_Avg = ifelse(P_outs > 0, round(P_Runs / P_outs, 2), P_Runs)
  ) %>%
  rename(Player = batter)

# ── PHASE-WISE BOWLING SPLIT ─────────────────────────────────
phase_bowling <- raw %>%
  mutate(
    over  = as.integer(over),
    phase = case_when(
      over <= 6              ~ "Powerplay",
      over >= 7 & over <= 15 ~ "Middle",
      over >= 16             ~ "Death",
      TRUE                   ~ NA_character_
    )
  ) %>%
  filter(!is.na(phase)) %>%
  group_by(bowler, phase, match_id) %>%
  summarise(
    balls = sum(valid_ball,    na.rm = TRUE),
    runs  = sum(runs_bowler,   na.rm = TRUE),
    wkts  = sum(bowler_wicket, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(bowler, phase) %>%
  summarise(
    PB_Inns  = n(),
    PB_Balls = sum(balls),
    PB_Runs  = sum(runs),
    PB_Wkts  = sum(wkts),
    .groups = "drop"
  ) %>%
  mutate(
    PB_Econ = ifelse(PB_Balls > 0, round(PB_Runs / (PB_Balls / 6), 2), 0),
    PB_Avg  = ifelse(PB_Wkts  > 0, round(PB_Runs / PB_Wkts, 2),  NA_real_),
    PB_SR   = ifelse(PB_Wkts  > 0, round(PB_Balls / PB_Wkts, 2), NA_real_)
  ) %>%
  rename(Player = bowler)

cat("Phase batting rows:", nrow(phase_batting), "\n")
cat("Phase bowling rows:", nrow(phase_bowling), "\n")

# ── Query: phase profile for a player ───────────────────────
get_phase_profile <- function(player_name) {

  bat <- phase_batting %>%
    filter(grepl(player_name, Player, ignore.case = TRUE))
  bwl <- phase_bowling %>%
    filter(grepl(player_name, Player, ignore.case = TRUE))

  cat("=========================================\n")
  cat(sprintf(" Phase Profile: %s\n", player_name))
  cat("=========================================\n")

  if (nrow(bat) > 0) {
    cat("\n[BATTING BY PHASE]\n")
    bat %>%
      mutate(phase = factor(phase,
                            levels = c("Powerplay", "Middle", "Death"))) %>%
      arrange(phase) %>%
      select(phase, P_Inns, P_Runs, P_BF, P_SR, P_Avg, P_4s, P_6s) %>%
      print(n = 3)
  }

  if (nrow(bwl) > 0) {
    cat("\n[BOWLING BY PHASE]\n")
    bwl %>%
      mutate(phase = factor(phase,
                            levels = c("Powerplay", "Middle", "Death"))) %>%
      arrange(phase) %>%
      select(phase, PB_Inns, PB_Balls, PB_Wkts, PB_Econ, PB_Avg) %>%
      print(n = 3)
  }

  invisible(list(batting = bat, bowling = bwl))
}

# ── Phase-adjusted value bonus ───────────────────────────────
# Death-over specialists are undervalued by career SR alone.
# Add a bonus to TotalValue based on death-phase SR premium.
# A death SR > 160 gets up to +15% bonus on TotalValue.

phase_value_bonus <- phase_batting %>%
  filter(phase == "Death", P_BF >= 20) %>%   # min 20 death balls
  mutate(
    death_premium = pmin((P_SR - 130) / 100, 0.15),  # caps at +15%
    death_premium = pmax(death_premium, 0)
  ) %>%
  select(Player, death_SR = P_SR, death_premium)

result <- result %>%
  left_join(phase_value_bonus, by = "Player") %>%
  mutate(
    death_premium = ifelse(is.na(death_premium), 0, death_premium),
    TotalValue    = round(TotalValue * (1 + death_premium))
  )

cat("Death-over bonus applied to",
    sum(result$death_premium > 0), "players\n")


# ── ARIMA ON SEASON-LEVEL PERFORMANCE ────────────────────────
library(forecast)

# ── Season-level stats (batting) ─────────────────────────────
season_stats <- raw %>%
  filter(valid_ball == 1) %>%
  mutate(season = as.integer(season)) %>%   # adjust column name
  group_by(batter, season, match_id) %>%
  summarise(
    runs  = sum(runs_batter, na.rm = TRUE),
    balls = n(),
    out   = max(as.integer(striker_out), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(batter, season) %>%
  summarise(
    S_Runs  = sum(runs),
    S_BF    = sum(balls),
    S_outs  = sum(out),
    .groups = "drop"
  ) %>%
  mutate(
    S_SR  = ifelse(S_BF   > 0, round(S_Runs / S_BF * 100, 2), 0),
    S_Avg = ifelse(S_outs > 0, round(S_Runs / S_outs, 2), S_Runs)
  ) %>%
  rename(Player = batter)

# ── ARIMA forecast helper ─────────────────────────────────────
# Returns point forecast + 80% CI for h steps ahead.
# Needs >= 4 seasons of data; returns NA otherwise.

arima_forecast_stat <- function(player_df, stat_col, h = 1) {
  vals <- player_df %>%
    arrange(season) %>%
    pull({{ stat_col }})

  vals <- vals[!is.na(vals)]
  if (length(vals) < 4)
    return(tibble(point = NA_real_, lo80 = NA_real_,
                  hi80 = NA_real_, n_seasons = length(vals)))

  fit <- tryCatch(
    auto.arima(ts(vals, frequency = 1),
               stepwise = TRUE, approximation = TRUE),
    error = function(e) NULL
  )
  if (is.null(fit))
    return(tibble(point = NA_real_, lo80 = NA_real_,
                  hi80 = NA_real_, n_seasons = length(vals)))

  fc <- forecast(fit, h = h, level = 80)
  tibble(
    point     = as.numeric(fc$mean)[h],
    lo80      = as.numeric(fc$lower)[h],
    hi80      = as.numeric(fc$upper)[h],
    n_seasons = length(vals)
  )
}

# ── Apply to all players ──────────────────────────────────────
cat("Fitting ARIMA — ~2 min for 767 players...\n")

arima_results <- season_stats %>%
  group_by(Player) %>%
  group_modify(~ {
    sr_fc  <- arima_forecast_stat(.x, S_SR)
    avg_fc <- arima_forecast_stat(.x, S_Avg)
    tibble(
      fc_SR      = sr_fc$point,
      fc_SR_lo   = sr_fc$lo80,
      fc_SR_hi   = sr_fc$hi80,
      fc_Avg     = avg_fc$point,
      fc_Avg_lo  = avg_fc$lo80,
      fc_Avg_hi  = avg_fc$hi80,
      n_seasons  = sr_fc$n_seasons
    )
  }) %>%
  ungroup()

cat("ARIMA complete:",
    sum(!is.na(arima_results$fc_SR)), "players forecasted\n")
cat("Skipped (< 4 seasons):",
    sum( is.na(arima_results$fc_SR)), "players\n")

# ── Merge into result and recompute TotalValue ────────────────
result <- result %>%
  left_join(arima_results, by = "Player") %>%
  mutate(
    # 70% ARIMA + 30% career blend where forecast exists
    SR_final  = ifelse(!is.na(fc_SR),
                       0.70 * fc_SR  + 0.30 * SR,  SR),
    Avg_final = ifelse(!is.na(fc_Avg),
                       0.70 * fc_Avg + 0.30 * Avg, Avg),

    # Recompute normalised features with blended stats
    SR_n  = pmin(pmax((SR_final  - 0) / (400 - 0), 0), 1),
    Avg_n = pmin(pmax((Avg_final - 0) / ( 87 - 0), 0), 1),

    # Recalculate TotalValue (keep Runs_n, BWkts_n, BEcon_n as-is)
    TotalValue = pmax(
      -52398284 +
        205459195 * Runs_n  +
        127031271 * SR_n    +
        -68103848 * Avg_n   +
        120791289 * BWkts_n +
        128452193 * BEcon_n,
      200000
    )
  )

# ── Query: show a player's ARIMA forecast ────────────────────
get_arima_forecast <- function(player_name) {
  rows <- arima_results %>%
    filter(grepl(player_name, Player, ignore.case = TRUE))

  if (nrow(rows) == 0) {
    cat("Player not found.\n"); return(invisible(NULL))
  }

  cat("=========================================\n")
  cat(sprintf(" ARIMA Forecast: %s\n", rows$Player[1]))
  cat("=========================================\n")
  cat(sprintf("  Seasons of data : %d\n",  rows$n_seasons[1]))
  cat(sprintf("  Forecast SR     : %.1f  [80%% CI: %.1f – %.1f]\n",
              rows$fc_SR[1], rows$fc_SR_lo[1], rows$fc_SR_hi[1]))
  cat(sprintf("  Forecast Avg    : %.1f  [80%% CI: %.1f – %.1f]\n",
              rows$fc_Avg[1], rows$fc_Avg_lo[1], rows$fc_Avg_hi[1]))
  cat("=========================================\n")

  invisible(rows)
}

# ── Try it ────────────────────────────────────────────────────
get_phase_profile("V Kohli")
get_phase_profile("JJ Bumrah")

get_arima_forecast("V Kohli")
get_arima_forecast("MS Dhoni")
get_arima_forecast("JJ Bumrah")

