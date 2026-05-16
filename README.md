# IPL-Auction-Analytics-Optimal-Bidding-System
This project is a complete sports analytics framework that transforms raw IPL ball-by-ball data into:  Player valuation models Auction bidding recommendations Tactical cricket insights Predictive performance forecasts  It combines cricket analytics, economics, machine learning concepts, and auction theory into one integrated decision-making system.


🚀 Project Objectives

The goal of this system is to simulate how IPL franchises evaluate players during auctions using:

Historical performance
Match context
Venue performance
Opposition-specific strengths
Phase-wise effectiveness
Future projections
Auction risk analysis

Instead of relying only on raw runs or wickets, this project attempts to create a realistic player valuation engine.

🧠 Core Concepts Used

This project combines:

Statistical Aggregation
Predictive Analytics
Regression Modeling
Monte Carlo Simulation
ARIMA Forecasting
Auction Theory
Risk Optimization
Sports Analytics
📂 Dataset

The model uses an IPL ball-by-ball dataset (IPL.csv) containing:

Batter statistics
Bowler statistics
Match metadata
Venue information
Team information
Dismissal events
Over-by-over events

The dataset is loaded and cleaned first before feature engineering begins.

⚙️ Project Workflow
1️⃣ Data Loading & Cleaning

The raw IPL ball-by-ball dataset is imported using read_csv().

Key preprocessing steps:
Convert character columns into numeric types
Handle missing values
Convert boolean dismissal indicators
Standardize ball-level information
Important columns:
Column	Meaning
runs_batter	Runs scored by batter
runs_bowler	Runs conceded by bowler
valid_ball	Whether delivery counts
bowler_wicket	Bowler-attributed wickets
striker_out	Whether batter got dismissed
2️⃣ Batting Statistics Aggregation

Ball-by-ball data is converted into:

Innings-level batting stats

Then aggregated into:

Career-level batting stats
Metrics computed:
Matches
Innings
Runs
Balls faced
Strike rate
Average
Highest score
Fours / Sixes
50s / 100s
Ducks

The strike rate formula used:

SR=
Balls
Runs
	​

×100

Average calculation:

Avg=
Outs
Runs
	​


Implemented in the batting aggregation section.

3️⃣ Bowling Statistics Aggregation

Bowling performance is similarly aggregated.

Metrics computed:
Bowling innings
Balls bowled
Runs conceded
Wickets
Economy rate
Bowling average
Bowling strike rate

Economy formula:

Economy=
Overs
Runs Conceded
	​


Bowling average:

Bowling Average=
Wickets
Runs Conceded
	​


Implemented here.

4️⃣ Player Role Classification

Players are automatically classified into:

Batsman
Bowler
All-rounder
Classification Logic
Condition	Role
Runs > 200 & Wickets > 20	All-rounder
Wickets > 20 & Runs ≤ 200	Bowler
Otherwise	Batsman

This creates dynamic role identification directly from statistics.

5️⃣ Regression-Based Player Valuation Model

This is the heart of the project.

The system estimates a player's financial value using:

Batting runs
Strike rate
Average
Wickets
Economy rate
Why Normalization Was Needed

Different cricket statistics exist on very different scales.

Example:

Metric	Range
Runs	0 – 8652
Wickets	0 – 221
Economy	0 – 36

Without normalization, runs would dominate the valuation model.

So all variables are scaled between 0 and 1 using Min-Max Normalization:

x
norm
	​

=
x
max
	​

−x
min
	​

x−x
min
	​

	​


Implemented in the value function.

Regression-Based Financial Model

A regression model trained on historical auction prices assigns monetary weights to player attributes.

Estimated coefficients:
Feature	Contribution
Runs	+20.55 Cr
Strike Rate	+12.70 Cr
Wickets	+12.08 Cr
Economy	+12.85 Cr
Average	Negative adjustment

Final valuation:

TotalValue=Intercept+BattingValue+BowlingValue

This produces realistic estimated market values.

6️⃣ Auction Uncertainty Modeling

IPL auctions are unpredictable.

A player’s final price depends on:

Team demand
Bidding wars
Current form
Squad balance

The project models this using uncertainty:

sd_value = TotalValue * 0.30

This means every player valuation includes ±30% volatility.

7️⃣ Optimal Bid Strategy (Auction Theory)

One of the most advanced parts of the project.

Instead of simply estimating value, the system computes the:

✅ Optimal Bid

using auction utility theory.

Utility Function

The utility of placing a bid is modeled as:

U(b)=P(win∣b)×E[V−b∣win]−risk_aversion×b

Where:

Term	Meaning
(P(win	b))
E[V−b]	Expected profit
risk_aversion	Franchise risk sensitivity
Monte Carlo Simulation

The project generates thousands of simulated auction outcomes using:

rnorm()

Then tests 100 different bid values to maximize expected utility.

This produces a mathematically optimized auction recommendation.

8️⃣ Validation Visualization

The model visualizes:

Estimated player value
Recommended optimal bid

using ggplot2.

Plot Features
Blue dots → Estimated market value
Red triangles → Optimal bid

This helps compare aggressive vs conservative bidding strategies.

9️⃣ Player Query System

The project includes an interactive player query engine.

Example:

get_player_bid(result, "V Kohli")

Returns:

Role
Runs
Strike rate
Wickets
Estimated value
Uncertainty
Optimal bid

The function also supports:

Partial names
Case-insensitive search

Implemented here.

🔟 Ground-Wise Performance Analytics

The system computes player performance at specific venues.

Examples:

Wankhede Stadium
Chepauk
Eden Gardens

Metrics include:

Runs
Strike rate
Average
Wickets
Economy

This helps franchises choose players suited to specific home grounds.

1️⃣1️⃣ Head-to-Head Matchup Analysis

The project builds batter-vs-bowler matchup analytics.

Example queries:

get_h2h(h2h, "Kohli", "Bumrah")

Metrics:

Balls faced
Runs scored
Dismissals
Strike rate
Boundary counts

Useful for strategic team planning.

1️⃣2️⃣ Opposition Team Analysis

The project calculates performance against specific IPL teams.

Example:

Kohli vs Mumbai Indians
Dhoni vs Kolkata Knight Riders

This provides opponent-specific tactical insights.

1️⃣3️⃣ Full Auction Report Generator

One function combines everything into a complete scouting report.

Example:

get_full_report(...)

The report includes:

Optimal bid
Venue analysis
Opposition analysis
Head-to-head matchups
Risk-adjusted value

This simulates a professional IPL analytics dashboard.

1️⃣4️⃣ Phase-Wise Performance Analysis

The innings is divided into:

Phase	Overs
Powerplay	1–6
Middle Overs	7–15
Death Overs	16–20

The model evaluates:

Batting SR by phase
Bowling economy by phase
Death-over specialists

Implemented here.

Death-Over Premium Logic

Players with exceptional death-over strike rates receive valuation bonuses.

Example:

Death SR > 160
Up to +15% valuation increase

This helps identify elite T20 finishers.

1️⃣5️⃣ ARIMA Forecasting for Future Performance

The project also predicts future player performance using time-series forecasting.

Library used:

library(forecast)
Forecasted Metrics
Future Strike Rate
Future Average

Using:

auto.arima()

The forecast blends:

70% future projection
30% career statistics

This creates future-adjusted valuations instead of relying only on past performance.
