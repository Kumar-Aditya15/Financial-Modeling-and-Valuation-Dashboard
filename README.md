# Financial-Modeling-and-Valuation-Dashboard
Interactive Financial Modeling and valuation dashboard built in R Shiny for Nifty 500 stocks. It performs DCF valuation with real-time growth and WACC inputs, along with price trends, moving averages, sensitivity heatmaps, and Monte Carlo simulation, enabling dynamic and data-driven investment analysis.
IB Valuation Dashboard (R Shiny)

 Overview

An interactive financial modeling dashboard built using R and Shiny to analyze Nifty 500 companies. It performs intrinsic valuation using a Discounted Cash Flow (DCF) model and enables real-time scenario analysis by adjusting key inputs like growth rate and WACC.

---

 Features

-  Stock price analysis (trends, moving averages, returns)
-  DCF-based intrinsic valuation
-  Sensitivity analysis (growth vs WACC heatmap)
-  Monte Carlo simulation for valuation risk
-  Financial statements integration

---

 Dashboard Preview

"Dashboard" (screenshots/dashboard.png)

---

 How It Works

- Select a company from the Nifty 500
- Adjust growth rate and WACC
- The dashboard dynamically updates intrinsic valuation
- Use charts and simulations to analyze valuation and risk

---

 Tech Stack

- R
- Shiny
- Plotly
- Quantmod
- TTR
- rvest
- DT

---

 How to Run

1. Clone this repository
2. Open in RStudio
3. Install required packages:

install.packages(c("shiny","shinydashboard","plotly","quantmod","TTR","readr","rvest","DT","zoo"))

4. Run the app:

shiny::runApp()

---

 Project Structure

IB-Valuation-Dashboard-R
│
├── app.R
├── README.md
└── screenshots/

---

 Use Cases

- Investment Banking analysis
- Equity Research
- Financial modeling practice
- Interactive finance learning

---

 Future Improvements

- Comparable valuation (PE, EV/EBITDA)
- Portfolio optimization
- Automated equity research report generation

---
