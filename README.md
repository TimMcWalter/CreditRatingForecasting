# Credit Rating Forecasting

Machine learning project for predicting **Fitch Long-Term Issuer Default Ratings** using financial and macroeconomic data.

---

## Overview

This project builds a multi-class classification model to predict corporate credit ratings across 23 categories.

Main model:
- Random Forest

Benchmarks:
- Linear Regression
- Neural Network

The focus is on **predictive performance**, not causal inference.

---

## Project Structure

```text
.
├── 1600code.r              # Main Random Forest pipeline
├── Linear_regression.R     # Linear model benchmark
├── Neural_network.R        # Neural network model
├── eda.r                   # Exploratory data analysis
├── getmacrodata.r          # Macro data (World Bank API)
├── presentation.r          # Plot generation
│
├── dataWithBlanks.csv      # Financial data
├── landsectordata.csv      # Sector / additional features
│
├── *.png                   # Output plots
└── README.md
```

---

## Data

- ~1,600 firms
- 72 predictors
- Target: Fitch credit rating (23 classes)

Features include:
- Financial ratios (profitability, leverage, liquidity)
- Absolute accounting values
- Market variables (market cap, P/E)
- Macroeconomic indicators (GDP, inflation, unemployment)

---

## Workflow

### 1. Preprocessing
- Merge datasets
- Add macro data via `WDI`
- Replace missing values with 0
- Add missingness indicators

### 2. Feature Engineering
- Ratios (ROA, margins, debt ratios, etc.)
- Scaling not required for Random Forest

### 3. Modeling
- Random Forest with cross-validation
- Hyperparameter tuning (`mtry`, `ntree`)
- Benchmarks: linear regression, neural network

### 4. Evaluation
- Accuracy
- Cohen’s Kappa
- Notch tolerance (±1, ±2, etc.)

---

## Results

Approximate final results from the project:

- Accuracy: ~30–33%
- ±1 notch: ~65%
- ±2 notches: ~80%+

Exact prediction is difficult due to:
- 23 classes
- class imbalance
- overlapping features

---

## Key Observations

- Random Forest outperforms simpler models
- Predictions are often close even when not exact
- Firm size and macro variables are strong predictors
- Performance is best in mid-range ratings such as BBB

---

## Limitations

- Cross-sectional data only
- Simple missing data handling
- No qualitative inputs such as management or strategy
- Class imbalance across ratings

---

## How to Run

Install packages:

```r
install.packages(c("randomForest", "caret", "dplyr", "WDI"))
```

Run main model:

```r
source("1600code.r")
```

Run benchmarks:

```r
source("Linear_regression.R")
source("Neural_network.R")
```

---

## Notes

- Predictive modeling setup inspired by Shmueli (2010)
- Results are based on cross-validation
- Data sourced from Bloomberg and World Bank

---

## Author

Timothée Lefebvre (HSG BBA Finanace and Data Science)
