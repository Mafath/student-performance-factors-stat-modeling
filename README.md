# Student Persistence Statistical Analysis (R)

An end-to-end statistical modeling project investigating the statement:

> **"Students who feel supported show higher persistence in their studies."**

This repository contains a full workflow in R, from data preparation and exploratory analysis to inferential testing and predictive modeling using the **Student Performance Factors** dataset.

---

## Project Snapshot

- **Domain:** Educational analytics / student outcomes
- **Language:** R
- **Dataset size:** 6,607 rows x 20 columns
- **Core outputs:** Statistical tests, fitted distributions, regression diagnostics, classification metrics, and report-ready visualizations
- **Main scripts:**
  - `analysis.R` (script-based pipeline)
  - `analysis.Rmd` (narrative report pipeline)
  - `analysis.html` (rendered report artifact)
  - `Student Performance Factors.csv` (input dataset)

---

## Problem Statement

Student persistence is modeled as sustained academic effort and progress.  
The project evaluates whether support-related factors (e.g., parental involvement, resource access, teacher quality, peer influence, tutoring) are associated with better persistence outcomes.

---

## Repository Structure

```text
.
├── analysis.R
├── analysis.Rmd
├── analysis.html
└── Student Performance Factors.csv
```

---

## Methodology Overview

The workflow is organized into analytical phases:

### 1) Data Loading and Preparation
- Loads student performance data from CSV.
- Detects standard missing values and blank-string missing entries.
- Performs complete-case filtering (drops rows with missing data).
- Validates categorical level integrity before encoding.
- Encodes:
  - **Ordinal variables** as ordered factors and numeric levels.
  - **Binary variables** as 0/1 indicators.

### 2) Feature Engineering
- Computes `Score_Improvement = Exam_Score - Previous_Scores`.
- Builds a composite **Persistence Index** from standardized components in the main analysis workflow.

### 3) Descriptive Analytics
- Summary statistics for numeric variables.
- Frequency distributions for categorical variables.
- Group-wise persistence summaries by support variables.
- Visualizations such as:
  - Histograms
  - Bar plots
  - Box plots
  - Correlation heatmaps
  - Scatter plots

### 4) Distribution Fitting
Fits and evaluates probability distributions for selected variables:
- **Normal**
- **Gamma**
- **Poisson**
- **Bernoulli/Binomial**

### 5) Inferential Analytics
Hypothesis testing with significance-based interpretation:
- One-way ANOVA (+ Tukey HSD where applicable)
- Welch two-sample t-test
- Two-proportion test
- Levene's test and F-test (variance comparisons)
- Pearson correlation test

### 6) Predictive Modeling
- **Multiple Linear Regression**
  - Full model and variable selection
  - VIF multicollinearity checks
  - Cook's distance influence checks
  - Diagnostic plots
- **Naive Bayes Classification**
  - Tertile-based persistence classes and confusion-matrix evaluation
- **Decision Tree Regression**
  - Interpretable non-linear benchmark against linear model

---

## Dataset

- **File:** `Student Performance Factors.csv`
- **Typical variables include:**
  - Academic behavior (`Hours_Studied`, `Attendance`, `Tutoring_Sessions`)
  - Support factors (`Parental_Involvement`, `Access_to_Resources`, `Teacher_Quality`, `Peer_Influence`)
  - Context/demographics (`Family_Income`, `School_Type`, `Gender`, etc.)
  - Outcome (`Exam_Score`)

Note: Given the nature of the dataset, replication prioritizes validating methodology over drawing broad population level conclusions.