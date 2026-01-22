# hotel-booking-cancellation-prediction
Predict hotel booking cancellations using ML (R) on real-world Portuguese hotel data.


# Hotel Booking Cancellation Prediction

## Overview
This project focuses on predicting the probability of hotel booking cancellations using real-world hotel demand data.
The goal is to support **Revenue Management** decisions by identifying bookings with a higher risk of cancellation.

The project applies **Machine Learning techniques in R** and compares multiple models to select the most effective one
based on discriminative performance rather than fixed-threshold classification.

---

## Dataset Description
The dataset consists of **real hotel booking data** from two hotels located in Portugal:
- **H1:** Resort hotel in the Algarve region
- **H2:** City hotel in Lisbon

The data covers bookings due to arrive between **July 1st, 2015 and August 31st, 2017**, including both:
- bookings that effectively arrived
- bookings that were canceled

Each observation represents a single hotel booking.

### Dataset characteristics
- Total observations: **117,424**
  - 40,060 (Resort Hotel)
  - 79,330 (City Hotel)
- Number of variables: **31**
- Data format: raw and preprocessed data
- Source: Hotels’ Property Management Systems (PMS) SQL databases
- All customer and hotel identifiers were removed to ensure privacy

The dataset is widely used for **research and education** in:
- Revenue Management
- Machine Learning
- Data Mining
- Hospitality Analytics

---

## Problem Definition
The objective is to build a predictive model that estimates the likelihood of a booking being canceled.

### Target Variable
- **is_canceled**
  - 1 = booking canceled
  - 0 = booking not canceled

The dataset is not strongly imbalanced, with a cancellation rate of approximately **37%**, which reflects a realistic business scenario.

---

## Feature Engineering
Several preprocessing and feature engineering steps were applied:
- Removal of variables causing potential data leakage
- Removal of customer and booking identifiers
- Recoding of the target variable as a binary factor
- Binning of quantitative variables
- Aggregation and recoding of categorical levels
- Country variable recoded using the `countrycode` library
- Outlier treatment on the Average Daily Rate (ADR)

All features were constructed using information available **before the arrival date**, preventing future information leakage.

---

## Data Splitting Strategy
A structured and realistic splitting strategy was adopted:
- **10%** of the data reserved as a completely independent **scoring set**
- Remaining **90%** split into training and validation sets
- Class proportions preserved across all subsets

This approach simulates real-world deployment conditions.

---

## Pre-processing
- No significant multicollinearity detected
- No zero-variance predictors
- Near-zero variance predictors identified and evaluated

---

## Model Selection
Feature importance was evaluated using **Boruta**.
A Decision Tree model was used to further reduce dimensionality.

The final models were trained using the **top 12 most important features**.

---

## Models Implemented
The following models were trained and compared:
- Logistic Regression (LASSO)
- Partial Least Squares (PLS)
- Naive Bayes
- Linear Discriminant Analysis (LDA)
- K-Nearest Neighbors (KNN)
- Decision Tree
- Random Forest
- Gradient Boosting
- Neural Network

---

## Evaluation Metric
Models were optimized using **ROC AUC**, as the main objective was to rank bookings by cancellation risk
rather than perform classification with a fixed threshold.

---

## Results
Random Forest and Gradient Boosting showed the best overall performance based on ROC curves.
To further discriminate between the two, **Lift and Cumulative Gain curves** were analyzed.

---

## Final Model
The **Random Forest** model was selected as the final model due to its superior and more stable performance.

### Threshold Optimization
- Optimal probability threshold: **0.38**
- Selected by maximizing the **F1-score**
- Allows improved detection of canceled bookings while maintaining good overall performance

---

## Conclusion
This project demonstrates how machine learning models can effectively support hotel revenue management strategies
by identifying high-risk bookings.  
The final Random Forest model provides a strong balance between predictive power and interpretability
in a realistic business scenario.

---

## Technologies Used
- R
- RStudio
- caret
- randomForest
- Boruta
- ggplot2
