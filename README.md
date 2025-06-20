# Karnataka Ozone Trend Forecasting – Time Series Analysis (2021–2023)

**Author:** Shreya Mishra, Sathwik Nag, Satya, Madhumitha  
**Role:** Data Analyst  
**Tools:** Python, Pandas, Statsmodels, Matplotlib, Seaborn, ARIMA, ETS, Ensemble Modeling

---

##  Project Overview

This project analyzes and forecasts **ozone pollution trends** in Karnataka, India, using real-world air quality data. It applies statistical modeling techniques to generate short-term forecasts and identify seasonal patterns, enabling data-driven recommendations for public health and policy action.

---

##  Objective

> To model, forecast, and visualize ozone level trends in Karnataka using time series techniques, and provide insights for environmental planning and pollution mitigation.

---

##  Dataset Details

- **Source:** Karnataka State Pollution Control Board (KSPCB)  
- **Timeframe:** August 2021 – August 2023  
- **Features:** Ozone, PM10, PM2.5, NO₂, CO, SO₂  
- **File:** `Cleaned_Karnataka_Air_Quality_Dataset.csv`

---

##  Key Workflows

### 1. Data Cleaning
- Handled missing values
- Converted columns to appropriate types
- Standardized time formats

### 2.  Exploratory Data Analysis (EDA)
- Trend visualizations of ozone and co-pollutants
- Seasonal patterns identified through plots

### 3.  Time Series Modeling
- **ARIMA**: Chosen based on AIC minimization (pdq(1,0,0))
- **ETS**: Compared manual and auto smoothing models
- **Ensemble Model**: Combined ARIMA + ETS + regression for robustness
- Log transformation used to stabilize variance

### 4.  Model Evaluation
- Adjusted R²: 0.85 (Regression)
- Final ARIMA selected for operational forecasting
- Clear seasonal peaks and stabilization trend by late 2023

---

##  Key Takeaways

- Ozone pollution shows predictable seasonal trends in Karnataka
- Forecasts show stabilization of levels post mid-2023
- Combined modeling approach enhances forecast reliability

---

##  Recommendations

- Strengthen monitoring around seasonal peaks
- Use forecasts to issue public health advisories
- Promote green infrastructure to reduce NOx precursors
- Deploy early warning systems in urban zones

---

##  Repository Contents

| File | Description |
|------|-------------|
| `Cleaned_Karnataka_Air_Quality_Dataset.csv` | Final cleaned dataset |
| `air_quality_forecast_report.pdf` | Project report & findings |
| `README.md` | This documentation file |

---

##  Tools & Libraries Used

- Python 3  
- Pandas, NumPy  
- Statsmodels (ARIMA, ETS)  
- Matplotlib, Seaborn  
- Jupyter Notebook


