# Coronavirus 2019 - R package 

## Purpose
This package does the following: 
* Download/tidy the COVID-19 data from the Johns Hopkins University repo
* Download/tidy the Google Mobility Report data
* Download/tidy the US Department of Agriculture data focused on population, education, and income
* Launch the COVID-19 dashboard

# Installation 

```r
install.packages("devtools")
devtools::install_github("JoshuaQChurch/covid19")
```

# Fetching latest data

Run the following code to get the most up-to-date data:
```r
covid19::download_all_data()
```

# Dashboard

Run the following to start the dashboard:
```r
covid19::dashboard()
```
