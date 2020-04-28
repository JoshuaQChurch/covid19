# Coronavirus 2019 - R package 

## Purpose
This package does the following: 
* Download/tidy the COVID-19 data from the Johns Hopkins University repo
* Download/tidy the Google Mobility Report data
* Download/tidy the US Department of Agriculture data focused on population, education, and income
* Launch an interactive COVID-19 dashboard

# Installation 

```r
install.packages("devtools")
devtools::install_github("JoshuaQChurch/covid19")
```

# Dashboard

Run the following code to start the dashboard:  
**NOTE:** This function is defaulted to fetch the latest data before launching the dashboard.
```r
covid19::dashboard()

# To disable auto-update in the dashboard, 
# set the parameter update to FALSE
covid19::dashboard(update = FALSE)
```

# Fetching latest data

Run the following code to get the most up-to-date data:  
**NOTE:** This is not required if `update == TRUE` in the `covid19::dashboard()` function.
```r
# Fetch the latest data, tidy, 
# and store in the default R library package path
covid19::download_all_data()

# To set a custom path, use the `path` argument.
path <- "path/to/dir"
covid19::download_all_data(path = path)
```
