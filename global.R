library(tidyverse)
library(shiny)
library(shinyjs)
library(leaflet)
library(DT)

## SETUP
facs <- read_csv("data/facilities.csv")[-1]
missiles <- read_csv("data/missiles.csv")[-1]
fac_caps <- read_csv("data/fac_caps_with_unknown.csv")[-1]
miss_caps <- read_csv("data/miss_captures.csv")[-1]

# get date range for the whole app
min_date = min(append(facs$start_date, missiles$start_date), na.rm = TRUE)
max_date = max(append(facs$start_date, missiles$start_date), na.rm = TRUE)

# replace NAs with a very high year so they are never counted as captured
facs$cap_date_low_res[is.na(facs$cap_date_low_res)] <- as.Date("3000-01-01")
facs$cap_date_high_res[is.na(facs$cap_date_high_res)] <- as.Date("3000-01-01")

# use the data.table package for faster indexing by date
setDT(facs)
setkey(facs, start_date, cap_date_low_res, cap_date_high_res) 
