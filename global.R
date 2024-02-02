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