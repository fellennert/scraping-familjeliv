##############################
#### Overview over scrape ####
##############################

library(tidyverse)
library(lubridate)

source("code/cleaning_helper_functions.R")

### ekonomik & juridik

ekojuri_raw <- read_csv("code/data.nosync/ekojuri_raw.csv")
ekojuri_clean <- read_csv("code/data.nosync/ekojuri_clean.csv")

ekojuri_raw %>% mutate(year = floor_date(date, "year")) %>% count(year)

ekojuri_20122019 <- ekojuri_clean %>% 
  filter(date < ymd("2020-01-01") & date > ymd("2012-09-30")) #%>% 
  distinct(content, .keep_all = TRUE)

### samhaelle

samhaelle_raw <- read_csv("code/data.nosync/samhaelle_raw.csv")
samhaelle_clean <- read_csv("code/data.nosync/samhaelle_clean.csv")
write_csv(samhaelle_clean, "code/data.nosync/samhalle_clean.csv")
