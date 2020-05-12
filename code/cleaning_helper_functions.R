############################################
######### cleaning familjeliv data ######### 
############################################

library(tidyverse)
library(lubridate)
library(magrittr)

quoted_user <- function(cleaned_tbl) {
  cleaned_tbl %<>% distinct(content, .keep_all = TRUE)
  cleaned_tbl$quoted_user <- character(length = nrow(cleaned_tbl))

  cleaned_no_quote <- cleaned_tbl %>% filter(quote_bin != 1)
  cleaned_quote <- cleaned_tbl %>% filter(quote_bin == 1)
  
  for (i in seq_along(cleaned_quote$quoted_user)) {
    cleaned_quote$quoted_user[i] <- str_split_fixed(cleaned_tbl$content[i], " skrev", 2)[, 1]
  }

  for (j in seq_along(cleaned_quote$quoted_user)) {
    cleaned_quote$quoted_user[j] <- if_else(str_detect(cleaned_quote$quoted_user[j], "^Anonym "),
                                            NA_character_,
                                            cleaned_quote$quoted_user[j])
  }

cleaned_tbl <- bind_rows(cleaned_no_quote, cleaned_quote) %>% 
  arrange(url, date, time)
}

clean_raw_data <- function(raw_tbl) {
  clean_tbl <- raw_tbl %>%
    select(-thread) %>% 
    filter(date < ymd("2020-04-18")) %>% 
    mutate(quote = case_when(quote_bin == 1 ~ TRUE,
                             quote_bin == 0 ~ FALSE),
         content_wo_quote = case_when(is.na(content_wo_quote) == TRUE ~ content,
                                   is.na(content_wo_quote) == FALSE ~ content_wo_quote),
         quote_bin = replace_na(quote_bin, 99)) %>% 
  arrange(url, date, time)
  
  clean_tbl <- quoted_user(clean_tbl)
  
  return(clean_tbl)
}

