############################################
######### cleaning familjeliv data ######### 
############################################

library(tidyverse)
library(lubridate)

quoted_user <- function(cleaned_tbl) {
  cleaned_tbl$quoted_user <- character(length = nrow(cleaned_tbl))
  for (i in seq_along(cleaned_tbl$quoted_user)) {
    cleaned_tbl$quoted_user[i] <- if_else(cleaned_tbl$quote_bin[i] == 1,
                                       cleaned_tbl$quoted_user[i] <- str_split_fixed(cleaned_tbl$content[i], " skrev", 2)[, 1], 
                                       cleaned_tbl$quoted_user[i] <- NA_character_)
    
    cleaned_tbl$quoted_user[i] <- if_else(str_detect(cleaned_tbl$quoted_user[i], "^Anonym "),
                                          NA_character_,
                                          cleaned_tbl$quoted_user[i])
    }
  return(cleaned_tbl)
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
