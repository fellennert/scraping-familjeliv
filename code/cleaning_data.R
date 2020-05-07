############################################
######### cleaning familjeliv data ######### 
############################################

library(tidyverse)


ekojuri <- read_csv("code/data.nosync/ekojuri/ekojuri.csv")

test <- ekojuri %>% slice(1:1000)

clean <- test %>% 
  select(-thread) %>% 
  mutate(quote = case_when(quote_bin == 1 ~ TRUE,
                           quote_bin == 0 ~ FALSE),
         content_quote = case_when(is.na(content_wo_quote) == TRUE ~ content,
                                   is.na(content_wo_quote) == FALSE ~ content_wo_quote),
         quote_bin = replace_na(quote_bin, 99))
