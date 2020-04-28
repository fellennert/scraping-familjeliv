################################################
######### scraping gamla.familjeliv.se ######### 
################################################

###############################################
################# thread urls #################
###############################################

# ekonomik + juridik
mainpage <- "http://gamla.familjeliv.se/Forum-"
eko_juri <- "19-"
eko_juri_links <- c("89/", "290/", "373/", "295/", "88/", "421/", "422/", "427/")
forum_links_ekojuri <- character(length = length(eko_juri_links))
for (i in seq_along(eko_juri_links)) {
  forum_links_ekojuri[i] <- paste0(mainpage, eko_juri, eko_juri_links[i])
}

# samhälle
samhaelle <- "26-"
samhaelle_links <- c("468/", "431/", "158/", "279/", "70/", "176/", "175/", "66/", "65/", "63/", "146/", "109/", "98/", "448/", "74/", "68/", "434/")
forum_links_samhaelle <- character(length = length(samhaelle_links))
for (i in seq_along(samhaelle_links)) {
  forum_links_samhaelle[i] <- paste0(mainpage, samhaelle, samhaelle_links[i])
}

###############################################
################# no of pages #################
###############################################

get_number_of_pages <- function(sub_link) {
  library(rvest)
  library(tidyverse)
  webpage <- read_html(sub_link)
  n_threads <- html_nodes(webpage, ".threads strong") %>% 
    html_text() %>% 
    as.numeric()
  n_pages <- floor(n_threads/20+1)
  return(n_pages)
}

build_links_for_subs <- function(sub_link, n_pages) {
  library(stringr)
  n <- 1:n_pages
  temp_link <- str_sub(sub_link, end=-2)
  links <- character(length = length(n))
  for (i in seq_along(n)) {
    links[i] <- paste0(temp_link, "-", n[i], "/")
  }
  return(links)
}

#############################################
################# get links #################
#############################################

get_thread_links <- function(sub_link) {
  library(rvest)
  page <- read_html(sub_link)
  links <- html_nodes(page, ".forumListing-thread a") %>%
    html_attr("href")
  return(links)
}

################################################
################# get no pages #################
################################################

get_thread_pages <- function(thread_link) {
  library(rvest)
  page <- read_html(paste0("http://gamla.familjeliv.se", thread_link))
  pages <- html_nodes(page, "#formupdate .selected a") %>% 
    html_text()
  
  return(as.numeric(pages[1]))
}

#############################################
############### scrape thread ############### 
#############################################

# (1) create list of singular thread-pages
build_links_for_threads <- function(thread_link, n_pages) {
  library(stringr)
  n <- 1:n_pages
  temp_link <- str_sub(thread_link, end=-6)
  links <- character(length = length(n))
  for (i in seq_along(n)) {
    links[i] <- paste0("http://gamla.familjeliv.se", temp_link, "-", n[i], ".html")
  }
  return(links)
}

# (2) read page
for (i in seq_along(thread_links)) {
  thread_page <- read_html(thread_links[i])
}

# (3) get content 
# (3.1) date
get_date <- function(thread_page, url) {
  library(lubridate)
  today <- as.character(today())
  yesterday <- as.character(today()-1)
  day_before_yesterday <- as.character(today()-2)
  months_chr <- c("jan", "feb", "mar", "apr", "maj", "jun", 
                  "jul", "aug", "sep", "okt", "nov", "dec", "xyz")
  months_num <- c(1:12, 0)
  months_tbl <- tibble(
    months_chr = months_chr, 
    months_num = months_num
  )
  month_pattern <- paste(months_tbl$months_chr, collapse = "|")
  
  date <- html_nodes(thread_page, ".date") %>%
    html_text() %>%
    str_remove_all("\n") %>%
    str_trim() %>%
    str_split_fixed("×", 2)
  
  date <- date[, 1]
  
  top_date <- html_nodes(thread_page, ".forum-top-date") %>% 
    html_text()
  
  if (str_detect(url, "-1.html")) {
    date <- c(top_date, date)
  }
  
  date_df <- enframe(date) %>%
    select(value) %>%
    mutate(V1 = paste0(value, "#0"))%>%
    mutate(time = str_extract(V1, "([0-2][0-9][:][0-5][0-9])"),
           post_number = map_chr(str_split(V1, "#"), 2),
           date = map_chr(str_split(V1, "([0-2][0-9][:][0-5][0-9])"), 1),
           date_numeric = case_when(str_detect(date, "Idag") ~ today,
                                    str_detect(date, "Igår") ~ yesterday,
                                    str_detect(date, "I förrgår") ~ day_before_yesterday),
           date_day = str_extract(date, "([0-3][0-9])|([0-9])"),
           date_year = if_else(str_detect(date, "([2][0][0-2][0-9])"),
                               str_extract(date, "([2][0][0-2][0-9])"),
                               "2020"),
           date_numeric = ymd(date_numeric),
           date_month_temp = if_else(is.na(date_numeric) == TRUE,
                                     str_extract(date, month_pattern),
                                     "0")) %>%
    left_join(months_tbl, by = c("date_month_temp" = "months_chr")) %>%
    mutate(date_month = if_else(date_month_temp == 0,
                                month(date_numeric),
                                months_num),
           date_day = if_else(is.na(date_day) == TRUE,
                              day(date_numeric),
                              as.integer(date_day)),
           date = paste(date_year, date_month, date_day, sep = "-")) %>%
    select(date, time) %>% 
    filter(!is.na(date) & !is.na(time))
  
  return(ymd(date_df[[1]]))
}

# (3.2) get time

get_time <- function(thread_page, url) {
  library(lubridate)
  today <- as.character(today())
  yesterday <- as.character(today()-1)
  day_before_yesterday <- as.character(today()-2)
  months_chr <- c("jan", "feb", "mar", "apr", "maj", "jun", "jul", "aug", "sep", "okt", "nov", "dec", "xyz")
  months_num <- c(1:12, 0)
  months_tbl <- tibble(
    months_chr = months_chr, 
    months_num = months_num
  )
  month_pattern <- paste(months_tbl$months_chr, collapse = "|")
  
  date <- html_nodes(thread_page, ".date") %>%
    html_text() %>%
    str_remove_all("\n") %>%
    str_trim() %>%
    str_split_fixed("×", 2)
  
  date <- date[, 1]
  
  top_date <- html_nodes(thread_page, ".forum-top-date") %>% 
    html_text()
  
  if (str_detect(url, "-1.html")) {
    date <- c(top_date, date)
  }
  
  date_df <- enframe(date) %>%
    select(value) %>%
    mutate(V1 = paste0(value, "#0"))%>%
    mutate(time = str_extract(V1, "([0-2][0-9][:][0-5][0-9])"),
           post_number = map_chr(str_split(V1, "#"), 2),
           date = map_chr(str_split(V1, "([0-2][0-9][:][0-5][0-9])"), 1),
           date_numeric = case_when(str_detect(date, "Idag") ~ today,
                                    str_detect(date, "Igår") ~ yesterday,
                                    str_detect(date, "I förrgår") ~ day_before_yesterday),
           date_day = str_extract(date, "([0-3][0-9])|([0-9])"),
           date_year = if_else(str_detect(date, "([2][0][0-2][0-9])"),
                               str_extract(date, "([2][0][0-2][0-9])"),
                               "2020"),
           date_numeric = ymd(date_numeric),
           date_month_temp = if_else(is.na(date_numeric) == TRUE,
                                     str_extract(date, month_pattern),
                                     "0")) %>%
    left_join(months_tbl, by = c("date_month_temp" = "months_chr")) %>%
    mutate(date_month = if_else(date_month_temp == 0,
                                month(date_numeric),
                                months_num),
           date_day = if_else(is.na(date_day) == TRUE,
                              day(date_numeric),
                              as.integer(date_day)),
           date = paste(date_year, date_month, date_day, sep = "-")) %>%
    select(date, time) %>% 
    filter(!is.na(date) & !is.na(time))

  return(date_df[[2]])
}

# (3.3) author's name

get_author <- function(thread_page, url) {
  author <- html_nodes(thread_page, ".compose_avatar_nick") %>%
    html_text() %>%
    str_remove_all("\n") %>%
    str_trim()
  author <- author[author != ""]
  
  if (str_detect(url, "-1.html$") == FALSE) {
    author <- author[-1]
  }
  return(author)
}

# (3.4) content

get_textual_content <- function(thread_page, url) {
  text <- html_nodes(thread_page, ".message") %>%
    html_text() %>%
    str_trim() %>%
    str_remove_all("\n") %>%
    str_remove_all("\t") %>%
    str_replace_all("[^[:alnum:]]", " ") %>%
    str_squish() 
  
  if (str_detect(url, "-1.html$") == FALSE) {
    text <- text[-1]
  }
  return(text)
}

# (3.5) quotes

get_quotes <- function (thread_page) {
  quotes <- html_nodes(thread_page, ".quote") %>% 
    html_text() %>%
    str_trim() %>%
    str_remove_all("\n") %>%
    str_remove_all("\t") %>%
    str_replace_all("[^[:alnum:]]", " ") %>%
    str_squish() 
  return(quotes)
}

# (4.) bind it together
    
output_tbl <- bind_rows(output_tbl, temp_tbl)
quotes <- quotes_list %>% unlist()

# (5.) remove quotes

remove_quotes <- function(quotes, output_tbl) {
  
  library(stringr)
  library(tidyverse)
  library(magrittr)
  
  for (j in seq_along(quotes)) {
    quotes[j] <- str_sub(quotes[j], start=-(2/3*str_length(quotes[j])))
  }
  output_tbl %<>%
    mutate(quote_bin = if_else(str_detect(content, "skrev.....................följande"),
                               1,
                               0),
           author = if_else(str_detect(author, "Anonym \\("),
                            NA_character_, 
                            author)) %>%
    distinct()
  output_w_quote <- output_tbl %>%
    filter(quote_bin == 1)
  output_wo_quote <- output_tbl %>%
    filter(quote_bin == 0)
  pattern <- paste(quotes, collapse = '|')
  output_w_quote$content_wo_quote <- character(length = nrow(output_w_quote))
  if (nrow(output_w_quote) != 0) {
    for (k in seq_along(output_w_quote)) {
      output_w_quote$content_wo_quote[k] <- str_split(output_w_quote$content[k], 
                                                      pattern = pattern, 
                                                      n = 2)[[1]][[2]]
      output_w_quote$content_wo_quote %<>% str_squish()
    }
  }
  
  output_wo_quote$content_wo_quote <- output_wo_quote$content
  output_tbl <- bind_rows(output_w_quote, output_wo_quote) %>%
    distinct(content_wo_quote, .keep_all = TRUE) %>%
    arrange(date, time)
  return(output_tbl)
}

### final scrape function ###

scrape_thread <- function(thread_link, n_pages) {
  
  pb$tick()$print()
  
  url_list <- build_links_for_threads(thread_link = thread_link, n_pages = n_pages)
  
  output_list <- list()
  quotes_list <- list()
  
  for (i in seq_along(url_list)) {
    thread_page <- read_html(url_list[[i]])
    output_list[[i]] <- tibble(
      date = get_date(thread_page = thread_page, url = url_list[[i]]),
      time = get_time(thread_page = thread_page, url = url_list[[i]]),
      author = get_author(thread_page = thread_page, url = url_list[[i]]),
      content = get_textual_content(thread_page = thread_page, url = url_list[[i]])
    )
    quotes_list[[i]] <- get_quotes(thread_page = thread_page)
  }
  
  output_tbl <- bind_rows(output_list)
  quotes <- quotes_list %>% unlist()
  
  if (length(quotes) == 0) {
    return(output_tbl)
  } else {
    return(remove_quotes(quotes = quotes, output_tbl = output_tbl))
  }
}

### get failed links

get_failed_links <- function(output_list, url_tbl) {
  output_list_transp <- output_list %>% transpose()
  success_tbl <- bind_rows(output_list_transp$result)
  error <- output_list_transp$error %>% map_lgl(is_null)
  failed_links <- url_tbl$thread_link[!error] %>%
    enframe() %>%
    left_join(url_tbl, by = c("value" = "thread_link")) %>% 
    select(thread_link = value, n_pages)
  return(failed_links)
}

### try_again

try_again <- function(output_list, url_tbl) {
  output_list_transp <- output_list %>% transpose()
  success_tbl <- bind_rows(output_list_transp$result)
  error <- output_list_transp$error %>% map_lgl(is_null)
  failed_links <- url_tbl$thread_link[!error] %>%
    enframe() %>%
    left_join(url_tbl, by = c("value" = "thread_link")) %>% 
    select(thread_link = value, n_pages)
  
  pb <- progress_estimated(length(failed_links$thread_link))
  scrape_failed <- pmap(failed_links, safely(scrape_thread))
  
  return(list(success_tbl, scrape_failed))
}
