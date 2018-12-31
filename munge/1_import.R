# Purpose: Get all the court date data from my mailbox and save it


# libraries ---------------------------------------------------------------

library(tidyverse)
library(gmailr)
library(here)
library(Rcpp)
sourceCpp(file = list.files(path = here("libs"), full.names = T))

# import ------------------------------------------------------------------
use_secret_file("~/crime_secret_2.json")

# Get the Messages that are sent by the court system

mssgs <- messages(
  search = "NCWACA"
)

# munge -------------------------------------------------------------------

# Check how many emails exist
emails <- length(mssgs[[1]][[1]])

for (i in 1:emails) {
  ids <- id(mssgs)
  Mn <- message(ids[i], user_id = "me")
  path <- "data/"
  save_attachments(Mn, attachment_id = NULL, path, user_id = "me")
}

# Un zip the files

zipped_files <- list.files(here("data"), pattern = ".zip", full.names = T)

# See the date ranges available
map_dfr(zipped_files, unzip, exdir = "data/out", list = TRUE)

# Unzip Out

map(zipped_files, unzip, exdir = "data/out")

# Read them

files <- list.files("data/out/FORSYTH/CRIMINAL", full.names = T, pattern = ".txt")
a <- ""
for(i in seq_along(files)){
  a <- concatenate(a, read_lines(file = files[[i]]))
}

str_extract_all(string = a, 
                pattern = "\\s(\\w{4}\\s\\d{6})\\s")->b
str_extract_all(string = a, 
                pattern = "\\s(\\w{6})\\s")->c
str_extract_all(string = a, 
                pattern = "\\((.)+PLEA")->d
str_extract_all(string = a, 
                pattern = "CLS:\\b[\\d]")->class
str_extract_all(string = a, 
                pattern = "ATTY:\\w+")->plaint_attorney
str_extract_all(string = a, 
                pattern = "\\d{6}\\s([A-z,]+)")->defendant_name
str_extract_all(string = a, 
                pattern = "\\s\\s(\\w+,\\w+|\\w+,\\w+,\\w+)")->plaintiff_name
str_extract_all(string = a, 
                pattern = "COURT DATE: (\\d{2})/(\\d{2})/(\\d{2})")->court_date

# Add a helper function for cleaning purposes
clean_nas <- function(x){
  if(identical(x, character(0))) NA_character_ else x
} 


# clean the regex ---------------------------------------------------------

case_num <-b%>%
  map(clean_nas) %>% 
  map(str_trim) %>% 
  enframe() %>% 
  unnest() %>% 
  rename(case_num = value)

charge <-d%>% 
  map(clean_nas) %>% 
  map(str_trim) %>% 
  enframe() %>% 
  unnest() %>% 
  rename(charge = value) %>% 
  mutate(charge = str_remove(charge, "PLEA"),
         charge = str_trim(charge, "both"))

class <- class %>% 
  map(clean_nas) %>% 
  map(str_trim) %>% 
  enframe() %>% 
  unnest() %>% 
  rename(class = value) %>% 
  mutate(class = str_remove(class, "CLS:"),
         class = str_trim(class, "both"))

plaint_attorney <- plaint_attorney %>% 
  map(clean_nas) %>% 
  map(str_trim) %>% 
  enframe() %>% 
  unnest() %>% 
  rename(plaint_attorney = value) %>% 
  mutate(plaint_attorney = str_remove(plaint_attorney, "CLS:"),
         plaint_attorney = str_trim(plaint_attorney, "both"))

defendant_name <- defendant_name %>% 
  map(clean_nas) %>% 
  map(str_trim) %>% 
  enframe() %>% 
  unnest() %>% 
  rename(defendant_name = value) %>% 
  mutate(defendant_name = str_remove(defendant_name, "\\d{6}\\s"))

plaintiff_name <- plaintiff_name %>% 
  map(clean_nas) %>% 
  map(str_trim) %>% 
  enframe() %>% 
  unnest() %>% 
  rename(plaintiff_name = value) %>% 
  mutate(plaintiff_name = str_remove(plaintiff_name, "\\d{6}\\s"))

court_date <- court_date %>% 
  map(clean_nas) %>% 
  map(str_trim) %>% 
  enframe() %>% 
  unnest() %>% 
  rename(court_date = value)

total_summary <- data_frame(
  case_num = case_num$case_num, 
  court_date = court_date$court_date,
  charge = charge$charge,
  class = class$class,
  plaint_attorney = plaint_attorney$plaint_attorney,
  defendant_name=defendant_name$defendant_name,
  plaintiff_name=plaintiff_name$plaintiff_name)

complete_data <- total_summary %>% 
  tidyr::fill(case_num, .direction = "down") %>% 
  tidyr::fill(court_date, .direction = "down") %>% 
  tidyr::fill(defendant_name, .direction = "down") %>% 
  tidyr::fill(plaintiff_name, .direction = "down") %>% 
  tidyr::fill(plaint_attorney, .direction = "down") %>% 
  tidyr::fill(charge, .direction = "up") %>% 
  filter(!is.na(charge), !is.na(class))
head(complete_data)
case_small <- complete_data %>% 
  filter(grepl(case_num , pattern =  "18CR")) %>% 
  mutate(case = str_remove_all(case_num, " ")) %>% 
  mutate(case = str_remove_all(case, "CR"),
         case = as.integer(case)) %>% 
  unique()



# bring in crime data -----------------------------------------------------

data <- read_rds("data/crime_data_for_app.RDS")

case_small %>% 
  left_join(data, by = "case") %>% 
  filter(!is.na(month))-> out
glimpse(case_small)

# Quick analysis

out %>% 
  count(case) %>% 
  ggplot(aes(n))+
  geom_histogram()

out %>% 
  count(call_type, case) %>% 
  group_by(call_type) %>% 
  summarise(avg_call = mean(n))->test
  ggplot(aes(call_type, n))+
  geom_boxplot()

# save --------------------------------------------------------------------


