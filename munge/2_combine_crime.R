# Purpose: Combine Court Data with Police Report Data


# libraries ---------------------------------------------------------------

library(tidyverse)
library(here)

# bring in crime data -----------------------------------------------------

police_data <- read_rds(here("data", "crime_data_for_app.RDS"))

court_data <- read_csv(here("outputs", "cleaned_court_information.csv"))

# Put Police and Court Name
court_data %>% 
  left_join(police_data, by = "case") %>% 
  filter(!is.na(month))-> out


# Separate First Name and Last Name
out <- out %>% 
  separate(defendant_name, sep = ",", into = c("last", "first", "middle", "other"))

# Bring in Voter Roll Data
voter_roll <- data.table::fread(here("data", "ncvoter34.txt"), nrows = 1e6)

# Reduce Voter Roll to Key Values
voter_roll_join <- voter_roll %>% 
  select(first_name, last_name, middle_name, 
         voter_reg_num,birth_year,
         race_code:birth_age, 
         drivers_lic) %>% 
  unique()

# See what Join
me <- out %>% 
  left_join(voter_roll_join, by = c("last" = "last_name", 
                                    "first" = "first_name",
                                    "middle" = "middle_name"
                                    )) %>% 
  unique() %>% 
  filter(!is.na(birth_year))


# analysis ----------------------------------------------------------------

# Quick analysis

me %>% 
  count(Ward, party_cd) %>% 
  group_by(Ward) %>% 
  mutate(perc = n/sum(n)) %>% 
  select(-n) %>% 
  spread(party_cd, perc)

# Ward and Race
me %>% 
  count(Ward, race_code, case_num) %>% 
  group_by(Ward, race_code) %>% 
  summarise(avg = mean(n),
            n = n()) %>%
  mutate_if(is.numeric, round, 2) %>% 
  unite(col = together, -Ward, -race_code, sep = " n=") %>% 
  spread(race_code, together)

# Ward and Gender
me %>% 
  count(Ward, gender_code, case_num) %>% 
  group_by(Ward, gender_code) %>% 
  summarise(avg = mean(n),
            n = n()) %>%
  mutate_if(is.numeric, round, 2) %>% 
  unite(col = together, -Ward, -gender_code, sep = " n=") %>% 
  spread(gender_code, together)

out %>% 
  count(Ward, case) %>%
  ggplot(aes(Ward, n))+
  geom_violin()+
  scale_y_continuous(breaks = c(1,2,3,4,5,6,7,8))

out %>% 
  filter(!is.na(Ward)) %>% 
  count(Ward, call_type, case) %>% 
  group_by(Ward, call_type) %>% 
  summarise(avg_call = mean(n),
            q25 = quantile(n, probs = .25),
            q75 = quantile(n, probs = .75),)->test
  
out %>% 
    count(call_type) %>% 
    top_n(10) ->top_call_type

test %>% 
  inner_join(top_call_type) %>% 
  ggplot(aes(Ward, avg_call))+
  geom_pointrange(aes(ymin = q25, ymax = q75 ))+
  coord_flip()+
  facet_wrap(~call_type)+
  scale_y_continuous(breaks = seq(1:6))+
  theme_minimal()

out %>% 
  filter(!is.na(Ward)) %>% 
  inner_join(top_call_type) %>% 
  count(Ward, call_type, case) %>% 
  ggplot(aes(nn, color = Ward, fill = Ward))+
  geom_density(alpha = 1/10)+
  facet_wrap(~call_type)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_minimal()
