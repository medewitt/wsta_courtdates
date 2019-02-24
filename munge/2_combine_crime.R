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
  separate(defendant_name, sep = ",", into = c("last", "first", "middle", "other")) %>% 
  mutate(court_date_d = lubridate::mdy(str_extract(court_date, "\\d+/\\d+/\\d+")))

# Filter out to make sure have latest court date

latest_court_date <- out %>% 
  group_by(case_num, charge, last, last, middle) %>% 
  filter(court_date_d == max(court_date_d))

out <- inner_join(out, latest_court_date)


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

voter_fl <- voter_roll_join %>% 
  rename(last = last_name,
         first = first_name,
         middle = middle_name)

# string matching ---------------------------------------------------------

library(fastLink)



matching <- fastLink(dfA = out, dfB = voter_fl, varnames = c("first", "last", "middle"),
                     stringdist.match = c("first", "last", "middle"),
                     partial.match = c("first", "last", "middle"))

matching2 <- fastLink(dfA = out, dfB = voter_fl, varnames = c("first", "last"),
                     stringdist.match = c("first", "last"),
                     partial.match = c("first", "last"))
summary(matching2)

agg_matching <- fastLink::aggregateEM(em.list = list(matching, matching2))

summary(matching)
summary(agg_matching)

matched_voter_rolls_1 <- getMatches(out, voter_fl, fl.out = matching, 
                                  threshold.match = .85)

matched_voter_rolls_2 <- getMatches(out, voter_fl, fl.out = matching2, 
                                  threshold.match = .85)

matched_voter_rolls <- full_join(matched_voter_rolls_1, matched_voter_rolls_2)

# analysis ----------------------------------------------------------------

# Quick analysis

matched_voter_rolls %>% 
  count(Ward, party_cd) %>% 
  group_by(Ward) %>% 
  mutate(perc = n/sum(n)) %>% 
  select(-n) %>% 
  spread(party_cd, perc)

# Ward and Race
matched_voter_rolls %>% 
  count(Ward, race_code, case_num) %>% 
  group_by(Ward, race_code) %>% 
  summarise(avg = mean(n),
            n = n()) %>%
  mutate_if(is.numeric, round, 2) %>% 
  unite(col = together, -Ward, -race_code, sep = " n=") %>% 
  spread(race_code, together)

matched_voter_rolls %>% 
  mutate(race_code = as.factor(race_code),
         race_code = fct_lump(race_code, 2)) %>% 
  count(Ward, race_code, case_num) %>% 
  group_by(Ward, race_code) %>% 
  ggplot(aes(race_code, n))+
  geom_boxplot()+
  geom_jitter()+
  facet_wrap(~Ward)+
  theme_minimal()

matched_voter_rolls %>% 
  mutate(race_code = as.factor(race_code),
         race_code = fct_lump(race_code, 2)) %>% 
  count(Ward, race_code, case_num) %>% 
  na.omit()->linear_data

fit <- lm(n ~ race_code, data = linear_data)
library(brms)

fit_2 <- brm(n ~  race_code +(1+race_code|Ward), data = linear_data)

summary(fit_2)

ranef(fit_2)

# Ward and Gender
matched_voter_rolls %>% 
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

matched_voter_rolls %>% 
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

matched_voter_rolls %>% 
  filter(!is.na(Ward)) %>% 
  inner_join(top_call_type) %>% 
  count(Ward, call_type, case) %>% 
  ggplot(aes(n, color = Ward, fill = Ward))+
  geom_density(alpha = 1/10)+
  facet_wrap(~call_type)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_minimal()

write_csv(matched_voter_rolls, here::here("outputs", "court_dates_to_voter_rolls.csv"))
