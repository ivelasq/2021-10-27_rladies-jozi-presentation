# Add fake data to real OpenFlights data

# Libraries
library(tidyverse)
library(hms)
library(lubridate)

# Data

df_routes_def <-
  read_csv(here::here("data", "df_routes_def.csv"))

# Create fake data --------------------------------------------------------

set.seed(2021)

# Create a random sequence of dates and times
x <-
  seq(as.POSIXct('2021/10/27'), as.POSIXct('2021/10/28'), by = "1 mins") # create random sequence

# Create dataset for table

df_routes_jozi <-
  df_routes_def %>%
  filter(src == 'JNB') %>%
  mutate(
    depart_time = str_sub(as.character(as_hms(sample(x[hour(x) > "09:01" &
                                    hour(x) < "17:00"],
                                n(),
                                replace = TRUE))), end = -4),
    terminal = as.factor(sample(LETTERS[1:4], n(), replace = TRUE)),
    gate = as.factor(sample(1:20, n(), replace = TRUE)),
    airline = as.factor(sample(LETTERS[1:6], n(), replace = TRUE)),
    air_no = as.factor(sample(400:999, n(), replace = TRUE)),
    flight_no = paste0(airline, air_no),
    status = sample(
      c("On Time", "Delayed", "Canceled"),
      prob = c(.8, .1, .1),
      n(),
      replace = TRUE
    ),
    delayed_time = as_hms(
      sample(x[hour(x) > "16:00" & hour(x) < "21:00"],
             n(),
             replace = TRUE)
    ),
    time_before_depart = delayed_time - 9,
    delayed_time = case_when(status == "Delayed" ~ paste0("Now ", str_sub(as.character(
      delayed_time
    ), end = -4)),
    TRUE ~ "")
    ,
    time_before_depart = as.numeric(time_before_depart),
    time_before_depart = case_when(status == "Canceled" ~ 0,
                                   TRUE ~ time_before_depart),
  ) %>%
  select(-airline,-air_no) %>%
  filter(!is.na(city.y))

write_csv(df_routes_jozi, here::here("data", "df_routes_jozi.csv"))
