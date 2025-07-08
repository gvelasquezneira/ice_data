library(tidyverse)
library(lubridate)
library(janitor)

#Download Data---- 

ice <- read_csv('data/ICE.csv')

glimpse(ice)

#Renaming tings-----


ice <- ice |> 
  rename(
    app_date = `Apprehension Date`,
    app_state = `Apprehension State`,
    app_county = `Apprehension County`,
    app_aor = `Apprehension AOR`,
    program = `Final Program`,
    program_group = `Final Program Group`,
    app_method = `Apprehension Method`,
    app_criminality = `Apprehension Criminality`,
    case_status = `Case Status`,
    case_category = `Case Category`,
    departed_date = `Departed Date`,
    departure_country = `Departure Country`,
    final_order = `Final Order Yes No`,
    final_order_date = `Final Order Date`,
    birth_date = `Birth Date`,
    birth_year = `Birth Year`,
    citizenship = `Citizenship Country`,
    gender = Gender,
    app_site = `Apprehension Site Landmark`,
    alien_file_num = `Alien File Number`,
    eid_case_id = `EID Case ID`,
    eid_subject_id = `EID Subject ID`,
    unique_id = `Unique Identifier`
  )



glimpse(ice)

all_aors <- ice |>
  group_by(app_aor) |>
  summarise(count = n()) |>
  arrange(desc(count))

aor_patterns <- ice |>
  mutate(aor_city = str_extract(app_aor, "^[^\\s]+")) |>
  group_by(aor_city) |>
  summarise(count = n()) |>
  arrange(desc(count))

#Florida ICE-----

florida_ice <- ice |>
  mutate(aor_city = str_extract(app_aor, "^[^\\s]+")) |>
  # First get Florida records or Miami AORs
  filter(app_state == "FLORIDA" | aor_city == "Miami") |>
  filter(!app_state %in% c("PUERTO RICO", "TEXAS", "ALABAMA", "VIRGIN ISLANDS", 
                           "PENNSYLVANIA", "NEW YORK", "NEW JERSEY", "MICHIGAN", 
                           "MASSACHUSETTS", "LOUISIANA", "ILLINOIS", "GEORGIA") | 
           is.na(app_state))

ice <- ice |>
  mutate(app_date = as.Date(app_date, format="%m/%d/%Y")) |>
  filter(!is.na(app_date)) |>
  mutate(app_month = floor_date(app_date, "month"))

glimpse(florida_ice)
glimpse(ice)

#Trump Term Starts----

ice_trump <- ice_trump |> 
  filter(app_date >= as.Date("2025-01-20")) 

florida_ice_trump <- florida_ice |> 
  filter(app_date >= as.Date("2025-01-20"))

glimpse(florida_ice)
glimpse(ice_trump)

#Biden Term Last Year -----

ice_biden <- ice |> 
  filter(app_date >= as.Date("2024-01-20")) |> 
  filter(app_date < as.Date("2024-06-01"))

#calc data-----

# This represents what percentage is in Florida. 

11617/95632*100

# This represents the pct increase from Biden last year to now

42812/95632*100

#Top States-----

top_states_trump <- ice_trump |>
  filter(!is.na(app_state)) |>
  group_by(app_state) |>
  summarise(arrests = n()) |>
  arrange(desc(arrests)) |>
  mutate(percentage = round(arrests / sum(arrests) * 100, 2))



