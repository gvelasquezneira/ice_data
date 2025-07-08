library(tidyverse)
library(lubridate)
library(janitor)

ice <- read_csv('data/ICE.csv')

glimpse(ice)


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

florida_ice <- ice |>
  mutate(aor_city = str_extract(app_aor, "^[^\\s]+")) |>
  # First get Florida records or Miami AORs
  filter(app_state == "FLORIDA" | aor_city == "Miami") |>
  # Then exclude Puerto Rico records
  filter(is.na(app_state) | app_state != "PUERTO RICO" | app_state != "TEXAS" | app_state != "ALABAMA" | app_state !='VIRGIN ISLANDS' | app_state !='PENNSYLVANIA' | app_state !='NEW YORK' | app_state !='NEW JERSEY' | app_state !='MICHIGAN' | app_state !='MASSACHUSETTS' | app_state !='LOUISIANA' | app_state !='ILLINOIS' | app_state !='GEORGIA')

glimpse(florida_ice)

florida_ice<- florida_ice |>
  # Extract just the date part and convert to proper date format
  mutate(app_date = as.Date(app_date, format="%m/%d/%Y %H:%M")) |>
  filter(!is.na(app_date)) |>
  # Group by month for better visualization
  mutate(app_month = floor_date(app_date, "month"))

glimpse(florida_ice)

florida_ice_trump <- florida_ice |> 
  filter(app_date >= as.Date("2025-01-20"))

glimpse(florida_ice)

monthly_fl_arrests <- florida_ice|>
  count(app_month) |>
  arrange(app_month)


ggplot(monthly_fl_arrests, aes(x = app_month, y = n)) +
  geom_col(fill = "steelblue") + 
  labs(
    title = "Florida ICE Arrests Over Time",
    subtitle = "Monthly arrest counts",
    x = NULL,
    y = "Number of Arrests",
    caption = "Source: ICE Apprehension Data
    *Note: June 2025 data is incomplete."
  ) +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "3 month",
    expand = c(0, 0) 
  ) +
  scale_y_continuous()+
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank() # Remove vertical grid lines for clarity
  )
  
  write_csv(monthly_fl_arrests, 'monthly_fl_arrests.csv')
  
florida_ice_deport_trump <- florida_ice_trump |> 
  filter(final_order == 'YES') |> 
  filter(case_status != 'ACTIVE') |> 
  filter(
  str_detect(case_status, "Excluded/Removed") |
    str_detect(case_status, "Deported/Removed") |
    str_detect(case_status, "Voluntary Departure") |
    str_detect(case_status, "Died")
)

monthly_fl_deport <- florida_ice_deport_trump|>
  count(departed_date) |>
  arrange(departed_date)

write_csv(monthly_fl_deport, 'monthly_fl_deport.csv')

florida_ice_trump |>
  count(gender) |>
  pivot_wider(names_from = gender, values_from = n) |>
  mutate(male_female_ratio = Male / Female)

# Female  Male Unknown male_female_ratio
# <int> <int>   <int>             <dbl>
#   1   1197 10236     155              8.55


citizenship_breakdown <- florida_ice_trump |>
  filter(!is.na(citizenship)) |>
  group_by(citizenship) |>
  summarise(count = n()) |>
  mutate(
    total = sum(count),
    percentage = count / total * 100,
    percentage_formatted = paste0(round(percentage, 1), "%")
  ) |>
  arrange(desc(count)) |> 
  select(citizenship, count, percentage)

write_csv(citizenship_breakdown, 'citizenship_breakdown.csv')

# write_csv(florida_ice_deport, 'florida_ice_deport.csv')

# Group similar case statuses for cleaner visualization
deportation_categorized <- florida_ice_deport_trump %>%
  mutate(
    case_category = case_when(
      case_category == "[8C] Excludable / Inadmissible - Administrative Final Order Issued" ~ "Inadmissible - Admin Order",
      case_category == "[16] Reinstated Final Order" ~ "Reinstated Order",
      case_category == "[3] Deportable - Administratively Final Order" ~ "Deportable - Admin Order",
      case_category == "[8G] Expedited Removal - Credible Fear Referral" ~ "Expedited - Credible Fear",
      case_category == "[8F] Expedited Removal" ~ "Expedited Removal",
      case_category == "[10] Visa Waiver Deportation / Removal" ~ "Visa Waiver Removal",
      case_category == "[11] Administrative Deportation / Removal" ~ "Administrative Removal",
      case_category == "[8H] Expedited Removal - Status Claim Referral" ~ "Expedited - Status Claim",
      case_category == "[5D] Final Order of Deportation / Removal - Deferred Action Granted" ~ "Deferred Action",
      case_category == "[14] Crewmen, Stowaways, S-Visa Holders, 235(c) Cases" ~ "Special Cases",
      case_category == "[12] Judicial Deportation / Removal" ~ "Judicial Removal",
      TRUE ~ case_category
    )
  ) %>%
  count(case_category) %>%
  arrange(desc(n)) %>%
  rename(count = n) %>%
  mutate(
    percentage = count/sum(count)*100,
    percentage_label = paste0(round(percentage, 1), "%"),
    case_category = fct_reorder(case_category, count)
  )

glimpse(florida_ice_trump)


florida_ice_trump_crim <- florida_ice_trump |> 
  group_by(app_criminality) |> 
  select(app_date, app_criminality) |> 
  filter(str_detect(app_criminality, 'Criminal'))

glimpse(florida_ice_trump_crim)

8923/11617*100
