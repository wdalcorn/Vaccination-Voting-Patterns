#-----Reading in-----#

Voter <- read.csv("/Users/wdalcorn/Desktop/countypres_2000-2024.csv", 
                  stringsAsFactors = FALSE)

Covid <- read.csv("/Users/wdalcorn/Desktop/Covid_data.csv", 
                  stringsAsFactors = FALSE)

Voter <- Voter %>%
  mutate(
    county_fips = ifelse(
      !is.na(county_fips) & county_fips != "",
      str_pad(county_fips, width = 5, side = "left", pad = "0"),
      NA_character_
    )
  )
#-----Cleaning-----#
Covid <- Covid %>%
  mutate(
    FIPS = ifelse(
      !is.na(FIPS) & FIPS != "",
      str_pad(FIPS, width = 5, side = "left", pad = "0"),
      NA_character_
    ),
    Date = as.Date(Date, format = "%m/%d/%Y")
  )

#2020 Election
election_2020 <- Voter %>%
  filter(year == 2020, 
         candidate %in% c("DONALD J TRUMP", "JOSEPH R BIDEN JR")) %>%
  group_by(county_fips, state, state_po, county_name) %>%
  summarise(
    trump_votes_2020 = sum(candidatevotes[candidate == "DONALD J TRUMP"], na.rm = TRUE),
    biden_votes_2020 = sum(candidatevotes[candidate == "JOSEPH R BIDEN JR"], na.rm = TRUE),
    total_votes_2020 = first(totalvotes),
    .groups = "drop"
  ) %>%
  mutate(
    trump_pct_2020 = (trump_votes_2020 / total_votes_2020) * 100,
    biden_pct_2020 = (biden_votes_2020 / total_votes_2020) * 100,
    winner_2020 = ifelse(trump_votes_2020 > biden_votes_2020, "REPUBLICAN", "DEMOCRAT"),
    margin_2020 = abs(trump_pct_2020 - biden_pct_2020)
  )

Covid_Nov2021 <- Covid %>%
  mutate(
    FIPS = ifelse(
      !is.na(FIPS) & FIPS != "",
      str_pad(FIPS, width = 5, side = "left", pad = "0"),
      NA_character_
    ),
    Date = as.Date(Date, format = "%m/%d/%Y")
  )


Covid_Nov2021 <- Covid %>%
  filter(Date == as.Date("2021-11-10")) %>%
  select(
    FIPS, 
    Recip_County, 
    Recip_State,
    Date,
    Series_Complete_Pop_Pct,
    Administered_Dose1_Pop_Pct,
    Series_Complete_18Plus,
    Series_Complete_18PlusPop_Pct,
    Completeness_pct,
    Metro_status,
    Census2019
  )

Covid_Nov2021 <- Covid_Nov2021 %>%
  mutate(
    FIPS = ifelse(
      !is.na(FIPS) & FIPS != "",
      str_pad(FIPS, width = 5, side = "left", pad = "0"),
      NA_character_
    )
  )

election_comparison <- election_2020 

election_comparison <- election_comparison %>%
  mutate(
    county_fips = ifelse(
      !is.na(county_fips) & county_fips != "",
      str_pad(as.character(county_fips), width = 5, side = "left", pad = "0"),
      NA_character_
    )
  )



merged_data <- election_comparison %>%
  inner_join(Covid_Nov2021, by = c("county_fips" = "FIPS"))

merged_clean <- merged_data %>%
  filter(
    !is.na(Series_Complete_Pop_Pct),
    Series_Complete_Pop_Pct > 0,
    Series_Complete_Pop_Pct <= 100,
    !is.na(trump_pct_2020),
    Completeness_pct >= 50
  )

#-----Visualizations -----#

county_map <- map_data("county")

map_data_prep <- merged_clean %>%
  mutate(
    region = tolower(state),
    subregion = tolower(county_name),
   
    subregion = str_remove(subregion, " county| parish| borough| census area| municipality| city and borough"),
   
    subregion = str_replace(subregion, "^saint ", "st "),
    subregion = str_replace(subregion, "^de kalb$", "dekalb"),
    subregion = str_replace(subregion, "^du page$", "dupage"),
    subregion = str_replace(subregion, "^la salle$", "lasalle"),
    subregion = str_replace(subregion, "^de soto$", "desoto"),
    region = ifelse(region == "district of columbia", "district of columbia", region),
    subregion = ifelse(region == "district of columbia", "washington", subregion)
  ) %>%
  select(region, subregion, 
         winner_2020,trump_pct_2020, 
         Series_Complete_Pop_Pct)

map_with_data <- county_map %>%
  left_join(map_data_prep, by = c("region", "subregion"))

#MAP 1
map_2020 <- ggplot(map_with_data, 
                   aes(x = long, y = lat, group = group, fill = winner_2020)) +
  geom_polygon(color = "white", size = 0.05) +
  scale_fill_manual(
    values = c("DEMOCRAT" = "#2E74C0", "REPUBLICAN" = "#CB454A"),
    na.value = "gray90",
    name = "Winner"
  ) +
  coord_fixed(1.3) +
  theme_void(base_size = 12) +
  labs(title = "2020 Presidential Election Results",
       subtitle = "County-level winners") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray40"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

print(map_2020)

#MAP 2
map_vaccination <- ggplot(map_with_data, 
                          aes(x = long, y = lat, group = group, 
                              fill = Series_Complete_Pop_Pct)) +
  geom_polygon(color = "white", size = 0.05) +
  scale_fill_gradient2(
    low = "#d73027",         
    mid = "#ffffbf",         
    high = "#1a9850",        
    midpoint = 45,           
    na.value = "gray90",
    name = "% Fully\nVaccinated",
    limits = c(0, 100),
    breaks = c(0, 25, 45, 65, 85, 100)
  ) +
  coord_fixed(1.3) +
  theme_void(base_size = 12) +
  labs(title = "COVID-19 Vaccination Rates by County",
       subtitle = "Population fully vaccinated as of November 10, 2021",
       caption = "Data: CDC") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray40"),
    plot.caption = element_text(hjust = 0.5, size = 9, color = "gray50"),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10)
  )

print(map_vaccination)

#Histogram
hist_vax <- ggplot(merged_clean, 
                   aes(x = Series_Complete_Pop_Pct, fill = winner_2020)) +
  
  
  geom_histogram(alpha = 0.6, bins = 40, position = "identity") +
  
 
  geom_vline(data = merged_clean %>% 
               group_by(winner_2020) %>% 
               summarise(mean_vax = mean(Series_Complete_Pop_Pct)),
             aes(xintercept = mean_vax, color = winner_2020),
             linetype = "dashed", linewidth = 1) +
  
  scale_fill_manual(
    values = c("DEMOCRAT" = "#2E74C0", "REPUBLICAN" = "#CB454A"),
    labels = c("Democratic Counties", "Republican Counties"),
    name = "2020 Winner"
  ) +
  scale_color_manual(
    values = c("DEMOCRAT" = "#2E74C0", "REPUBLICAN" = "#CB454A"),
    guide = "none"
  ) +
  
  
  labs(
    title = "Distribution of County Vaccination Rates",
    x = "Population Fully Vaccinated (%, November 2021)",
    y = "Number of Counties",
    caption = "Dashed lines show group means"
  ) +
  
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

print(hist_vax)
