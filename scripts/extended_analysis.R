library(tidyverse)
library(ggplot2)
#Loading in
Voter <- read.csv("/Users/wdalcorn/Desktop/countypres_2000-2024.csv", 
                  stringsAsFactors = FALSE)
Covid <- read.csv("/Users/wdalcorn/Desktop/Covid_data.csv", 
                  stringsAsFactors = FALSE)
#----- Fix FIPS Codes & Dates-----#

Voter <- Voter %>%
  mutate(
    county_fips = ifelse(
      !is.na(county_fips) & county_fips != "",
      str_pad(county_fips, width = 5, side = "left", pad = "0"),
      NA_character_
    )
  )

Covid <- Covid %>%
  mutate(
    FIPS = ifelse(
      !is.na(FIPS) & FIPS != "",
      str_pad(FIPS, width = 5, side = "left", pad = "0"),
      NA_character_
    ),
    Date = as.Date(Date, format = "%m/%d/%Y")
  )

#-----Election Data-----#

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

#2024 election
election_2024 <- Voter %>%
  filter(year == 2024, 
         candidate %in% c("DONALD J TRUMP", "KAMALA D HARRIS")) %>%
  group_by(county_fips) %>%
  summarise(
    trump_votes_2024 = sum(candidatevotes[candidate == "DONALD J TRUMP"], na.rm = TRUE),
    harris_votes_2024 = sum(candidatevotes[candidate == "KAMALA D HARRIS"], na.rm = TRUE),
    total_votes_2024 = first(totalvotes),
    .groups = "drop"
  ) %>%
  mutate(
    trump_pct_2024 = (trump_votes_2024 / total_votes_2024) * 100,
    harris_pct_2024 = (harris_votes_2024 / total_votes_2024) * 100,
    winner_2024 = ifelse(trump_votes_2024 > harris_votes_2024, "REPUBLICAN", "DEMOCRAT"),
    margin_2024 = abs(trump_pct_2024 - harris_pct_2024)
  )

#merge them
election_comparison <- election_2020 %>%
  left_join(election_2024, by = "county_fips") %>%
  mutate(
    county_flipped = winner_2020 != winner_2024,
    trump_change = trump_pct_2024 - trump_pct_2020,
    turnout_change = total_votes_2024 - total_votes_2020,
    turnout_change_pct = ((total_votes_2024 - total_votes_2020) / total_votes_2020) * 100
  )

#-----Covid Data Cleaning (NOV 2021)-----#
Covid_Nov2021 <- Covid %>%
  mutate(
    FIPS = ifelse(
      !is.na(FIPS) & FIPS != "",
      str_pad(FIPS, width = 5, side = "left", pad = "0"),
      NA_character_
    ),
    Date = as.Date(Date, format = "%m/%d/%Y")
  )


#Pick a date before vaccine mandate, still prime covid
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

#Fix Fips again
Covid_Nov2021 <- Covid_Nov2021 %>%
  mutate(
    FIPS = ifelse(
      !is.na(FIPS) & FIPS != "",
      str_pad(FIPS, width = 5, side = "left", pad = "0"),
      NA_character_
    )
  )

election_comparison <- election_comparison %>%
  mutate(
    county_fips = ifelse(
      !is.na(county_fips) & county_fips != "",
      str_pad(as.character(county_fips), width = 5, side = "left", pad = "0"),
      NA_character_
    )
  )



#Merge the two
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

#Validation Checks

cat("=== DATA MERGE VALIDATION ===\n")
cat("Rows in 2020 election data:", nrow(election_2020), "\n")
cat("Rows in 2024 election data:", nrow(election_2024), "\n")
cat("Rows in Nov 2021 COVID data:", nrow(Covid_Nov2021), "\n")
cat("Rows after merge:", nrow(merged_data), "\n")
cat("Rows after cleaning:", nrow(merged_clean), "\n")
cat("Counties lost in cleaning:", nrow(merged_data) - nrow(merged_clean), "\n\n")

# Check Texas specifically
cat("=== TEXAS CHECK ===\n")
cat("Texas in merged_clean:", nrow(merged_clean %>% filter(state == "TEXAS")), "\n\n")

cat("=== MISSING DATA CHECK ===\n")
cat("Counties with NA in Series_Complete_Pop_Pct:", 
    sum(is.na(merged_data$Series_Complete_Pop_Pct)), "\n")
cat("Counties with vaccination rate = 0:", 
    sum(merged_data$Series_Complete_Pop_Pct == 0, na.rm = TRUE), "\n")
cat("Counties with low completeness (<50%):", 
    sum(merged_data$Completeness_pct < 50, na.rm = TRUE), "\n\n")

cat("=== FIPS CODE CHECK ===\n")
cat("All FIPS codes are 5 characters:", 
    all(nchar(merged_clean$county_fips) == 5), "\n\n")

# Summary statistics by 2020 winner
cat("=== VACCINATION BY 2020 ELECTION WINNER ===\n")
summary_by_winner <- merged_clean %>%
  group_by(winner_2020) %>%
  summarise(
    n_counties = n(),
    mean_vax = mean(Series_Complete_Pop_Pct, na.rm = TRUE),
    median_vax = median(Series_Complete_Pop_Pct, na.rm = TRUE),
    sd_vax = sd(Series_Complete_Pop_Pct, na.rm = TRUE),
    min_vax = min(Series_Complete_Pop_Pct, na.rm = TRUE),
    max_vax = max(Series_Complete_Pop_Pct, na.rm = TRUE)
  )
print(summary_by_winner)

# Correlation
correlation <- cor(merged_clean$trump_pct_2020, 
                   merged_clean$Series_Complete_Pop_Pct)
cat("\nCorrelation (Trump % vs Vaccination %):", round(correlation, 4), "\n")

# T-test
t_test <- t.test(Series_Complete_Pop_Pct ~ winner_2020, data = merged_clean)
cat("T-test p-value:", format.pval(t_test$p.value, digits = 3), "\n")

# Linear model
model <- lm(Series_Complete_Pop_Pct ~ trump_pct_2020, data = merged_clean)
cat("Linear Model R-squared:", round(summary(model)$r.squared, 4), "\n")
cat("Slope (change in vax % per 1% Republican):", 
    round(coef(model)[2], 4), "\n\n")

cat("=== DATA CLEANING COMPLETE ===\n")
cat("Ready for visualization with COMPLETE data including Texas!\n")
cat("Main dataset: merged_clean (", nrow(merged_clean), " counties)\n")


#-----Visualization 1 -----#

#create margin category
merged_clean <- merged_clean %>%
  mutate(
    county_type = ifelse(margin_2020 > 15, "Landslide Counties", "Competitive Counties"),
    county_type = factor(county_type, levels = c("Landslide Counties", "Competitive Counties"))
  )

#Calculate Statistics
stats_by_type <- merged_clean %>%
  group_by(county_type) %>%
  summarise(
    n = n(),
    correlation = cor(trump_pct_2020, Series_Complete_Pop_Pct),
    r_squared = cor(trump_pct_2020, Series_Complete_Pop_Pct)^2
  )
print(stats_by_type)
overall_cor <- cor(merged_clean$trump_pct_2020, merged_clean$Series_Complete_Pop_Pct)
overall_model <- lm(Series_Complete_Pop_Pct ~ trump_pct_2020, data = merged_clean)
overall_p <- summary(overall_model)$coefficients[2, 4]



#-----Plot 1-----#

#cleaning
merged_clean <- merged_clean %>%
  select(-county_type)

# Now recreate it with the correct labels
merged_clean <- merged_clean %>%
  mutate(
    county_type = ifelse(margin_2020 > 15, "Landslide", "Competitive"),
    county_type = factor(county_type, levels = c("Landslide", "Competitive"))
  )


#Build
main_plot_single <- ggplot(merged_clean, 
                           aes(x = trump_pct_2020, 
                               y = Series_Complete_Pop_Pct, 
                               color = winner_2020,
                               shape = county_type)) +
  
  # Points with jitter
  geom_point(alpha = 0.4, 
             size = 1.2,
             position = position_jitter(width = 0.3, height = 0.3)) +
  
  # Regression lines by county type
  geom_smooth(aes(linetype = county_type),
              method = "lm", 
              se = TRUE,
              alpha = 0.15,
              linewidth = 1.2,
              color = "black") +
  
  # Colors for winner
  scale_color_manual(
    values = c("DEMOCRAT" = "#2E74C0", "REPUBLICAN" = "#CB454A"),
    labels = c("Democratic", "Republican"),
    name = "2020 Winner"
  ) +
  
  # Shapes for county type
  scale_shape_manual(
    values = c("Landslide" = 16, "Competitive" = 17),
    labels = c("Landslide (>15pt margin)", "Competitive (≤15pt margin)"),
    name = "County Type"
  ) +
  
  # Line types for regression
  scale_linetype_manual(
    values = c("Landslide" = "solid", "Competitive" = "dashed"),
    labels = c(paste0("Landslide (r = ", round(stats_by_type$correlation[1], 2), ")"), 
               paste0("Competitive (r = ", round(stats_by_type$correlation[2], 2), ")")),
    name = "Regression"
  ) +
  
  # Labels
  labs(
    title = "Vaccination Politicization: Strongest in Partisan Strongholds",
    x = "Republican Vote Share (%, 2020 Election)",
    y = "Population Fully Vaccinated (%, November 2021)",
    caption = "Data: MIT Election Lab & CDC (November 10, 2021) | Points jittered | Shaded regions show 95% confidence intervals"
  ) +
  
  # Theme
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30"),
    plot.caption = element_text(size = 8, color = "gray50", hjust = 0),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5)
  ) +
  
  guides(
    color = guide_legend(order = 1, override.aes = list(size = 3, alpha = 1)),
    shape = guide_legend(order = 2, override.aes = list(size = 3, alpha = 1)),
    linetype = guide_legend(order = 3)
  )

print(main_plot_single)


#-----SECOND PLOT -----#
main_plot_panels <- ggplot(merged_clean, 
                           aes(x = trump_pct_2020, 
                               y = Series_Complete_Pop_Pct, 
                               color = winner_2020)) +
  
  # Points
  geom_point(alpha = 0.5, size = 1.5) +
  
  # Regression line with confidence interval
  geom_smooth(method = "lm", 
              se = TRUE,
              alpha = 0.2,
              linewidth = 1.2,
              color = "black") +
  
  # Facet by county type
  facet_wrap(~county_type, ncol = 2) +
  
  # Colors
  scale_color_manual(
    values = c("DEMOCRAT" = "#2E74C0", "REPUBLICAN" = "#CB454A"),
    labels = c("Democratic", "Republican"),
    name = "2020 Winner"
  ) +
  
  # Labels
  labs(
    title = "Vaccination Politicization Across County Types",
    subtitle = paste0("Republican counties averaged 42.4% vaccinated vs 55.5% in Democratic counties",
                      "\nOverall: r = ", round(overall_cor, 3), 
                      ", p < 0.001, n = ", nrow(merged_clean), " counties"),
    x = "Republican Vote Share (%, 2020 Election)",
    y = "Population Fully Vaccinated (%, November 2021)",
    caption = "Data: MIT Election Lab & CDC (November 10, 2021) | Shaded regions show 95% confidence intervals"
  ) +
  
  # Theme
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30"),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "gray95", color = NA),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5)
  )

print(main_plot_panels)

#-----MAP Plots-----#

#read in
county_map <- map_data("county")

#clean
map_data_prep <- merged_clean %>%
  mutate(
    region = tolower(state),
    subregion = tolower(county_name),
    # Remove suffixes
    subregion = str_remove(subregion, " county| parish| borough| census area| municipality| city and borough"),
    # Standardize "Saint" to "st"
    subregion = str_replace(subregion, "^saint ", "st "),
    # Remove spaces in compound names (dekalb, dupage, lasalle, desoto)
    subregion = str_replace(subregion, "^de kalb$", "dekalb"),
    subregion = str_replace(subregion, "^du page$", "dupage"),
    subregion = str_replace(subregion, "^la salle$", "lasalle"),
    subregion = str_replace(subregion, "^de soto$", "desoto"),
    # Handle DC
    region = ifelse(region == "district of columbia", "district of columbia", region),
    subregion = ifelse(region == "district of columbia", "washington", subregion)
  ) %>%
  select(region, subregion, 
         winner_2020, winner_2024, 
         trump_pct_2020, trump_pct_2024,
         Series_Complete_Pop_Pct)

# Merge with map data
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
map_2024 <- ggplot(map_with_data, 
                   aes(x = long, y = lat, group = group, fill = winner_2024)) +
  geom_polygon(color = "white", size = 0.05) +
  scale_fill_manual(
    values = c("DEMOCRAT" = "#2E74C0", "REPUBLICAN" = "#CB454A"),
    na.value = "gray90",
    name = "Winner"
  ) +
  coord_fixed(1.3) +
  theme_void(base_size = 12) +
  labs(title = "2024 Presidential Election Results",
       subtitle = "County-level winners") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray40"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

print(map_2024)



#MAP 3
map_vaccination <- ggplot(map_with_data, 
                          aes(x = long, y = lat, group = group, 
                              fill = Series_Complete_Pop_Pct)) +
  geom_polygon(color = "white", size = 0.05) +
  scale_fill_gradient2(
    low = "#d73027",         # Red for low vaccination
    mid = "#ffffbf",         # Yellow for medium
    high = "#1a9850",        # Green for high vaccination
    midpoint = 45,           # Center on 45%
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



#Timeline

#Make sure Covid dates are converted properly
if(class(Covid$Date)[1] != "Date") {
  Covid$Date <- as.Date(Covid$Date, format = "%m/%d/%Y")
}

# Get monthly snapshots to reduce data points
vacc_timeline <- Covid %>%
  filter(Date >= as.Date("2021-01-01"), Date <= as.Date("2023-05-01")) %>%
  mutate(month = floor_date(Date, "month")) %>%
  # For each county-month, get the last available date's data
  group_by(FIPS, month) %>%
  filter(Date == max(Date)) %>%
  ungroup() %>%
  select(FIPS, month, Date, Series_Complete_Pop_Pct, Administered_Dose1_Pop_Pct)

cat("Timeline data spans:", min(vacc_timeline$Date), "to", max(vacc_timeline$Date), "\n")
cat("Total county-month observations:", nrow(vacc_timeline), "\n\n")

#merge
timeline_with_election <- vacc_timeline %>%
  inner_join(
    election_comparison %>% select(county_fips, winner_2020),
    by = c("FIPS" = "county_fips")
  ) %>%
  filter(!is.na(winner_2020))

cat("Timeline observations after merge:", nrow(timeline_with_election), "\n")

#calculate average
timeline_summary <- timeline_with_election %>%
  group_by(month, winner_2020) %>%
  summarize(
    avg_fully_vacc = mean(Series_Complete_Pop_Pct, na.rm = TRUE),
    avg_one_dose = mean(Administered_Dose1_Pop_Pct, na.rm = TRUE),
    n_counties = n(),
    .groups = "drop"
  ) %>%
  filter(!is.na(avg_fully_vacc))

cat("Timeline summary rows:", nrow(timeline_summary), "\n")
head(timeline_summary, 10)


#PLOT
gap_nov2021 <- timeline_summary %>%
  filter(month == as.Date("2021-11-01")) %>%
  select(winner_2020, avg_fully_vacc) %>%
  pivot_wider(names_from = winner_2020, values_from = avg_fully_vacc) %>%
  mutate(gap = DEMOCRAT - REPUBLICAN) %>%
  pull(gap)

timeline_plot <- ggplot(timeline_summary, 
                        aes(x = month, y = avg_fully_vacc, 
                            color = winner_2020, group = winner_2020)) +
  
  # Lines
  geom_line(linewidth = 1.5) +
  geom_point(size = 2, alpha = 0.7) +
  
  # Key date markers
  geom_vline(xintercept = as.Date("2021-05-01"), 
             linetype = "dashed", color = "gray50", alpha = 0.5) +
  annotate("text", x = as.Date("2021-05-01"), y = 5, 
           label = "Vaccines\nWidely Available", 
           size = 3, hjust = 0, color = "gray40") +
  
  geom_vline(xintercept = as.Date("2021-09-09"), 
             linetype = "dashed", color = "gray50", alpha = 0.5) +
  annotate("text", x = as.Date("2021-09-09"), y = 65, 
           label = "Biden Mandate\nAnnounced", 
           size = 3, hjust = 0, color = "gray40") +
  
  # Highlight November 2021 (your analysis date)
  geom_vline(xintercept = as.Date("2021-11-01"), 
             linetype = "solid", color = "black", alpha = 0.3, linewidth = 1) +
  annotate("text", x = as.Date("2021-11-01"), y = 25, 
           label = paste0("Analysis Date\nGap: ", round(gap_nov2021, 1), " pts"), 
           size = 3.5, hjust = 0.5, fontface = "bold", color = "black") +
  
  # Colors
  scale_color_manual(
    values = c("DEMOCRAT" = "#2E74C0", "REPUBLICAN" = "#CB454A"),
    labels = c("Democratic Counties", "Republican Counties"),
    name = "2020 Winner"
  ) +
  
  # Scales
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, 10)) +
  
  # Labels
  labs(
    title = "Vaccination Gap Emerged Early and Persisted",
    subtitle = "Average county vaccination rates by 2020 election winner (January 2021 - May 2023)",
    x = "Date",
    y = "Population Fully Vaccinated (%)",
    caption = "Data: CDC & MIT Election Lab | Lines show monthly averages across all counties"
  ) +
  
  # Theme
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(timeline_plot)


#BOX PLOT

box_plot <- ggplot(merged_clean, 
                   aes(x = winner_2020, y = Series_Complete_Pop_Pct, 
                       fill = winner_2020)) +
  
  # Box plot
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  
  # Add mean as diamond
  stat_summary(fun = mean, geom = "point", 
               shape = 23, size = 4, fill = "white", color = "black") +
  
  # Add mean labels
  stat_summary(fun = mean, geom = "text", 
               aes(label = paste0(round(..y.., 1), "%")),
               vjust = -1.5, size = 4, fontface = "bold") +
  
  # Add p-value annotation
  annotate("text", x = 1.5, y = 95, 
           label = "p < 0.001 ***\n13.1 point gap", 
           size = 4, fontface = "bold") +
  
  # Colors
  scale_fill_manual(
    values = c("DEMOCRAT" = "#2E74C0", "REPUBLICAN" = "#CB454A"),
    labels = c("Democratic\nCounties", "Republican\nCounties")
  ) +
  
  # Labels
  labs(
    title = "Vaccination Rates by County Partisanship",
    subtitle = "Democratic counties had significantly higher vaccination rates",
    x = "2020 Election Winner",
    y = "Population Fully Vaccinated (%, November 2021)"
  ) +
  
  scale_x_discrete(labels = c("Democratic\nCounties", "Republican\nCounties")) +
  
  # Theme
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50"),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

print(box_plot)


#Histogram
hist_vax <- ggplot(merged_clean, 
                   aes(x = Series_Complete_Pop_Pct, fill = winner_2020)) +
  
  # Overlapping histograms
  geom_histogram(alpha = 0.6, bins = 40, position = "identity") +
  
  # Add mean lines
  geom_vline(data = merged_clean %>% 
               group_by(winner_2020) %>% 
               summarise(mean_vax = mean(Series_Complete_Pop_Pct)),
             aes(xintercept = mean_vax, color = winner_2020),
             linetype = "dashed", linewidth = 1) +
  
  # Colors
  scale_fill_manual(
    values = c("DEMOCRAT" = "#2E74C0", "REPUBLICAN" = "#CB454A"),
    labels = c("Democratic Counties", "Republican Counties"),
    name = "2020 Winner"
  ) +
  scale_color_manual(
    values = c("DEMOCRAT" = "#2E74C0", "REPUBLICAN" = "#CB454A"),
    guide = "none"
  ) +
  
  # Labels
  labs(
    title = "Distribution of County Vaccination Rates",
    x = "Population Fully Vaccinated (%, November 2021)",
    y = "Number of Counties",
    caption = "Dashed lines show group means"
  ) +
  
  # Theme
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

print(hist_vax)


#Hist 2
hist_margin <- ggplot(merged_clean, 
                      aes(x = margin_2020, fill = winner_2020)) +
  
  # Histogram
  geom_histogram(bins = 40, alpha = 0.7, position = "identity") +
  
  # Mark the 15-point cutoff
  geom_vline(xintercept = 15, linetype = "dashed", 
             color = "black", linewidth = 1) +
  annotate("text", x = 15, y = 100, 
           label = "15 point margin\n(Landslide cutoff)", 
           hjust = -0.1, size = 3.5, fontface = "italic") +
  
  # Colors
  scale_fill_manual(
    values = c("DEMOCRAT" = "#2E74C0", "REPUBLICAN" = "#CB454A"),
    labels = c("Democratic Counties", "Republican Counties"),
    name = "2020 Winner"
  ) +
  
  # Labels
  labs(
    title = "Distribution of Victory Margins",
    subtitle = "Most counties had decisive victories (>15 point margin)",
    x = "Victory Margin (percentage points, 2020 Election)",
    y = "Number of Counties",
    caption = "Margin = absolute difference between Republican and Democratic vote share"
  ) +
  
  # Theme
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

print(hist_margin)
