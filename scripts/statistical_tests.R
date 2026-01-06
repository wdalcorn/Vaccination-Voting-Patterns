

# Linear regression
model <- lm(Series_Complete_Pop_Pct ~ trump_pct, data = merged_data)
summary(model)



merged_clean <- merged_clean %>%
  mutate(winner = ifelse(trump_pct > 50, "Trump", "Biden"))

# Check group sizes
table(merged_clean$winner)

# Check means by group
merged_clean %>%
  group_by(winner) %>%
  summarise(
    n = n(),
    mean_vax = mean(Series_Complete_Pop_Pct, na.rm = TRUE),
    sd_vax = sd(Series_Complete_Pop_Pct, na.rm = TRUE),
    median_vax = median(Series_Complete_Pop_Pct, na.rm = TRUE)
  )


t_test_result <- t.test(Series_Complete_Pop_Pct ~ winner, data = merged_clean)
print(t_test_result)

# Welch's t-test (better if variances are unequal - this is the default)
# This is what t.test() does by default

# Or specify equal variances if you want traditional t-test
t_test_equal <- t.test(Series_Complete_Pop_Pct ~ winner, 
                       data = merged_clean, 
                       var.equal = TRUE)
print(t_test_equal)


merged_clean <- merged_data %>%
  filter(!is.na(Series_Complete_Pop_Pct),
         Series_Complete_Pop_Pct > 0)
summary(merged_clean$Completeness_pct)

correlation <- cor(merged_clean$trump_pct, merged_clean$Series_Complete_Pop_Pct)
print(correlation)

model <- lm(Series_Complete_Pop_Pct ~ trump_pct, data = merged_clean)
summary(model)

library(ggplot2)
ggplot(merged_clean, aes(x = trump_pct, y = Series_Complete_Pop_Pct)) +
  geom_point(alpha = 0.4, color = "steelblue", size = 2) +
  geom_smooth(method = "lm", color = "red", se = TRUE, linewidth = 1.2) +
  labs(
    title = "County Voting Behavior vs COVID-19 Vaccination Rates",
    subtitle = paste("August 2021 | n =", nrow(merged_clean), "counties | r =", 
                     round(correlation, 3)),
    x = "Trump Vote Share (%, 2020 Election)",
    y = "Population Fully Vaccinated (%, August 2021)",
    caption = "Data sources: MIT Election Lab, CDC"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40"),
    panel.grid.minor = element_blank()
  )



summary_stats <- merged_clean %>%
  group_by(winner) %>%
  summarise(
    mean_vax = mean(Series_Complete_Pop_Pct),
    se = sd(Series_Complete_Pop_Pct) / sqrt(n()),
    ci_lower = mean_vax - 1.96 * se,
    ci_upper = mean_vax + 1.96 * se,
    n = n()
  )
print(summary_stats)
