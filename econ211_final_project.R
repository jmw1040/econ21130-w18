# ECON 211 Final Project
library(dplyr)
# HPRP Data:
df <- read.csv("HPRP_Data.csv", header = TRUE, stringsAsFactors = FALSE)

df = df %>%
  mutate(total_beds = emergency_shelter_beds + supportive_housing_beds + transitional_housing_beds)
df = df %>%
  mutate(frac_unsheltered = unsheltered_ct/total_homeless,
         frac_supporitve_beds = supportive_housing_beds/total_beds, 
         frac_temp_beds = 1 - frac_supportive_beds)
# Missing values
df = df %>%
  filter(!is.na(df$total_beds))

# Two-stage regression, instrumenting for city-level variables

# Original regression, without including city factor (in this case, amount of money received per homeless)
reg1 <- lm(outcome ~ avg_duration, data = df)
reg1b <- lm(residuals(reg1) ~ df$amt_per_homeless + df$frac_unsheltered)

temp = df$outcome - reg1b$coefficients[2]* df$amt_per_homeless - reg1b$coefficients[3] * df$frac_unsheltered
finalfinal = lm(temp ~ df$avg_duration)
