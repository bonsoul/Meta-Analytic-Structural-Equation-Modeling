# SEM + data handling
library(lavaan)
library(dplyr)

# Diagnostics & visualization
library(semPlot)
library(psych)


# Example: recode factors
df1$prefer <- factor(df$prefer,
                    levels = c("Complete Physical Attendance", "Hybrid Mode"))

df1$certaindays_hw <- factor(df$certaindays_hw,
                            levels = c("Maybe", "No", "Yes"))

# Check structure
str(df1)



# Suppose you hypothesize two latent factors:
#   Wellbeing = sleep_bal, relaxed, self_time
#   Productivity = prod_inc, time_bp, time_dp, travel_time

cfa_model <- '
  Wellbeing =~ sleep_bal + relaxed + self_time
  Productivity =~ prod_inc + time_bp + time_dp + travel_time
'

cfa_fit <- cfa(cfa_model, data = df, std.lv = TRUE)

summary(cfa_fit, fit.measures = TRUE, standardized = TRUE)

