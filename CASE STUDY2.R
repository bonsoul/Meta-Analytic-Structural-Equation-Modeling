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

# 1. Check variable types
str(df)

# 2. Check for zero variance variables
sapply(df, function(x) var(as.numeric(x), na.rm = TRUE))

# 3. Fit a simpler model first (just to test)
library(lavaan)
test_model <- '
  wellbeing =~ sleep_bal + relaxed + self_time
'
test_fit <- sem(test_model, data = df, std.lv = TRUE)
summary(test_fit, fit.measures = TRUE, standardized = TRUE)




# Structural Model (SEM)

sem_model <- '
  # Measurement part
  Wellbeing =~ sleep_bal + relaxed + self_time
  Productivity =~ prod_inc + time_bp + time_dp + travel_time

  # Structural part (hypothesis)
  Productivity ~ Wellbeing
'

sem_fit <- sem(sem_model, data = df, std.lv = TRUE)

summary(sem_fit, fit.measures = TRUE, standardized = TRUE)


# Modification indices (suggests extra paths)
modificationindices(sem_fit, sort.=TRUE, maximum.number=10)


