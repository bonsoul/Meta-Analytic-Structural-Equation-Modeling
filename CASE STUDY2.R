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


sapply(df1[, c("prod_inc","time_bp","time_dp","travel_time")], 
       function(x) var(x, na.rm=TRUE))

cor(df1[, c("prod_inc","time_bp","time_dp","travel_time")], use="pairwise.complete.obs")


prod_model <- '
  Productivity =~ prod_inc + time_bp + travel_time
'
prod_fit <- sem(prod_model, data = df1, std.lv = TRUE)
summary(prod_fit, standardized=TRUE, fit.measures=TRUE)


df1$Productivity_score <- scale(df1$prod_inc) + 
  scale(df1$time_bp) + 
  scale(df1$travel_time)

sem_model_comp <- '
  Wellbeing =~ sleep_bal + relaxed + self_time
  Productivity_score ~ Wellbeing
'
sem_fit_comp <- sem(sem_model_comp, data = df1, std.lv = TRUE)
summary(sem_fit_comp, fit.measures=TRUE, standardized=TRUE)


colnames(df1)





df1_std <- df1 %>%
  mutate(across(c(sleep_bal, relaxed, self_time,
                  prod_inc, time_bp, time_dp, travel_time),
                scale))


# 3. Fit a simpler model first (just to test)
library(lavaan)
test_model <- '
  wellbeing =~ sleep_bal + relaxed + self_time
'
test_fit <- sem(test_model, data = df1_std, std.lv = TRUE)
summary(test_fit, fit.measures = TRUE, standardized = TRUE)




# Structural Model (SEM)

# Simplified SEM with just measurement part (like your first model)
sem_model_reduced <- '
  Wellbeing =~ sleep_bal + relaxed + self_time
  Productivity =~ prod_inc + time_bp + travel_time
'
sem_fit_reduced <- sem(sem_model_reduced, data = df1, std.lv = TRUE)
summary(sem_fit_reduced, fit.measures = TRUE, standardized = TRUE)



# Modification indices (suggests extra paths)
modificationindices(sem_fit_reduced, sort.=TRUE, maximum.number=10)


