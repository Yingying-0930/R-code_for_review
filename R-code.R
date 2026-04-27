# ============================================================================
# Title: Changes in Body Weight and Body Fat Percentage Before and After 
#        16-Day Dietary Controlled Trial
# Author: Yingying Li
# Date: 2026-04-19
# Environment: R version 4.5.2
# ============================================================================


# Load packages
library(ggplot2)
library(ggpubr)
library(effsize)
library(tidyr)
library(dplyr)

# ========================== 1. Simulated data generation ==========================

set.seed(20260419)

n_male <- 10
n_female <- 12

data <- data.frame(
  ID = 1:22,
  Gender = rep(c("male", "female"), c(n_male, n_female)),
  Age = rep(c(30, 25), c(n_male, n_female)),
  Weight1 = round(c(rnorm(n_male, 85, 8), rnorm(n_female, 60, 6)), 1),
  Body_Fat_Percentage1 = round(c(rnorm(n_male, 29, 3), rnorm(n_female, 36, 3)), 1)
)

data$Weight2 <- round(data$Weight1 - rnorm(22, 2, 1.5), 1)

data$Body_Fat_Percentage2 <- round(data$Body_Fat_Percentage1 + rnorm(22, -0.2, 1.5), 1)

data$Weight1 <- pmax(data$Weight1, 45)
data$Weight2 <- pmax(data$Weight2, 42)
data$Body_Fat_Percentage1 <- pmin(pmax(data$Body_Fat_Percentage1, 18), 50)
data$Body_Fat_Percentage2 <- pmin(pmax(data$Body_Fat_Percentage2, 18), 50)


str(data)

# ========================== 2. Descriptive Statistics ==========================

desc_stats <- data %>%
  summarise(
    # Initial body weight and body fat percentage (Weight1 and Body_Fat_Percentage1)
    mean_weight1 = mean(Weight1, na.rm = TRUE),
    sd_weight1 = sd(Weight1, na.rm = TRUE),
    median_weight1 = median(Weight1, na.rm = TRUE),
    q1_weight1 = quantile(Weight1, 0.25, na.rm = TRUE),
    q3_weight1 = quantile(Weight1, 0.75, na.rm = TRUE),
    iqr_weight1 = IQR(Weight1, na.rm = TRUE),
    # Weight and body fat percentage after intervention (Weight2 and Body_Fat_Percentage2)
    mean_fat1 = mean(Body_Fat_Percentage1, na.rm = TRUE),
    sd_fat1 = sd(Body_Fat_Percentage1, na.rm = TRUE),
    mean_fat2 = mean(Body_Fat_Percentage2, na.rm = TRUE),
    sd_fat2 = sd(Body_Fat_Percentage2, na.rm = TRUE)
  )
print(desc_stats)

# ========================== 3. Outlier Detection ==========================

# Body weight change
data$Weight_Change <- data$Weight2 - data$Weight1
  # Standard deviation method (±3SD criterion)
    mean_weight <- mean(data$Weight_Change, na.rm = TRUE)
    sd_weight <- sd(data$Weight_Change, na.rm = TRUE)
    data$Outlier1_3SD1 <- abs(data$Weight_Change - mean_weight) > 3*sd_weight
  # IQR method (boxplot principle)
    Q <- quantile(data$Weight_Change, probs = c(0.25, 0.75), na.rm = TRUE)
    IQR_weight <- Q[2] - Q[1]
    data$Outlier_IQR1 <- data$Weight_Change < (Q[1] - 1.5 * IQR_weight) | data$Weight_Change > (Q[2] + 1.5 * IQR_weight)
    
# Body fat change
data$Fat_Change <- data$Body_Fat_Percentage2 - data$Body_Fat_Percentage1
  # Standard deviation method (±3SD criterion)
    mean_fat <- mean(data$Fat_Change, na.rm = TRUE)
    sd_fat <- sd(data$Fat_Change, na.rm = TRUE)
    data$Outlier_3SD2 <- abs(data$Fat_Change - mean_fat) > 3 * sd_fat
  # IQR method (boxplot principle)
    Q <- quantile(data$Fat_Change, probs = c(0.25, 0.75), na.rm = TRUE)
    IQR_fat <- Q[2] - Q[1]
    data$Outlier_IQR2 <- data$Fat_Change < (Q[1] - 1.5 * IQR_fat) | data$Fat_Change > (Q[2] + 1.5 * IQR_fat)
  
# ========================== 4. Normality Test ==========================

# Body weight change
  # Histogram
  hist(data$Weight_Change, 
     main = "Histogram of Weight Differences",
     xlab = "Weight Change (kg)",
     col = "lightblue",
     prob = TRUE)
  lines(density(data$Weight_Change, na.rm = TRUE), col = "red", lwd = 2)
  curve(dnorm(x, mean = mean(data$Weight_Change, na.rm = TRUE), 
            sd = sd(data$Weight_Change, na.rm = TRUE)), 
      add = TRUE, col = "blue", lwd = 2)
  legend("topright", legend = c("Density", "Normal"), 
       col = c("red", "blue"), lty = 1)

  # Q-Q plot
  qqnorm(data$Weight_Change, main = "Q-Q Plot for Weight Differences")
  qqline(data$Weight_Change, col = "red")

  # Shapiro-Wilk test
  shapiro_test <- shapiro.test(data$Weight_Change)
  print(shapiro_test)
  
# Body fat change
  # Histogram
  hist(data$Fat_Change, 
     main = "Histogram of Fat Percentage Differences",
     xlab = "Fat Percentage Change (%)",
     col = "lightblue",
     prob = TRUE)
  lines(density(data$Fat_Change, na.rm = TRUE), col = "red", lwd = 2)
  curve(dnorm(x, mean = mean(data$Fat_Change, na.rm = TRUE), 
            sd = sd(data$Fat_Change, na.rm = TRUE)), 
      add = TRUE, col = "blue", lwd = 2)
  legend("topright", legend = c("Density", "Normal"), 
       col = c("red", "blue"), lty = 1)

  # Q-Q plot
  qqnorm(data$Fat_Change, main = "Q-Q Plot for Fat Percentage Differences")
  qqline(data$Fat_Change, col = "red")

  # Shapiro-Wilk test
  shapiro_test <- shapiro.test(data$Fat_Change)
  print(shapiro_test)

# ========================== 5. Paired t-test ==========================

# Paired t-test for weight change
  weight_ttest <- t.test(data$Weight1, data$Weight2, paired = TRUE)
  print(weight_ttest)

# Paired t-test for body fat percentage change
  fat_ttest <- t.test(data$Body_Fat_Percentage1, data$Body_Fat_Percentage2, paired = TRUE)
  print(fat_ttest)

# ========================== 6. Effect Size Calculation ==========================

weight_cohen <- cohen.d(data$Weight2, data$Weight1, paired = TRUE)
fat_cohen <- cohen.d(data$Body_Fat_Percentage2, data$Body_Fat_Percentage1, paired = TRUE)

print(weight_cohen)
print(fat_cohen)

# ========================== 7. Data Visualization Preparation ==========================

# Convert data to long format
long_data <- data %>%
  select(ID, Weight1, Weight2, Body_Fat_Percentage1, Body_Fat_Percentage2) %>%
  pivot_longer(
    cols = -ID,
    names_to = c("Measure", "Time"),
    names_pattern = "(.*)([0-9]+)"  # Split between letters and numbers
  ) %>%
  mutate(Time = ifelse(Time == "1", "Pre", "Post")) %>%
  pivot_wider(
    names_from = Measure,
    values_from = value
  ) %>%
  mutate(Time = factor(Time, levels = c("Pre", "Post")))

# Check conversion results
head(long_data)

# ========================== 8. Visualization ==========================

# Weight change plot
  weight_plot <- ggplot(long_data, aes(x = Time, y = Weight, group = ID)) +
  geom_line(color = "gray", alpha = 0.6) +
  geom_point(aes(color = Time), size = 2) +
  stat_summary(fun = mean, geom = "line", group = 1, color = "red2", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", group = 1, color = "red2", size = 3) +
  scale_color_manual(values = c("Pre" = "#C1421E", "Post" = "#6FB7BC")) +
  labs(title = "Pre- vs. Post-Dietary Intervention Body Weight Changes",
       subtitle = paste0("Paired t-test: p = ", format(weight_ttest$p.value, digits = 3)),
       y = "Weight (kg)") +
  theme_minimal() +
  theme(legend.position = "none")
  print(weight_plot)
  
# Body fat percentage change plot
  fat_plot <- ggplot(long_data, aes(x = Time, y = Body_Fat_Percentage, group = ID)) +
  geom_line(color = "gray", alpha = 0.6) +
  geom_point(aes(color = Time), size = 2) +
  stat_summary(fun = mean, geom = "line", group = 1, color = "blue2", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", group = 1, color = "blue2", size = 3) +
  scale_color_manual(values = c("Pre" = "#C1421E", "Post" = "#6FB7BC")) +
  labs(title = "Pre- vs. Post-Dietary Intervention Body Fat Percentage Changes",
       subtitle = paste0("Paired t-test: p = ", round(fat_ttest$p.value, 4)),
       y = "Body Fat Percentage (%)") +
  theme_minimal() +
  theme(legend.position = "none")
  print(fat_plot)
  
# Combined plot
  combined_plot <- ggarrange(weight_plot, fat_plot, ncol = 2)
  print(combined_plot)

# Save plots
ggsave("combined_changes.pdf", combined_plot, width = 12, height = 5)

# ========================== 9. Gender-stratified Analysis ==========================

# Descriptive statistics by gender
  gender_stats <- data %>%
  group_by(Gender) %>%
  summarise(
    n = n(),
    weight_diff_mean = mean(Weight2 - Weight1, na.rm = TRUE),
    weight_diff_sd = sd(Weight2 - Weight1, na.rm = TRUE),
    fat_diff_mean = mean(Body_Fat_Percentage2 - Body_Fat_Percentage1, na.rm = TRUE),
    fat_diff_sd = sd(Body_Fat_Percentage2 - Body_Fat_Percentage1, na.rm = TRUE)
  )
  print(gender_stats)

# Weight change by gender
  gender_weight <- ggplot(data, aes(x = Gender, y = Weight_Change, fill = Gender)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(
    fun = mean, 
    geom = "point", 
    shape = 18,
    size = 4, 
    color = "red2",
    show.legend = FALSE
  ) +
  stat_summary(
    fun = mean, 
    geom = "text", 
    aes(label = round(..y.., 2)),
    vjust = -0.7,
    color = "black",
    size = 4
  ) +
  scale_fill_manual(values = c("female" = "#E27C6E", "male" = "#6FB7BC")) +
  labs(title = "Weight Change by Gender",
       y = "Change in Body Weight (kg)") +
  theme_minimal()
  print(gender_weight)
  
# Body fat change by gender
  gender_fat <- ggplot(data, aes(x = Gender, y = Fat_Change, fill = Gender)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(
    fun = mean, 
    geom = "point", 
    shape = 18,
    size = 4, 
    color = "red2",
    show.legend = FALSE
  ) +
  stat_summary(
    fun = mean, 
    geom = "text", 
    aes(label = round(..y.., 2)),
    vjust = -2,
    color = "black",
    size = 4
  ) +
  scale_fill_manual(values = c("female" = "#E27C6E", "male" = "#6FB7BC")) +
  labs(title = "Body Fat Change by Gender",
       y = "Change in Body Fat Percentage (%)") +
  theme_minimal()
  print(gender_fat)
  
# Combine gender plots
  gender_plots <- ggarrange(gender_weight, gender_fat, ncol = 2)
  print(gender_plots)
  ggsave("gender_changes.pdf", gender_plots, width = 10, height = 5)

# ============================================================================
# Title: Association between Dietary Carbohydrate Intake and Blood Ketone 
#        Concentrations in Healthy Chinese Adults by Sex
# Author: Yingying Li
# Date: 2026-04-20
# Environment: R version 4.5.2
# ============================================================================

# Load packages
library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(MuMIn)
  
# ========================== 1. Simulated data generation ==========================
  
set.seed(20260420)
  
n_male <- 10
n_female <- 12

participant_data <- data.frame(
  ID = 1:22,
  gender = rep(c(1, 2), c(n_male, n_female)), 
  age = round(rnorm(22, 28, 7), 0),
  BMI_continuous = round(rnorm(22, 25, 3.5), 1),
  sleep_hours = round(runif(22, 5, 9), 1),
  daily_steps = round(runif(22, 1000, 15000)),
  HOMA_IR = round(runif(22, 1.5, 3.0), 2),
  Est_VAT_mass = round(runif(22, 200, 800), 1),
  lean_body = round(runif(22, 25000, 70000), 0),
  EE = round(runif(22, 20, 55), 2) 
)

participant_data$BMI <- cut(participant_data$BMI_continuous,
                            breaks = c(-Inf, 18.5, 25, 30, Inf),
                            labels = c(1, 2, 3, 4),
                            right = FALSE)

participant_data$Sleep <- cut(participant_data$sleep_hours,
                              breaks = c(-Inf, 7, Inf),
                              labels = c(0, 1),
                              right = FALSE)

participant_data$Sport <- cut(participant_data$daily_steps,
                              breaks = c(-Inf, 2500, 5000, 7500, 10000, 12500, Inf),
                              labels = c(1, 2, 3, 4, 5, 6),
                              right = FALSE)

  
CHO_levels <- c(20, 40, 50, 70, 90)
D3H <- data.frame()

for (i in 1:22) {
  random_intercept <- rnorm(1, 0, 0.1)
  
  for (cho in CHO_levels) {
    cho_c <- cho - 45
    value <- 0.5 + 0.06 * cho_c - 0.0012 * cho_c^2 +
      ifelse(participant_data$gender[i] == 1, 0.1, -0.05) +
      -0.005 * (participant_data$age[i] - 28) +
      0.0002 * participant_data$Est_VAT_mass[i] +
      random_intercept + rnorm(1, 0, 0.08)
    
    value <- round(pmax(value, 0.05), 2)
    
    D3H <- rbind(D3H, data.frame(
      ID = participant_data$ID[i], CHO = cho, value = value,
      age = participant_data$age[i], gender = participant_data$gender[i],
      BMI_continuous = participant_data$BMI_continuous[i],
      BMI = participant_data$BMI[i],
      Sleep = participant_data$Sleep[i],
      Sport = participant_data$Sport[i],
      HOMA_IR = participant_data$HOMA_IR[i],
      Est_VAT_mass = participant_data$Est_VAT_mass[i],
      lean_body = participant_data$lean_body[i],
      EE = participant_data$EE[i]
    ))
  }
}

head(D3H)
  
# Descriptive statistics for continuous BMI

BMI_stats <- data.frame(
  Variable = "BMI_continuous",
  Mean = mean(participant_data$BMI_continuous),
  SD = sd(participant_data$BMI_continuous),
  Median = median(participant_data$BMI_continuous),
  IQR = IQR(participant_data$BMI_continuous)
)

print(BMI_stats)

# ========================== 2. Data Cleaning ==========================
  
# Convert ID to factor
D3H$ID <- as.factor(D3H$ID)
  
# Convert categorical variables to factors (Gender coding: 1=Male, 2=Female)
D3H$gender <- factor(D3H$gender, levels = c(1, 2), labels = c("Male", "Female"))
D3H$BMI <- factor(D3H$BMI)
D3H$Sleep <- factor(D3H$Sleep)
D3H$Sport <- factor(D3H$Sport)
  
# Center predictor variables (reduce collinearity)
D3H$CHO_centered <- scale(D3H$CHO, center = TRUE, scale = FALSE)
  
# ========================== 3. Outlier Detection ==========================
  
Q1 <- quantile(D3H$value, 0.25, na.rm = TRUE)
Q3 <- quantile(D3H$value, 0.75, na.rm = TRUE)
IQR_D3H <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_D3H
upper_bound <- Q3 + 1.5 * IQR_D3H
outliers <- which(D3H$value < lower_bound | D3H$value > upper_bound)
  
# Boxplot visualization
p_box <- ggplot(D3H, aes(y = value)) +
    geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.size = 3) +
    labs(title = "Figure S1: Boxplot of Ketone Values",
         y = "Ketone (mmol/L)") +
    theme_minimal()
print(p_box)
  
# ========================== 4. Exploratory Analysis ==========================
  
# Individual trajectories
p1_D3H <- ggplot(D3H, aes(x = CHO, y = value, group = ID, color = ID)) +
    geom_point(size = 2) +
    geom_line(alpha = 0.5) +
    labs(title = "Figure 1: Individual Trajectories by Carbohydrate Intake",
         x = "Carbohydrate Intake (g/d)", 
         y = "Ketone (mmol/L)") +
    theme_minimal() +
    theme(legend.position = "none")
print(p1_D3H )
  
# ========================== 5. Model Building and Comparison ==========================
  
# Fit linear model
lm_linear <- lm(value ~ CHO, data = D3H)
  
# Fit nonlinear model (quadratic)
lm_quadratic <- lm(value ~ CHO + I(CHO^2), data = D3H) # I(CHO^2) adds quadratic term for CHO
  
# Method 1: Check model summary, focus on significance of quadratic term
summary(lm_quadratic)
# Check p-value for `I(CHO^2)` term.
# If p < 0.05, quadratic term is significant, supporting nonlinear relationship.
  
# Method 2: Likelihood Ratio Test
# For models fit with lm(), use anova()
anova(lm_linear, lm_quadratic)
# Check output p-value.
# If p < 0.05, models are significantly different, supporting nonlinear relationship.
  
# Method 3: Compare information criteria (AIC or BIC)
AIC(lm_linear, lm_quadratic)
BIC(lm_linear, lm_quadratic)
# Smaller AIC/BIC values indicate better model fit. Difference >2 suggests significant improvement.
  
  
# Model 1: Linear mixed model (CHO only)
model1 <- lmer(value ~ CHO_centered + (1 | ID), data = D3H)
summary(model1)
  
# Model 2: Quadratic polynomial + age + gender
model2 <- lmer(value ~ poly(CHO_centered, 2) + gender + age + (1 | ID), data = D3H)
summary(model2)
  
# Model 3: Full model (including other covariates)
model3 <- lmer(value ~ poly(CHO_centered, 2) + gender + age + BMI + Sport + Sleep + (1 | ID), 
                 data = D3H,
                 control = lmerControl(optimizer = "bobyqa"))
summary(model3)
# Add HOMA_IR as a covariate
model3_1 <- lmer(value ~ poly(CHO_centered, 2) + gender + age + BMI + HOMA_IR + Sport + Sleep + (1 | ID), 
                   data = D3H,
                   control = lmerControl(optimizer = "bobyqa"))
summary(model3_1)
# Add Est_VAT_mass as a covariate 
model3_2 <- lmer(value ~ poly(CHO_centered, 2) + gender + age + BMI + HOMA_IR + Est_VAT_mass + Sport + Sleep + (1 | ID), 
                   data = D3H,
                   control = lmerControl(optimizer = "bobyqa"))
summary(model3_2)
# Model comparison
  model_comparison1 <- anova(model1, model2, model3)
  model_comparison2 <- anova(model1, model2, model3_1)
  model_comparison3 <- anova(model1, model2, model3_2)
  
# Select best model
final_model <- model2

# To investigate the influence of lean body mass and energy expenditure on sex differences
adjusted_model <- lmer(value ~ poly(CHO_centered, 2) + gender + lean_body + EE + (1 | ID), data = D3H)
summary(adjusted_model )
  
# ========================== 6. Model Diagnostics ==========================
  
# Extract residuals
residuals <- resid(final_model)
fitted_vals <- fitted(final_model)
  
# Residual diagnostics
p2_D3H <- data.frame(Fitted = fitted_vals, Residuals = residuals)
  
p2_D3H_a <- ggplot(p2_D3H, aes(x = Fitted, y = Residuals)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = "Figure 2a: Residuals vs Fitted",
         x = "Fitted values", y = "Residuals") +
    theme_minimal()
print(p2_D3H_a)
  
p2_D3H_b <- ggplot(p2_D3H, aes(sample = Residuals)) +
    geom_qq() +
    geom_qq_line(color = "red") +
    labs(title = "Figure 2b: Q-Q Plot of Residuals",
         x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
print(p2_D3H_b)
  
# Residual normality test
shapiro_resid <- shapiro.test(residuals)
print(shapiro_resid)
  
# Check influential points
large_residuals <- which(abs(residuals) > 3 * sd(residuals))
print(large_residuals)
  
# Calculate R²
r2_values <- r.squaredGLMM(final_model)
cat("Marginal R²:", round(r2_values[1, "R2m"], 3), "\n")
cat("Conditional R²:", round(r2_values[1, "R2c"], 3), "\n")
  
# Model summary
print(summary(final_model))
  
# ========================== 7. Threshold Analysis in 20-30 Age Subgroup ==========================
  
D3H_young <- D3H[D3H$age >= 20 & D3H$age <= 30, ]
  
D3H_young$ID <- as.factor(D3H_young$ID)
D3H_young$gender <- factor(D3H_young$gender)
D3H_young$CHO_centered <- scale(D3H_young$CHO, center = TRUE, scale = FALSE)
  
final_model_young <- lmer(value ~ poly(CHO_centered, 2) + gender + age + (1 | ID), data = D3H_young)
summary(final_model_young)

# To investigate the influence of lean body mass and energy expenditure on sex differences in the young people
adjusted_model_young <- lmer(value ~ poly(CHO_centered, 2) + gender + lean_body + EE + (1 | ID), data = D3H_young)
summary(adjusted_model_young)
  
CHO_mean <- mean(D3H_young$CHO, na.rm = TRUE)
  
# Get gender levels
gender_levels <- levels(D3H_young$gender)
  if(is.null(gender_levels)) gender_levels <- unique(D3H_young$gender)
  cat("\nGender levels:", paste(gender_levels, collapse = ", "), "\n")
  
# Define prediction function (population-level predictions only)
  predict_fun <- function(model, newdata) {
    pred_value <- predict(model, newdata = newdata, re.form = NA)
    return(pred_value)
  }
  
# Define root-finding function (find CHO intake where blood ketone = 0.5)
# Note: age is fixed at its mean value for threshold calculation
solve_for_cho <- function(cho_value, target_value = 0.5, gender_val, age_val) {
    new_data <- data.frame(
      CHO_centered = cho_value - CHO_mean,
      gender = gender_val,
      age = age_val
    )
    predicted_value <- predict(final_model_young, newdata = new_data, re.form = NA)
    return(predicted_value - target_value)
  }
  
# Calculate threshold for each gender (using mean age)
mean_age <- mean(D3H_young$age, na.rm = TRUE)
  
solutions <- list()
  for(gender_val in gender_levels) {
    cho_range <- range(D3H_young$CHO, na.rm = TRUE)
    solution <- tryCatch({
      uniroot(function(x) solve_for_cho(x, 0.5, gender_val, mean_age), 
              interval = cho_range)$root
    }, error = function(e) {
      extended_range <- c(cho_range[1] - 50, cho_range[2] + 50)
      uniroot(function(x) solve_for_cho(x, 0.5, gender_val, mean_age), 
              interval = extended_range)$root
    })
    solutions[[as.character(gender_val)]] <- solution
  }
  threshold_results <- data.frame(
    gender = names(solutions),
    CHO_threshold_g_per_day = round(unlist(solutions), 1)
  )
print(threshold_results)
  
# ========================== 8. Bootstrap Confidence Intervals ==========================
  
set.seed(123)
  
threshold_CI_results <- do.call(rbind, lapply(gender_levels, function(g) {
    
  boot_results <- bootMer(final_model_young, FUN = function(m) {
      CHO_seq <- seq(min(D3H_young$CHO), max(D3H_young$CHO), length.out = 200)
      new_data <- data.frame(
        CHO_centered = CHO_seq - CHO_mean,
        gender = g,
        age = mean_age
      )
      pred_values <- predict(m, newdata = new_data, re.form = NA)
      CHO_seq[which.min(abs(pred_values - 0.5))]
    }, nsim = 1000)
    
  data.frame(
      gender = g,
      Threshold = boot_results$t0,
      CI_lower = quantile(boot_results$t, 0.025),
      CI_upper = quantile(boot_results$t, 0.975)
    )
  }))
  
print(threshold_CI_results)
  
# ========================== 9. Prepare Plotting Data ==========================
  
CHO_seq <- seq(min(D3H_young$CHO), max(D3H_young$CHO), length.out = 200)
  
plot_data_ci <- list()
  
for(gender_val in gender_levels) {
    new_data <- data.frame(
      CHO_centered = CHO_seq - CHO_mean,
      gender = gender_val,
      age = mean_age
    )
    
  # Point predictions
  pred_values <- predict(final_model_young, newdata = new_data, re.form = NA)
    
  # Bootstrap confidence bands
  boot_pred <- bootMer(final_model_young, FUN = function(m) {
      predict(m, newdata = new_data, re.form = NA)
    }, nsim = 1000)
    
    pred_matrix <- boot_pred$t
    lower_ci <- apply(pred_matrix, 2, quantile, 0.025, na.rm = TRUE)
    upper_ci <- apply(pred_matrix, 2, quantile, 0.975, na.rm = TRUE)
    
    plot_data_ci[[gender_val]] <- data.frame(
      CHO = CHO_seq,
      value_pred = pred_values,
      lower_ci = lower_ci,
      upper_ci = upper_ci,
      gender = gender_val
    )
  }
  
  plot_data_ci <- do.call(rbind, plot_data_ci)
  
# Threshold points data
solution_points <- data.frame(
    CHO = threshold_results$CHO_threshold_g_per_day,
    value = 0.5,
    gender = threshold_results$gender
  )
  
# Ensure gender columns are both character type for merging
solution_points$gender <- as.character(solution_points$gender)
threshold_CI_results$gender <- as.character(threshold_CI_results$gender)
  
# Merge data for labels
label_data <- merge(solution_points, threshold_CI_results, by = "gender")
  
# Round CHO threshold to 1 decimal in label_data
label_data$CHO <- round(label_data$CHO, 1)
label_data$CI_lower <- round(label_data$CI_lower, 1)
label_data$CI_upper <- round(label_data$CI_upper, 1)
  
# Plot
p3_D3H <- ggplot() +
    geom_ribbon(data = plot_data_ci, 
                aes(x = CHO, ymin = lower_ci, ymax = upper_ci, fill = gender),
                alpha = 0.2) +
    geom_point(data = D3H_young, 
               aes(x = CHO, y = value, color = gender), 
               alpha = 0.5, size = 2) +
    geom_line(data = plot_data_ci, 
              aes(x = CHO, y = value_pred, color = gender), 
              linewidth = 1.2) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "darkgrey", linewidth = 0.8) +
    geom_vline(data = solution_points, 
               aes(xintercept = CHO, color = gender),
               linetype = "dashed", linewidth = 0.8, alpha = 0.7) +
    geom_point(data = solution_points, 
               aes(x = CHO, y = value, color = gender), 
               size = 4, shape = 17) +
    geom_text(data = label_data, 
              aes(x = CHO + 5, y = 0.55, 
                  label = paste0(CHO, " g/d\n(95% CI: ", CI_lower, "-", CI_upper, ")"), 
                  color = gender),
              vjust = -0.5, size = 3.5, fontface = "bold") +
    labs(subtitle = paste("Age subgroup: 20-30 years | Age fixed at", round(mean_age, 1), "years"),
         x = "Carbohydrate Intake (g/d)", 
         y = "Venous Blood β-Hydroxybutyrate Concentration (mmol/L)",
         color = "Gender", fill = "Gender") +
    scale_color_manual(values = c("Male" = "#6FB7BC", "Female" = "#C1421E")) +
    scale_fill_manual(values = c("Male" = "#6FB7BC", "Female" = "#C1421E")) +
    theme_classic() +
    theme(plot.subtitle = element_text(hjust = 0.5, size = 10),
          legend.position = "top")
  
print(p3_D3H)
ggsave("young_subgroup_plot_with_CI.pdf", p3_D3H, width = 8, height = 6)
  
# ============================================================================
# Title: Association between Dietary Carbohydrate Intake and CKM-Monitored 
#        Ketone Concentrations in Healthy Chinese Adults by Gender
# Author: Yingying Li
# Date: 2026-04-20
# Environment: R version 4.5.2
# ============================================================================

# The same analytical pipeline was applied to the CKM-monitored ketone data.
# Due to the identical statistical approach, the code is omitted here for brevity.
# The CKM BHB analysis followed exactly the same steps as above.
# The only difference was the outcome variable (CKM-measured ketone instead of venous ketone).

# ============================================================================
# Title: Agreement Analysis between CKM Monitoring and Blood Biochemical Ketone Detection Methods
# Description: Considering within-subject repeated measurements, analyzing agreement between 
#              two ketone detection methods grouped by ID
# Author: Yingying Li
# Date: 2026-04-20
# Environment: R version 4.5.2
# ============================================================================

# Load packages
library(dplyr)
library(ggplot2)
library(irr)
library(BlandAltmanLeh)

# ========================1. Simulated data generation =========================

set.seed(20260420)

simulated_ba <- data.frame(
  ID = rep(1:22, each = 5),
  gender = rep(sample(1:2, 22, replace = TRUE), each = 5),
  CHO = rep(c(20, 40, 50, 70, 90), 22),
  value = round(runif(110, 0.2, 2.5), 2),
  D3H = round(runif(110, 0.2, 2.5), 2)
)

head(simulated_ba)

# ========================2. Aggregate by subject ==========================

summary_data <- simulated_ba %>%
  group_by(ID) %>%
  summarise(
    CKM_mean = mean(value, na.rm = TRUE),
    D3H_mean = mean(D3H, na.rm = TRUE),
    mean_diff = mean(value - D3H, na.rm = TRUE),
    mean_measurement = mean((value + D3H) / 2, na.rm = TRUE)
  )
head(summary_data)

# ========================3. Bland-Altman Analysis =========================

bias <- mean(summary_data$mean_diff)
sd_diff <- sd(summary_data$mean_diff)
lower_loa <- bias - 1.96 * sd_diff
upper_loa <- bias + 1.96 * sd_diff

# Bland-Altman plot
BA_plot <- ggplot(summary_data, aes(x = mean_measurement, y = mean_diff)) +
  geom_point(size = 3, alpha = 0.7, color = "grey60") +
  geom_hline(yintercept = bias, color = "#6FB7BC", linewidth = 1, linetype = "solid") +
  geom_hline(yintercept = lower_loa, color = "#C1421E", linewidth = 1, linetype = "dashed") +
  geom_hline(yintercept = upper_loa, color = "#C1421E", linewidth = 1, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5, linetype = "dotted") +
  labs(x = "Average β-Hydroxybutyrate Concentration (mmol/L)", 
       y = "Difference (CKM - D3H) (mmol/L)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank())

print(BA_plot)
ggsave("Bland_Altman_plot.pdf", BA_plot, width = 8, height = 6)

# ========================4. ICC Analysis =================================

icc_summary <- simulated_ba %>%
  group_by(ID) %>%
  summarise(
    CKM_mean = mean(value, na.rm = TRUE),
    D3H_mean = mean(D3H, na.rm = TRUE)
  )

icc_result <- icc(icc_summary[, c("CKM_mean", "D3H_mean")], 
                  model = "twoway", 
                  type = "agreement", 
                  unit = "single")

print(icc_result)

# ============================================================================
# Title: Analysis of Changes in Resting Energy Expenditure and Substrate Oxidation Rates 
#        Before and After a 16-Day Dietary Controlled Trial
# Author: Yingying Li
# Date: 2026-04-19
# Environment: R version 4.5.2
# ============================================================================

# Load required packages
library(ggplot2)
library(ggpubr)
library(effsize)
library(tidyr)
library(dplyr)

# ========================== 1. Simulated data generation ==========================

set.seed(20260419)

resting_metabolic_data <- data.frame(
  ID = 1:22,
  EE1 = round(rnorm(22, 34, 7), 2),
  CHO1 = round(rnorm(22, 10, 3.5), 2),
  FAT1 = round(rnorm(22, 1.4, 1), 2),
  EE2 = round(rnorm(22, 34, 7), 2),
  CHO2 = round(rnorm(22, 4.2, 1.8), 2),
  FAT2 = round(rnorm(22, 4.1, 1.7), 2)
)

resting_metabolic_data$EE1 <- pmax(resting_metabolic_data$EE1, 15)
resting_metabolic_data$EE2 <- pmax(resting_metabolic_data$EE2, 15)
resting_metabolic_data[, c("CHO1", "CHO2")] <- lapply(resting_metabolic_data[, c("CHO1", "CHO2")], pmax, 1)
resting_metabolic_data[, c("FAT1", "FAT2")] <- lapply(resting_metabolic_data[, c("FAT1", "FAT2")], pmax, 0.1)

# ========================== 2. Descriptive Statistics ==========================

desc_stats <- resting_metabolic_data %>%
  summarise(
    mean_EE1 = mean(EE1, na.rm = TRUE),
    sd_EE1 = sd(EE1, na.rm = TRUE),
    mean_EE2 = mean(EE2, na.rm = TRUE),
    sd_EE2 = sd(EE2, na.rm = TRUE),
    mean_CHO1 = mean(CHO1, na.rm = TRUE),
    sd_CHO1 = sd(CHO1, na.rm = TRUE),
    mean_CHO2 = mean(CHO2, na.rm = TRUE),
    sd_CHO2 = sd(CHO2, na.rm = TRUE),
    mean_FAT1 = mean(FAT1, na.rm = TRUE),
    sd_FAT1 = sd(FAT1, na.rm = TRUE),
    mean_FAT2 = mean(FAT2, na.rm = TRUE),
    sd_FAT2 = sd(FAT2, na.rm = TRUE)
  )
print(desc_stats)

# ========================== 3. Outlier Detection ==========================

# 3.1 Outlier detection for EE change
resting_metabolic_data$EE_Change <- resting_metabolic_data$EE2 - resting_metabolic_data$EE1
# Standard deviation method (±3SD criterion)
mean_EE <- mean(resting_metabolic_data$EE_Change, na.rm = TRUE)
sd_EE <- sd(resting_metabolic_data$EE_Change, na.rm = TRUE)
resting_metabolic_data$Outlier_3SD_EE <- abs(resting_metabolic_data$EE_Change - mean_EE) > 3 * sd_EE
# IQR method (boxplot principle)
Q_EE <- quantile(resting_metabolic_data$EE_Change, probs = c(0.25, 0.75), na.rm = TRUE)
IQR_EE <- Q_EE[2] - Q_EE[1]
resting_metabolic_data$Outlier_IQR_EE <- resting_metabolic_data$EE_Change < (Q_EE[1] - 1.5 * IQR_EE) | 
  resting_metabolic_data$EE_Change > (Q_EE[2] + 1.5 * IQR_EE)

# 3.2 Outlier detection for CHO change
resting_metabolic_data$CHO_Change <- resting_metabolic_data$CHO2 - resting_metabolic_data$CHO1
# Standard deviation method (±3SD criterion)
mean_CHO <- mean(resting_metabolic_data$CHO_Change, na.rm = TRUE)
sd_CHO <- sd(resting_metabolic_data$CHO_Change, na.rm = TRUE)
resting_metabolic_data$Outlier_3SD_CHO <- abs(resting_metabolic_data$CHO_Change - mean_CHO) > 3 * sd_CHO
# IQR method (boxplot principle)
Q_CHO <- quantile(resting_metabolic_data$CHO_Change, probs = c(0.25, 0.75), na.rm = TRUE)
IQR_CHO <- Q_CHO[2] - Q_CHO[1]
resting_metabolic_data$Outlier_IQR_CHO <- resting_metabolic_data$CHO_Change < (Q_CHO[1] - 1.5 * IQR_CHO) | 
  resting_metabolic_data$CHO_Change > (Q_CHO[2] + 1.5 * IQR_CHO)

# 3.3 Outlier detection for FAT change
resting_metabolic_data$FAT_Change <- resting_metabolic_data$FAT2 - resting_metabolic_data$FAT1
# Standard deviation method (±3SD criterion)
mean_FAT <- mean(resting_metabolic_data$FAT_Change, na.rm = TRUE)
sd_FAT <- sd(resting_metabolic_data$FAT_Change, na.rm = TRUE)
resting_metabolic_data$Outlier_3SD_FAT <- abs(resting_metabolic_data$FAT_Change - mean_FAT) > 3 * sd_FAT
# IQR method (boxplot principle)
Q_FAT <- quantile(resting_metabolic_data$FAT_Change, probs = c(0.25, 0.75), na.rm = TRUE)
IQR_FAT <- Q_FAT[2] - Q_FAT[1]
resting_metabolic_data$Outlier_IQR_FAT <- resting_metabolic_data$FAT_Change < (Q_FAT[1] - 1.5 * IQR_FAT) | 
  resting_metabolic_data$FAT_Change > (Q_FAT[2] + 1.5 * IQR_FAT)

# ========================== 4. Normality Tests ==========================
# EE change
# Histogram
hist(resting_metabolic_data$EE_Change, 
     main = "Histogram of Resting Energy Expenditure Changes (kcal/h)",
     xlab = "Resting Energy Expenditure Changes (kcal/h)",
     col = "lightblue",
     prob = TRUE)
lines(density(resting_metabolic_data$EE_Change, na.rm = TRUE), col = "red", lwd = 2)
curve(dnorm(x, mean = mean(resting_metabolic_data$EE_Change, na.rm = TRUE), 
            sd = sd(resting_metabolic_data$EE_Change, na.rm = TRUE)), 
      add = TRUE, col = "blue", lwd = 2)
legend("topright", legend = c("Density", "Normal"), 
       col = c("red", "blue"), lty = 1)

# Q-Q plot
qqnorm(resting_metabolic_data$EE_Change, main = "Q-Q Plot for Resting Energy Expenditure Changes")
qqline(resting_metabolic_data$EE_Change, col = "red")

# Shapiro-Wilk tests
shapiro_test_EE <- shapiro.test(resting_metabolic_data$EE_Change)
print(shapiro_test_EE)

# CHO change 
# Histogram
hist(resting_metabolic_data$CHO_Change, 
     main = "Histogram of CHO Oxidation Rate Differences",
     xlab = "CHO Oxidation Rate Change (g/h)",
     col = "lightblue",
     prob = TRUE)
lines(density(resting_metabolic_data$CHO_Change, na.rm = TRUE), col = "red", lwd = 2)
curve(dnorm(x, mean = mean(resting_metabolic_data$CHO_Change, na.rm = TRUE), 
            sd = sd(resting_metabolic_data$CHO_Change, na.rm = TRUE)), 
      add = TRUE, col = "blue", lwd = 2)
legend("topright", legend = c("Density", "Normal"), 
       col = c("red", "blue"), lty = 1)

# Q-Q plot
qqnorm(resting_metabolic_data$CHO_Change, main = "Q-Q Plot for CHO Oxidation Rate Differences")
qqline(resting_metabolic_data$CHO_Change, col = "red")

# Shapiro-Wilk tests
shapiro_test_CHO <- shapiro.test(resting_metabolic_data$CHO_Change)
print(shapiro_test_CHO)

# FAT change 
# Histogram
hist(resting_metabolic_data$FAT_Change, 
     main = "Histogram of Fat Oxidation Rate Differences",
     xlab = "Fat Oxidation Rate Change (g/h)",
     col = "lightblue",
     prob = TRUE)
lines(density(resting_metabolic_data$FAT_Change, na.rm = TRUE), col = "red", lwd = 2)
curve(dnorm(x, mean = mean(resting_metabolic_data$FAT_Change, na.rm = TRUE), 
            sd = sd(resting_metabolic_data$FAT_Change, na.rm = TRUE)), 
      add = TRUE, col = "blue", lwd = 2)
legend("topright", legend = c("Density", "Normal"), 
       col = c("red", "blue"), lty = 1)

# Q-Q plot
qqnorm(resting_metabolic_data$FAT_Change, main = "Q-Q Plot for Fat Oxidation Rate Differences")
qqline(resting_metabolic_data$FAT_Change, col = "red")

# Shapiro-Wilk tests
shapiro_test_FAT <- shapiro.test(resting_metabolic_data$FAT_Change)
print(shapiro_test_FAT)

# ========================== 5. Paired t-tests ==========================

# 5.1 Paired t-test for energy expenditure
EE_ttest <- t.test(resting_metabolic_data$EE1, resting_metabolic_data$EE2, paired = TRUE)
print(EE_ttest)

# 5.2 Paired t-test for carbohydrate oxidation rate
CHO_ttest <- t.test(resting_metabolic_data$CHO1, resting_metabolic_data$CHO2, paired = TRUE)
print(CHO_ttest)

# 5.3 Paired t-test for fat oxidation rate
FAT_ttest <- t.test(resting_metabolic_data$FAT1, resting_metabolic_data$FAT2, paired = TRUE)
print(FAT_ttest)

# ========================== 6. Effect Size Calculation ==========================

EE_cohen <- cohen.d(resting_metabolic_data$EE2, resting_metabolic_data$EE1, paired = TRUE)
CHO_cohen <- cohen.d(resting_metabolic_data$CHO2, resting_metabolic_data$CHO1, paired = TRUE)
FAT_cohen <- cohen.d(resting_metabolic_data$FAT2, resting_metabolic_data$FAT1, paired = TRUE)

print(EE_cohen)
print(CHO_cohen)
print(FAT_cohen)

# ========================== 7. Data Visualization Preparation ==========================

# Convert data to long format
long_data <- resting_metabolic_data %>%
  select(ID, EE1, EE2, CHO1, CHO2, FAT1, FAT2) %>%
  pivot_longer(
    cols = -ID,
    names_to = c("Measure", "Time"),
    names_pattern = "(.*)([0-9]+)"  # Split between letters and numbers
  ) %>%
  mutate(Time = ifelse(Time == "1", "Pre", "Post")) %>%
  pivot_wider(
    names_from = Measure,
    values_from = value
  ) %>%
  mutate(Time = factor(Time, levels = c("Pre", "Post")))

# Check conversion results
head(long_data)

# ========================== 8. Visualization ==========================

# 8.1 Energy expenditure change plot
EE_plot <- ggplot(long_data, aes(x = Time, y = EE, group = ID)) +
  geom_line(color = "gray", alpha = 0.6) +
  geom_point(aes(color = Time), size = 2) +
  stat_summary(fun = mean, geom = "line", group = 1, color = "red2", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", group = 1, color = "red2", size = 3) +
  scale_color_manual(values = c("Pre" = "#C1421E", "Post" = "#6FB7BC")) +
  labs(title = "Pre- vs. Post-Intervention Energy Expenditure",
       subtitle = paste0("Paired t-test: p = ", format(EE_ttest$p.value, digits = 3)),
       y = "Energy Expenditure (kcal/h)") +
  theme_minimal() +
  theme(legend.position = "none")
print(EE_plot)

# 8.2 Carbohydrate oxidation rate change plot
CHO_plot <- ggplot(long_data, aes(x = Time, y = CHO, group = ID)) +
  geom_line(color = "gray", alpha = 0.6) +
  geom_point(aes(color = Time), size = 2) +
  stat_summary(fun = mean, geom = "line", group = 1, color = "red2", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", group = 1, color = "red2", size = 3) +
  scale_color_manual(values = c("Pre" = "#C1421E", "Post" = "#6FB7BC")) +
  labs(title = "Pre- vs. Post-Intervention Carbohydrate Oxidation",
       subtitle = paste0("Paired t-test: p = ", format(CHO_ttest$p.value, digits = 3)),
       y = "Carbohydrate Oxidation Rate (g/h)") +
  theme_minimal() +
  theme(legend.position = "none")
print(CHO_plot)

# 8.3 Fat oxidation rate change plot
FAT_plot <- ggplot(long_data, aes(x = Time, y = FAT, group = ID)) +
  geom_line(color = "gray", alpha = 0.6) +
  geom_point(aes(color = Time), size = 2) +
  stat_summary(fun = mean, geom = "line", group = 1, color = "blue2", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", group = 1, color = "blue2", size = 3) +
  scale_color_manual(values = c("Pre" = "#C1421E", "Post" = "#6FB7BC")) +
  labs(title = "Pre- vs. Post-Intervention Fat Oxidation",
       subtitle = paste0("Paired t-test: p = ", format(FAT_ttest$p.value, digits = 3)),
       y = "Fat Oxidation Rate (g/h)") +
  theme_minimal() +
  theme(legend.position = "none")
print(FAT_plot)

# 8.4 Combined plot (CHO and FAT side by side)
combined_plot <- ggarrange(CHO_plot, FAT_plot, ncol = 2)
print(combined_plot)
ggsave("Figure_combined_oxidation.pdf", combined_plot, width = 12, height = 5)

