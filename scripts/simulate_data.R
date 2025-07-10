# Load required libraries
library(dplyr)
library(cbsodataR)
library(tidyr)
library(fastDummies)

# =====================================
# SIMULATION PARAMETERS
# =====================================

# Sample size
n <- 10000

# Random seed for reproducibility
random_seed <- 1234567

# Target variable parameters
target_intercept <- -5
target_noise_sd <- 1.25
target_coef_scale <- 0.15

# Feature coefficients for target variable Y
target_coefficients <- c(
  x1 = 0.8,   # Strong positive effect
  x2 = 0.6,   # Moderate positive effect  
  x3 = -0.4,  # Moderate negative effect
  x4 = 0.8,   # Strong positive effect
  x5 = 0.05,  # Weak positive effect
  x6 = 0.3,   # Moderate positive effect
  x7 = 0.2,   # Weak positive effect
  x8 = 0.1,   # Weak positive effect
  x9 = 0.03,  # Very weak positive effect
  x10 = 0.4   # Moderate positive effect
)

# Feature generation parameters
feature_params <- list(
  # Binary features
  x1 = list(base_prob = 0.3, gender_effect = 0.2, nationality_effect = 0.1),
  x2 = list(base_prob = 0.4, age_effect = 0.15, age_groups = c("25 tot 30 jaar", "30 tot 35 jaar", "35 tot 40 jaar")),
  x3 = list(base_prob = 0.5, nationality_effect = -0.2),
  x4 = list(base_prob = 0.35, gender_effect = 0.1, age_effect = -0.05, age_groups = c("65 tot 70 jaar", "70 tot 75 jaar")),
  
  # Continuous features
  x5 = list(base_mean = 10, gender_effect = 2, nationality_effect = -1.5, sd = 3),
  x6 = list(base_mean = 50, age_effect = 10, age_groups = c("45 tot 50 jaar", "50 tot 55 jaar", "55 tot 60 jaar", "60 tot 65 jaar", "65 tot 70 jaar", "70 tot 75 jaar"), sd = 15),
  x7 = list(base_mean = 20, nationality_effect = -5, gender_effect = 2, sd = 8),
  x8 = list(base_mean = 5, age_effect = 3, age_groups = c("15 tot 20 jaar", "20 tot 25 jaar"), sd = 2),
  
  # Count features  
  x9 = list(base_lambda = 2, gender_effect = 1.5, nationality_effect = 0.8),
  x10 = list(base_lambda = 1.5, age_effect = 0.5, age_groups = c("30 tot 35 jaar", "35 tot 40 jaar", "40 tot 45 jaar"))
)

# Set seed for reproducibility
set.seed(random_seed)

# Fetch and prepare CBS demographic data
cbs_data <- cbs_get_data('03743')
data <- cbs_data |>
  cbs_add_label_columns() |>
  filter(
    Perioden_label == "2023", # only look at year 2023
    Nationaliteiten_label %in% c("Nederlands", "Totaal niet-Nederlandse nationaliteit"),
    Leeftijd_label %in% c(
      "15 tot 20 jaar", "20 tot 25 jaar", "25 tot 30 jaar", "30 tot 35 jaar",
      "35 tot 40 jaar", "40 tot 45 jaar", "45 tot 50 jaar", "50 tot 55 jaar",
      "55 tot 60 jaar", "60 tot 65 jaar", "65 tot 70 jaar", "70 tot 75 jaar"
    ),
    Geslacht_label %in% c("Mannen", "Vrouwen")
  ) |>
  select(Nationaliteiten_label, Geslacht_label, Leeftijd_label, BevolkingOp1Januari_1) |>
  mutate(
    prob = BevolkingOp1Januari_1 / sum(BevolkingOp1Januari_1),
    combination = paste(Nationaliteiten_label, Geslacht_label, Leeftijd_label, sep = ";")
  ) 

# Generate simulated demographic data from aggregated CBS data
simulated_data <- tibble(
  combination = sample(data$combination, size = n, replace = TRUE, prob = data$prob)
) |>
  separate(
    combination,
    into = c("Nationaliteiten_label", "Geslacht_label", "Leeftijd_label"),
    sep = ";"
  ) |>
  fastDummies::dummy_cols()

# Generate X1-X10 features correlated with demographics using parameters
simulated_data <- simulated_data |>
  mutate(
    # Binary features
    x1 = rbinom(n, 1, feature_params$x1$base_prob + 
                feature_params$x1$gender_effect * Geslacht_label_Mannen + 
                feature_params$x1$nationality_effect * Nationaliteiten_label_Nederlands),
    
    x2 = rbinom(n, 1, feature_params$x2$base_prob + 
                feature_params$x2$age_effect * ifelse(Leeftijd_label %in% feature_params$x2$age_groups, 1, 0)),
    
    x3 = rbinom(n, 1, feature_params$x3$base_prob + 
                feature_params$x3$nationality_effect * Nationaliteiten_label_Nederlands),
    
    x4 = rbinom(n, 1, feature_params$x4$base_prob + 
                feature_params$x4$gender_effect * Geslacht_label_Mannen + 
                feature_params$x4$age_effect * ifelse(Leeftijd_label %in% feature_params$x4$age_groups, 1, 0)),
    
    # Continuous features
    x5 = rnorm(n, mean = feature_params$x5$base_mean + 
               feature_params$x5$gender_effect * Geslacht_label_Mannen + 
               feature_params$x5$nationality_effect * Nationaliteiten_label_Nederlands, 
               sd = feature_params$x5$sd),
    
    x6 = rnorm(n, mean = feature_params$x6$base_mean + 
               feature_params$x6$age_effect * ifelse(Leeftijd_label %in% feature_params$x6$age_groups, 1, 0), 
               sd = feature_params$x6$sd),
    
    x7 = rnorm(n, mean = feature_params$x7$base_mean + 
               feature_params$x7$nationality_effect * Nationaliteiten_label_Nederlands + 
               feature_params$x7$gender_effect * Geslacht_label_Mannen, 
               sd = feature_params$x7$sd),
    
    x8 = rnorm(n, mean = feature_params$x8$base_mean + 
               feature_params$x8$age_effect * ifelse(Leeftijd_label %in% feature_params$x8$age_groups, 1, 0), 
               sd = feature_params$x8$sd),
    
    # Count features
    x9 = rpois(n, lambda = feature_params$x9$base_lambda + 
               feature_params$x9$gender_effect * Geslacht_label_Mannen + 
               feature_params$x9$nationality_effect * Nationaliteiten_label_Nederlands),
    
    x10 = rpois(n, lambda = feature_params$x10$base_lambda + 
                feature_params$x10$age_effect * ifelse(Leeftijd_label %in% feature_params$x10$age_groups, 1, 0)),
    
    # Target variable Y with noise using parameters
    noise = rnorm(n, 0, target_noise_sd),
    linear_combination = target_intercept + target_coef_scale * (
      target_coefficients["x1"] * x1 + 
      target_coefficients["x2"] * x2 + 
      target_coefficients["x3"] * x3 + 
      target_coefficients["x4"] * x4 + 
      target_coefficients["x5"] * x5 + 
      target_coefficients["x6"] * x6 + 
      target_coefficients["x7"] * x7 + 
      target_coefficients["x8"] * x8 + 
      target_coefficients["x9"] * x9 + 
      target_coefficients["x10"] * x10
    ) + noise,
    y = rbinom(n, 1, plogis(linear_combination))
  )


# Fit logistic regression model to predict Y using X1-X10
model <- glm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, 
             family = "binomial", data = simulated_data)
summary(model)

# Model performance summary
cat("Positive cases:", sum(simulated_data$y), "out of", nrow(simulated_data), "\n")
cat("Proportion of positive cases:", mean(simulated_data$y), "\n")

# Export dataset to CSV (exclude intermediate variables)
export_data <- simulated_data |>
  select(-noise, -linear_combination)

write.csv(export_data, "simulated_data.csv", row.names = FALSE)
cat("Dataset exported to simulated_data.csv\n")

