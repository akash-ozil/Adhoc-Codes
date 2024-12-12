calculate_likelihood <- function(data, target_var, group_var) {
  # Required libraries
  require(dplyr)
  
  # Calculate frequencies and percentages for each group
  result <- data %>%
    # Convert group_var to character if it's not already
    mutate(!!sym(group_var) := as.character(!!sym(group_var))) %>%
    # Group by the variable of interest
    group_by(!!sym(group_var)) %>%
    # Calculate frequencies for 1/0 in target variable
    summarise(
      yes_count = sum(!!sym(target_var) == 1),
      no_count = sum(!!sym(target_var) == 0),
      .groups = 'drop'
    ) %>%
    # Calculate total counts
    mutate(
      total_yes = sum(yes_count),
      total_no = sum(no_count)
    ) %>%
    # Calculate percentages and likelihood ratio
    mutate(
      yes_percent = round(yes_count/total_yes * 100, 1),
      no_percent = round(no_count/total_no * 100, 1),
      likelihood_ratio = round((yes_count/total_yes)/(no_count/total_no), 2)
    ) %>%
    # Format the output
    mutate(
      Yes = paste0(yes_count, " (", yes_percent, ")"),
      No = paste0(no_count, " (", no_percent, ")"),
      `Likelihood ratio` = likelihood_ratio
    ) %>%
    # Select and arrange final columns
    select(!!sym(group_var), Yes, No, `Likelihood ratio`) %>%
    arrange(desc(likelihood_ratio))
  
  return(result)
}

# Example usage:
# Assuming your data is in a data frame called 'df' with columns:
# - smoking_habit: categories like "≥40", "20-40", "0-20", "Never smoked or smoked for <1 yr"
# - airway_disease: 1 or 0

# example_data <- data.frame(
#   smoking_habit = c("≥40", "20-40", "0-20", "Never smoked"),
#   airway_disease = c(1, 0, 1, 0)
# )
# 
# result <- calculate_likelihood(
#   data = example_data,
#   target_var = "airway_disease",
#   group_var = "smoking_habit"
# )
