#  Load packages
library(tidyverse)
library(lubridate)

# QUESTION 1: 
# Open the workspace icums_22_sample.RData. You need to download this file and 
# put it in the project folder.
{
  # Load ICUMS 2022 data and HS data
  load("data/icums_22_sample.RData")
}

# QUESTION 2: 
# Inspect the data, what does this data set show? What can this data be used for?
{
  View(icums_22_sample)
  str(icums_22_sample)
  head(icums_22_sample, 10)
  tail(icums_22_sample, 10)
  summarise(icums_22_sample)
}

# QUESTION 3: 
# Select a specific subset of the data, for example only the columns: tradeflow, 
# month_name, hs_code, and cif
{
  # Method 1
  trade_data_subset_1 <- icums_22_sample[, c("tradeflow", "month_name", "hs_code", "cif")]
}

# QUESTION 4:
# Now repeat question 3, but use a different method to do it. Which method do you 
# think is better to use, and why? Bonus: would you be able to find a third method to do this?
{
  # Method 2
  trade_data_subset_2 <- subset(icums_22_sample, select = c(tradeflow, month_name, hs_code, cif))
  # Method 3
  trade_data_subset_3 <- select(icums_22_sample, c(tradeflow, month_name, hs_code, cif))
  # Method 4
  trade_data_subset_4 <- icums_22_sample %>% 
    select(tradeflow, month_name, hs_code, cif)
}

# QUESTION 5:
# Select only the columns in which the values are numeric
{
trade_data_numeric <- icums_22_sample %>% 
  select(where(is.numeric))
}

# QUESTION 6:
# Get more information on the data by creating a table of how many observations there are for each month.
{
  trade_data_obs_per_month <- icums_22_sample %>% 
    group_by(month_name) %>% 
    summarise(count = n())
}

# QUESTION 7:
# Use the function “aggregate” to find the average imports for every month.
{
  trade_data_avg_imports_per_month_1 <- aggregate(icums_22_sample, cif ~ month_name, sum)
}

# QUESTION 8:
# Repeat question 7, now using a pipe.
{
  trade_data_avg_imports_per_month_2 <- icums_22_sample %>% 
    group_by(month_name) %>% 
    summarise(avg_imports = mean(cif, na.rm = TRUE))
}

# QUESTION 9:
# Add a new column “quarter” which shows which quarter of the year every observation is from.
{
  trade_data_add_quarter_1 <- icums_22_sample %>% 
    mutate(quarter = quarter(month))
}

# QUESTION 9:
# Load the new dataframe hs_codes.RData into R, and merge this to the icums_22_sample data. 
# Carefully decide on which method you use for this.
{
  load("data/hs_codes.RData")
  # Method 1
  merged_data_1 <- merge(icums_22_sample, hs_codes, by = "hs_code")
  # Method 2
  merged_data_2 <- merge(icums_22_sample, hs_codes, by = "hs_code", all.x = TRUE)
  # Method 3
  merged_data_3 <- merge(hs_codes, icums_22_sample, by = "hs_code", all.y = TRUE)
  # Method 4
  merged_data_4 <- icums_22_sample %>% 
    merge(hs_codes, by = "hs_code")
  # Method 5
  merged_data_5 <- left_join(icums_22_sample, hs_codes, by = "hs_code")
  # Method 6
  merged_data_6 <- icums_22_sample %>% 
    left_join(hs_codes, by = "hs_code")
}

