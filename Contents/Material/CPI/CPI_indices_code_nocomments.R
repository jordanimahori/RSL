### Libraries
library(readxl)    # needed to load in excel data
library(tidyverse) # needed for data wrangling  
library(lubridate) # needed to work with dates in R
library(glue)      # needed to work with strings in R
library(diffdf)    # to compare different dataframes


### Name input files 
input_file <- "CPI November 2022.xlsx"

### Name weight file
path_weights_file_new <- "item_weight.xlsx"

### Name output file
target_file <- "Indices_Jan2021_toNov2022.xlsx"

## other inputs
start_ref_period <- "2021-01-01"
end_ref_period <- "2021-12-01"

## Previous indices to make correct chain link
previous_indices <- "Indices_Jan2021_toOct2022.xlsx"

# Load CPI data into R
# we do not want to see the warnings
raw_CPI_Data <- suppressWarnings(read_excel(input_file))



new_CPI_data <- raw_CPI_Data %>%
  select(-COICOP6 ) %>%
  mutate(COICOP6 = COICOP06 )

#inspect problem cases
new_CPI_data %>% 
  filter(is.na(COICOP6)) %>% 
  nrow


new_CPI_data %>%
  group_by(COICOP6) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

new_CPI_data %>% 
  summarise(my_var = n_distinct(COICOP6))

# Weights - new file
  # Load weights data into R
  Finalweight_new  <- read_excel(path = path_weights_file_new, sheet = 1)
  
  # Match region codes to region letters
  region_letters <- data.frame(region_code = c(1:16), 
                               region_letters = c("W", "C", "G", "V", "E",
                                                  "As", "WN", "Ah", "B", "BE",
                                                  "OT", "N", "S", "NE", "UE",
                                                  "UW"))

  # Reshape the weights file
  weights_long_format <- Finalweight_new %>%
    rename(value = weight,
           region_code = region1,
           urban_rural = locality) %>%
    left_join(region_letters, by = "region_code") %>%
    select( coicop02:cc6nom,region_letters, region_code, urban_rural, value)



# check if weights and prices match
count(new_CPI_data, COICOP6 ) %>% 
  full_join(count(Finalweight_new, coicop06),
            by = c("COICOP6" = "coicop06")) %>% 
  filter(is.na(n.x) | is.na(n.y))


# make sure IDs are unique
new_CPI_data %>% 
  count(Uniques_ID) %>% 
  filter(n != 1) %>% 
  arrange(desc(n))

# Now generate indices
Indices <- new_CPI_data %>%
  pivot_longer(cols = c(matches("^PR_"), matches("^QTY_")), 
               names_to = c('.value', 'date'), 
               names_sep = "_") %>%
  mutate(date = dmy(paste0("01",date))) %>%
  mutate(unit_price = PR / QTY) %>%
  arrange(Uniques_ID, date) %>%
  group_by(Uniques_ID) %>%
  mutate(rel_price_dif = unit_price / lag(unit_price, 1)) %>%
  mutate(rel_price_dif = ifelse(is.na(rel_price_dif), 1, rel_price_dif))  %>%
  group_by(COICOP6, date, Region_Code, Urban_rural)  %>%
  summarise(prod_pr_diff = prod(rel_price_dif),
            n_prices     = n(),
            index_diff   = prod_pr_diff^(1/n_prices), .groups = 'keep') %>%
  mutate(index_diff = ifelse(is.na(index_diff), 1, index_diff)) %>%
  ungroup() %>%
  group_by(COICOP6, Region_Code, Urban_rural) %>%
  mutate(index = cumprod(index_diff) * 100) %>%
  ungroup() %>%
  inner_join(weights_long_format, by = c("COICOP6" = "coicop06", 
                                         "Region_Code" = "region_code",
                                         "Urban_rural" = "urban_rural"))  %>%
  arrange(Region_Code, Urban_rural, COICOP6, date) %>% 
  rename(weight = value) %>%
  mutate(coicop02 = substr(COICOP6, 1, 2 )) %>%
  mutate(coicop03 = substr(COICOP6, 1, 4 )) %>%
  mutate(coicop04 = substr(COICOP6, 1, 6 )) %>%
  mutate(coicop05 = substr(COICOP6, 1, 8 )) %>% 
  mutate("food_non_food" = ifelse(coicop02 == "01", "food", "non-food"))

new_CPI_data %>% 
  count(Imp_Local)

Indices_import <- new_CPI_data%>%
  filter(Imp_Local %in% c("F_Imp","F_imp","Nf_imp" ,"Nf_Imp","NF_imp" ,"NF_Imp")) %>%
  pivot_longer(cols = c(matches("^PR_"), matches("^QTY_")), 
               names_to = c('.value', 'date'), 
               names_sep = "_") %>%
  mutate(date = dmy(paste0("01",date))) %>%
  mutate(unit_price = PR / QTY) %>%
  arrange(Uniques_ID, date) %>%
  group_by(Uniques_ID) %>%
  mutate(rel_price_dif = unit_price / lag(unit_price, 1)) %>%
  mutate(rel_price_dif = ifelse(is.na(rel_price_dif), 1, rel_price_dif))  %>%
  group_by(COICOP6 , date, Region_Code, Urban_rural)  %>%
  summarise(prod_pr_diff = prod(rel_price_dif),
            n_prices = n(),
            index_diff = prod_pr_diff^(1/n_prices), .groups = 'keep') %>%
  mutate(index_diff = ifelse(is.na(index_diff), 1, index_diff)) %>%
  ungroup() %>%
  group_by(COICOP6, Region_Code, Urban_rural) %>%
  mutate(index = cumprod(index_diff) * 100) %>%
  ungroup() %>%
  inner_join(weights_long_format, by = c("COICOP6" = "coicop06", 
                                         "Region_Code" = "region_code",
                                         "Urban_rural" = "urban_rural"))  %>%
  arrange(Region_Code, Urban_rural, COICOP6, date) %>% 
  rename(weight = value) %>%
  mutate(coicop02 = substr(COICOP6, 1, 2 )) %>%
  mutate(coicop03 = substr(COICOP6, 1, 4 )) %>%
  mutate(coicop04 = substr(COICOP6, 1, 6 )) %>%
  mutate(coicop05 = substr(COICOP6, 1, 8 )) %>% 
  mutate("food_non_food" = ifelse(coicop02 == "01", "food", "non-food"))


  
  
Indices_local <- new_CPI_data%>%
  filter(Imp_Local %in% c("F_Local", "Nf_Local", "NF_Local")) %>%
  pivot_longer(cols = c(matches("^PR_"), matches("^QTY_")), 
               names_to = c('.value', 'date'), 
               names_sep = "_") %>%
  mutate(date = dmy(paste0("01",date))) %>%
  mutate(unit_price = PR / QTY) %>%
  arrange(Uniques_ID, date) %>%
  group_by(Uniques_ID) %>%
  mutate(rel_price_dif = unit_price / lag(unit_price, 1)) %>%
  mutate(rel_price_dif = ifelse(is.na(rel_price_dif), 1, rel_price_dif))  %>%
  group_by(COICOP6 , date, Region_Code, Urban_rural)  %>%
  summarise(prod_pr_diff = prod(rel_price_dif),
            n_prices = n(),
            index_diff = prod_pr_diff^(1/n_prices), .groups = 'keep') %>%
  mutate(index_diff = ifelse(is.na(index_diff), 1, index_diff)) %>%
  ungroup() %>%
  group_by(COICOP6, Region_Code, Urban_rural) %>%
  mutate(index = cumprod(index_diff) * 100) %>%
  ungroup() %>%
  inner_join(weights_long_format, by = c("COICOP6" = "coicop06", 
                                         "Region_Code" = "region_code",
                                         "Urban_rural" = "urban_rural"))  %>%
  arrange(Region_Code, Urban_rural, COICOP6, date) %>% 
  rename(weight = value) %>%
  mutate(coicop02 = substr(COICOP6, 1, 2 )) %>%
  mutate(coicop03 = substr(COICOP6, 1, 4 )) %>%
  mutate(coicop04 = substr(COICOP6, 1, 6 )) %>%
  mutate(coicop05 = substr(COICOP6, 1, 8 )) %>% 
  mutate("food_non_food" = ifelse(coicop02 == "01", "food", "non-food"))


computation_function <- function(Indices_data = Indices,
                                 dissagregation,
                                 start_ref_period =  start_ref_period,
                                 end_ref_period = end_ref_period){
  res <- Indices_data %>%
    group_by(date, !!! rlang::syms(dissagregation)) %>%
    mutate(sum_weights = sum (weight, na.rm = TRUE)) %>%
    summarise(CPI = sum((index * weight)/ sum_weights, na.rm = TRUE), .groups = 'keep') %>%
    ungroup() %>%
    group_by( !!! rlang::syms(dissagregation)) %>%
    mutate(ref_period_CPI = mean(CPI[date >= ymd(start_ref_period) & date <= ymd(end_ref_period)])) %>%
    mutate(CPI = CPI/ref_period_CPI * 100) %>%
    select(-ref_period_CPI)%>%
    ungroup()
  
  res
  
}


National_Overall_CPI <- computation_function(Indices_data = Indices,
                                             dissagregation = "",
                                             start_ref_period =  start_ref_period,
                                             end_ref_period = end_ref_period) %>% 
  mutate(date = glue("CPI_{year(date)}_{str_pad(month(date),2, 'left', '0') }")) %>% 
  pivot_wider(values_from = CPI, names_from = date)

National_CC2 <- computation_function(Indices_data = Indices,
                                     dissagregation = "coicop02",
                                     start_ref_period =  start_ref_period,
                                     end_ref_period = end_ref_period) %>% 
  mutate(date = glue("CPI_{year(date)}_{str_pad(month(date),2, 'left', '0') }")) %>% 
  pivot_wider(values_from = CPI, names_from = date)


National_CC3 <- computation_function(Indices_data = Indices,
                                     dissagregation = "coicop03",
                                     start_ref_period =  start_ref_period,
                                     end_ref_period = end_ref_period)%>% 
  mutate(date = glue("CPI_{year(date)}_{str_pad(month(date),2, 'left', '0') }")) %>% 
  pivot_wider(values_from = CPI, names_from = date)

National_CC4 <- computation_function(Indices_data = Indices,
                                     dissagregation = "coicop04",
                                     start_ref_period =  start_ref_period,
                                     end_ref_period = end_ref_period)%>% 
  mutate(date = glue("CPI_{year(date)}_{str_pad(month(date),2, 'left', '0') }")) %>% 
  pivot_wider(values_from = CPI, names_from = date)

National_CC5 <- computation_function(Indices_data = Indices,
                                     dissagregation = "coicop05",
                                     start_ref_period =  start_ref_period,
                                     end_ref_period = end_ref_period) %>% 
  mutate(date = glue("CPI_{year(date)}_{str_pad(month(date),2, 'left', '0') }")) %>% 
  pivot_wider(values_from = CPI, names_from = date)


National_CC6_unweighted <-  new_CPI_data %>%
  pivot_longer(cols = c(matches("^PR_"), matches("^QTY_")), 
               names_to = c('.value', 'date'), 
               names_sep = "_") %>%
  # add actual date format from variable
  mutate(date = dmy(paste0("01",date))) %>%
  # delete variable (no longer needed now we have the dates)
  # calculate unit price
  mutate(unit_price = PR / QTY) %>%
  # arrange product and then date
  arrange(Uniques_ID, date) %>%
  # group by product
  group_by(Uniques_ID)  %>%
  mutate(rel_price_dif = unit_price / lag(unit_price, 1)) %>%
  mutate(rel_price_dif = ifelse(is.na(rel_price_dif), 1, rel_price_dif))  %>%
  group_by(COICOP6, date)  %>%
  summarise(prod_pr_diff = prod(rel_price_dif),
            n_prices = n(),
            index_diff = prod_pr_diff^(1/n_prices), .groups = 'keep') %>%
  mutate(index_diff = ifelse(is.na(index_diff), 1, index_diff)) %>%
  ungroup() %>%
  group_by(COICOP6)  %>%
  mutate(index = cumprod(index_diff) * 100) %>%
  select(-prod_pr_diff,
         -n_prices,
         -index_diff
  ) %>%
  ungroup() %>% 
  select(COICOP6,date, index) %>%
  group_by(COICOP6) %>%
  mutate(ref_period_CPI = mean(index[date >= ymd(start_ref_period) &date <= ymd(end_ref_period)])) %>%
  mutate(CPI = index/ref_period_CPI * 100) %>%
  select(-ref_period_CPI, - index) %>%
  ungroup() %>% 
  mutate(date = glue("CPI_{year(date)}_{str_pad(month(date),2, 'left', '0') }")) %>% 
  pivot_wider(values_from = CPI, names_from = date)


National_CC6_weighted <- computation_function(Indices_data = Indices,
                                              dissagregation = "COICOP6",
                                              start_ref_period =  start_ref_period,
                                              end_ref_period = end_ref_period) %>% 
  mutate(date = glue("CPI_{year(date)}_{str_pad(month(date),2, 'left', '0') }")) %>% 
  pivot_wider(values_from = CPI, names_from = date)


Region_Overall_CPI <- computation_function(Indices_data = Indices,
                                           dissagregation = c("region_letters", "Region_Code"),
                                           start_ref_period =  start_ref_period,
                                           end_ref_period = end_ref_period) %>% 
  mutate(date = glue("CPI_{year(date)}_{str_pad(month(date),2, 'left', '0') }")) %>% 
  pivot_wider(values_from = CPI, names_from = date) %>% 
  arrange(Region_Code )

Region_CC2 <- computation_function(Indices_data = Indices,
                                   dissagregation = c("region_letters", "Region_Code", "coicop02"),
                                   start_ref_period =  start_ref_period,
                                   end_ref_period = end_ref_period)%>% 
  mutate(date = glue("CPI_{year(date)}_{str_pad(month(date),2, 'left', '0') }")) %>% 
  pivot_wider(values_from = CPI, names_from = date) %>% 
  arrange(Region_Code, coicop02 )

Region_CC3 <- computation_function(Indices_data = Indices,
                                   dissagregation = c("region_letters", "Region_Code", "coicop03"),
                                   start_ref_period =  start_ref_period,
                                   end_ref_period = end_ref_period)%>% 
  mutate(date = glue("CPI_{year(date)}_{str_pad(month(date),2, 'left', '0') }")) %>% 
  pivot_wider(values_from = CPI, names_from = date) %>% 
  arrange(Region_Code, coicop03 )


Region_CC4 <- computation_function(Indices_data = Indices,
                                   dissagregation = c("region_letters", "Region_Code", "coicop04"),
                                   start_ref_period =  start_ref_period,
                                   end_ref_period = end_ref_period)%>% 
  mutate(date = glue("CPI_{year(date)}_{str_pad(month(date),2, 'left', '0') }")) %>% 
  pivot_wider(values_from = CPI, names_from = date) %>% 
  arrange(Region_Code, coicop04 )


National_food_non_food <- computation_function(Indices_data = Indices,
                                               dissagregation = "food_non_food",
                                               start_ref_period =  start_ref_period,
                                               end_ref_period = end_ref_period)%>% 
  mutate(date = glue("CPI_{year(date)}_{str_pad(month(date),2, 'left', '0') }")) %>% 
  pivot_wider(values_from = CPI, names_from = date)

Region_food_non_food <- computation_function(Indices_data = Indices,
                                             dissagregation = c("region_letters", "Region_Code", "food_non_food"),
                                             start_ref_period =  start_ref_period,
                                             end_ref_period = end_ref_period)%>% 
  mutate(date = glue("CPI_{year(date)}_{str_pad(month(date),2, 'left', '0') }")) %>% 
  pivot_wider(values_from = CPI, names_from = date) %>% 
  arrange(Region_Code )


National_import <- computation_function(Indices_data = Indices_import,
                                        dissagregation= "",
                                        start_ref_period =  start_ref_period,
                                        end_ref_period = end_ref_period)%>% 
  mutate(date = glue("CPI_{year(date)}_{str_pad(month(date),2, 'left', '0') }")) %>% 
  pivot_wider(values_from = CPI, names_from = date)

National_local <- computation_function(Indices_data = Indices_local,
                                       dissagregation= "",
                                       start_ref_period =  start_ref_period,
                                       end_ref_period = end_ref_period)%>% 
  mutate(date = glue("CPI_{year(date)}_{str_pad(month(date),2, 'left', '0') }")) %>% 
  pivot_wider(values_from = CPI, names_from = date)


National_import_food_non_food <- computation_function(Indices_data = Indices_import,
                                                      dissagregation = "food_non_food",
                                                      start_ref_period =  start_ref_period,
                                                      end_ref_period = end_ref_period)%>% 
  mutate(date = glue("CPI_{year(date)}_{str_pad(month(date),2, 'left', '0') }")) %>% 
  pivot_wider(values_from = CPI, names_from = date)

National_local_food_non_food <- computation_function(Indices_data = Indices_local,
                                                     dissagregation = "food_non_food",
                                                     start_ref_period =  start_ref_period,
                                                     end_ref_period = end_ref_period)%>% 
  mutate(date = glue("CPI_{year(date)}_{str_pad(month(date),2, 'left', '0') }")) %>% 
  pivot_wider(values_from = CPI, names_from = date)

Region_import <- computation_function(Indices_data = Indices_import,
                                      dissagregation = c("region_letters", "Region_Code"),
                                      start_ref_period =  start_ref_period,
                                      end_ref_period = end_ref_period)%>% 
  mutate(date = glue("CPI_{year(date)}_{str_pad(month(date),2, 'left', '0') }")) %>% 
  pivot_wider(values_from = CPI, names_from = date)%>% 
  arrange(Region_Code )


Region_local <- computation_function(Indices_data = Indices_local,
                                     dissagregation = c("region_letters", "Region_Code"),
                                     start_ref_period =  start_ref_period,
                                     end_ref_period = end_ref_period)%>% 
  mutate(date = glue("CPI_{year(date)}_{str_pad(month(date),2, 'left', '0') }")) %>% 
  pivot_wider(values_from = CPI, names_from = date)%>% 
  arrange(Region_Code )



Region_import_food_non_food <- computation_function(Indices_data = Indices_import,
                                                    dissagregation = c("region_letters", "Region_Code", "food_non_food"),
                                                    start_ref_period =  start_ref_period,
                                                    end_ref_period = end_ref_period)%>% 
  mutate(date = glue("CPI_{year(date)}_{str_pad(month(date),2, 'left', '0') }")) %>% 
  pivot_wider(values_from = CPI, names_from = date)

Region_local_food_non_food <- computation_function(Indices_data = Indices_local,
                                                   dissagregation = c("region_letters", "Region_Code","food_non_food"),
                                                   start_ref_period =  start_ref_period,
                                                   end_ref_period = end_ref_period)%>% 
  mutate(date = glue("CPI_{year(date)}_{str_pad(month(date),2, 'left', '0') }")) %>% 
  pivot_wider(values_from = CPI, names_from = date)


National_Urban_Rural <- computation_function(Indices_data = Indices,
                                             dissagregation = "Urban_rural",
                                             start_ref_period =  start_ref_period,
                                             end_ref_period = end_ref_period)%>% 
  mutate(date = glue("CPI_{year(date)}_{str_pad(month(date),2, 'left', '0') }")) %>% 
  pivot_wider(values_from = CPI, names_from = date)


Region_Urban_Rural <- computation_function(Indices_data = Indices,
                                           dissagregation = c("region_letters", "Region_Code","Urban_rural"),
                                           start_ref_period =  start_ref_period,
                                           end_ref_period = end_ref_period)%>% 
  mutate(date = glue("CPI_{year(date)}_{str_pad(month(date),2, 'left', '0') }")) %>% 
  pivot_wider(values_from = CPI, names_from = date)


# Link to previous data
{
  
  National_Overall_CPI_prev <- read_excel(previous_indices, sheet = 'National_Overall_CPI')
  
  sheet_National_Overall_CPI <- National_Overall_CPI %>%
    select(tail(names(.), 1)) %>%
    bind_cols(National_Overall_CPI_prev, .) 
  
  
  National_CC2_prev <- read_excel(previous_indices, sheet = 'National_CC2')
  
  sheet_National_CC2 <- National_CC2 %>%
    select(head(names(.), 1), tail(names(.), 1)) %>%
    left_join(National_CC2_prev,., by = c( "coicop02"= "coicop02" ))
  
  
  National_CC3_prev <- read_excel(previous_indices, sheet = 'National_CC3')
  
  sheet_National_CC3 <- National_CC3 %>%
    select(head(names(.), 1), tail(names(.), 1)) %>%
    left_join(National_CC3_prev,., by = c( "coicop03"= "coicop03" )) 
  
  National_CC4_prev <- read_excel(previous_indices, sheet = 'National_CC4')
  
  sheet_National_CC4 <- National_CC4 %>%
    select(head(names(.), 1), tail(names(.), 1)) %>%
    left_join(National_CC4_prev,., by = c( "coicop04"= "coicop04"))
  
  National_CC5_prev <- read_excel(previous_indices, sheet = 'National_CC5')
  
  sheet_National_CC5 <- National_CC5 %>%
    select(head(names(.), 1), tail(names(.), 1)) %>%
    left_join(National_CC5_prev,., by = c( "coicop05"= "coicop05" )) 
  
  
  
  National_CC6_unweighted_prev <- read_excel(previous_indices, sheet = 'National_CC6_unweighted')
  
  sheet_National_CC6_unweighted <-  National_CC6_unweighted %>%
    select(head(names(.), 1), tail(names(.), 1)) %>%
    left_join(National_CC6_unweighted_prev,., by = c( "COICOP6"= "COICOP6"))
  
  
  
  National_CC6_weighted_prev <- read_excel(previous_indices, sheet = 'National_CC6_weighted')
  
  sheet_National_CC6_weighted <-  National_CC6_weighted %>%
    select(head(names(.), 1), tail(names(.), 1)) %>%
    left_join(National_CC6_weighted_prev,., by = c( "COICOP6"= "COICOP6"))
  
  
  Regional_Overall_CPI_prev <- read_excel(previous_indices, sheet = 'Region_Overall_CPI')
  
  sheet_regional_Overall_CPI <- Region_Overall_CPI %>%
    select(head(names(.), 2), tail(names(.), 1)) %>%
    left_join(Regional_Overall_CPI_prev,., by = c( "Region_Code"= "Region_Code" )) 
  
  
  Region_CC2_prev <- read_excel(previous_indices, sheet = 'Region_CC2') %>%
    mutate(coicop02 = stringr::str_pad(as.character(coicop02),width = 2, pad = "0", side  = "left"))
  
  sheet_Regional_CC2 <- Region_CC2 %>%
    select(head(names(.), 3), tail(names(.), 1)) %>%
    left_join(Region_CC2_prev,., by = c( "coicop02"= "coicop02",
                                         "Region_Code"= "Region_Code")) 
  
  Region_CC3_prev <- read_excel(previous_indices, sheet = 'Region_CC3')
  
  sheet_Regional_CC3 <- Region_CC3 %>%
    select(head(names(.), 3), tail(names(.), 1)) %>%
    left_join(Region_CC3_prev,., by = c( "coicop03"= "coicop03",
                                         "Region_Code"= "Region_Code"))
  
  
  Region_CC4_prev <- read_excel(previous_indices, sheet = 'Region_CC4')
  
  sheet_Regional_CC4 <- Region_CC4 %>%
    select(head(names(.), 3), tail(names(.), 1)) %>%
    left_join(Region_CC4_prev,., by = c( "coicop04"= "coicop04",
                                         "Region_Code"= "Region_Code"))
  
  
  
  National_Non_FOOD_CPI_prev <- read_excel(previous_indices, sheet = 'National_food_non_food')
  
  sheet_National_Non_FOOD_CPI <- National_food_non_food %>%
    select(head(names(.), 1), tail(names(.), 1)) %>%
  	left_join(National_Non_FOOD_CPI_prev,., by = c("food_non_food" = "food_non_food")) 
  
  
  
  Region_Non_FOOD_CPI_prev <- read_excel(previous_indices, sheet = 'Region_food_non_food')
  
  sheet_Region_Non_FOOD_CPI <- Region_food_non_food %>%
    select(head(names(.), 3), tail(names(.), 1)) %>%
    left_join(Region_Non_FOOD_CPI_prev,., by = c( "Region_Code", "region_letters",
    																							"food_non_food")) 
  
  
  Local_CPI_prev <- read_excel(previous_indices, sheet = 'National_local')
  
  sheet_Local_CPI<- National_local %>%
    select(tail(names(.), 1)) %>%
    bind_cols(Local_CPI_prev,.) 
  
  
  Import_CPI_prev <- read_excel(previous_indices, sheet = 'National_import')
  
  sheet_Import_CPI<- National_import %>%
    select(tail(names(.), 1)) %>%
    bind_cols(Import_CPI_prev,.) 
  
  
  Regional_local_CPI_prev <- read_excel(previous_indices, sheet = 'Region_local')
  
  sheet_Regional_Local_CPI <- Region_local %>%
    select(head(names(.), 2), tail(names(.), 1)) %>%
    left_join(Regional_local_CPI_prev,., by = c( "Region_Code"= "Region_Code" )) 
  
  Regional_Import_CPI_prev <- read_excel(previous_indices, sheet = 'Region_import')
  
  sheet_Regional_Import_CPI <- Region_import %>%
    select(head(names(.), 2), tail(names(.), 1)) %>%
    left_join(Regional_Import_CPI_prev,., by = c( "Region_Code"= "Region_Code" )) 
  
  
  
  National_FNF_Local_prev <- read_excel(previous_indices, sheet = 'National_local_food_non_food')
  
  sheet_National_Food_Non_Food_Food_Local <- National_local_food_non_food %>%
    select(head(names(.), 1), tail(names(.), 1)) %>%
    left_join(National_FNF_Local_prev,., by = c( "food_non_food"= "food_non_food" )) 
  
  
  National_FNF_Imported_prev <- read_excel(previous_indices, sheet = 'National_import_food_non_food')
  
  sheet_National_Food_Non_Food_Food_Imported <- National_import_food_non_food %>%
    select(head(names(.), 1), tail(names(.), 1)) %>%
    left_join(National_FNF_Imported_prev,., by = c( "food_non_food"= "food_non_food" )) 
  
  
  
  Regional_FNF_Local_prev <- read_excel(previous_indices, sheet = 'Region_local_food_non_food')
  
  sheet_Regional_Food_Non_Food_Food_Local <- Region_local_food_non_food %>%
    select(head(names(.), 3), tail(names(.), 1)) %>%
    left_join(Regional_FNF_Local_prev,., by = c( "food_non_food"= "food_non_food",
                                                 "Region_Code" = "Region_Code")) 
  
  
  Regional_FNF_Imported_prev <- read_excel(previous_indices, sheet = 'Region_import_food_non_food')
  
  sheet_Regional_Food_Non_Food_Food_Imported <- Region_import_food_non_food %>%
    select(head(names(.), 3), tail(names(.), 1)) %>%
    left_join(Regional_FNF_Imported_prev,., by = c( "food_non_food"= "food_non_food",
                                                    "Region_Code" = "Region_Code")) 
  

  National_Urban_Rural_prev <- read_excel(previous_indices, sheet = 'National_Urban_Rural')
  
  sheet_National_Urban_Rural <- National_Urban_Rural %>%
    select(head(names(.), 1), tail(names(.), 1)) %>%
    left_join(National_Urban_Rural_prev,., by = c( "Urban_rural"= "Urban_rural" )) 
  
  
  
  Regional_Urban_Rural_prev <- read_excel(previous_indices, sheet = 'Region_Urban_Rural')
  
  sheet_Regional_Urban_Rural <- Region_Urban_Rural %>%
    select(head(names(.), 3), tail(names(.), 1)) %>%
    left_join(Regional_Urban_Rural_prev,., by = c( "Urban_rural"= "Urban_rural",
                                                   "Region_Code" = "Region_Code")) 
  }


# Write Excel output
writexl::write_xlsx(list(National_Overall_CPI     = sheet_National_Overall_CPI,
                         National_CC2             = sheet_National_CC2,
                         National_CC3             = sheet_National_CC3,
                         National_CC4             = sheet_National_CC4,
                         National_CC5             = sheet_National_CC5,
                         National_CC6_unweighted  = sheet_National_CC6_unweighted,
                         National_CC6_weighted    = sheet_National_CC6_weighted,
                         Region_Overall_CPI       = sheet_regional_Overall_CPI,
                         Region_CC2               = sheet_Regional_CC2, 
                         Region_CC3               = sheet_Regional_CC3, 
                         Region_CC4               = sheet_Regional_CC4, 
                         National_Non_Food        = sheet_National_Non_FOOD_CPI,
                         Region_Non_FOOD          = sheet_Region_Non_FOOD_CPI,
                         National_Local           = sheet_Local_CPI, 
                         National_Import          = sheet_Import_CPI,
                         Regional_Local           = sheet_Regional_Local_CPI,
                         Regional_Import          = sheet_Regional_Import_CPI,
                         National_FNF_Local       = sheet_National_Food_Non_Food_Food_Local,
                         National_FNF_Imported    = sheet_National_Food_Non_Food_Food_Imported, 
                         Regional_FNF_Local       = sheet_Regional_Food_Non_Food_Food_Local,
                         Regional_FNF_Imported    = sheet_Regional_Food_Non_Food_Food_Imported,
                         National_Urban_Rural     = sheet_National_Urban_Rural,
                         Regional_Urban_Rural     = sheet_Regional_Urban_Rural
), target_file)




