---
title: "Assignment 4 Trade - 2021 import ICUMS: Download, check, visualize, extract"
author: "Femke van den Bos"
lastchanged: "17-03-2023"
output: "Extractions from ICUMS 2021 import data" 
---
  
### Setup ###
{
  # Load necessary libraries
  library(tidyverse)
  library(janitor)
  library(lubridate)

  # make sure time and time zone is set to English and GMT
  Sys.setlocale("LC_TIME", "English") 
  
}


### Read 2021 ICUMS data - only do this ONCE, as it takes very long ###
{
  # Path for all data files 
  pathname_icums_21 <- "import/input/ICUMS 2021 - Import"
  
  # Create file list, with the names of all the files in the folder
  icums_import_file_list_21 <- list.files(pathname_icums_21)
  
  # Make list of date from all the excel sheets
  import_icums_21 <- lapply(icums_import_file_list_21, function(x) { # This function loops over all files
    sheets <- readxl::excel_sheets(paste(pathname_icums_21, x, sep = "/")) # Per file it gives us a list of sheet names
    lapply(sheets, function(y) { # This function then loops over all sheets
      readxl::read_excel(paste(pathname_icums_21, x, sep = "/"), 
                         sheet = y, 
                         col_names = TRUE, skip = 4)
    })
  })
  
  # Now make into dataframe, by unlisting and binding rows
  import_icums_21_list <- unlist(import_icums_21, recursive = FALSE)
  import_icums_21_input <- do.call("bind_rows", import_icums_21_list)

  # Drop dataframes we don't use anymore
  rm(icums_import_file_list_21, import_icums_21, import_icums_21_list, pathname_icums_21)
}

# After having run this once, go to your environment (top-right) and click on the save button there!
# Save your environment under the name "import_icums_21.RData", in the folder import/input (NOT in the ICUMS 2021 - Import subfolder!!)
# The from now on, you do not have to run the "Read 2021 ICUMS data" code anymore,
# but instead you can run:
load("import/input/import_icums_21.RData")
# This is much faster!!


### Rename columns and select what we want to use ###
{
  # Clean Import data
  import_icums_21_clean <- import_icums_21_input %>%
    mutate(date = convert_to_date(HEAD_TIME_PROC, character_fun = lubridate::dmy),
           year = as.numeric(format(date, "%Y")),
           month = as.numeric(format(date, "%m")),
           month_name = month.name[month],
           day = as.numeric(format(date, "%d")),
           warehouse = 0,
           re = 0,
           reg9 = 0,
           tradeflow = "Import") %>%
    rename(hs_code = ITEMS_HS_CODE,
           country_abb = HEAD_COUNTRY_ORIG_DEST,
           country_cons = COUNTRY_OF_CONSIGNMENT,
           cif = ITEMS_CIF_GHC,
           fob = ITEMS_FOB_GHC,
           net_mass = ITEMS_NET_MASS,
           gross_mass = ITEMS_GROSS_MASS,
           desc = ITEMS_GOODS_DESC,
           head_cust_off = HEAD_CUST_OFF_CODE,
           head_decl_no = HEAD_DECL_NO,
           head_declarant_id = HEAD_DECLARANT_ID,
           head_declarant_name = HEAD_DECLARANT_NAME,
           head_porter_code = HEAD_IMPORTER_CODE,
           head_porter_name = HEAD_IMPORTER_NAME) %>%
    select(tradeflow, date, year, month, month_name, day, hs_code, country_abb, country_cons,
           cif, fob, net_mass, gross_mass, desc, warehouse, re, reg9,
           head_cust_off, head_decl_no, head_declarant_id, head_declarant_name,
           head_porter_code, head_porter_name)
}


### Add your own checks of the data here ###
# You can think of:
# Yearly totals (cif) - do the numbers make sense?
# Minimum and maximum cif values - are there outliers?
# Monthly numbers - any strange/missing months?
# Zero/negative values - should that be possible?
# Hint: look at the trade vulnerability ICUMS code! You could use some of the checks from that code!
# However, also add some of your own checks!!
{
  
}


### Create your own visual checks here ###
# This means that you make some figures to check your data
# You can think of:
# Monthly totals
# Totals for the major HS codes/countries
# Number of observations per month/week/day
# Histograms
# Hint: Again, look at the trade vulnerability ICUMS code! You could use some of the checks from that code!
# However, also add some of your own figures!!
{
  
}


### If necessary: remove some outliers here ###
# In your checks you should have discovered if there are any issues with the data
# We would rather not do any manual outlier removals here
# But it might be necessary to remove some extreme values to get reliable results
# Think about whether this is necessary and then decide whether you want to create a new dataset without outliers
{
  
}


### Extract data ###
# Follow the instructions in the assignment to do some data extractions here!
{
  
}


# Bonus: Could you think of an extraction you might want to do (which was not in the assignment)? 
# Try to do that here yourself!
{
  
}


### Bonus: Create extra visualizations ###
# So far we have created visualizations to check the data
# It would also be nice to create visualizations which we could publish
# So now try to make figures that would be interesting for a trade publications
# Think of doing a quarterly trade publication, which figures should be in there?
# Try to make one (or more) such figure yourself!
{
  
}

