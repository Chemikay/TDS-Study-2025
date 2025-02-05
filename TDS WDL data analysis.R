library(tidyverse)
library(data.table)
library(readxl)
library(janitor)
library(here)

# Pull in all original WDL data sets into one dataframe
# Unzip folder containing WDL data to a temporary directory
unzip(zipfile = here("WDL_data.zip"), exdir = tempdir())

filelist <- list.files(
  path = file.path(tempdir(), "WDL_data"),    
  pattern = "*.csv$",
  full.names = FALSE
) 

dset <- 
  list.files(
    file.path(tempdir(), "WDL_data"),    
    pattern = "*.csv$",
    full.names = TRUE
  ) %>%
  lapply(read.csv) 

attr(dset, "names") <- filelist

dset1 <- rbindlist(dset, idcol= "id", fill = TRUE)

dset1 <- clean_names(dset1)

View(dset1)

names(dset1)

unique(dset1$analyte)

# filter out all unneeded analytes 
dset2 <- filter(dset1, analyte == "Field Specific Conductance" | analyte == "Dissolved Sulfate" | analyte == "Dissolved Sulfate" | analyte == "Dissolved Sulfate"
                | analyte == "Total Dissolved Solids" | analyte == "Total Suspended Solids" | analyte == "Dissolved Nitrate" | analyte == "Specific Conductance"
                | analyte == "Dissolved Bromide" | analyte == "Dissolved Chloride") 

#evaluate RPD between sample and lab dup for TDS


#evaluate RPD between sample and field dup for TDS

#filter out samples without TDS data, count remaining = 1914
dset2_TDSonly <- filter(dset2, analyte == "Total Dissolved Solids") 

dset2_TDSonly <- gfg_data4 %>% 
  pivot_wider(id_cols = c(long_station_name, data_owner, sample_code, collection_date),
              names_from = analyte2, values_from = result,
              values_fill = NA


dset2_wide <- dset2 %>% 
  pivot_wider(id_cols = c(long_station_name, data_owner, sample_code, collection_date, sample_type,lab_dup),
              names_from = analyte, values_from = result,
              values_fill = NA) 
