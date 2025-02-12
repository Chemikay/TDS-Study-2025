library(tidyverse)
library(data.table)
library(readxl)
library(janitor)
library(lubridate)
library(flextable)
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

# filter out all unneeded analytes since all field parameters were exported
dset2 <- filter(dset1, analyte == "Field Specific Conductance" | analyte == "Dissolved Sulfate" | analyte == "Total Dissolved Solids" 
                | analyte == "Total Suspended Solids" | analyte == "Dissolved Nitrate" | analyte == "Specific Conductance"
                | analyte == "Dissolved Bromide" | analyte == "Dissolved Chloride") 

#count lab dups, field collected blanks, and field dups submitted for TDS
dset2_tds <- filter(dset2, analyte == "Total Dissolved Solids")

str(dset2_tds)

dset2_tds_qc <- mutate(dset2_tds, qc_category = case_when(lab_dup == "Y"~"lab_dup",
                       grepl("Blank", sample_type)~"field_blank",
                       grepl("Replicate", sample_type) | grepl("Duplicate", sample_type) & lab_dup !="Y"~"field_dup",
                       TRUE~"normal"))

count(dset2_tds_qc, qc_category)
#qc_category     n
#<char> <int>
#1: field_blank   165
#2:   field_dup   179
#3:     lab_dup   184
#4:      normal  1386


#remove all qc samples 
dset3 <- filter(dset2_tds_qc, qc_category == "normal")
#remaining normal TDS samples: 1386

#pull back in other analytes of interest to parse out qc samples, need to account for lab dups that are done on a field duplicate
#this doesn't properly account for field dups which serve as parent for lab dup RPD so need to account for that later
dset2_qc <- mutate(dset2, qc_category = case_when(lab_dup == "Y"~"lab_dup",
                                                     grepl("Blank", sample_type)~"field_blank",
                                                     grepl("Replicate", sample_type) | grepl("Duplicate", sample_type) & lab_dup !="Y"~"field_dup",
                                                     TRUE~"normal"))

#remove all qc samples 
dset3 <- filter(dset2_qc, qc_category == "normal")

# Look for duplicates using analyte, sample code, and collection date as
# unique identifiers
r = dset3 %>% 
  count(sample_code, collection_date, analyte) %>% 
  filter(n > 1)

#there are 32 duplicates, 2 are dissolved chloride and remainder are field specific conductance.I brought up Chloride observation to QA and data owner to handle 
#separately, will choose first value. Spot checking the Field SC duplicates, there is a different status assigned but values are the same. Also choosing first value


#pivot wider to group by sample ID, pick first instance of sample ID present to remove these 34 dup values
dset3_wide <- dset3 %>% 
  pivot_wider(id_cols = c(sample_code, collection_date, data_owner, station_number),
              names_from = analyte, values_from = result,
              values_fill = NA, values_fn = first)

dset3_wide <- clean_names(dset3_wide)

# Look for duplicates using sample code
dset3_wide %>% 
  count(sample_code) %>% 
  filter(n > 1)
#no duplicates so good to go

str(dset3_wide)

#reformat date
dset3_wide$collection_date <- as.POSIXct(dset3_wide$collection_date, format="%m/%d/%Y %H:%M")

#dset3_wide = mutate(dset3_wide, collection_date = as.Date("2022-11-01")) 

str(dset3_wide)

#reformat remaining fields
dset3_wide <- mutate(dset3_wide, total_dissolved_solids = as.numeric(total_dissolved_solids),
                     field_specific_conductance = as.numeric(field_specific_conductance),
                     specific_conductance = as.numeric(specific_conductance),
                     dissolved_sulfate = as.numeric(dissolved_sulfate),
                     dissolved_bromide = as.numeric(dissolved_bromide),
                     dissolved_chloride = as.numeric(dissolved_chloride),
                     dissolved_nitrate = as.numeric(dissolved_nitrate))

#filter out any samples without TDS
dset3_wide <- dset3_wide %>%
  drop_na(total_dissolved_solids)

#number of samples with just TDS is 1386, total observations in dset3_wide

#create new analyte column for any anions methods
dset4_wide <- mutate(dset3_wide, anions = case_when(dissolved_sulfate != "NA" | dissolved_bromide != "NA" | dissolved_chloride != "NA"
                                                    | dissolved_nitrate != "NA" ~ 'yes', TRUE ~ 'no'))

#number of samples with TDS and any of dissolved sulfate, nitrate, bromide, chloride 
count(dset4_wide, anions == "yes") #1321 TDS samples also submitted for any anions, 65 not


#number of samples with TDS and TSS
count(dset4_wide, total_suspended_solids !="NA") #1046 TDS samples also submitted for TSS, 340 not

#summary of TDS values
summary(dset4_wide$total_dissolved_solids)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#28.5    93.0   169.5  1335.7   354.0 29400.0 

str(dset4_wide$collection_date)

#breakdown TDS sample submissions by month
ggplot(dset4_wide, aes(x = collection_date)) +
  geom_freqpoly()+
  labs(
    title = "Frequency of TDS sample submissions, 6/1/2023-5/31/2024",
    x = "Collection Date",
    y = "Sample Count"
  )

tableA <- flextable(dset4_wide)



#breakdown TDS concentration frequency observed

count(dset4_wide, total_dissolved_solids > 20000)
#23 out of 1386 TDS samples with concentration > 20,000 mg/L = 1.66% of samples

count(dset4_wide, total_dissolved_solids > 10000)
#56 out of 1386 TDS samples with concentration > 10,000 mg/L = 4.04% of samples

count(dset4_wide, total_dissolved_solids > 2500)
#146 out of 1386 TDS samples with concentration > 10,000 mg/L = 10.5% of samples

ggplot(dset4_wide, aes(x = total_dissolved_solids)) +
  geom_freqpoly(bins=50)+
  xlim(0, 10000)+
  labs(
    title = "Frequency of TDS concentrations, 6/1/2023-5/31/2024",
    x = "TDS (mg/L)",
    y = "Sample Count"
  )

#evaluate RPD between sample and lab dup for TDS to get lab method variance

#reassign qc category to properly assign "parent" to lab dup due to earlier issue with the parent samples that are classified as field dup  
lab_dup1 <- mutate(dset2_tds, lab_dup_category = case_when(lab_dup == "Y"~"lab_dup",
                                                            TRUE~"not_dup"))
  

lab_dup1_wide <- lab_dup1 %>% 
  pivot_wider(id_cols = c(long_station_name, data_owner, sample_code, collection_date),
              names_from = lab_dup_category, values_from = result,
              values_fill = NA)

lab_dup1_wide2 <- filter(lab_dup1_wide, lab_dup !="NA")
#count of 184 lab dups is same as earlier, good to go

str(lab_dup1_wide2)

#change character values to numeric for rpd calculation
lab_dup1_wide2$not_dup <- as.numeric(lab_dup1_wide2$not_dup)
lab_dup1_wide2$lab_dup <- as.numeric(lab_dup1_wide2$lab_dup)

str(lab_dup1_wide2)

# perform RPD calculation 
lab_dup_rpd <- lab_dup1_wide2 %>% 
  mutate(rpd =  100 * ((not_dup - lab_dup) / ((not_dup + lab_dup) / 2)))

hist(lab_dup_rpd$rpd, breaks=200, main = "Lab Dup RPD distribution") 


## good up to here now. 

#evaluate RPD between sample and field dup for TDS to get field method variance

#reassign qc category to properly assign "parent" to field dup including when used as lab dup  
field_dup1 <- mutate(dset2_tds, field_dup_category = case_when(grepl("Replicate", sample_type) | grepl("Duplicate", sample_type)~"field_dup",
                                                           TRUE~"not_dup"))

field_dup2 <- mutate(field_dup1, main_id = case_when(parent_sample !="0" ~ paste(parent_sample),
                                                       TRUE ~ paste(sample_code)))

#remove all lab dups 
field_dup3 <- filter(field_dup2, lab_dup == "")


field_dup3_wide <- field_dup3 %>% 
  pivot_wider(id_cols = c(long_station_name, data_owner, main_id, collection_date),
              names_from = field_dup_category, values_from = result,
              values_fill = NA)

field_dup3_wide2 <- filter(field_dup3_wide, field_dup !="NA")
#count of 179 lab dups is same as earlier, good to go

str(field_dup3_wide2)

#change character values to numeric for rpd calculation
field_dup3_wide2$not_dup <- as.numeric(field_dup3_wide2$not_dup)
field_dup3_wide2$field_dup <- as.numeric(field_dup3_wide2$field_dup)

str(field_dup3_wide2)

# perform RPD calculation 
field_dup_rpd <- field_dup3_wide2 %>% 
  mutate(rpd =  100 * ((not_dup - field_dup) / ((not_dup + field_dup) / 2)))

hist(field_dup_rpd$rpd, breaks=200, main = "Field Dup RPD distribution") 
s

