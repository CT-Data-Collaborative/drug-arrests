library(dplyr)
library(datapkg)
library(tidyr)
library(gsubfn)

##################################################################
#
# Processing Script for Drug Arrests
# Created by Jenna Daly
# On 08/20/2018
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))
raw_data <- dir(path_to_raw, recursive=T, pattern = "Crime") 

#Create new population data set for all years
source('./scripts/getPopulation.R')

drug_arrests <- data.frame(stringsAsFactors=F)
for (i in 1:length(raw_data)) {
  current_file <- read.csv(paste0(path_to_raw, "/", raw_data[i]), stringsAsFactors = F, header=T, check.names=F)
  names(current_file)[1] <- "Crime"
  #Remove rows without Ages
  current_file <- current_file[grepl("[0-9]", current_file$Crime),]
  #Isolate "Drug" rows
  current_file <- current_file[grep("DrugTot", current_file$Crime),]
  #convert wide to long
  last_col <- ncol(current_file)
  current_file_long <- gather(current_file, Indicator, Value, 2:last_col, factor_key=TRUE)
  #Assign Age column
  current_file_long$Age <- gsub("([a-zA-Z ]+)(<?[0-9+-]+$)", "\\2", current_file_long$Crime)
  #Remove Ages from Crime column
  current_file_long$Crime <- gsub("[^a-zA-Z]", "", current_file_long$Crime)
  get_year <- as.numeric(substr(unique(unlist(gsub("[^0-9]", "", unlist(raw_data[i])), "")), 1, 4))
  current_file_long$Year <- get_year
  drug_arrests <- rbind(drug_arrests, current_file_long)
}

drug_arrests$Indicator <- as.character(drug_arrests$Indicator)

#Fix names for merge
drug_arrests$Indicator[drug_arrests$Indicator == "CT"] <- "Connecticut"

#Removing values for county (where name is both a county and a town)
years <- c("2010", "2011", "2012", "2013") #Years where indicators are counties not towns
indicators <- c("Hartford", "Windham", "Tolland", "New London", "New Haven", "Litchfield", "Fairfield")
drug_arrests <- drug_arrests[!(drug_arrests$Year %in% years & drug_arrests$Indicator %in% indicators),]

drug_arrests$Indicator <- gsub(" PD", "", drug_arrests$Indicator)
drug_arrests$Indicator <- gsub(" CSP", "", drug_arrests$Indicator)
drug_arrests$Indicator <- gsub(" Municipal", "", drug_arrests$Indicator)
drug_arrests$Indicator[which(grepl("Groton", drug_arrests$Indicator))] <- "Groton"
drug_arrests$Indicator[which(grepl("Putnam", drug_arrests$Indicator))] <- "Putnam"

#Merge in FIPS (to remove non-towns)
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

fips <- as.data.frame(fips)

drug_arrests_fips <- merge(drug_arrests, fips, by.x = "Indicator", by.y = "Town", all.y=T)

#Aggregate towns
drug_arrests_fips <- unique(drug_arrests_fips %>% 
  group_by(Year, Age, Indicator, Crime) %>% 
  mutate(Value = sum(Value)))

# Assign age group flags (one age may belong to mutliple groups)
drug_arrests_fips$`10 years and over flag` <- "TRUE"
drug_arrests_fips$`10 to 17 years flag` <- "FALSE"
drug_arrests_fips$`18 years and over flag` <- "FALSE"
drug_arrests_fips$`18 to 24 years flag` <- "FALSE" 

#Assign flags based on Age column
drug_arrests_fips$`10 years and over flag`[drug_arrests_fips$`Age` == "<10"] <- "FALSE"

x1017 <- c("10-12", "13-14", "15", "16", "17")
drug_arrests_fips$`10 to 17 years flag`[drug_arrests_fips$`Age` %in% x1017] <- "TRUE"

over17 <- c("18", "19", "20", "21", "22", "23", "24", "25-29", "30-34", "35-39", "40-44", 
            "45-49", "50-54", "55-59", "60-64", "65+")
drug_arrests_fips$`18 years and over flag`[drug_arrests_fips$`Age` %in% over17] <- "TRUE"

x1824 <- c("18", '19', "20", "21", "22", "23", "24")
drug_arrests_fips$`18 to 24 years flag`[drug_arrests_fips$`Age` %in% x1824] <- "TRUE"

#Aggregate age groups based on flag
drug_arrests_calc <- drug_arrests_fips %>% 
  group_by(Indicator, Year, `10 years and over flag`) %>% 
  mutate(`10 years and over` = ifelse(`10 years and over flag` == "TRUE", sum(Value), 0))

drug_arrests_calc <- drug_arrests_calc %>% 
  group_by(Indicator, Year, `10 to 17 years flag`) %>% 
  mutate(`10 to 17 years` = ifelse(`10 to 17 years flag` == "TRUE", sum(Value), 0))

drug_arrests_calc <- drug_arrests_calc %>% 
  group_by(Indicator, Year, `18 years and over flag`) %>% 
  mutate(`18 years and over` = ifelse(`18 years and over flag` == "TRUE", sum(Value), 0))

drug_arrests_calc <- drug_arrests_calc %>% 
  group_by(Indicator, Year, `18 to 24 years flag`) %>% 
  mutate(`18 to 24 years` = ifelse(`18 to 24 years flag` == "TRUE", sum(Value), 0))

#Create total column
drug_arrests_calc <- drug_arrests_calc %>% 
  group_by(Indicator, Year) %>% 
  mutate(Total = sum(Value))

#Complete df with all totals
drug_arrests_totals <- drug_arrests_calc %>% 
  group_by(Indicator, Year, FIPS) %>% 
  summarise(`10 years and over` = max(`10 years and over`), 
            `10 to 17 years` = max(`10 to 17 years`), 
            `18 years and over` = max(`18 years and over`), 
            `18 to 24 years` = max(`18 to 24 years`), 
            `Total` = max(Total))

##########################################################################################################
#Create CT values for 2015 (2015 file does not have CT level values)
CT_2015 <- drug_arrests_calc[drug_arrests_calc$Year == "2015",]

#Add up all totals
CT_2015_calc <- CT_2015 %>% 
  group_by(Age) %>% 
  summarise(`10 years and over` = sum(`10 years and over`), 
            `10 to 17 years` = sum(`10 to 17 years`), 
            `18 years and over` = sum(`18 years and over`), 
            `18 to 24 years` = sum(`18 to 24 years`), 
            `Total` = sum(Total))

CT_2015_final <- CT_2015_calc %>% 
  group_by() %>% 
  summarise(`10 years and over` = max(`10 years and over`), 
            `10 to 17 years` = max(`10 to 17 years`), 
            `18 years and over` = max(`18 years and over`), 
            `18 to 24 years` = max(`18 to 24 years`), 
            `Total` = max(Total))

#Create columns
CT_2015_final$Indicator <- "Connecticut"
CT_2015_final$FIPS <- "09"
CT_2015_final$Year <- 2015

drug_arrests_totals <- as.data.frame(drug_arrests_totals)

#Merge CT 2015 with rest of data
drug_arrests_totals <- rbind(drug_arrests_totals, CT_2015_final)

#convert wide to long
drug_arrests_totals <- gather(drug_arrests_totals, `Age Range`, Value, 4:8, factor_key=TRUE)
####################################################################################################

## read population data for denominators in rate calculations
pops <- read.csv(paste0(path_to_raw, "/", "populations.csv"), stringsAsFactors = F, header=T, check.names=F)

# Helper function for MOE
calcMOE <- function(x, y, moex, moey) {
  moex2 <- moex^2
  moey2 <- moey^2
  d <- x/y
  d2 <- d^2
  
  radicand <- ifelse(
    moex2 < (d2 * moey2),
    moex2 + (d2 * moey2),
    moex2 - (d2 * moey2)
  )
  
  return(sqrt(radicand)/y)
}

pops$FIPS <- gsub("^", "0", pops$FIPS)

percents <- merge(drug_arrests_totals, pops, by = c("Year", "Age Range", "FIPS"))

#Rates are calculated per 10000
percents <- percents %>% 
  mutate(Pop = (Pop/1e4), 
         MOE = (MOE/1e4))

# calculate rates with population denominators,
# keep MOES, calculating appropriately
percents <- percents %>% 
  mutate(`Drug Arrests` = round((Value / Pop), 2),
         `Margins of Error` = round((calcMOE(Value, Pop, 0, MOE)), 2),
         `Measure Type` = "Rate (per 10,000)")

nulls <- c("Value", "Pop", "MOE")
percents[nulls] <- NULL

# melt percents
percents <- gather(percents, Variable, Value, 5:6, factor_key=F)
percents$Variable <- as.character(percents$Variable)

## FINAL STEPS
# add extra data
drug_arrests_totals$`Measure Type` <- "Number"
drug_arrests_totals$Variable <- "Drug Arrests"

percents <- as.data.frame(percents)
drug_arrests_totals <- as.data.frame(drug_arrests_totals)

# combine number and rate measures into one dataset
drug_arrests_complete <- rbind(drug_arrests_totals, percents)

#Assign factors for sorting
drug_arrests_complete$`Age Range` <- factor(drug_arrests_complete$`Age Range`, levels = c("Total", "10 years and over", "10 to 17 years", "18 to 24 years", "18 years and over"))

# Order and sort columns
drug_arrests_complete <- drug_arrests_complete %>% 
  select(Indicator, FIPS, Year, `Age Range`, `Measure Type`, Variable, Value) %>% 
  rename(Town = Indicator) %>% 
  arrange(Town, Year, `Age Range`, `Measure Type`)

# Write to File
write.table(
  drug_arrests_complete,
  file.path(getwd(), "data", "drug-arrests_2015.csv"),
  sep = ",",
  row.names = F,
  na = "-9999"
)
