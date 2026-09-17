############################################
# Calculate 3yr weight average EUC for 2025,
#    from CREEL data

# This is most similar to **Option C** of 
#   the TWG Recommendations for Catch Record 
#   Card-based Puget Sound Recreational Catch 
#   Estimation: Calculate an EUC using the most 
#   recent 3 years(for 2025, this would 
#   include creel data from 2023, 2024 and 2025) 
#   of creel surveys of crabbers who reported 
#   catch in the area... 
#
# Author: Mary Fisher, SITC 
#  (contact: mfisher@swinomish.nsn.us)
#
#
############################################

library(tidyverse)
library(janitor)
library(here)


# Data --------------------------------------------------------------------

#2023
EUC.Table.23 <- read.csv(here('EUC','EUC_2023.csv')) |> 
  clean_names() |> rename(marine_area=areas_23)

#2024
EUC.Table.24 <- read.csv(here('EUC','EUC_2024.csv'))|> 
  clean_names() |> rename(marine_area=areas_24)

#2025
EUC.Table.25 <- read.csv(here('EUC','EUC_2025.csv'))|> 
  clean_names() |> rename(marine_area=areas_25)

#Combine
EUC.Table <- bind_rows(EUC.Table.23, EUC.Table.24, EUC.Table.25)



# Focal regions -----------------------------------------------------------
#Areas that have had a focus creel in the last three years (MA 8-1, 8-2, 9, and 11 for 2025)

#Calculate an EUC using the most recent 3 years of creel surveys of crabbers who reported 
# catch in the area using the TRP recommended calculations (𝑅෠ଷ from Hankin et al 2021). 
# This would include information from crabbers who were part
# of the focus creels and those who were part of the other creels but who caught crab in 
# the focus creel area.

#get data for focal areas
focal_euc_data <- filter(EUC.Table, marine_area %in% c("8_1","8_2","9","11"))

#get weighted average
focal_euc_data <- focal_euc_data |> 
  group_by(marine_area) |> 
  mutate(marine_area_contacts=sum(total_contacts),
         marine_area_success=sum(n_boats_euc)) |> 
  ungroup() |> 
  mutate(annual_weight_all = total_contacts/marine_area_contacts,
         annual_weight_success = n_boats_euc/marine_area_success) |> 
  ## multiply annual EUC with weights
  mutate(euc_weight_all=euc*annual_weight_all,
         euc_weight_success=euc*annual_weight_success)

focal_euc <- focal_euc_data |> 
  group_by(marine_area) |> 
  summarise(avg_euc_all = sum(euc_weight_all),
            avg_euc_success = sum(euc_weight_success))



# non-focal areas ---------------------------------------------------------

#Areas that have not had a focus creel in the last three years (MA 4, 5, 6, 7, 10, 12, 13 for 2025):

#Apply an average Puget Sound-wide EUC estimate for all areas, calculated using the most recent 3 years
#of creel data (for 2025, this would be 2023, 2024 and 2025), with each year weighted by the number of
#interviews as done for R3-hat from Hankin et al (2021). Because some areas have received much more
#intensive sampling than others, the TWG recommends a slight variation of this method in which the EUC
#is also weighted by area, with weighting based on the CRC catch proportion by Marine Area from those
#same three years. Weighting by area needs to be tested before this method is implemented; the TWG
#suggested some ideas if it does not work.


