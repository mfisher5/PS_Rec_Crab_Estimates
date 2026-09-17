############################################
# Calculate annual EUC from CREEL data
#
# Authors: Blair Winnacott, Katelyn Bosley, WDFW
#    edited 9/17/2026 by Mary Fisher, SITC
#
#
############################################



# Load Packages -----------------------------------------------------------

library(tidyverse)
# library(readxl)
# library(rio)
# library(dplyr)
# library(data.table)
# library(tidyr)
# library(ggplot2)
# library(writexl)
# library(eeptools)
library(magrittr)
library(cmocean)
library(patchwork)
library(R.utils)
library(tools)
# library(installr)
# library(chron)
# library(scales)

# for bootstrapping
library(boot)
library(forcats)
library(perm)

# added MCF 
library(here)

############################################################################
############################################################################
############################################################################
############################################################################


# 2025 ---------------------------------------------------------------

# Set up 
#Input.directory=paste0("YOUR WORKING DIRECTORY")
# Input.directory=paste0(getwd(),"/Creel Data")
Input.directory=here('EUC','Creel_Data','2025_Creel')

set.seed(12345)


# Define directory and read data in
Input.directory.25=paste0(Input.directory,"/Creel_Data_Shared_2025_Updated.xlsx")
boat <- readxl::read_excel(Input.directory.25)
boat.25=as.data.frame(boat)

#Combine 7N and 7S into 7
boat.25$marine_area_fished[boat.25$marine_area_fished=="7N"]="7"
boat.25$marine_area_fished[boat.25$marine_area_fished=="7S"]="7"

# 1) Filter data for summer fishery and MA7 winter during September (e.g., all data for 2025)
boat.25=boat.25 %>% filter(marine_area_fished=="7" & creel_date <= "2025-09-30" | 
                             marine_area_fished=="6" & creel_date <= "2025-09-01" |
                             marine_area_fished=="8_1" & creel_date <= "2025-09-01" |
                             marine_area_fished=="8_2" & creel_date <= "2025-09-01" |
                             marine_area_fished=="9" & creel_date <= "2025-09-01" |
                             marine_area_fished=="10" & creel_date <= "2025-09-01" |
                             marine_area_fished=="11" & creel_date <= "2025-09-01" |
                             marine_area_fished=="12" & creel_date <= "2025-09-01")

#get total number of interviews in these periods
total.contacts <- boat.25 |> group_by(marine_area_fished) |> summarize(total.contacts=length(unique(Boat_ID)))

# 2) Remove boats where CRCs not checked due to reasons other than non-compliance
#non-compliance = refused to show CRCs/did not have them in possession
boat.25 = boat.25 %>% filter(is.na(no_crcs_checked_reason) | no_crcs_checked_reason=="No CRCs" | no_crcs_checked_reason=="No Cooperation")

#Identify over-recorded crab and remove any crab that were over-recorded by making the number of crab recorded equal what was in possession
boat.25$over_recorded.yn <- ifelse(boat.25$n_crab_recorded > boat.25$n_dungeness_retained, "yes", "no") 
boat.25$over_recorded.yn <- ifelse(is.na(boat.25$over_recorded.yn), "no", boat.25$over_recorded.yn) 
boat.25$n_crab_recorded <- ifelse(boat.25$over_recorded.yn=="yes", boat.25$n_dungeness_retained, boat.25$n_crab_recorded) 

#check no NA crab recorded when Dungeness in possession
check1 = boat.25 %>% filter(is.na(n_crab_recorded) & n_dungeness_retained>0)

#set the number of bootstrap samplings
nboots<-10000

## EUC Calculation and Tables

areas.25=unique(boat.25$marine_area_fished)
boat.25.list=list()
boat.25.success=list()
n.boats.euc=c()
dung.kept.euc=c()
recorded.euc=c()
unrecorded.euc=c()
EUC=c()
N.boats=c()
EUC.boot.mean=c()
EUC.boot.SD=c()
boot.EUC.25=list()
EUC.boot.lowerCI=c()
EUC.boot.upperCI=c()
margin_error=c()


#creating the containers for the bootstraps

for(i in 1:length(areas.25))
{
  boat.25.list[[i]]=boat.25 %>% filter(marine_area_fished==areas.25[i])

  #Successful contacts made in survey
  boat.25.success[[i]] = boat.25.list[[i]] %>% filter(n_dungeness_retained>0)
  n.boats.euc[i]=length(unique(na.omit(boat.25.success[[i]]$Boat_ID)))

  #Dungeness crab retained
  dung.kept.euc[i]=sum(boat.25.success[[i]]$n_dungeness_retained,na.rm=T)
  
  #Crab recorded on CRCs
  recorded.euc[i]=sum(boat.25.success[[i]]$n_crab_recorded,na.rm=T)

  #Crab unrecorded on CRCs
  unrecorded.euc[i]=dung.kept.euc[i]-recorded.euc[i]

  #EUC
  EUC[i]=unrecorded.euc[i]/recorded.euc[i]
  
  
  #complete a bootstrap calculation for the means
 
    boot.EUC.25[[i]]<-data.frame(matrix(NA,nrow=nboots,ncol=3))
 
  for(j in 1:nboots){

    boot_dat<-boat.25.success[[i]][sample(nrow(boat.25.success[[i]]),n.boats.euc[i],replace=T),]
    
    #Dungeness crab retained
    dung.kept.euc.boot=sum(boot_dat$n_dungeness_retained,na.rm=T)
    
    #Crab recorded on CRCs
    recorded.euc.boot=sum(boot_dat$n_crab_recorded,na.rm=T)
    
    #Crab unrecorded on CRCs
    unrecorded.euc.boot=dung.kept.euc.boot-recorded.euc.boot
    
    #EUC
    boot.val=unrecorded.euc.boot/recorded.euc.boot
    
    
    boot.EUC.25[[i]][j,1]=areas.25[i]
    boot.EUC.25[[i]][j,2]=j
    boot.EUC.25[[i]][j,3]=boot.val
  }
  
  N.boats[i]=n.boats.euc[i]
  EUC.boot.mean[i]=mean(boot.EUC.25[[i]][,3],na.rm=T)
  EUC.boot.SD[i]=sd(boot.EUC.25[[i]][,3],na.rm=T)
  EUC.boot.lowerCI[i]=quantile(boot.EUC.25[[i]][,3], probs = 0.025)
  EUC.boot.upperCI[i]=quantile(boot.EUC.25[[i]][,3], probs =  0.975)
  margin_error[i] <- (EUC.boot.upperCI[i]  - EUC.boot.lowerCI[i] ) / 2
  
  
}


#Combine into a Table
Year=2025
EUC.Table.25=as.data.frame(cbind(Year,areas.25,n.boats.euc,dung.kept.euc,unrecorded.euc,recorded.euc,EUC,EUC.boot.mean,EUC.boot.SD,EUC.boot.lowerCI,EUC.boot.upperCI,margin_error))
EUC.Table.25$EUC=as.numeric(EUC.Table.25$EUC)
EUC.Table.25$EUC.boot.mean=as.numeric(EUC.Table.25$EUC.boot.mean)
EUC.Table.25$EUC.boot.SD=as.numeric(EUC.Table.25$EUC.boot.SD)
EUC.Table.25=na.omit(EUC.Table.25)
EUC.Table.25$margin_error=as.numeric(EUC.Table.25$margin_error)
EUC.Table.25$perMOE=EUC.Table.25$margin_error/EUC.Table.25$EUC
EUC.Table.25$CV=EUC.Table.25$EUC.boot.SD/EUC.Table.25$EUC
EUC.Table.25 %<>% left_join(total.contacts, by=c('areas.25'='marine_area_fished'))
EUC.Table.25 %<>% relocate(total.contacts,.before='n.boats.euc')

EUC.Table.25$areas.25<-factor(EUC.Table.25$areas.25,levels=c("6","7", "8_1", "8_2","9", "10", "11", "12"))
EUC.Table.25<-EUC.Table.25 %>% arrange(EUC.Table.25$areas.25)
write.csv(EUC.Table.25,here('EUC','EUC_2025.csv'))

rm(list=ls()) # added MCF -- this is critical to ensure objects aren't accidentally carried across years. 

#######################################################
#######################################################
#######################################################


# 2024 ---------------------------------------------------------------

# Set up 
#Input.directory=paste0("YOUR WORKING DIRECTORY")
# Input.directory=paste0(getwd(),"/Creel Data")
Input.directory=here('EUC','Creel_Data','2024_Creel')

set.seed(12345)

# Define directory and read data in
Input.directory.24=paste0(Input.directory,"/Creel_Data_Shared_2024.xlsx")
boat <- readxl::read_excel(Input.directory.24)
boat.24=as.data.frame(boat)

#Combine 7N and 7S into 7
boat.24$marine_area[boat.24$marine_area=="7N"]="7"
boat.24$marine_area[boat.24$marine_area=="7S"]="7"

#Filter data for summer fishery and MA7 winter during September
boat.24=boat.24 %>% filter(marine_area=="7" & creel_date <= "2024-09-30" | 
                             marine_area=="6" & creel_date <= "2024-09-02" |
                             marine_area=="8_1" & creel_date <= "2024-09-02" |
                             marine_area=="8_2" & creel_date <= "2024-09-02" |
                             marine_area=="9" & creel_date <= "2024-09-02" |
                             marine_area=="10" & creel_date <= "2024-09-02" |
                             marine_area=="11" & creel_date <= "2024-09-02" |
                             marine_area=="12" & creel_date <= "2024-09-02")
#get total number of interviews in these periods
total.contacts.24 <- boat.24 |> group_by(marine_area) |> summarize(total.contacts=length(unique(Boat_ID)))

#Remove boats where CRCs not checked due to reasons other than non-compliance
#non-compliance = refused to show CRCs/did not have them in possession
boat.24 = boat.24 %>% filter(is.na(cards_0) | cards_0=="No CRCs" | cards_0=="No Cooperation")

#Identify over-recorded crab and remove any crab that were over-recorded by making the number of crab recorded equal what was in possession
boat.24$over_recorded.yn <- ifelse(boat.24$n_crab_recorded > boat.24$n_dung_boat, "yes", "no") 
boat.24$over_recorded.yn <- ifelse(is.na(boat.24$over_recorded.yn), "no", boat.24$over_recorded.yn) 
boat.24$n_crab_recorded <- ifelse(boat.24$over_recorded.yn=="yes", boat.24$n_dung_boat, boat.24$n_crab_recorded) 

#check no NA crab recorded when Dungeness in possession
check1 = boat.24 %>% filter(is.na(n_crab_recorded) & n_dung_boat>0)

#set the number of bootstrap samplings
nboots<-10000

## EUC Calculation and Tables

areas.24=unique(boat.24$marine_area)
boat.24.list=list()
boat.24.success=list()
n.boats.euc=c()
dung.kept.euc=c()
recorded.euc=c()
unrecorded.euc=c()
n.crabbers.euc=c()
EUC=c()
N.boats=c()
EUC.boot.mean=c()
EUC.boot.SD=c()           
boot.EUC.24=list()
EUC.boot.lowerCI=c()
EUC.boot.upperCI=c()
margin_error=c()


for(i in 1:length(areas.24)){
  boat.24.list[[i]]=boat.24 %>% filter(marine_area==areas.24[i])
  
  #Successful contacts made in survey
  boat.24.success[[i]] = boat.24.list[[i]] %>% filter(n_dung_boat>0)
  n.boats.euc[i]=length(unique(na.omit(boat.24.success[[i]]$Boat_ID)))
  
  #Dungeness crab retained
  dung.kept.euc[i]=sum(boat.24.success[[i]]$n_dung_boat,na.rm=T)
  
  #Crab recorded on CRCs
  recorded.euc[i]=sum(boat.24.success[[i]]$n_crab_recorded,na.rm=T)
  
  #Crab unrecorded on CRCs
  unrecorded.euc[i]=dung.kept.euc[i]-recorded.euc[i]
  
  #EUC
  EUC[i]=unrecorded.euc[i]/recorded.euc[i]
  
  
  #complete a bootstrap calculation for the means
  
  boot.EUC.24[[i]]<-data.frame(matrix(NA,nrow=nboots,ncol=2))
  
  for(j in 1:nboots){
    
    boot_dat<-boat.24.success[[i]][sample(nrow(boat.24.success[[i]]),n.boats.euc[i],replace=T),]
    
    #Dungeness crab retained
    dung.kept.euc.boot=sum(boot_dat$n_dung_boat,na.rm=T)
    
    #Crab recorded on CRCs
    recorded.euc.boot=sum(boot_dat$n_crab_recorded,na.rm=T)
    
    #Crab unrecorded on CRCs
    unrecorded.euc.boot=dung.kept.euc.boot-recorded.euc.boot
    
    #EUC
    boot.val=unrecorded.euc.boot/recorded.euc.boot
    
    boot.EUC.24[[i]][j,1]=areas.24[i]
    boot.EUC.24[[i]][j,2]=j
    boot.EUC.24[[i]][j,3]=boot.val
  }
  
  N.boats[i]=n.boats.euc[i]
  EUC.boot.mean[i]=mean(boot.EUC.24[[i]][,3],na.rm=T)
  EUC.boot.SD[i]=sd(boot.EUC.24[[i]][,3],na.rm=T)
  EUC.boot.lowerCI[i]=quantile(boot.EUC.24[[i]][,3], probs = 0.025)
  EUC.boot.upperCI[i]=quantile(boot.EUC.24[[i]][,3], probs =  0.975)
  margin_error[i] <- (EUC.boot.upperCI[i]  - EUC.boot.lowerCI[i] ) / 2

}

#Combine into a Table
Year=2024
EUC.Table.24=as.data.frame(cbind(Year,areas.24,n.boats.euc,dung.kept.euc,unrecorded.euc,recorded.euc,EUC,EUC.boot.mean,EUC.boot.SD,EUC.boot.lowerCI,EUC.boot.upperCI,margin_error))
EUC.Table.24$EUC=as.numeric(EUC.Table.24$EUC)
EUC.Table.24$EUC.boot.mean=as.numeric(EUC.Table.24$EUC.boot.mean)
EUC.Table.24$EUC.boot.SD=as.numeric(EUC.Table.24$EUC.boot.SD)
EUC.Table.24=na.omit(EUC.Table.24)
EUC.Table.24$margin_error=as.numeric(EUC.Table.24$margin_error)
EUC.Table.24$perMOE=EUC.Table.24$margin_error/EUC.Table.24$EUC
EUC.Table.24$CV=EUC.Table.24$EUC.boot.SD/EUC.Table.24$EUC
EUC.Table.24 %<>% left_join(total.contacts.24, by=c('areas.24'='marine_area'))
EUC.Table.24 %<>% relocate(total.contacts,.before='n.boats.euc')


EUC.Table.24$areas.24<-factor(EUC.Table.24$areas.24,levels=c("6","7", "8_1", "8_2","9", "10", "11", "12"))
EUC.Table.24<-EUC.Table.24 %>% arrange(EUC.Table.24$areas.24)
write.csv(EUC.Table.24,here('EUC','EUC_2024.csv'))

rm(list=ls()) # added MCF -- this is critical to ensure objects aren't accidentally carried across years. 

#######################################################
#######################################################
#######################################################

# 2023 --------------------------------------------------------------------


# Set up 
#Input.directory=paste0("YOUR WORKING DIRECTORY")
# Input.directory=paste0(getwd(),"/Creel Data")
Input.directory=here('EUC','Creel_Data','2023_Creel')

set.seed(12345)

# Define input directory and read in
Input.directory.23=paste0(Input.directory,"/Creel_Data_Shared_2023_Updated.xlsx")
boat <- readxl::read_excel(Input.directory.23)
boat.23 = as.data.frame(boat)

#check no NA crab recorded when Dungeness in possession
check1 = boat.23 %>% filter(is.na(crab_recorded_crc) & n_dung_crab>0)
#Convert these missing values to 0
boat.23$crab_recorded_crc[is.na(boat.23$crab_recorded_crc) & boat.23$n_dung_crab>0]=0

#Combine 7N and 7S into 7
boat.23$marine_area[boat.23$marine_area=="7N"]="7"
boat.23$marine_area[boat.23$marine_area=="7S"]="7"

#Filter data for summer fishery and MA7 winter during September
boat.23=boat.23 %>% filter(marine_area=="7" & creel_date <= "2023-09-30" | 
                             marine_area=="6" & creel_date <= "2023-09-04" |
                             marine_area=="8_1" & creel_date <= "2023-09-04" |
                             marine_area=="8_2" & creel_date <= "2023-09-04" |
                             marine_area=="9" & creel_date <= "2023-09-04" |
                             marine_area=="10" & creel_date <= "2023-09-04" |
                             marine_area=="11" & creel_date <= "2023-09-04" |
                             marine_area=="12" & creel_date <= "2023-09-04")
#get total number of interviews in these periods
total.contacts.23 <- boat.23 |> group_by(marine_area) |> summarize(total.contacts=length(unique(boat_intveriew_ID)))

#Remove the last columns (purpose for de-duplication to boat level) 
boat.23=boat.23[,-c(34:43)]

#Reduce flat file down to the boat level (Remove duplicate entries - product of a flat file)
boat.23=boat.23[!duplicated(boat.23),]

### ### ###
## match 2023 data to categories in 2024,2025 data
boat.23 %<>%
  mutate(n_crcs_checked=case_when(
    ## no crab
    is.na(n_dung_crab) | n_dung_crab==0 ~ NA,
    ## checked CRC
    # crc_possession_yn=='yes' & crc_permission_yn=='yes' & !is.na(n_crab_recorded) ~ 1, ## updated 1/23/2026. affects 2 records.
    crc_possession_yn=='yes' & crc_permission_yn=='yes' ~ 1,
    ## checked CRC (sometimes permission column is NA)
    crc_possession_yn=='yes' & is.na(crc_permission_yn) & !is.na(n_dung_crab) ~ 1,
    ## didn't check CRC (no permission)
    crc_possession_yn=='yes' & crc_permission_yn=='no' ~ 0,
    ## didn't check CRC (despite possession and permission)  ## merged with "checked CRC" on 1/23/2026
    # crc_possession_yn=='yes' & crc_permission_yn=='yes' & is.na(n_crab_recorded) ~ 0,
    ## didn't check CRC (no CRC in possession)
    crc_possession_yn=='no' ~ 0,
    .default=99 ## use to check for errors
  ),
  no_crcs_checked_reason=case_when(
    ## no crab
    is.na(n_dung_crab) | n_dung_crab==0 ~ NA,
    ## checked CRC
    # crc_possession_yn=='yes' & crc_permission_yn=='yes' & !is.na(n_crab_recorded) ~ NA, ## updated 1/23/2026. affects 2 records.
    crc_possession_yn=='yes' & crc_permission_yn=='yes' ~ NA,
    ## checked CRC (sometimes permission column is NA)
    crc_possession_yn=='yes' & is.na(crc_permission_yn) & !is.na(n_dung_crab) ~ NA,
    ## didn't check CRC (despite possession and permission) ## merged with "checked CRC" on 1/23/2026
    # crc_possession_yn=='yes' & crc_permission_yn=='yes' & is.na(n_crab_recorded) ~ 'Not Checking CRCs',
    ## didn't check CRC (no permission)
    crc_possession_yn=='yes' & crc_permission_yn=='no' ~ 'No Cooperation',
    ## didn't check CRC (no CRC in possession)
    crc_possession_yn=='no' ~ 'No CRCs',
    .default='99' ## use to check for errors
  ))

#check for errors
# View(filter(boat.23, n_crcs_checked==99))
# View(filter(boat.23, no_crcs_checked_reason==99))

# 2) Remove boats where CRCs not checked due to reasons other than non-compliance - we can skip this for 2023
#are there cases when creelers weren't checking CRCs? NO 
boat.23 |> group_by(n_crcs_checked, no_crcs_checked_reason) |> 
  summarise(n=n(),b=length(unique(boat_intveriew_ID)))

#convert to boat level
boat.23=boat.23 %>%
  group_by(boat_intveriew_ID, marine_area) %>%
  summarise(n_dung_crab=sum(n_dung_crab,na.rm=T),
            crab_recorded_crc=sum(crab_recorded_crc,na.rm=T))

#Identify over-recorded crab and remove any crab that were over-recorded by making the number of crab recorded equal what was in possession
boat.23$over_recorded.yn <- ifelse(boat.23$crab_recorded_crc > boat.23$n_dung_crab, "yes", "no") 
boat.23$over_recorded.yn <- ifelse(is.na(boat.23$over_recorded.yn), "no", boat.23$over_recorded.yn) 
boat.23$crab_recorded_crc <- ifelse(boat.23$over_recorded.yn=="yes", boat.23$n_dung_crab, boat.23$crab_recorded_crc) 

#set the number of bootstrap samplings
nboots<-10000


areas.23=unique(boat.23$marine_area)
boat.23.list=list()
boat.23.success=list()
n.boats.euc=c()
dung.kept.euc=c()
recorded.euc=c()
unrecorded.euc=c()
EUC=c()
N.boats=c()
EUC.boot.mean=c()
EUC.boot.SD=c()
boot.EUC.23=list()
EUC.boot.lowerCI=c()
EUC.boot.upperCI=c()
margin_error=c()



for(i in 1:length(areas.23))
{
  boat.23.list[[i]]=boat.23 %>% filter(marine_area==areas.23[i])
  
  #Successful contacts made in survey
  boat.23.success[[i]] = boat.23.list[[i]] %>% filter(n_dung_crab>0)
  n.boats.euc[i]=length(unique(na.omit(boat.23.success[[i]]$boat_intveriew_ID)))
  
  #Dungeness crab retained
  dung.kept.euc[i]=sum(boat.23.success[[i]]$n_dung_crab,na.rm=T)
  
  #Crab recorded on CRCs
  recorded.euc[i]=sum(boat.23.success[[i]]$crab_recorded_crc,na.rm=T)
  
  #Crab unrecorded on CRCs
  unrecorded.euc[i]=dung.kept.euc[i]-recorded.euc[i]
  
  #EUC
  EUC[i]=unrecorded.euc[i]/recorded.euc[i]
  
  
  
  #complete a bootstrap calculation for the means
  
  boot.EUC.23[[i]]<-data.frame(matrix(NA,nrow=nboots,ncol=2))
  
  for(j in 1:nboots){
    
    boot_dat<-boat.23.success[[i]][sample(nrow(boat.23.success[[i]]),n.boats.euc[i],replace=T),]
    
    #Dungeness crab retained
    dung.kept.euc.boot=sum(boot_dat$n_dung_crab,na.rm=T)
    
    #Crab recorded on CRCs
    recorded.euc.boot=sum(boot_dat$crab_recorded_crc,na.rm=T)
    
    #Crab unrecorded on CRCs
    unrecorded.euc.boot=dung.kept.euc.boot-recorded.euc.boot
    
    #EUC
    boot.val=unrecorded.euc.boot/recorded.euc.boot
    
    boot.EUC.23[[i]][j,1]=areas.23[i]
    boot.EUC.23[[i]][j,2]=j
    boot.EUC.23[[i]][j,3]=boot.val
  }
  
  N.boats[i]=n.boats.euc[i]
  EUC.boot.mean[i]=mean(boot.EUC.23[[i]][,3],na.rm=T)
  EUC.boot.SD[i]=sd(boot.EUC.23[[i]][,3],na.rm=T) 
  EUC.boot.lowerCI[i]=quantile(boot.EUC.23[[i]][,3], probs = 0.025)
  EUC.boot.upperCI[i]=quantile(boot.EUC.23[[i]][,3], probs =  0.975)
  margin_error[i] <- (EUC.boot.upperCI[i]  - EUC.boot.lowerCI[i] ) / 2
}

#Combine into a Table
Year=2023
EUC.Table.23=as.data.frame(cbind(Year,areas.23,n.boats.euc,dung.kept.euc,unrecorded.euc,recorded.euc,EUC,EUC.boot.mean,EUC.boot.SD,EUC.boot.lowerCI,EUC.boot.upperCI,margin_error))
EUC.Table.23$EUC=as.numeric(EUC.Table.23$EUC)
EUC.Table.23$EUC.boot.mean=as.numeric(EUC.Table.23$EUC.boot.mean)
EUC.Table.23$EUC.boot.SD=as.numeric(EUC.Table.23$EUC.boot.SD)
EUC.Table.23=na.omit(EUC.Table.23)
EUC.Table.23$margin_error=as.numeric(EUC.Table.23$margin_error)
EUC.Table.23$perMOE=EUC.Table.23$margin_error/EUC.Table.23$EUC
EUC.Table.23$CV=EUC.Table.23$EUC.boot.SD/EUC.Table.23$EUC
EUC.Table.23 %<>% left_join(total.contacts.23, by=c('areas.23'='marine_area'))
EUC.Table.23 %<>% relocate(total.contacts,.before='n.boats.euc')

EUC.Table.23$areas.23<-factor(EUC.Table.23$areas.23,levels=c("6","7", "8_1", "8_2","9", "10", "11", "12"))
EUC.Table.23<-EUC.Table.23 %>% arrange(EUC.Table.23$areas.23)
write.csv(EUC.Table.23,here('EUC','EUC_2023.csv'))

##############################################
# working up some plots to visualize the data distributions


#2025
boot.df.25<-as.data.frame(do.call(rbind,boot.EUC.25))
colnames(boot.df.25)<-c('areas.25','iter','EUC')
boot.df.25$EUC<-as.numeric(boot.df.25$EUC)
boot.df.25$areas.25<-factor(boot.df.25$areas.25,levels=c("6","7", "8_1", "8_2","9", "10", "11", "12"))


ggplot(boot.df.25, aes(x=EUC)) +
  geom_histogram(fill='grey70',col='black')+
  geom_vline(data=EUC.Table.25, aes(xintercept=EUC,group=areas.25,colour = 'EUC'),lwd=0.8)+
  geom_vline(data=EUC.Table.25, aes(xintercept=EUC.boot.mean,group=areas.25,color='EUC boot'),lwd=0.8)+
  scale_color_manual(values = c("EUC" = "red", "EUC boot" = "blue"), name = "Source") +
  theme_bw()+
  ggtitle('2025 Dataset')+
  facet_wrap(.~areas.25,ncol=1)


#2024
boot.df.24<-as.data.frame(do.call(rbind,boot.EUC.24))
colnames(boot.df.24)<-c('areas.24','iter','EUC')
boot.df.24$EUC<-as.numeric(boot.df.24$EUC)
boot.df.24$EUC<-as.numeric(boot.df.24$EUC)
boot.df.24$areas.24<-factor(boot.df.24$areas.24,levels=c("6","7", "8_1", "8_2","9", "10", "11", "12"))


ggplot(boot.df.24, aes(x=EUC)) +
  geom_histogram(fill='grey70',col='black')+
  geom_vline(data=EUC.Table.24, aes(xintercept=EUC,group=areas.24,colour = 'EUC'),lwd=0.8)+
  geom_vline(data=EUC.Table.24, aes(xintercept=EUC.boot.mean,group=areas.24,color='EUC boot'),lwd=0.8)+
  scale_color_manual(values = c("EUC" = "red", "EUC boot" = "blue"), name = "Source") +
  theme_bw()+
  ggtitle('2024 Dataset')+
  facet_wrap(.~areas.24,ncol=1)


#2023
boot.df.23<-as.data.frame(do.call(rbind,boot.EUC.23))
colnames(boot.df.23)<-c('areas.23','iter','EUC')
boot.df.23$EUC<-as.numeric(boot.df.23$EUC)
boot.df.23$areas.23<-factor(boot.df.23$areas.23,levels=c("6","7", "8_1", "8_2","9", "10", "11", "12"))

ggplot(boot.df.23, aes(x=EUC)) +
  geom_histogram(fill='grey70',col='black')+
  geom_vline(data=EUC.Table.23, aes(xintercept=EUC,group=areas.23,colour = 'EUC'),lwd=0.8)+
  geom_vline(data=EUC.Table.23, aes(xintercept=EUC.boot.mean,group=areas.23,color='EUC boot'),lwd=0.8)+
  scale_color_manual(values = c("EUC" = "red", "EUC boot" = "blue"), name = "Source") +
  theme_bw()+
  ggtitle('2023 Dataset')+
  facet_wrap(.~areas.23,ncol=1)


