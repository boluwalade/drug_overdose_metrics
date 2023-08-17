library(tidyverse)
library(janitor)
library(lubridate)
library(eeptools)


patients <- read_csv("datasets/patients.csv") %>% clean_names()
encounters <- read_csv("datasets/encounters.csv") %>% clean_names()
allergies <- read_csv("datasets/allergies.csv") %>% clean_names()
medications <- read_csv("datasets/medications.csv") %>% clean_names()
procedures <- read_csv("datasets/procedures.csv") %>% clean_names()


#--------------------------------------------------#
#           DRUG OVERDOSE ENCOUNTER COHORT         #
#--------------------------------------------------#

#Rename encounter_id
encounters <- encounters %>% 
  rename(encounter_id = id)

#Create Drug Overdose encounter cohort
drug_overdose_coht <- patients %>%
  select(patient_id = id,birthdate, deathdate) %>%  #rename id as patient_id
  inner_join(encounters, by = c("patient_id" = "patient")) %>% #inner join patients and encounters
  filter(reasondescription == "Drug overdose") %>% #filter drug overdose encounter
  mutate(encounter_start_date = date(ymd_hms(start)),
         encounter_stop_date = date(ymd_hms(stop)),
         birthdate = ymd(birthdate),
         age = floor(age_calc(birthdate,encounter_stop_date,
                              units = "years"))) %>% #calculate patient age at the time of encounter
  filter(encounter_stop_date > ymd("1999-07-15")) %>% #encounter occurred after July 15,1999
  filter(age %in% c(18:35)) %>%  #patient age between 18 and 35
  select(-birthdate) %>%  #remove birthdate variables
  distinct(.,.keep_all = T)


#----------------------------------------#
#     CREATE ADDITIONAL FIELDS           #
#----------------------------------------#


# 1. DEATH_AT_VISIT_IND

drug_overdose_coht <- drug_overdose_coht %>% 
  mutate(DEATH_AT_VISIT_IND = case_when(deathdate %within%
                                          interval(encounter_start_date,encounter_stop_date) ~ 1,
                                        TRUE ~ 0)) #death date within encounter date

# 2. CURRENT MEDS COUNT

drug_overdose_coht_meds <- drug_overdose_coht %>% 
  inner_join(select(medications,
                    patient,
                    medication_description =description,
                    medication_start_date = start,
                    medication_stop_date = stop),
             by = c("patient_id" = "patient"))

drug_overdose_coht$patient_id %>% unique() %>% length()
drug_overdose_coht_meds$patient_id %>% unique() %>% length()

#Note: 7 patients had no medications

drug_overdose_coht_meds <- drug_overdose_coht_meds %>% 
  mutate(meds_indicator= ifelse(is.na(medication_stop_date) & 
                                  medication_start_date < encounter_start_date,1,
                                ifelse(!is.na(medication_stop_date) &
                                         encounter_start_date %within%
                                         interval(medication_start_date,
                                                  medication_stop_date),1,0))) %>%
  group_by(patient_id,encounter_start_date) %>% 
  mutate(COUNT_CURRENT_MEDS = sum(meds_indicator)) %>% 
  ungroup() %>% 
  select(-meds_indicator)


# 3. CURRENT OPIOID INDICATOR

#opiod filter
opiod_list <- paste("Oxycodone-acetaminophen 100ML","Hydromorphone 325 MG","Fentanyl 100 MCG",sep = "|")


drug_overdose_coht_meds <- drug_overdose_coht_meds %>% 
  mutate(CURRENT_OPIOID_IND = ifelse(is.na(medication_stop_date) & 
                                       medication_start_date < encounter_start_date&
                                       str_detect(medication_description,opiod_list),1,
                                     ifelse(!is.na(medication_stop_date) &
                                              encounter_start_date %within%
                                              interval(medication_start_date,
                                                       medication_stop_date) &
                                              str_detect(medication_description,opiod_list),1,0))) %>% 
  select(patient_id,encounter_id,deathdate,
         medication_start_date,medication_stop_date,COUNT_CURRENT_MEDS,CURRENT_OPIOID_IND) %>% 
  distinct(.,.keep_all = T)

#Note:Patient id (3b3b7748-a006-466d-9788-b3d6e922ffc4)  was prescribed opioids after death


#4,5,6 READMISSION_90_DAY_IND, READMISSION_30_DAY_IND,FIRST_READMISSION_DATE

drug_overdose_coht_readmm <- drug_overdose_coht %>%
  group_by(patient_id) %>%
  arrange(encounter_start_date,.by_group = T) %>% 
  mutate(days_diff = encounter_start_date- lag(encounter_stop_date),#diff between current encounter start and previous encounter stop date
         days_diff2 = lead(days_diff),
         lag_day = lag(encounter_stop_date),
         READMISSION_90_DAY_IND = case_when(days_diff2 <= 90 ~ 1, #new encounter within 90 days of an old encounter
                                            TRUE ~ 0),
         READMISSION_30_DAY_IND = case_when(days_diff2 <= 30 ~ 1, #new encounter within 30 days of an old encounter
                                            TRUE ~ 0),
         FIRST_READMISSION_DATE = ifelse(READMISSION_90_DAY_IND == 1,
                                         sort(as.Date(encounter_start_date))[2],NA)) %>% # second minimum date for a drug overdose encounter
  select(patient_id,encounter_id,age,READMISSION_90_DAY_IND,encounter_start_date,encounter_stop_date,
         DEATH_AT_VISIT_IND,READMISSION_30_DAY_IND,FIRST_READMISSION_DATE)

#convert to date format
drug_overdose_coht_readmm$FIRST_READMISSION_DATE <- as.Date(drug_overdose_coht_readmm$FIRST_READMISSION_DATE,
                                                            origin = "1970-01-01") 

#Fill up FIRST_READMISSION_DATE field with appropriate values
drug_overdose_coht_readmm<- drug_overdose_coht_readmm %>%
  group_by(patient_id) %>%
  fill(FIRST_READMISSION_DATE) %>%
  fill(FIRST_READMISSION_DATE, .direction = "up")


#-----------------------------------------#
#       CREATE AND EXPORT FINAL DATASET   #
#-----------------------------------------#

#join drug_overdose_coht_readmm data set with drug_overdose_coht_meds data set

drug_overdose_coht_final<- drug_overdose_coht_readmm %>%
  left_join(select(drug_overdose_coht_meds,-encounter_id),by = c("patient_id")) %>% 
  mutate(across(c(CURRENT_OPIOID_IND,COUNT_CURRENT_MEDS),~replace_na(.,0))) %>% #replace NA with 0 in patients without medications 
  #select and rename fields
  select(PATIENT_ID =patient_id,
         ENCOUNTER_ID = encounter_id,
         HOSPITAL_ENCOUNTER_DATE = encounter_start_date,
         AGE_AT_VISIT = age,DEATH_AT_VISIT_IND,
         COUNT_CURRENT_MEDS,CURRENT_OPIOID_IND,READMISSION_90_DAY_IND,
         READMISSION_30_DAY_IND,FIRST_READMISSION_DATE) %>% 
  distinct(.,.keep_all = T)

write.csv(drug_overdose_coht_final,"drug_overdose_coht_final.csv")



