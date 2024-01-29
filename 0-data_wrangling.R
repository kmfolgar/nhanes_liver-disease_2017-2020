## Data Processing
library(tidyverse)
library(haven)
library(janitor)
library(nhanesA)

## Demographics 
demo_p <- read_xpt("data/P_DEMO.XPT") %>% 
            select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH3, DMDEDUC2, INDFMPIR, 
                   RIDSTATR, SDMVPSU, SDMVSTRA, WTMECPRP) %>% 
            nhanesTranslate("P_DEMO", colnames = colnames(.), data=.)

saveRDS(demo_p, "data/translated_nhanes/demo_p.rds")


## BMI
bmx_p <- read_xpt("data/P_BMX.XPT") %>% 
          select(SEQN, BMXBMI) #%>% 
          #nhanesTranslate("BMX_J", colnames = colnames(.), data=.)

saveRDS(bmx_p, "data/translated_nhanes/bmx_p.rds")

## Diabetes
diq_p <- read_xpt("data/P_DIQ.XPT") %>% 
          select(SEQN, DIQ010, DIQ050, DIQ070) %>% 
          nhanesTranslate("P_DIQ", colnames = colnames(.), data=.)

saveRDS(diq_p, "data/translated_nhanes/diq_p.rds")

## Glycohemoglobin
ghb_p <- read_xpt("data/P_GHB.XPT") %>% 
          select(SEQN, LBXGH) #%>% 
            #nhanesTranslate("GHB_J", colnames = colnames(.), data=.)

saveRDS(ghb_p, "data/translated_nhanes/ghb_p.rds")

## Alcohol Use Questionnaire
alq_p <-  read_xpt("data/P_ALQ.XPT") %>% 
            select(SEQN, ALQ121, ALQ130, ALQ142, ALQ111) %>% 
            nhanesTranslate("P_ALQ", colnames = colnames(.), data=.)

saveRDS(alq_p, "data/translated_nhanes/alq_p.rds")

## Hepatitis B: Core antibody, Surface antigen, and Hepatitis D antibody
hep_b_1 <- read_xpt("data/P_HEPBD.XPT") %>% 
            select(SEQN, LBXHBC, LBDHBG) %>% 
            nhanesTranslate("P_HEPBD", colnames = colnames(.), data=.)

saveRDS(hep_b_1, "data/translated_nhanes/hep_b_1.rds")

## Hepatitis B: Surface Antibody
hep_b_2 <- read_xpt("data/P_HEPB_S.XPT") %>% 
            select(SEQN, LBXHBS) %>% 
            nhanesTranslate("P_HEPB_S", colnames = colnames(.), data=.)

saveRDS(hep_b_2, "data/translated_nhanes/hep_b_2.rds")


## Hepatitis C: RNA (HCV-RNA), Confirmed Antibody (INNO-LIA), & Genotype 
hep_c <- read_xpt("data/P_HEPC.XPT") %>% 
          select(SEQN, LBDHCI) %>% 
          nhanesTranslate("P_HEPC", colnames = colnames(.), data=.)

saveRDS(hep_c, "data/translated_nhanes/hep_c.rds")

## Standard Biochemistry Profile
biopro_p <- read_xpt("data/P_BIOPRO.XPT") %>% 
              select(SEQN, LBXSATSI, LBXSASSI) #%>% 
              #nhanesTranslate("BIOPRO_J", colnames = colnames(.), data=.)

saveRDS(biopro_p, "data/translated_nhanes/biopro_p.rds")

## Liver Ultrasound Transient Elastography
lux_p <- read_xpt("data/P_LUX.XPT") %>%
          select(SEQN, LUXSMED, LUXCAPM, LUAXSTAT) %>% 
          nhanesTranslate("P_LUX", colnames = colnames(.), data=.)

saveRDS(lux_p, "data/translated_nhanes/lux_p.rds")

## Hospital Utilization & Access to Care
huq_p <- read_xpt("data/P_HUQ.XPT") %>% 
          select(SEQN, HUQ030) %>% 
          nhanesTranslate("P_HUQ", colnames = colnames(.), data=.)

saveRDS(huq_p, "data/translated_nhanes/huq_p.rds")

## Health Insurance
hiq_p <- read_xpt("data/P_HIQ.XPT") %>% 
          select(SEQN, HIQ011) %>% 
          nhanesTranslate("P_HIQ", colnames = colnames(.), data=.)

saveRDS(hiq_p, "data/translated_nhanes/hiq_p.rds")


mcq_p <- read_xpt("data/P_MCQ.XPT") %>% 
          select(SEQN,MCQ160L) %>% 
          nhanesTranslate("P_MCQ", colnames = colnames(.), data=.)

saveRDS(mcq_p, "data/translated_nhanes/mcq_p.rds")


demo_p %>% 
  left_join(bmx_p, by="SEQN") %>% 
  left_join(diq_p, by="SEQN") %>% 
  left_join(ghb_p, by="SEQN") %>% 
  left_join(alq_p, by="SEQN") %>% 
  left_join(hep_b_1, by="SEQN") %>% 
  left_join(hep_b_2, by="SEQN") %>%
  left_join(hep_c, by="SEQN") %>%
  left_join(biopro_p, by="SEQN") %>%
  left_join(huq_p, by="SEQN") %>% 
  left_join(hiq_p, by="SEQN") %>% 
  left_join(mcq_p, by="SEQN") %>% 
  left_join(lux_p, by="SEQN") -> nhanes

rm(list = ls()[grepl("_", ls())])

## DEMOGRAPHICS
nhanes %>% 
  mutate(age = RIDAGEYR,
       ## Age
       age1 = case_when(#RIDAGEYR>=18 & RIDAGEYR<20  ~ "18-20",
         RIDAGEYR>=20 & RIDAGEYR<40  ~ "20-39",
         RIDAGEYR>=20 & RIDAGEYR<60  ~ "40-59",
         RIDAGEYR>=60 ~ "60+"),
       ## Gender
       gender= RIAGENDR,
       ## Race/Ethnicity
       race= case_when( RIDRETH3=="Non-Hispanic White"               ~ "Non-Hispanic White",
                        RIDRETH3=="Mexican American"                 ~ "Hispanic",
                        RIDRETH3=="Other Hispanic"                   ~ "Hispanic",
                        RIDRETH3=="Non-Hispanic Black"               ~ "Non-Hispanic Black",
                        RIDRETH3=="Non-Hispanic Asian"               ~ "Non-Hispanic Asian",
                        RIDRETH3=="Other Race - Including Multi-Rac" ~ "Other Race"),
       
       ## Education - 3 categories
       education3=case_when(DMDEDUC2=="Less than 9th grade"              ~ "Less than high school",
                           DMDEDUC2=="9-11th grade (Includes 12th grad" ~ "Less than high school",
                           DMDEDUC2=="High school graduate/GED or equi" ~ "Completed High school",
                           DMDEDUC2=="Some college or AA degree"        ~ "Some college or above",
                           DMDEDUC2=="College graduate or above"        ~ "Some college or above",
                           DMDEDUC2=="Don't Know" | DMDEDUC2=="Refuse"  ~ NA_character_),
       
       education=case_when(DMDEDUC2=="Less than 9th grade"              ~ "Less than high school",
                            DMDEDUC2=="9-11th grade (Includes 12th grad" ~ "Less than high school",
                            DMDEDUC2=="High school graduate/GED or equi" ~ "Completed High school or above",
                            DMDEDUC2=="Some college or AA degree"        ~ "Completed High school or above",
                            DMDEDUC2=="College graduate or above"        ~ "Completed High school or above",
                            DMDEDUC2=="Don't Know" | DMDEDUC2=="Refuse"  ~ NA_character_),
       
       ## Poverty
       poverty = case_when(INDFMPIR <2 ~ "Yes",
                           INDFMPIR>=2 ~ "No"),
       
       ## Health insurance
       health_insurance = case_when(HIQ011=="No" ~ "No", 
                                    HIQ011=="Yes" ~ "Yes"),
       
       ## Regular access healthcare 
       regular_healthcare = case_when(HUQ030=="There is no place" ~ "No",
                                      HUQ030=="There is more than one place" | 
                                      HUQ030=="Yes" ~ "Yes"),
       ### Overall Health Insurance + Access to Healthcare
       access_healthcare = if_else(health_insurance==1 & regular_healthcare==1, "Yes", "No"),
       
       ## RISK FACTORS 
       ## BMI
       #bmi = case_when(BMXBMI<18.5              ~ "Underweight",
      #                 BMXBMI>=18.5 & BMXBMI<25 ~ "Normal",
      #                 BMXBMI>=25   & BMXBMI<30 ~ "Overweight",
      #                 BMXBMI>=30               ~ "Obesity"),
      bmi=BMXBMI,
      
       ##Diabetes
       Diabetes_Doctor = case_when(DIQ010=="Yes"         ~ "Yes",
                                   DIQ010=="No"          ~ "No",
                                   DIQ010=="Borderline"  ~ "No"),
       
       Diabetes_Meds= case_when(DIQ050=="Yes" | DIQ070=="Yes" ~ "Yes",
                                DIQ050=="No"  & DIQ070=="No"  ~ "No"),
       
       Diabetes_Lab=case_when(LBXGH>=6.5 ~ "Yes" ,
                              LBXGH<6.5  ~ "No" ),
       
       diabetes = case_when(Diabetes_Doctor =="Yes" | 
                              Diabetes_Meds   =="Yes" | 
                              Diabetes_Lab    =="Yes" ~ "Yes",
                            Diabetes_Doctor =="No"  & 
                              Diabetes_Lab    =="No"  ~ "No"),
       ## Known Hepatic Disease
       aware_liver_disease = case_when(MCQ160L=="Yes" ~ "Yes",
                                       MCQ160L=="No" ~ "No"),
       
       ## Hepatitis 
       HepB_Status = case_when(LBXHBC=="Negative" & 
                                 LBXHBS=="Negative" ~ "No Hepatitis B",
                               LBXHBC=="Positive" & 
                                 LBXHBS=="Negative" ~ "Past Hepatitis B",
                               LBXHBC=="Negative" & 
                                 LBXHBS=="Positive" ~ "Immune",
                               LBXHBC=="Positive" & 
                                 LBDHBG=="Positive" ~ "Chronic Hepatitis B"),
       HepB_Status2 = case_when(HepB_Status=="No Hepatitis B" ~ "No Hepatitis B",
                                HepB_Status=="Past Hepatitis B" ~ "Exposure to HBV",
                                HepB_Status=="Chronic Hepatitis B" ~ "Exposure to HBV",
                                HepB_Status=="Immune" ~ "Immune"),
       
       HepC = case_when(LBDHCI=="Positive"                        ~ "Positive Anti-HCV but no RNA",
                        LBDHCI=="Negative" | 
                          LBDHCI=="Negative Screening HCV Antibody" ~ "No Hepatitis C",
                        LBDHCI=="Positive HCV RNA"                ~ "Current Infection"),
       
       viral = case_when(HepB_Status=="Past Hepatitis B" |
                           HepB_Status=="Chronic Hepatitis B" |
                           HepC=="Positive Anti-HCV but no RNA" |
                           HepC=="Current Infection" ~ "Yes", 
                         (HepB_Status=="No Hepatitis B" |
                            HepB_Status=="Immune") &
                           HepC=="No Hepatitis C"~ "No"),
       
       ## Alcohol 
       ## Times per week
       alcohol_factor = case_when(ALQ121=="Every day" ~ 7,
                                  ALQ121=="Nearly every day" ~ 5.5,
                                  ALQ121=="3 to 4 times a week" ~ 3.5,
                                  ALQ121=="2 times a week" ~ 2,
                                  ALQ121=="Once a week" ~ 1,
                                  ALQ121=="2 to 3 times a month" ~ 0.625,
                                  ALQ121=="Once a month" ~ 0.25,
                                  ALQ121=="7 to 11 times in the last year" ~ 0.20,
                                  ALQ121=="3 to 6 times in the last year" ~ 0.10,
                                  ALQ121=="1 to 2 times in the last year" ~ 0.052),
       ## Avg Drinks per Time
       alcohol_times = if_else(ALQ130==777 | ALQ130==999, NA_real_, ALQ130),
       ## Drinks per time per Week 
       alcohol_drinks_week = (alcohol_times * alcohol_factor),
       
       ## Heavy Drinkers Definition: 
       ##### >3 drinks/day OR >5 drinks in a single day PER MONTH (MEN)
       ##### >2 drinks/day OR >4 drinks in a single day PER MONTH (WOMEN)
       step1_heavy_drinkers = case_when((gender=="Female" & alcohol_drinks_week>14) | 
                                          (gender=="Male" & alcohol_drinks_week>21) ~ 1),
       step3_heavy_drinkers = case_when((ALQ142=="Every day" | 
                                           ALQ142=="Nearly every day" | 
                                           ALQ142=="3 to 4 times a week" |
                                           ALQ142=="2 times a week" | 
                                           ALQ142=="Once a week" | 
                                           ALQ142=="2 to 3 times a month" |
                                           ALQ142=="Once a month") ~ 1),
       heavy_drinkers = case_when(step1_heavy_drinkers==1 | step3_heavy_drinkers==1 ~ 1),
       
       ## Moderate Drinkers Definition: 
       ##### <= 3 drinks/day AND <5 drinks in a single day per month (MEN)
       ##### <= 2 drinks/ day AND <4 drinks in a single day per month (WOMEN)
       step1_moderate_drinkers = case_when((gender=="Female" & alcohol_drinks_week>=7 & alcohol_drinks_week<=14) | 
                                             (gender=="Male" & alcohol_drinks_week>=7  & alcohol_drinks_week<=21) ~ 1),
       step3_moderate_drinkers = case_when((ALQ142=="7 to 11 times in the last year" |
                                              ALQ142=="3 to 6 times in the last year" |
                                              ALQ142=="1 to 2 times in the last year" |
                                              ALQ142=="Never in the last year") ~ 1),
       moderate_drinkers = case_when(step1_moderate_drinkers==1 & step3_moderate_drinkers==1 ~ 1),
       ## Ocassional Drinkers
       #### <1 drinking day per week AND <5 drinks in a single day per month (MEN)
       #### <1 drinking day per week AND <4 drinks in a single day per month (WOMEN)
       step1_ocassional_drinkers = case_when((gender=="Female" & alcohol_drinks_week<7) | 
                                               (gender=="Male" & alcohol_drinks_week<7) ~ 1),
       step3_ocassional_drinkers = case_when((ALQ142=="7 to 11 times in the last year" | 
                                                ALQ142=="3 to 6 times in the last year" | 
                                                ALQ142=="1 to 2 times in the last year" | 
                                                ALQ142=="Never in the last year") ~ 1),
       ocassional_drinkers = case_when(step1_ocassional_drinkers==1 & step3_ocassional_drinkers==1 ~ 1),
       
       ## Alcohol_Variable
       alcohol_variable1 = case_when(ALQ111=="No"  ~ "Lifetime abstainers", 
                                    # Lifetime abstainers:   <12 drinks in their lifetime
                                    ALQ111=="Yes" & ALQ121=="Never in the last year" ~ "Current abstainers", 
                                    #Current abstainers:  No drinks in the past 12 months but drank in the past.
                                    heavy_drinkers==1 ~ "Heavy drinkers", 
                                    moderate_drinkers==1 ~ "Moderate drinkers",
                                    ocassional_drinkers==1 ~ "Occasional drinkers"),
      alcohol_variable = case_when(ALQ111=="No"  ~ "Lifetime abstainers", 
                                    # Lifetime abstainers:   <12 drinks in their lifetime
                                    ALQ111=="Yes" & ALQ121=="Never in the last year" ~ "Current abstainers", 
                                    #Current abstainers:  No drinks in the past 12 months but drank in the past.
                                    heavy_drinkers==1 ~ "Heavy drinkers", 
                                    moderate_drinkers==1 ~ "Moderate drinkers",
                                    ocassional_drinkers==1 ~ "Moderate drinkers"),
      
       ## Liver Enzymes
       ALT_elevated = case_when((LBXSATSI>=31 & 
                                   RIAGENDR=="Female") | 
                                  (LBXSATSI>=40 & 
                                     RIAGENDR=="Male")   ~ "Yes",
                                (LBXSATSI< 31 & 
                                   RIAGENDR=="Female") | 
                                  (LBXSATSI< 40 &
                                     RIAGENDR=="Male")   ~  "No"),
       
       AST_elevated = case_when((LBXSASSI>=31 & RIAGENDR=="Female") | 
                                  (LBXSASSI>=37 & RIAGENDR=="Male")      ~ "Yes",
                                (LBXSASSI< 31 & RIAGENDR=="Female") | 
                                  (LBXSASSI< 37 & RIAGENDR=="Male")      ~ "No"),
       
       ## Liver fibrosis
       Liver_fibrosis = case_when(LUXSMED>= 8.6 & LUAXSTAT=="Complete" ~ "Yes",
                                  LUXSMED<  8.6 & LUAXSTAT=="Complete" ~ "No"),
       
       ## Liver Fat 
       Liver_fat = case_when(LUXCAPM >= 311 & LUAXSTAT=="Complete" ~ "Yes",
                             LUXCAPM <  311 & LUAXSTAT=="Complete" ~ "No"),
       
       ##Liver inflammation (ALT AST)
       Liver_inflammation = case_when((ALT_elevated=="Yes" & !is.na(AST_elevated)) | 
                                        (AST_elevated=="Yes" & !is.na(ALT_elevated))   ~ "Yes",
                                      ALT_elevated=="No" & AST_elevated=="No"       ~ "No")) -> nhanes

nhanes %>% 
  mutate(complete_case = case_when(!is.na(age1) & 
                                     !is.na(gender) & 
                                     #(!is.na(race) & race!="Other Race") &
                                     !is.na(education) & 
                                     !is.na(poverty) &
                                     !is.na(health_insurance) &
                                     !is.na(regular_healthcare) &
                                    # (!is.na(bmi) & bmi!= "Underweight") &
                                     !is.na(diabetes) & 
                                     !is.na(aware_liver_disease) &
                                     !is.na(viral) & 
                                     !is.na(alcohol_variable) & 
                                     !is.na(ALT_elevated) & 
                                     !is.na(AST_elevated) & 
                                     !is.na(Liver_fibrosis) & 
                                     !is.na(Liver_fat) & 
                                     !is.na(Liver_inflammation)  &
                                     (RIDAGEYR>=20 & RIDSTATR=="Both interviewed and MEC examine") ~ 1), 
         ##Risk Factors binary
         step1_risk_factors = case_when(viral=="Yes" | 
                                          bmi=="Obesity" | 
                                          bmi=="Overweight" |
                                          diabetes=="Yes" | 
                                          aware_liver_disease=="Yes" |
                                          alcohol_variable=="Heavy drinkers" ~ "Yes"),
         risk_factors = case_when(is.na(step1_risk_factors) & complete_case==1 ~ "No",
                                  step1_risk_factors=="Yes" ~ "Yes")) %>% 
  mutate(
    # Alcohol subset
    complete_case_alcohol = case_when(complete_case==1 & (heavy_drinkers==1 | 
                                                            moderate_drinkers==1 | 
                                                            ocassional_drinkers==1) ~ 1))-> nhanes

table(nhanes$complete_case)
# 3030 (withouth Underweigth & Other Race)
# 3255 (with Underweigth & Other Race)

# table(nhanes$complete_case_alcohol)
# 3030 (withouth Underweigth & Other Race)
# 3255 (with Underweigth & Other Race)

nhanes %>% 
  saveRDS("2-processed_data/data_nhanes_processsed.rds")
