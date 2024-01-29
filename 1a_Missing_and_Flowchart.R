## Flowchart & Missing Analysis.

library(tidyverse)
library(DiagrammeR)
library(mice)
library(DiagrammeRsvg)
library(rsvg)

data_nhanes_1 <- readRDS("2-processed_data/data_nhanes_processsed.rds")

# Missing Pattern 

data_nhanes_1 %>% 
  filter(RIDAGEYR>=20 & RIDSTATR=="Both interviewed and MEC examine") %>% 
  select(age1, gender, race, education,poverty, 
         bmi, diabetes, viral, aware_liver_disease,
         alcohol_variable, health_insurance, regular_healthcare,
         Liver_fibrosis, Liver_fat, Liver_inflammation, complete_case) %>% 
  md.pattern(rotate.names = TRUE) %>% 
  invisible() 

# Flowchart 
### Total NHANES Sample 
Ns <- list()

### Total NHANES
Ns$a <-  dim(data_nhanes_1)[1]

## MEC + >20a
Ns$a1 <- dim(data_nhanes_1 %>% 
               filter(RIDAGEYR>=20 & RIDSTATR=="Both interviewed and MEC examine"))[1]

## Demographics 
Ns$a2 <- dim(data_nhanes_1 %>% 
               filter(RIDAGEYR>=20 & RIDSTATR=="Both interviewed and MEC examine") %>% 
               filter(!is.na(age1) & 
                        !is.na(gender) & 
                        #(!is.na(race) & race!="Other Race") &
                        !is.na(education) &
                        !is.na(poverty)))[1]

##Other Race
Ns$a2_a <- dim(data_nhanes_1 %>% 
                filter(RIDAGEYR>=20 & RIDSTATR=="Both interviewed and MEC examine") %>% 
                filter(!is.na(age1) & 
                         !is.na(gender) & 
                         (!is.na(race) & race!="Other Race") &
                         !is.na(education) &
                         !is.na(poverty)))[1]

## Survey Risk Factors
Ns$a3 <- dim(data_nhanes_1 %>% 
               filter(RIDAGEYR>=20 & RIDSTATR=="Both interviewed and MEC examine") %>% 
               filter(!is.na(age1) & 
                        !is.na(gender) & 
                        (!is.na(race) & race!="Other Race") & 
                        !is.na(education) &
                        !is.na(poverty) &
                        !is.na(aware_liver_disease) &
                        !is.na(alcohol_variable) &
                        !is.na(health_insurance) &
                        !is.na(regular_healthcare)))[1]


## ## Physical Evaluated Risk Factors
Ns$a4 <- dim(data_nhanes_1 %>% 
               filter(RIDAGEYR>=20 & RIDSTATR=="Both interviewed and MEC examine") %>% 
               filter(!is.na(age1) & 
                        !is.na(gender) & 
                        (!is.na(race) & race!="Other Race") & 
                        !is.na(education) &
                        !is.na(poverty) &
                        !is.na(diabetes) & 
                        !is.na(viral) &
                        !is.na(aware_liver_disease) &
                        !is.na(alcohol_variable) &
                        !is.na(health_insurance) &
                        !is.na(regular_healthcare)))[1]

##Underweight
Ns$a4_a <- dim(data_nhanes_1 %>% 
               filter(RIDAGEYR>=20 & RIDSTATR=="Both interviewed and MEC examine") %>% 
               filter(!is.na(age1) & 
                        !is.na(gender) & 
                        (!is.na(race) & race!="Other Race") & 
                        !is.na(education) &
                        !is.na(poverty) &
                        (!is.na(bmi) & bmi!="Underweight") & 
                        !is.na(diabetes) & 
                        !is.na(viral) &
                        !is.na(aware_liver_disease) &
                        !is.na(alcohol_variable) &
                        !is.na(health_insurance) &
                        !is.na(regular_healthcare)))[1]

## Liver Fat
Ns$a5 <- dim(data_nhanes_1 %>% 
               filter(RIDAGEYR>=20 & RIDSTATR=="Both interviewed and MEC examine") %>% 
               filter(!is.na(age1) & 
                        !is.na(gender) & 
                        (!is.na(race) & race!="Other Race") & 
                        !is.na(education) &
                        !is.na(poverty) &
                        (!is.na(bmi) & bmi!="Underweight") & 
                        !is.na(diabetes) & 
                        !is.na(viral) &
                        !is.na(aware_liver_disease) &
                        !is.na(alcohol_variable) &
                        !is.na(health_insurance) &
                        !is.na(regular_healthcare) &
                        !is.na(Liver_fat)))[1]

##Liver Inflammation
Ns$a6 <- dim(data_nhanes_1 %>% 
               filter(RIDAGEYR>=20 & RIDSTATR=="Both interviewed and MEC examine") %>% 
               filter(!is.na(age1) & 
                        !is.na(gender) & 
                        (!is.na(race) & race!="Other Race") & 
                        !is.na(education) &
                        !is.na(poverty) &
                        (!is.na(bmi) & bmi!="Underweight") & 
                        !is.na(diabetes) & 
                        !is.na(viral) &
                        !is.na(aware_liver_disease) &
                        !is.na(alcohol_variable) &
                        !is.na(health_insurance) &
                        !is.na(regular_healthcare) &
                        !is.na(Liver_inflammation)))[1]

##Liver Fibrosis
Ns$a7 <- dim(data_nhanes_1 %>% 
               filter(RIDAGEYR>=20 & RIDSTATR=="Both interviewed and MEC examine") %>% 
               filter(!is.na(age1) & 
                        !is.na(gender) & 
                        (!is.na(race) & race!="Other Race") & 
                        !is.na(education) &
                        !is.na(poverty) &
                        (!is.na(bmi) & bmi!="Underweight") & 
                        !is.na(diabetes) & 
                        !is.na(viral) &
                        !is.na(aware_liver_disease) &
                        !is.na(alcohol_variable) &
                        !is.na(health_insurance) &
                        !is.na(regular_healthcare) &
                        !is.na(Liver_fibrosis)))[1]


## Total Sample
Ns$a8 <- dim(data_nhanes_1 %>% 
               filter(RIDAGEYR>=20 & RIDSTATR=="Both interviewed and MEC examine") %>% 
               filter(!is.na(age1) & 
                        !is.na(gender) & 
                        (!is.na(race) & race!="Other Race") & 
                        !is.na(education) &
                        !is.na(poverty) &
                        (!is.na(bmi) & bmi!="Underweight") & 
                        !is.na(diabetes) & 
                        !is.na(viral) &
                        !is.na(aware_liver_disease) &
                        !is.na(alcohol_variable) &
                        !is.na(health_insurance) &
                        !is.na(regular_healthcare) &
                        !is.na(Liver_inflammation) &
                        !is.na(Liver_fat) & 
                        !is.na(Liver_fibrosis)))[1]

#minus


Ns$b1 <- Ns$a - Ns$a1
Ns$b2 <- Ns$a1 - Ns$a2
Ns$b3 <- Ns$a2 - Ns$a2_a
Ns$b4 <- Ns$a2_a - Ns$a3
Ns$b5 <- Ns$a3 - Ns$a4
Ns$b6 <- Ns$a4 - Ns$a4_a
Ns$b7 <- Ns$a4_a - Ns$a5


## Plot
grViz("
digraph graph2 {

graph [rankdir = D]

# node definitions with substituted label text
node [shape = rectangle, width = 4, fillcolor = Navy, fixedsize = true]
a [label = '@@1']
a1 [label = '@@2']
a2 [label = '@@3']
a2_a [label = '@@4']
a3 [label = '@@5']
a4 [label = '@@6']
a4_a [label = '@@7']
a5 [label = '@@8']
a6 [label = '@@9']
a7 [label = '@@10']
a8 [label = '@@11']


node [shape = oval, width = 4, fillcolor = Black]
b1 [label = '@@12']
b2 [label = '@@13']
b3 [label = '@@14']
b4 [label = '@@15']
b5 [label = '@@16']
b6 [label = '@@17']
b7 [label = '@@18']

a -> a1 -> a2 -> a2_a -> a3 -> a4 -> a4_a;
a->b1;
a1->b2;
a2->b3;
a2_a->b4;
a3->b5;
a4->b6;
a4_a-> a5;
a4_a-> a6;
a4_a-> a7;
a4_a-> b7;
a5-> a8;
a6-> a8;
a7-> a8

}

[1]: paste0('Total NHANES Sample [n = ', Ns$a, ']')
[2]: paste0('Complete MEC & >20 years [n = ', Ns$a1, ']')
[3]: paste0('Complete Demographics [n=',  Ns$a2, ']')
[4]: paste0('Exclude Other Race [n=', Ns$a2_a,']')
[5]: paste0('Complete Survey Risk Factors [n=', Ns$a3,']')
[6]: paste0('Complete Physical Evaluated Risk Factors [n=', Ns$a4,']')
[7]: paste0('Exclude Underweight [n=', Ns$a4_a,']')
[8]: paste0('Liver Fat  [n=', Ns$a5,']')
[9]: paste0('Liver Inflammation  [n=', Ns$a6,']')
[10]: paste0('Liver Fibrosis  [n=', Ns$a7,']')
[11]: paste0('Total Analytical Sample [n=', Ns$a8,']')
[12]: paste0(' [n=', Ns$b1,']')
[13]: paste0(' [n=', Ns$b2,']')
[14]: paste0(' [n=', Ns$a3,']')
[15]: paste0(' [n=', Ns$b4,']')
[16]: paste0(' [n=', Ns$b5,']')
[17]: paste0(' [n=', Ns$b6,']')
[18]: paste0(' [n=', Ns$b7,']')



") #-> graph
#\\n 
#graph %>% 
#  DiagrammeRsvg::export_svg() %>%
#  charToRaw %>% 
#  rsvg_png("3-plots_tables/flowchart.png")

