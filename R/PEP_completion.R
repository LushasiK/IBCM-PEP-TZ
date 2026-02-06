## E Rees
## Outputs the  
##  - number of patients attending clinic who require PEP
##  - number of patients who complete PEP
##  - doses by region (and the number)First, second third, fourth and shortage)

library(tidyverse)
human_dat <- read_csv("data/processed_HF_filtered.csv")

## Creating a new "Dose" column and identifying PEP shortages
h_dat_PEP_all <- human_dat %>% 
  filter(VISIT_STATUS != "positive_clinical_signs" & !is.na(VISIT_STATUS) & VISIT_STATUS != "fifth") %>% 
  mutate(Dose = ifelse(VISIT_STATUS == "first" & PEP_RECOMMENDED == "yes" & PEP_AVAILABLE == "yes","First",
                       ifelse(VISIT_STATUS == "first" & PEP_RECOMMENDED == "yes" & PEP_AVAILABLE == "no", "Shortage",
                              ifelse(VISIT_STATUS == "first" & PEP_RECOMMENDED == "no", "NR",
                                     ifelse(VISIT_STATUS == "second" & PEP_AVAILABLE == "yes", "Second",
                                            ifelse(VISIT_STATUS == "third" & PEP_AVAILABLE == "yes", "Third",
                                                   ifelse(VISIT_STATUS == "forth" & PEP_AVAILABLE == "yes", "Fourth", "Shortage"
                                                   ))))))) 

## Removing those where PEP is not recommended or where district is not known
h_dat_PEP <- h_dat_PEP_all %>% 
  filter(Dose != "NR") %>%  ## 2213 first visits didn't require PEP 
  filter(DISTRICT != "NANA")

print(paste0("The number of patients who attend clinic is ", nrow(h_dat_PEP_all), " and of these ", nrow(h_dat_PEP_all) - nrow(h_dat_PEP), " do not require PEP" ))

h_dat_PEP_first_all = h_dat_PEP_all %>% 
  filter(VISIT_STATUS == "first") %>% 
  filter(!is.na(PEP_RECOMMENDED)) ## 2 patients had NA PEP_RECOMMENDED status

h_dat_PEP_first <- h_dat_PEP_all %>% 
  filter(VISIT_STATUS == "first") %>% 
  filter(Dose != "NR") %>%  ## 2213 first visits didn't require PEP 
  filter(DISTRICT != "NANA")

print(paste0("The number of first patient visits is ", nrow(h_dat_PEP_first_all), " and of these ", nrow(h_dat_PEP_first_all) - nrow(h_dat_PEP_first), " do not require PEP" ))

h_dat_PEP_first_sus = h_dat_PEP_first %>% filter(Suspect == "Suspect")
h_dat_PEP_first_hel = h_dat_PEP_first %>% filter(Suspect == "Healthy")

print(paste0("Of these ",nrow(h_dat_PEP_first_sus)," are suspect, and ", nrow(h_dat_PEP_first_hel), " are healthy"))

pep_doses = data.frame(table(h_dat_PEP$VISIT_STATUS, h_dat_PEP$Dose))
total_pep = sum(pep_doses$Freq)
pep_doses = pep_doses %>%  pivot_wider(names_from = Var2, values_from = Freq)

print(paste0("The number of patients who complete PEP is ", pep_doses$Third[4], " out of ", pep_doses$First[1], " (", round((pep_doses$Third[4]/pep_doses$First[1])*100,2), "%)" ))

pep_doses_region = h_dat_PEP %>% 
  group_by(REGION, Dose) %>% 
  dplyr::summarise(n = n())

pep_doses_region = pep_doses_region %>% 
  pivot_wider(names_from = "Dose", values_from = "n") %>% 
  ungroup()

pep_doses_region = pep_doses_region %>% 
  mutate(complete = round((Third/(First))*100,2))

pep_doses_region = pep_doses_region %>%
  select(REGION, First, Second, Third, Fourth, Shortage, complete) 

write_csv(pep_doses_region, "outputs/pep_doses_region.csv")

