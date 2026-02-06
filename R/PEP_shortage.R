## Calculating the number where PEP is recommended, but is not available
## E REES

library(tidyverse)

## Read in data
human_dat <- read_csv("data/processed_HF_filtered.csv")  


# human_dat_all = human_dat
## Select variables required, and only look at first visits
human_dat <- human_dat %>% 
  filter(VISIT_STATUS == "first") %>% 
  filter(!is.na(PEP_RECOMMENDED))

human_dat_pep_rec = human_dat %>% 
  filter(PEP_RECOMMENDED == "yes") %>% 
  filter(!is.na(HOW_USED))

human_dat_pep_nu = human_dat %>% 
  filter(PEP_RECOMMENDED == "yes") %>% 
  filter(PEP_AVAILABLE == "yes") %>% 
  filter(is.na(HOW_USED))

table(human_dat_pep_rec$HOW_USED,useNA = "always")
print(paste0("Between July 2018 and December 2024 there were ", nrow(human_dat_pep_rec), " first patient visits where PEP was recommended" ))
print(paste0("Only ",nrow(human_dat_pep_nu)," patients were recorded as not receiving their first dose"))

## Caclulate the PEP shortage overall
human_dat_nr = as.data.frame(table(human_dat$PEP_RECOMMENDED))

pep_available_overall <- as.data.frame(table(human_dat$PEP_RECOMMENDED, human_dat$PEP_AVAILABLE))

pep_available_overall
total = sum(pep_available_overall$Freq)
shortage = (pep_available_overall %>% filter(Var1 == "yes" & Var2 == "no"))$Freq
shortage_perc = (shortage/total)*100
pep_rec_num = nrow(human_dat) - human_dat_nr[human_dat_nr$Var1 == "no",2]
perc_rec = round((pep_rec_num/ nrow(human_dat))*100,2)

print(paste0("There are ", nrow(human_dat), " first patient visits, and of these ", perc_rec, "% (", pep_rec_num, ") PEP was recommended")) 
print(paste0("There are ", nrow(human_dat), " first patient visits, and of these ", human_dat_nr[human_dat_nr$Var1 == "yes",2], " PEP was recommended")) 
print(paste0("There are ", shortage," individuals who where PEP is recommended, but PEP is not available (", round(shortage_perc,digits = 2),"%)" ))

## Caclulate the PEP shortage by region

pep_available <- as.data.frame(table( human_dat$REGION,human_dat$PEP_AVAILABLE))

pep_available <- pep_available %>% 
  pivot_wider(names_from = Var2, values_from = Freq) %>% 
  mutate(perc_available = (no/(yes+no))*100)

write_csv(pep_available, "outputs/pep_available_region.csv")

