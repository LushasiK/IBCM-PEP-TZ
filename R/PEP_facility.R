## Breaking down PEP visits by facility and facility type
## E REES

library(tidyverse)
library(sf)

human_dat <- read_csv("data/processed_HF_filtered.csv")
fac <- read_rds("data/facilities_loc_centroids.RDS")

table(fac$location_status)

study_regs <- c("Mara", "Lindi", "Morogoro", "Mtwara")
human_dat = human_dat[which(human_dat$REGION %in% study_regs),]

human_dat <- human_dat %>% 
  filter(VISIT_STATUS == "first") 

pep_received <- human_dat %>% 
  filter(PEP_RECOMMENDED=="yes" & PEP_AVAILABLE == "yes")

table(pep_received$facility_type, useNA = "always")

urban_settings <- c("Musoma", "Morogoro", "Lindi", "Mtwara")

pep_received <- pep_received %>% 
  mutate(urban = ifelse(DISTRICT %in% urban_settings, "urban","rural"))

patients_urban =as_tibble(as.data.frame(table(pep_received$urban)))

perc_rural = round((patients_urban[1,2]/(patients_urban[2,2] + patients_urban[1,2]))*100,2)

pep_received_fac <- pep_received %>% 
  group_by(facility_type) %>% 
  dplyr::summarise(n = n()) %>% 
  na.omit()

pep_received_fac <- pep_received_fac %>% 
  mutate(Percentage = round((n/sum(n))*100,2))

pep_received_fac<-data.frame(lapply(pep_received_fac, as.character), stringsAsFactors=FALSE)

pep_received_fac <- pep_received_fac %>% 
  mutate(n = as.numeric(n), Percentage = as.numeric(Percentage)) %>% 
  adorn_totals("row") 

total = pep_received_fac[6,2]
hosp_dis = total - pep_received_fac[1,2]

write_csv(pep_received_fac, "outputs/pep_received_facility.csv")

fac <- fac %>% 
  st_drop_geometry() %>%
  select(-lon,-lat,-centroid)

pep_received <- pep_received %>% 
  left_join(fac, by = c("new_FACILITY" = "name_facility")) 

pep_received_loc <- pep_received %>%
  mutate(location_status = ifelse(location_status == "Semi -Urban", "Semi-Urban", location_status)) %>%
  group_by(location_status) %>%
  dplyr::summarise(n = n()) %>%
  na.omit()

pep_received_loc <- pep_received_loc %>%
  mutate(Percentage = round((n/sum(n))*100,2))

write_csv(pep_received_loc, "outputs/pep_received_loc.csv")

facility_sum <- fac %>% 
  group_by(location_status) %>% 
  dplyr::summarise(n = n()) %>% 
  mutate(Percentage = round((n/sum(n))*100,2))

urban_semiurban = facility_sum[[3,2]] + facility_sum[[2,2]] 
total_fac = facility_sum[[1,2]] + facility_sum[[2,2]] + facility_sum[[3,2]]

print(paste0(round((urban_semiurban/total_fac)*100,1),"% of facilities are located in urban/ semi-urban areas (",urban_semiurban,"/",total_fac,")"))

print(paste0("Only ",pep_received_loc[[1,3]],"% patients receiving PEP in rural areas, despite the majority of bite patients come from rural areas (",perc_rural,")%"))



