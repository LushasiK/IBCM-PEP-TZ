## E Rees
## Code for Figure 4 
## Also outputs the completion rate by administration route and the number of vaccinations by administration route (ID PEP doses (ID/ total); Table 2)

library(janitor)
library(ggthemes)
library(tidyverse)
library(cowplot)
library(ragg)

human_dat <- read_csv("data/processed_HF_filtered.csv")

## add doses
h_dat_PEP <- human_dat %>% 
  filter(VISIT_STATUS != "positive_clinical_signs" & !is.na(VISIT_STATUS) & VISIT_STATUS != "fifth") %>% 
  mutate(Dose = ifelse(VISIT_STATUS == "first" & PEP_RECOMMENDED == "yes" & PEP_AVAILABLE == "yes","First",
                       ifelse(VISIT_STATUS == "first" & PEP_RECOMMENDED == "yes" & PEP_AVAILABLE == "no", "Shortage",
                              ifelse(VISIT_STATUS == "first" & PEP_RECOMMENDED == "no", "NR",
                                     ifelse(VISIT_STATUS == "second" & PEP_AVAILABLE == "yes", "Second",
                                            ifelse(VISIT_STATUS == "third" & PEP_AVAILABLE == "yes", "Third",
                                                   ifelse(VISIT_STATUS == "forth" & PEP_AVAILABLE == "yes", "Fourth", "Shortage"
                                                   ))))))) %>% 
  filter(Dose != "NR") %>%  ## 2214 first visits didn't require PEP 
  filter(DISTRICT != "NANA")

table(h_dat_PEP$VISIT_STATUS, h_dat_PEP$Dose)

# yearly data for plotting
h_dat_PEP_type <- h_dat_PEP %>% 
  filter(Dose != "Shortage") %>% 
  group_by(REGION, DISTRICT, Dose, Year, HOW_USED) %>% 
  dplyr::summarise(n = n()) %>% 
  filter(!is.na(HOW_USED)) %>% 
  mutate(`Vaccine type` = ifelse(HOW_USED == "id","Intradermal","Intramuscular")) 

## Output the dose and type to calculate PEP completion by vaccine type
h_dat_PEP_dose_type <- h_dat_PEP %>% 
  group_by(REGION, DISTRICT, Dose, Year, HOW_USED) %>% 
  dplyr::summarise(n = n())

## Summarise by region, district and year
h_dat_PEP <- h_dat_PEP %>%
  group_by(REGION, DISTRICT, Dose, Year) %>%
  dplyr::summarise(n = n())

## Change shortage to negative
h_dat_PEP <- h_dat_PEP %>%
  mutate(n = ifelse(Dose == "Shortage", n*-1,n)) %>%
  filter(Dose != "Fourth")

## Changing dose to a factor to re-order bars ggplot
## This also drops the fourth dose from the bar plot
h_dat_PEP$Dose <- factor(h_dat_PEP$Dose,levels = c("First","Second","Third","Shortage"))

## Process data to get the completion rate 
## Get the number of first doses
dat_first <- h_dat_PEP_dose_type %>% 
  filter(Dose == "First") %>% 
  group_by(Year, HOW_USED) %>% 
  dplyr::summarise(first = sum(n)) %>% 
  drop_na()

dat_first_im <- dat_first %>% filter(HOW_USED == "im")
dat_first_id <- dat_first %>% filter(HOW_USED == "id")

## Get the number of last doses for ID and IM

dat_last_im <- h_dat_PEP_dose_type %>% 
  filter(Dose == "Third") %>% 
  filter(HOW_USED == "im") %>% 
  group_by(Year, HOW_USED) %>% 
  dplyr::summarise(Last = sum(n)) %>% 
  drop_na()

dat_last_id <- h_dat_PEP_dose_type %>% 
  filter(Dose == "Third") %>% 
  filter(HOW_USED == "id") %>% 
  group_by(Year, HOW_USED) %>% 
  dplyr::summarise(Last = sum(n)) %>% 
  drop_na()

## Join first and doses back together
dat_im <- dat_first_im %>% 
  left_join(dat_last_im) %>% 
  replace(is.na(.), 0) %>% 
  mutate(Completed = (Last/(first+Last)))

dat_id <- dat_first_id %>% 
  left_join(dat_last_id) %>% 
  replace(is.na(.), 0) %>% 
  mutate(Completed = (Last/(first+Last)))

dat_all <- dat_im %>% 
  rbind(dat_id)

dat_all <- dat_all %>% 
  mutate(HOW_USED = ifelse(HOW_USED == "id","Intradermal","Intramuscular"))  

colnames(dat_all) <- c("Year","Administration route","First dose", "Third dose" ,"PEP completed")

write_csv(dat_all, "outputs/completion_admin_route.csv")


## Group by Dose and year and calculate proportion for figure 4a
h_dat_PEP_grp <- h_dat_PEP %>% 
  group_by(Dose, Year) %>% 
  dplyr::summarise(n = sum(n)) 

h_dat_PEP_grp <- h_dat_PEP_grp %>% 
  ungroup() %>% 
  group_by(Year) %>%
  mutate(proportion = ifelse(Dose == "First", 1, n / sum(n[Dose == "First"]))) 

p1 <-ggplot(h_dat_PEP_grp, aes(fill=Dose, y=proportion, x=factor(Year))) + 
  geom_bar(position="dodge",stat="identity") +
  ylab("Vaccination (proportion)") +
  scale_fill_manual(values = c("#666666","#969696","#c0c0c0", "#ae2012")) +
  xlab("") +
  theme_clean() +
  theme(legend.position="top", # set legend to appear above plot
        legend.title = element_blank(),
        legend.text = element_text(size=12,face = "plain"),
        strip.text.x = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1,size = 12),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(),
        legend.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_blank())

p1


h_dat_PEP_type <- h_dat_PEP_type %>% 
  filter(Dose == "First") %>% 
  mutate(`Administration route` = `Vaccine type`)

h_dat_PEP_type <- h_dat_PEP_type %>% 
  group_by(REGION, `Administration route`, Year) %>% 
  dplyr::summarise(n = sum(n))

h_dat_PEP_type_table <- h_dat_PEP_type %>% 
  pivot_wider(names_from = `Administration route`, values_from = n) %>% 
  mutate(Total = Intradermal + Intramuscular) %>% 
  mutate(Intradermal_p = (Intradermal/Total)*100,
         Intramuscular_p = (Intramuscular/Total)*100) 

h_dat_PEP_type_table_region = h_dat_PEP_type_table %>% 
  group_by(REGION) %>% 
  dplyr::summarise(Intradermal = sum(Intradermal),
                   Intramuscular = sum(Intramuscular),
                   Total = sum(Total)) %>% 
  mutate(Intradermal_p = (Intradermal/Total)*100,
         Intramuscular_p = (Intramuscular/Total)*100) %>% 
  select(-Intramuscular_p)

write_csv(h_dat_PEP_type_table_region, "outputs/PEP_type_region.csv")

h_dat_PEP_type_table_year = h_dat_PEP_type_table %>% 
  group_by(Year) %>% 
  dplyr::summarise(Intradermal = sum(Intradermal),
                   Intramuscular = sum(Intramuscular),
                   Total = sum(Total)) %>% 
  mutate(Intradermal_p = (Intradermal/Total)*100,
         Intramuscular_p = (Intramuscular/Total)*100) 

write_csv(h_dat_PEP_type_table_year, "PEP_type_year.csv")

h_dat_PEP_type_table_year = h_dat_PEP_type_table_year %>% adorn_totals("row")

print(paste0("Overall, ",round(((h_dat_PEP_type_table_year$Intradermal[7]/h_dat_PEP_type_table_year$Total[7])*100),2)," % (", h_dat_PEP_type_table_year$Intradermal[7], "/",h_dat_PEP_type_table_year$Total[7] ,") of vaccines administered were via the ID route"))

## figure 4b plotting number of vaccination by admin route
p2 <- h_dat_PEP_type %>% 
  ggplot() +
  geom_bar(aes(x=as.factor(Year), y=n, fill=`Administration route`), stat="identity",position = "dodge")+
  scale_fill_manual(values = c("#666666", "#ae2012")) +
  ylab("Number of vaccinations") +
  xlab("Year") +
  theme_clean() +
  theme(legend.position="top", # set legend to appear above plot
        legend.text = element_text(size=12,face = "plain"),
        strip.text.x = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1,size = 12),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(),
        legend.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_blank())
p2


plot_grid(p1,p2, labels = "AUTO")


png(filename="figs/fig4_pep.png",
    width=950, height=300)
plot_grid(p1,p2, labels = "AUTO")
dev.off()


ragg::agg_tiff(
  filename = "figs/fig4_pep.tiff",
  width = 10,
  height = 3.5,
  units = "in",
  res = 300
)
plot_grid(p1, p2, labels = "AUTO")
dev.off()
