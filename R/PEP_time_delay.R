
## Calculating the time delay between biting incident and clinic visit
## E REES

library(tidyverse)

## Read in data
human_dat <- read_csv("data/processed_HF_filtered.csv")

## Select variables required, and only look at first visits, where PEP is Recommended AND PEP is available
human_dat <- human_dat %>% 
  filter(VISIT_STATUS == "first") %>% 
  filter(PEP_RECOMMENDED == "yes" & PEP_AVAILABLE == "yes")

## Now calculate the number of days between date bitten and the date visit
human_dat <- human_dat %>% 
  mutate(time_delay = DATE_VISIT - DATE_BITTEN) %>% 
  mutate(time_delay_num = as.numeric(time_delay)) ## Change to numeric for calculation purposes

## Overall mean time delay
mean(human_dat$time_delay)
model = lm(human_dat$time_delay_num ~ 1)
ci = confint(model, level=0.95)

print(paste0("Overall there was a mean time delay of ", round(mean(human_dat$time_delay),2) ," days (",round(ci[1],2)," - ",round(ci[2],2)," days) ,between biting incidence and clinic visit date (n = ", nrow(human_dat),")"))

## Output time delay by region for the paper
human_dat_reg <- human_dat %>% 
  group_by(REGION) %>% 
  dplyr::summarise(time_delay_mean = round(mean(time_delay_num,na.rm = T),2), 
            time_delay_median = median(time_delay_num, na.rm = T),
            lower = round(t.test(time_delay_num)$conf.int[1],2),
            upper = round(t.test(time_delay_num)$conf.int[2],2),
            count = n())
human_dat_reg

human_dat_reg <- human_dat_reg %>% 
  mutate(mean = paste0(time_delay_mean, " (", lower, "-",upper, ")")) %>% 
  select(REGION, count, mean)

write_csv(human_dat_reg,"outputs/time_delay.csv")

## Create a binary variable (time delay over two days)
human_dat <- human_dat %>% 
  filter(time_delay >= 0) %>% 
  mutate(time_delay_2 = ifelse(time_delay <=2,0,1)) ## time delay over 2 days

human_dat_2 = human_dat %>% 
  group_by(time_delay_2) %>% 
  dplyr::summarise(n = n()) %>% 
  pivot_wider(names_from = time_delay_2, values_from = n) %>% 
  rowwise() %>% 
  mutate(total = sum(`0`,`1`)) %>% 
  mutate(prop_0 = `0`/total,
         prop_1 = `1`/total) %>% 
  ungroup()

print(paste0("Overall, ",round((human_dat_2$prop_1)*100,2),"% (",human_dat_2$`1`,"/",human_dat_2$total,") had a time delay ≥ two days between the biting incident and the clinic visit date."))

# prop.test(human_dat_2$`1`,human_dat_2$total, conf.level = 0.95)

human_dat_grp_2 <- human_dat %>% 
  group_by(REGION, time_delay_2) %>% 
  dplyr::summarise(n = n()) %>% 
  pivot_wider(names_from = time_delay_2, values_from = n) 

human_dat_grp_2 <- human_dat_grp_2%>% 
  ungroup() %>% 
  mutate(total = rowSums(.[2:3])) %>% 
  mutate(prop_0 = `0`/total,
         prop_1 = `1`/total,
         perc_1 = prop_1*100) 

print(paste0("This varied by region (Lindi = ",round((human_dat_grp_2[[1,6]])*100,2),"%, Mara = ", round((human_dat_grp_2[[2,6]])*100,2), "%, Morogo = ", round((human_dat_grp_2[[3,6]])*100,2),"% and Mtwara = ", round((human_dat_grp_2[[4,6]])*100,2), "%)"))

human_dat_reg <- human_dat_reg %>% 
  mutate(percent_grtr2 = round(human_dat_grp_2$perc_1,2))

write_csv(human_dat_reg,"outputs/time_delay.csv")

## How does this differ when the animal is known?

## 1 cases is missing whether or not the animal is known or unknown
sum(is.na(human_dat$known))

## Group data by known and a time delay over two days
human_dat_grp_2_known <- human_dat %>% 
  filter(!is.na(known)) %>% 
  group_by(time_delay_2, known) %>% 
  dplyr::summarise(n = n()) %>% 
  pivot_wider(names_from = time_delay_2, values_from = n) %>% 
  mutate(total = rowSums(.[2:3])) %>% 
  mutate(prop_0 = `0`/total,
         prop_1 = `1`/total,
         perc_1 = prop_1*100) %>% 
  ungroup()

## Caclulate the difference between proportions
prop_test = prop.test( human_dat_grp_2_known$`0`, human_dat_grp_2_known$total, conf.level=.95 )

print(paste0("The time delay differed depending on whether the dog was known or not, with ",
             round(human_dat_grp_2_known[[1,7]],2),"% time delay of ≥ two days when the dog was unknown (n = ",
             human_dat_grp_2_known[[1,2]], "), compared with ",round(human_dat_grp_2_known[[2,7]],2),
             "% when the dog was known (n = ", human_dat_grp_2_known[[2,2]],
             "), with a difference between proportions of ",round((prop_test$estimate[[1]] - prop_test$estimate[[2]])*100,2),
             "% (",round((prop_test$conf.int[[1]])*100,2),"% - ",round((prop_test$conf.int[[2]])*100,2),"%)."))

