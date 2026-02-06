## E Rees
## Calculating distance travelled to access PEP
## Figure 3 

# library(plyr)
library(tidyverse)
library(sf)
library(geosphere)
library(scales)
library(ggthemes)
library(cowplot)
library(ggspatial)
library(boot)
library(ragg)

human_dat <- read_csv("data/processed_HF_filtered.csv")
centroids <- read_csv("data/GIS/wards_master_2022.csv")
region_shp <- st_read("data/GIS", "regions_2022_population")
district_shp <- st_read("data/GIS", "districts_2022_population")
ward_shp <- st_read("data/GIS", "TZ_Ward_2012_pop")
prot_areas <- st_read("data/GIS", "Protected_areas")
facilities_loc <- read_rds("data/facilities_loc_centroids.RDS")

study_regs <- c("Mara", "Lindi", "Morogoro", "Mtwara")

names(prot_areas)[names(prot_areas) == 'Reg_ID'] <- 'Region_Nam'

## filter for first visits, PEP recommended and available, and study regions and study dates
human_dat_pep <- human_dat %>% 
  filter(VISIT_STATUS == "first") %>% 
  filter(PEP_RECOMMENDED == "yes" & PEP_AVAILABLE == "yes") %>% 
  filter(DISTRICT != "NANA") 

## remove any records where the facility has not been matched
fac_missing <- human_dat_pep %>% 
  filter(is.na(facility_type))

human_dat_pep <- human_dat_pep %>% 
  filter(!is.na(facility_type))

## 1. how many have travelled outside of the district to access PEP?
human_dat_pep <- human_dat_pep %>% 
  mutate(travelled = ifelse(DISTRICT != district_legacy_name_of_facility, "yes","no"))
dat_travelled = data.frame(table(human_dat_pep$travelled))

(dat_travelled$Freq[[2]]/(dat_travelled$Freq[[1]]+dat_travelled$Freq[[2]]))*100

print(paste0(round((dat_travelled$Freq[[2]]/(dat_travelled$Freq[[1]]+dat_travelled$Freq[[2]]))*100,2), "% (",dat_travelled[dat_travelled$Var1 == "yes",2],"/",dat_travelled[dat_travelled$Var1 == "no",2], ") travelled outside of their district to access PEP"))
dat_travelled_region = tibble(data.frame(table(human_dat_pep$REGION, human_dat_pep$travelled)))

dat_travelled_region = dat_travelled_region %>% 
  pivot_wider(names_from = Var2, values_from = Freq) %>% 
  mutate(percent_travelled = (yes/(yes+no))*100)

write_csv(dat_travelled_region, "outputs/distance_travelled_reg.csv")

## break down by year
summary_pep_year <- data.frame(table(human_dat_pep$Year, human_dat_pep$travelled))

summary_pep_year <- tibble(summary_pep_year)
summary_pep_year <- summary_pep_year %>% 
  pivot_wider(names_from = Var2, values_from = Freq) %>% 
  mutate(percent = (yes/(yes+no))*100)
summary_pep_year

write_csv(summary_pep_year, "outputs/distance_travelled_year.csv")

## 2. Process data to calculate distance in km between facility ward and home ward
human_dat_pep <- human_dat_pep %>% 
  mutate(Loc_ID_w_facility = paste0("Ward-", region_name_of_facility, "-", district_legacy_name_of_facility, "-", ward_name_of_facility))

human_dat_pep <- human_dat_pep %>% 
  left_join(centroids, by = c("Loc_ID_w" = "Loc_ID"))

human_dat_pep <- human_dat_pep %>% 
  left_join(centroids, by = c("Loc_ID_w_facility" = "Loc_ID"))

human_dat_pep <- human_dat_pep %>% 
  filter(!is.na(longitude.x) & !is.na(longitude.y))

loc_home <- cbind(human_dat_pep$longitude.x, human_dat_pep$latitude.x)
loc_fac <- cbind(human_dat_pep$longitude.y, human_dat_pep$latitude.y)

## Calculte distance 
distance <- distHaversine(loc_home, loc_fac)
human_dat_pep <- human_dat_pep %>% 
  mutate(distance = distance)

## assign whether they are urban or rural based on home district
urban_settings <- c("Musoma MC", "Morogoro MC", "Lindi MC", "Mtwara MC")

human_dat_pep <- human_dat_pep %>% 
  mutate(urban = ifelse(counc_name.x %in% urban_settings, "urban","rural"))

mean(human_dat_pep$distance,na.rm = TRUE)
median(human_dat_pep$distance)

print(paste0("Overall, the mean distance travelled to access PEP was ",round(mean(human_dat_pep$distance)/1000,2), " km"))

dist_urban = mean(human_dat_pep$distance[human_dat_pep$urban == "urban"])
dist_rural = mean(human_dat_pep$distance[human_dat_pep$urban == "rural"])

print(paste0("Overall, the mean distance travelled to access PEP for urban areas was ",round(dist_urban/1000,2), " km"))
print(paste0("Overall, the mean distance travelled to access PEP for rural areas was ",round(dist_rural/1000,2), " km"))

## Look at the mean distance travelled by region
# Define a function to compute the mean (converting meters to km)
boot_mean <- function(data, indices) {
  return(mean(data[indices], na.rm = TRUE) / 1000)
}

human_dat_pep_sum <- human_dat_pep %>%
  group_by(REGION) %>%
  dplyr::summarize(mean = round(mean(distance, na.rm=TRUE)/1000,2),
                   median = round(median(distance, na.rm=TRUE)/1000,2),
                   lower_CI = round(quantile(boot(distance, boot_mean, R = 1000)$t, 0.025), 2),
                   upper_CI = round(quantile(boot(distance, boot_mean, R = 1000)$t, 0.975), 2)
  )

# Compute the mean, median, and bootstrap confidence intervals
human_dat_pep_sum <- human_dat_pep %>%
  group_by(REGION) %>%
  dplyr::summarise(
    mean = round(mean(distance, na.rm = TRUE) / 1000, 2),
    median = round(median(distance, na.rm = TRUE) / 1000, 2),
    lower_CI = round(quantile(boot(data = distance, statistic = boot_mean, R = 1000)$t, 0.025), 2),
    upper_CI = round(quantile(boot(data = distance, statistic = boot_mean, R = 1000)$t, 0.975), 2)
  )

pairwise.wilcox.test(human_dat_pep$distance, human_dat_pep$REGION, p.adjust.method = "bonferroni")

write_csv(human_dat_pep_sum, "outputs/mean_PEP_distance_km.csv")

## Look at the mean distance travelled by district
human_dat_pep_district <- human_dat_pep %>%
  group_by(DISTRICT) %>%
  dplyr::summarise(
    mean = round(mean(distance, na.rm = TRUE) / 1000, 2),
    median = round(median(distance, na.rm = TRUE) / 1000, 2),
    lower_CI = round(quantile(boot(data = distance, statistic = boot_mean, R = 1000)$t, 0.025), 2),
    upper_CI = round(quantile(boot(data = distance, statistic = boot_mean, R = 1000)$t, 0.975), 2)
  )
write_csv(human_dat_pep_district, "outputs/mean_PEP_distance_km_district.csv")


## 3. Create figure 3
## process data for figure
human_dat_pep_ward <- human_dat_pep %>% 
  group_by(WARD, REGION, DISTRICT, Loc_ID_w) %>% 
  dplyr::summarise(distance = mean(distance),
                   n = n()) %>% 
  mutate(distance_km = distance/1000) %>% 
  ungroup()

# Subset shapefiles for study regions
study_regs_shp <- subset(region_shp, region_shp$Region_Nam %in% study_regs)
study_dists_shp <- subset(district_shp, district_shp$Region_Nam %in% study_regs)
study_wards_shp <- subset(ward_shp, ward_shp$Region_Nam %in% study_regs)

study_wards_shp <- study_wards_shp %>% 
  left_join(human_dat_pep_ward, by = c("Loc_ID" = "Loc_ID_w"))

human_dat_pep_ward <- human_dat_pep_ward %>% 
  left_join(centroids, by = c("Loc_ID_w" = "Loc_ID"))

study_wards_shp <- study_wards_shp %>% 
  mutate(distance_km = ifelse(distance_km >= 300, 300, distance_km))

max_distance = plyr::round_any(max(study_wards_shp$distance_km,na.rm = T),10,f = ceiling)
min_distance = 0

plot_distances <- function(wards, regions, districts, region_name, facilities_loc, prot_areas, add_scale = FALSE){
  
  # Filter data for each region
  wards <- wards %>% filter(Region_Nam == region_name)
  regions <- regions %>% filter(Region_Nam == region_name)
  districts <- districts %>% filter(Region_Nam == region_name)
  facilities <- facilities_loc %>% filter(Region_facility == region_name)
  prot <- st_intersection(prot_areas, regions)
  
  # Define region-specific bounding box
  bbox_region <- st_bbox(regions)
  
  p <- ggplot() +
    geom_sf(data = districts, fill = NA, color = "black") +
    geom_sf(data = regions, color = "dimgrey", fill = NA) +
    geom_sf(data = wards, aes(fill = distance_km), color = NA) +
    scale_fill_gradient(name = "Distance travelled (km)",
                        low = "#fefae0", high = "#ce4257",
                        na.value = "lightgrey",
                        labels = scales::comma,
                        limits = c(min_distance, max_distance)) +  
    geom_sf(data = prot, color = "#aec3b0", fill = "#aec3b0") +
    geom_sf(data = facilities, aes(geometry = centroid, shape = `Facility type`)) +
    scale_shape_manual(values = c(3, 15, 4, 19, 17)) +
    ggtitle(region_name) +
    theme_map() +
    theme(plot.title = element_text(hjust = 0.5, size = 13),
          legend.position = "none",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) +  # Hide legends for individual plots
    coord_sf(xlim = c(bbox_region["xmin"], bbox_region["xmax"]), 
             ylim = c(bbox_region["ymin"], bbox_region["ymax"]))  # Standardize cropping
  
  # Add scale bar ONLY if specified
  if (add_scale) {
    p <- p + annotation_scale(location = "br", width_hint = 0.2)
  }
  
  return(p)
}

# Create individual plots (one with scale bar)
mara_plot <- plot_distances(study_wards_shp, study_regs_shp, study_dists_shp, "Mara", facilities_loc, prot_areas)
lindi_plot <- plot_distances(study_wards_shp, study_regs_shp, study_dists_shp, "Lindi", facilities_loc, prot_areas)
morogoro_plot <- plot_distances(study_wards_shp, study_regs_shp, study_dists_shp, "Morogoro", facilities_loc, prot_areas)
mtwara_plot <- plot_distances(study_wards_shp, study_regs_shp, study_dists_shp, "Mtwara", facilities_loc, prot_areas, add_scale = TRUE)  # Only this one gets scale bar

# Extract shared legend from one plot
legend <- get_legend(mara_plot + theme(legend.position = "right"))

# Arrange plots in a 2x2 grid (hiding scale bars in other plots)
final_plot <- plot_grid(
  plot_grid(lindi_plot, mara_plot, morogoro_plot, mtwara_plot, ncol = 2, labels = c("A", "B", "C", "D")),
  legend,
  ncol = 2,
  rel_widths = c(3, 0.8)  # Adjust width to fit legend
)

# Display final plot
final_plot

png(filename = "figs/fig3.distance_travelled_map.png",width = 650, height = 500)
final_plot
dev.off()

## outputting for journal format
ragg::agg_tiff(
  filename = "figs/fig3_distance_travelled_map.tiff",
  width = 6.5,        # in inches
  height = 5.5,       # in inches
  units = "in",
  res = 300         # resolution in DPI
)
final_plot
dev.off()
