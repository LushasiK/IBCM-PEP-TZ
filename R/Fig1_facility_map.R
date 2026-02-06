## E Rees
## Figure 1 - map of population by district and facility locations

library(tidyverse)
library(sf)
library(ggthemes)
library(scales)
library(ragg)

study_regs <- c("Mara", "Lindi", "Morogoro", "Mtwara")
facilities <- read.csv("data/facilities_lat_log.csv", na.strings="")
# facilities <- read_rds("data/facilities_loc_centroids.RDS")

# Prepare geodata
# Load shapefiles
region_shp <- st_read("data/GIS", "regions_2022_population")
district_shp <- st_read("data/GIS", "districts_2022_population")
ward_shp <- st_read("data/GIS", "TZ_Ward_2012_pop")
prot_areas <- st_read("data/GIS", "Protected_areas")
names(prot_areas)[names(prot_areas) == 'Reg_ID'] <- 'Region_Nam'

# Subset shapefiles for study regions
study_regs_shp <- subset(region_shp, region_shp$Region_Nam %in% study_regs)
study_dists_shp <- subset(district_shp, district_shp$Region_Nam %in% study_regs)
study_wards_shp <- subset(ward_shp, ward_shp$Region_Nam %in% study_regs)

facilities <- facilities %>% 
  filter(Region_facility != "Arusha") %>% 
  dplyr::rename("Facility type" = Facility.type)

facilities_loc <- facilities %>% 
  filter(Longitude != "NA") %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) 


############################
# Map

region_names <- data.frame(name = study_regs,
                           y = c(-1, -9,-6.3,-11.3),
                           x = c(35.2, 40.2,35.8,40)) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) 

region_map <- ggplot() +
  geom_sf(data = study_dists_shp, aes(fill=Population), color = NA) +
  scale_fill_gradient(name="Population per district \n(2022 census)",
                      low = "#FFF5C2", high = "#9d0208", labels = comma) +
  geom_sf(data = prot_areas,
          color="#C9C7BE", fill="#C9C7BE") + 
  geom_sf(data = region_shp,
          color="dimgrey", fill=NA) +
  geom_sf(data = facilities_loc, aes(shape = `Facility type`)) +
  scale_shape_manual(values=c(3,15, 4,19, 17)) +
  geom_sf(data = study_regs_shp,
          fill=NA, color = "#3D3D3D",lwd=0.7) +
  geom_sf_text(data = region_names, aes(label = name),fontface = "bold") +
  theme_map() +
  theme(legend.position = "right") +
  xlab("") + 
  ylab("")
region_map

png(filename="figs/fig1.png",width=800, height=500)
region_map
dev.off()

ragg::agg_tiff(
  filename = "figs/fig1.tiff",
  width = 7,
  height = 5,
  units = "in",
  res = 300
)
region_map
dev.off()
