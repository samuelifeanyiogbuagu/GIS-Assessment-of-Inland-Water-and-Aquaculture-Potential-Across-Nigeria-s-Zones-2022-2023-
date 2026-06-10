#Below is a fully integrated R workflow to :
#Creates geopolitical zones
#Calculates inland water area by zone
#Creates AWUI
#Creates Aquaculture Potential Index (API)
#Produces three choropleth maps
#Exports all outputs

##Required Packages
# Install once
install.packages(c("sf","dplyr","ggplot2","viridis"))

# Load libraries
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)


#STEP 1: LOAD STATE SHAPEFILE
states <- st_read("NGA_adm1.shp")

#STEP 2: REMOVE NON-STATE FEATURE
states <- states %>%
  filter(NAME_1 != "Water body")

#STEP 3: CREATE GEOPOLITICAL ZONE LOOKUP TABLE
zone_lookup <- data.frame(
  NAME_1 = c(
    
    # South East
    "Abia","Anambra","Ebonyi","Enugu","Imo",
    
    # South South
    "Akwa Ibom","Bayelsa","Cross River",
    "Delta","Edo","Rivers",
    
    # South West
    "Ekiti","Lagos","Ogun",
    "Ondo","Osun","Oyo",
    
    # North Central
    "Benue","Kogi","Kwara",
    "Nassarawa","Niger","Plateau",
    "Federal Capital Territory",
    
    # North East
    "Adamawa","Bauchi","Borno",
    "Gombe","Taraba","Yobe",
    
    # North West
    "Jigawa","Kaduna","Kano",
    "Katsina","Kebbi","Sokoto",
    "Zamfara"
  ),
  
  zone = c(
    rep("South East",5),
    rep("South South",6),
    rep("South West",6),
    rep("North Central",7),
    rep("North East",6),
    rep("North West",7)
  )
)

#STEP 4: ASSIGN STATES TO ZONES
states_zone <- states %>%
  left_join(zone_lookup, by = "NAME_1")

#STEP 5: VERIFY MATCHES
states_zone %>%
  filter(is.na(zone)) %>%
  select(NAME_1)

#STEP 6: DISSOLVE STATES INTO GEOPOLITICAL ZONES
zones <- states_zone %>%
  group_by(zone) %>%
  summarise()

#STEP 7: SAVE GEOPOLITICAL ZONES SHAPEFILE
st_write(
  zones,
  "Nigeria_Geopolitical_Zones.shp",
  delete_layer = TRUE
)

#STEP 8: CREATE GEOPOLITICAL MAP
zone_map <- ggplot(zones) +
  geom_sf(
    aes(fill = zone),
    color = "black",
    linewidth = 0.4
  ) +
  labs(
    title = "Nigeria Geopolitical Zones",
    fill = "Zone"
  ) +
  theme_minimal()

zone_map

#STEP 9: LOAD INLAND WATER SHAPEFILE
water <- st_read("Nigeria_inland_water_lines.shp")

#STEP 10: PROJECT BOTH LAYERS
zones_proj <- st_transform(zones, 32632)

water_proj <- st_transform(water, 32632)

#STEP 11: INTERSECT WATER WITH ZONES
water_zone <- st_intersection(
  water_proj,
  zones_proj
)

#STEP 12: CALCULATE INLAND WATER AREA
water_zone$water_area_km2 <-
  as.numeric(st_area(water_zone)) / 1000000

#STEP 13: SUMMARIZE WATER AREA BY ZONE
zone_water <- water_zone %>%
  group_by(zone) %>%
  summarise(
    inland_water_area_km2 =
      sum(water_area_km2, na.rm = TRUE)
  )

#STEP 14: EXPORT WATER AREA TABLE
write.csv(
  zone_water %>% st_drop_geometry(),
  "Zone_Inland_Water_Area.csv",
  row.names = FALSE
)

#STEP 15: CREATE AQUACULTURE DATASET
final_data <- data.frame(
  zone = c(
    "North Central",
    "North East",
    "North West",
    "South East",
    "South South",
    "South West"
  ),
  
  aquaculture_holdings = c(
    124140,
    36480,
    122400,
    102710,
    376680,
    234940
  )
)

#STEP 16: MERGE WATER + AQUACULTURE
final_data <- zone_water %>%
  st_drop_geometry() %>%
  left_join(final_data, by = "zone")

#STEP 17: CALCULATE AWUI
final_data$AWUI <-
  final_data$aquaculture_holdings /
  final_data$inland_water_area_km2

#STEP 18: CALCULATE WATER SHARE
final_data$water_share_pct <-
  100 *
  final_data$inland_water_area_km2 /
  sum(final_data$inland_water_area_km2)

final_data$water_share_pct <-
  round(final_data$water_share_pct,2)

#STEP 19: CALCULATE API
#Normalize values 
final_data$water_norm <-
  final_data$inland_water_area_km2 /
  max(final_data$inland_water_area_km2)

final_data$aquaculture_norm <-
  final_data$aquaculture_holdings /
  max(final_data$aquaculture_holdings)

#Aquaculture Potential Index
final_data$API <-
  0.6 * final_data$water_norm +
  0.4 * final_data$aquaculture_norm

#STEP 20: JOIN BACK TO SPATIAL DATA
zones_map <- zones %>%
  left_join(final_data, by = "zone")

#MAP 1: INLAND WATER AREA
map_water <- ggplot(zones_map) +
  
  geom_sf(
    aes(fill = water_area_km2),
    color = "black"
  ) +
  
  geom_sf_text(
    aes(label = round(water_area_km2,0))
  ) +
  
  scale_fill_viridis_c(
    option = "C",
    name = "Water Area\n(km²)"
  ) +
  
  labs(
    title = "Inland Water Area by Geopolitical Zone"
  ) +
  
  theme_minimal()

map_water


#MAP 2: AWUI
map_awui <- ggplot(zones_map) +
  
  geom_sf(
    aes(fill = AWUI),
    color = "black"
  ) +
  
  geom_sf_text(
    aes(label = round(AWUI,1))
  ) +
  
  scale_fill_viridis_c(
    option = "D",
    name = "AWUI"
  ) +
  
  labs(
    title = "Aquaculture Water Utilization Index"
  ) +
  
  theme_minimal()

map_awui

#MAP 3: API
map_api <- ggplot(zones_map) +
  
  geom_sf(
    aes(fill = API),
    color = "black"
  ) +
  
  geom_sf_text(
    aes(label = round(API,2))
  ) +
  
  scale_fill_viridis_c(
    option = "A",
    name = "API"
  ) +
  
  labs(
    title = "Aquaculture Potential Index"
  ) +
  
  theme_minimal()

map_api

#SAVE MAPS
ggsave(
  "Map_1_Inland_Water_Area.png",
  map_water,
  width = 10,
  height = 8,
  dpi = 300
)

ggsave(
  "Map_2_AWUI.png",
  map_awui,
  width = 10,
  height = 8,
  dpi = 300
)

ggsave(
  "Map_3_API.png",
  map_api,
  width = 10,
  height = 8,
  dpi = 300
)

#FINAL RANKING TABLE
ranking <- final_data %>%
  arrange(desc(API))

ranking

#Export:
write.csv(
  ranking,
  "Aquaculture_Potential_Ranking.csv",
  row.names = FALSE
)


  ## 📄 License & Terms of Use
  
 This project is licensed under the **MIT License** - see below for details.

### The MIT License (MIT)

Copyright (c) 2026

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
  
  The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

---
  
  ### 💡 Terms of Use & Attribution
  
  If you use this workflow, data, or the policy metrics (AWUI/API) in a academic paper, policy brief, or commercial report, please provide appropriate attribution:
  
  * **Citation** > "Spatial Analysis of Aquaculture Potential and Water Utilization in Nigeria's Geopolitical Zones, GitHub Repository, 2026."
* **Data Disclaimer:** The spatial boundaries and hydrographic data used in this workflow are derived from public domain administrative maps (`NGA_adm1` and `Nigeria_inland_water_lines`). Users should verify localized spatial accuracy before utilizing these metrics for boundary-sensitive engineering project decisions.
