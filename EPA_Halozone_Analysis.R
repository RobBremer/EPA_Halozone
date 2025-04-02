## EPA Halozone Project

# Read in Packages
install.packages("googlesheets4")
library(tidyverse)
library(ggplot2)
library(readr)
library(readxl)
library(googlesheets4)
library(googledrive)
library(janitor)


# Read in Spreadsheets

nuts_metadata_202411 <- read_excel("C:/USers/Robert.Bremer/Downloads/EPAHalozone_DataSheets_202411.xlsx", sheet = "nuts_sample_log")%>%
  mutate(chl_a_sample_id = as.character(as.numeric(chl_a_sample_id))) %>%
  mutate(chl_a_vol_filtered = as.numeric(chl_a_vol_filtered)) %>%
  mutate(nutrients_sample_id = as.numeric(nutrients_sample_id))

exo_handwritten_202411 <- read_excel("C:/Users/Robert.Bremer/Downloads/EPAHalozone_DataSheets_202411.xlsx", sheet = "EXO_file_handwritten")

nutrients202411 <- read_excel("C:/Users/Robert.Bremer/Downloads/EPA Halozone WS24314 Nutrient Data.xlsx") %>%
  clean_names() %>%
  select(c(sample_id, si, no2, no3, n_n, nh4, po4)) %>%
  rename(nutrients_sample_id = sample_id) %>%
  mutate(nutrients_sample_id = str_replace(nutrients_sample_id, "EPA-",""))

chlorophyll202411 <- read_excel("C:/Users/Robert.Bremer/Downloads/EPA_Halozone_Chl-a_2024-11.xlsx") %>%
  clean_names() %>%
  rename(chl_a_sample_id = tube)

nuts_chl_combined <- nuts_metadata_202411 %>%
  merge(nutrients202411) %>%
  merge(chlorophyll202411)



# Graphs

ggplot(nuts_chl_combined, aes(x = station, y = si))+
  geom_()

#### Copying in from VSCode this should work
halo_site_coords <- read_excel("C:/Users/Robert.Bremer/Downloads/EPA_Halozone_Sample_Points.xlsx") %>%
  clean_names() %>%
  rename(decimalLongitude = longitude) %>%
  rename(decimalLatitude = latitude) %>%
  rename(station = name)

halo_metadata <- read_excel("C:/Users/Robert.Bremer/Downloads/EPAHalozone_DataSheets_202411.xlsx", sheet = "EXO_file_handwritten") %>%
  remove_empty("cols") %>%
  mutate(temp = as.numeric(temp))

halo_nuts_metadata <- read_excel("C:/Users/Robert.Bremer/Downloads/EPAHalozone_DataSheets_202411.xlsx", sheet = "nuts_sample_log") %>%
  mutate(chl_a_sample_id = as.numeric(chl_a_sample_id)) %>%
  mutate(nutrients_sample_id = as.numeric(nutrients_sample_id)) %>%
  mutate(chl_a_vol_filtered = as.numeric(chl_a_vol_filtered)) %>%
  mutate(chl_a_sample_id = as.numeric(chl_a_sample_id))


halo_nutrients <- read_excel("C:/Users/Robert.Bremer/Downloads/EPA Halozone WS24314 Nutrient Data.xlsx") %>%
  clean_names() %>%
  mutate(sample_id = gsub("EPA-","", sample_id)) %>%
  mutate(sample_id = as.numeric(sample_id))

halo_chl <- read_excel("C:/Users/Robert.Bremer/Downloads/EPA_Halozone_Chl-a_2024-11.xlsx") %>%
  clean_names() %>%
  mutate(tube = as.numeric(tube)) %>%
  drop_na(tube)

halo_data <- halo_site_coords %>%
  full_join(., halo_metadata, by = ("station")) %>%
  full_join(., halo_nuts_metadata, by = c("station", "inf_depth")) %>%
  full_join(., halo_nutrients, by = c("nutrients_sample_id" = "sample_id")) %>%
  full_join(., halo_chl, by = c("chl_a_sample_id" = "tube")) %>%
  drop_na(chl_a_sample_id)

## Graphs
ggplot(halo_data, aes(x = chl_a_sample_id, y = chl_a_ug_l, fill = transect, label = station))+
  geom_violin()+
  geom_text(size = 4)+
  labs(title = "Chlorophyll A", y = "Chlorophyll A (ug/L)", x = "Chlorophyll A Sample ID")

ggplot(halo_data, aes(x = nutrients_sample_id, y = si, fill = transect, label = station))+
  geom_violin()+
  geom_text(size = 4)+
  labs(title = "Silica", y = "Silica (uM)", x = "Nutrients Sample ID")

ggplot(halo_data, aes(x = nutrients_sample_id, y = n_n, fill = transect, label = station))+
  geom_violin()+
  geom_text(size = 4)+
  labs(title = "NO2 + NO3",y = "NO2 +NO3 (uM)", x = "Nutrients Sample ID")

ggplot(halo_data, aes(x = nutrients_sample_id, y = nh4, fill = transect, label = station))+
  geom_violin()+
  geom_text(size = 4)+
  labs(title = "NH4", y = "NH4 (uM)", x = "Nutrients Sample ID")

ggplot(halo_data, aes(x = nutrients_sample_id, y = po4, fill = transect, label = station))+
  geom_violin()+
  geom_text(size = 4)+
  labs(title = "PO4", y = "PO4 (uM)", x = "Nutrients Sample ID")
