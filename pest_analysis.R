
# install.packages(c("sf", "tmap"))
library(tidyverse)
library(sf)       
library(tmap)
library(readxl)
library(stringr)
# install.packages("rmapshaper")
library(rmapshaper)
library(scales)
library(patchwork)
# install.packages("ggtext")
library(ggtext)

# Part 1: Regulated Areas  -------------------------------------------------


## Download Data -------------------------------------------------------

# Create a folder named "data" in the current working directory to store the data
dir.create("data")

# Agrilus planipennis (emerald ash borer)
EAB_url <- "https://open.canada.ca/data/dataset/69f4ecd8-9761-4d40-b0f9-e186f2fcce5b/resource/05da66b3-645d-4530-aa18-53bd2b67edfb/download/emerald-ash-borer-surveillance-data-2002-to-2020-en.xlsx"
EAB_destfile <- "data/emerald-ash-borer-surveillance-data-2002-to-2020-en.xlsx"
download.file(EAB_url, EAB_destfile)
# Agrilus glabripennis (Asian longhorned beetle)
ALB_url <- "https://open.canada.ca/data/dataset/7d4e165c-d513-4eae-bbf9-795eb6137926/resource/e6c3fbbf-4665-41ac-8724-49a9d0481c75/download/asian-longhorned-beetle-surveillance-data-2010-2020.xlsx"
ALB_destfile <- "data/asian-longhorned-beetle-surveillance-data-2010-2020.xlsx"
download.file(ALB_url, ALB_destfile)
# Adelges tusgae (hemlock woolly adelgid)
HWA_url <- "https://open.canada.ca/data/dataset/385a21ab-1f9d-4db7-bd11-20221efd20b4/resource/295973db-e6a9-4df5-85fa-54e0341d4aeb/download/cfia_acia-1568-v1-hemlock_woolly_adelgid_surveillance_data_2010-2023-en.csv"
HWA_destfile <- "data/cfia_acia-1568-v1-hemlock_woolly_adelgid_surveillance_data_2010-2023-en.csv"
download.file(HWA_url, HWA_destfile)

## Check the Data  -----------------------------------------------

EAB_data <- read_xlsx("data/emerald-ash-borer-surveillance-data-2002-to-2020-en.xlsx")
ALB_data <- read_xlsx("data/asian-longhorned-beetle-surveillance-data-2010-2020.xlsx")
HWA_data <- read_csv("data/cfia_acia-1568-v1-hemlock_woolly_adelgid_surveillance_data_2010-2023-en.csv")


### EAB_data ----------------------------------------------------------------


str(EAB_data)
summary(EAB_data)
# Count total missing values in the entire data frame
print(sum(is.na(EAB_data)))
# Count missing values per column
print(colSums(is.na(EAB_data))) # 5446 missing values in COMMUNITY.
unique(EAB_data$PROVINCE)
# check the distribution of sampling locations
ggplot(EAB_data, 
       aes(x = PROVINCE)) +
  geom_bar() +
  labs(
    x = "PROVINCE",
    y = "Frequency"
  ) +
  theme_minimal() # ON takes up the most of the surveys (more than 40,000), then QC (~5000)


unique(EAB_data$RESULT)
ggplot(EAB_data, 
       aes(x = RESULT)) +
  geom_bar() +
  labs(
    x = "RESULT",
    y = "Frequency"
  ) +
  theme_minimal() 

summary(EAB_data$YEAR)

ggplot(EAB_data, 
       aes(x = YEAR)) +
  geom_bar() +
  labs(
    x = "YEAR",
    y = "Frequency"
  ) +
  theme_minimal()

# Create the distribution of NA data
ggplot(subset(EAB_data, RESULT == "DETECTED" & is.na(COMMUNITY)), 
       aes(x = YEAR)) +
  geom_bar() +
  labs(
    title = "Frequency of NA COMMUNITY Values in DETECTED Results by Year",
    x = "Year",
    y = "Frequency"
  ) +
  theme_minimal()

ggplot(subset(EAB_data, is.na(COMMUNITY)), 
       aes(x = RESULT)) +
  geom_bar() +
  labs(
    title = "Frequency of NA COMMUNITY Values in RESULT",
    x = "RESULT",
    y = "Frequency"
  ) +
  theme_minimal() # most of NAs are in NOT DETECTED
EAB_missing_community_summary <- EAB_data %>%
  filter(is.na(COMMUNITY)) %>%
  group_by(RESULT) %>%
  summarize(Count = n(), .groups = "drop") %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 1))
EAB_missing_community_summary

ggplot(subset(EAB_data, RESULT == "DETECTED" & is.na(COMMUNITY)), 
       aes(x = PROVINCE)) +
  geom_bar() +
  labs(
    x = "Province",
    y = "Frequency"
  ) +
  theme_minimal()




### ALB_data ----------------------------------------------------------------


str(ALB_data)
print(sum(is.na(ALB_data)))
unique(ALB_data$PROVINCE)
ggplot(ALB_data, 
       aes(x = PROVINCE)) +
  geom_bar() +
  labs(
    x = "PROVINCE",
    y = "Frequency"
  ) +
  theme_minimal()
unique(ALB_data$RESULT) # All locations are NOT DETECTED?!
summary(ALB_data$YEAR)
ggplot(ALB_data, 
       aes(x = YEAR)) +
  geom_bar() +
  labs(
    x = "YEAR",
    y = "Frequency"
  ) +
  theme_minimal()

### HWA_data ----------------------------------------------------------------


str(HWA_data)
print(sum(is.na(HWA_data)))
print(colSums(is.na(HWA_data))) # 440 missing values in Community
unique(HWA_data$Prov)

# Standardize data
EAB_data$SURVEY <- str_to_title(EAB_data$SURVEY)
ALB_data$SURVEY <- str_to_title(ALB_data$SURVEY)

# Convert headers to uppercase and rename "PROV" to "PROVINCE"
names(HWA_data) <- toupper(names(HWA_data))
names(HWA_data)[names(HWA_data) == "PROV"] <- "PROVINCE"
# Standardize the province values to uppercase 
HWA_data$PROVINCE <- toupper(HWA_data$PROVINCE)
# Convert the RESULT column values to uppercase
HWA_data$RESULT <- toupper(HWA_data$RESULT)

# now check the Standardize data
unique(HWA_data$PROVINCE)
ggplot(HWA_data, 
       aes(x = PROVINCE)) +
  geom_bar() +
  labs(
    title = "Frequency of PROVINCE",
    x = "Year",
    y = "Frequency"
  ) +
  theme_minimal()


unique(HWA_data$RESULT)

ggplot(HWA_data, 
       aes(x = RESULT)) +
  geom_bar() +
  labs(
    x = "RESULT",
    y = "Frequency"
  ) +
  theme_minimal()

summary(HWA_data$YEAR)

ggplot(subset(HWA_data, is.na(COMMUNITY)), 
       aes(x = RESULT)) +
  geom_bar() +
  labs(
    title = "Frequency of NA COMMUNITY Values in RESULT",
    x = "RESULT",
    y = "Frequency"
  ) +
  theme_minimal() # most of NAs are in DETECTED
HWA_missing_community_summary <- HWA_data %>%
  filter(is.na(COMMUNITY)) %>%
  group_by(RESULT) %>%
  summarize(Count = n(), .groups = "drop") %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 1))
HWA_missing_community_summary

ggplot(subset(HWA_data, RESULT == "DETECTED" & is.na(COMMUNITY)), 
       aes(x = YEAR)) +
  geom_bar() +
  labs(
    title = "Frequency of NA COMMUNITY Values in DETECTED Results by Year",
    x = "Year",
    y = "Frequency"
  ) +
  theme_minimal()
# lots of NAs in 2018 and 2019


ggplot(subset(HWA_data, RESULT == "DETECTED" & is.na(COMMUNITY)), 
       aes(x = PROVINCE)) +
  geom_bar() +
  labs(
    title = "Frequency of NA COMMUNITY Values in DETECTED Results by Province",
    x = "Province",
    y = "Frequency"
  ) +
  theme_minimal()
# looks like Nova Scotia lost a lot of data in COMMUNITY

ggplot(subset(HWA_data, YEAR==2018 & RESULT == "DETECTED" & is.na(COMMUNITY)), 
       aes(x = PROVINCE)) +
  geom_bar() +
  labs(
    title = "Frequency of NA COMMUNITY Values in DETECTED Results by Province",
    x = "Province",
    y = "Frequency"
  ) +
  theme_minimal()
# COMMUNITY NAs in 2018 are all from Nova Scotia!

ggplot(subset(HWA_data, YEAR==2019 & RESULT == "DETECTED" & is.na(COMMUNITY)), 
       aes(x = PROVINCE)) +
  geom_bar() +
  labs(
    title = "Frequency of NA COMMUNITY Values in DETECTED Results by Province",
    x = "Province",
    y = "Frequency"
  ) +
  theme_minimal()
# In 2019, both NS and ON have COMMUNITY NAs


head(filter(HWA_data %>% 
              filter(YEAR==2018 & PROVINCE=="NOVA SCOTIA" & !is.na(COMMUNITY))))
head(filter(HWA_data %>% 
              filter(YEAR==2018 & PROVINCE=="NOVA SCOTIA" & is.na(COMMUNITY))))
head(filter(HWA_data %>% 
              filter(YEAR==2018 & PROVINCE=="NOVA SCOTIA" & !is.na(COMMUNITY) &
                       RESULT == "NOT DETECTED")))
head(filter(HWA_data %>% 
              filter(YEAR==2018 & PROVINCE=="NOVA SCOTIA" & !is.na(COMMUNITY) & 
                       RESULT == "DETECTED")))
# NS does not have COMMUNITY data in 2018 with DETECTED result

head(filter(HWA_data %>% 
              filter(YEAR==2019 & PROVINCE=="NOVA SCOTIA" & !is.na(COMMUNITY) &
                       RESULT == "DETECTED")))
# NS does not have COMMUNITY data in 2019 with DETECTED result!

## Attempt to improve missing values in COMMUNITY --------------------------

# This function takes a data frame containing the columns LATITUDE, LONGITUDE, and COMMUNITY,
# and imputes missing COMMUNITY values by using rows with matching LATITUDE and LONGITUDE.
# For each group of rows that share the same LATITUDE and LONGITUDE:
#   - If at least one row has a non-NA COMMUNITY value, all missing values in that group are replaced
#     with the first non-NA COMMUNITY value found.
#   - If all rows have missing COMMUNITY values, they remain unchanged.

# a helper function that imputes COMMUNITY within one group of same LATITUDE and LONGITUDE.
impute_group <- function(d) {
  if (all(is.na(d$COMMUNITY))) {
    # If all COMMUNITY values are NA in this group, leave as is.
    return(d)
  } else {
    # Otherwise, pick the first non-NA COMMUNITY value.
    fill_value <- d$COMMUNITY[which(!is.na(d$COMMUNITY))[1]]
    # Replace NA values with this fill_value
    d$COMMUNITY[is.na(d$COMMUNITY)] <- fill_value
    return(d)
  }
}

# the main function.
impute_community <- function(df) {
  df %>% 
    group_by(LATITUDE, LONGITUDE) %>% 
    group_modify(~ impute_group(.x)) %>% 
    ungroup()
}

EAB_data <- impute_community(EAB_data)
print(colSums(is.na(EAB_data))) # missing values in COMMUNITY decreased from 5446 to 4430.

HWA_data <- impute_community(HWA_data)
print(colSums(is.na(HWA_data))) # missing values in COMMUNITY decreased from 440 to 375



## DETECTED locations -------------------------------

# Only include DETECTED locations
EAB_detected <- EAB_data %>% filter(RESULT == "DETECTED")
unique(EAB_detected$PROVINCE) # "ONTARIO" "QUEBEC" "MANITOBA" "NOVA SCOTIA" "NEW BRUNSWICK"

HWA_detected <- HWA_data %>% filter(RESULT == "DETECTED")
unique(HWA_detected$PROVINCE) # "ONTARIO"     "NOVA SCOTIA"

# Convert these filtered data frames to sf objects:
EAB_sf_detected <- st_as_sf(EAB_detected, coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE)
HWA_sf_detected <- st_as_sf(HWA_detected, coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE)

# Reproject to a metric CRS
EAB_proj_detected <- st_transform(EAB_sf_detected, 3978)
HWA_proj_detected <- st_transform(HWA_sf_detected, 3978)

# Create buffers around the detected points
buffer_dist <- 10000  
EAB_buffers_detected <- st_buffer(EAB_proj_detected, dist = buffer_dist)
HWA_buffers_detected <- st_buffer(HWA_proj_detected, dist = buffer_dist)

# Union the buffers to create a single "regulated area" polygon for each pest
EAB_regulated_detected <- st_transform(st_union(EAB_buffers_detected), 4326)
HWA_regulated_detected <- st_transform(st_union(HWA_buffers_detected), 4326)

# Create sf objects for mapping
EAB_poly_detected <- st_sf(Pest = "Emerald Ash Borer", geometry = EAB_regulated_detected)
HWA_poly_detected <- st_sf(Pest = "Hemlock Woolly Adelgid", geometry = HWA_regulated_detected)

# Combine them for plotting:
regulated_detected_areas <- rbind(EAB_poly_detected, HWA_poly_detected)



## Pic 1. Map ----------------------------------------------------------

# Canada map
Canada_url <- "https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/lpr_000b16a_e.zip"
Canada_destfile <- "data/lpr_000b16a_e.zip"
download.file(Canada_url, Canada_destfile, method = "auto")
unzip(Canada_destfile, exdir = "data/lpr_000b16a_e")

canada_map <- st_read("data/lpr_000b16a_e/lpr_000b16a_e.shp")
# Reproject to EPSG:4326 if necessary for consistency
canada_map <- st_transform(canada_map, 4326)


canada_map_simplified <- ms_simplify(canada_map, keep = 0.01)

canada <- ggplot() +
  geom_sf(data = canada_map_simplified, fill = "gray90", color = "black", size = 0.2) +
  #geom_sf_text(data= canada_map_simplified, aes(label = PRENAME), size = 3, fontface = "bold", color = "darkblue")+
  geom_sf(data = regulated_detected_areas, aes(fill = Pest), alpha = 0.4) +
  theme_minimal() +
  labs(fill = "Pest Species") +
  scale_fill_manual(values = c("Emerald Ash Borer" = "blue",
                               "Hemlock Woolly Adelgid" = "red"))

canada

# show the infected provinces

canada_subset <- canada_map_simplified %>%
  filter(PRENAME %in% c("Ontario", "Quebec", "New Brunswick", "Nova Scotia", "Manitoba"))

prov <- ggplot() +
  geom_sf(data = canada_subset, fill = "gray90", color = "black", size = 0.2) +
  geom_sf(data = regulated_detected_areas, aes(fill = Pest), alpha = 0.4) +
  geom_sf_text(data = canada_subset, aes(label = PRENAME), size = 3, fontface = "bold", color = "darkblue") +
  labs(fill = "Pest Species", x = "", y = "") +  # Remove default "x" and "y" labels
  scale_fill_manual(values = c("Emerald Ash Borer" = "blue",
                               "Hemlock Woolly Adelgid" = "red")) +
  theme_minimal()

prov
  
map_pic <- canada / prov + plot_annotation(tag_levels = "A")

map_pic



# Part 2: Number of Detections Over Time ------------------

## Summarize Detections Per Year -----------------------------------

# Filter for 2015 to present 
EAB_data_filtered <- EAB_data %>% filter(YEAR >= 2015)
str(EAB_data_filtered)
ggplot(EAB_data_filtered, 
       aes(x = PROVINCE)) +
  geom_bar() +
  labs(
    x = "PROVINCE",
    y = "Frequency"
  ) +
  theme_minimal()


ALB_data_filtered <- ALB_data %>% filter(YEAR >= 2015)
str(ALB_data_filtered)
ggplot(ALB_data_filtered, 
       aes(x = PROVINCE)) +
  geom_bar() +
  labs(
    x = "PROVINCE",
    y = "Frequency"
  ) +
  theme_minimal()

HWA_data_filtered <- HWA_data %>% filter(YEAR >= 2015)
str(HWA_data_filtered)
ggplot(HWA_data_filtered, 
       aes(x = PROVINCE)) +
  geom_bar() +
  labs(
    x = "PROVINCE",
    y = "Frequency"
  ) +
  theme_minimal()



EAB_summary <- EAB_data_filtered %>%
  group_by(SURVEY, YEAR, RESULT) %>%
  summarize(Count = n(), .groups = "drop")

ALB_summary <- ALB_data_filtered %>%
  group_by(SURVEY, YEAR, RESULT) %>%
  summarize(Count = n(), .groups = "drop")

HWA_summary <- HWA_data_filtered %>%
  group_by(SURVEY, YEAR, RESULT) %>%
  summarize(Count = n(), .groups = "drop")

# Combine the three summaries
detections <- bind_rows(EAB_summary, ALB_summary, HWA_summary)



## Pic 2. Total Survey ------------------------------------------------------


ggplot(detections, aes(x = YEAR, y = Count, fill = RESULT)) +
  geom_col(position = "stack") +
  geom_text(aes(label = Count), 
            position = position_stack(vjust = 0.5), 
            color = "black", 
            size = 4) +
  facet_wrap(~ SURVEY, scales = "free_y") +
  theme_minimal(base_size = 10) +
  scale_fill_manual(values = c("DETECTED" = "#C38EC7",
                               "NOT DETECTED" = "#CCCCFF")) +
  theme(
    panel.grid.major = element_blank(),      
    panel.grid.minor = element_blank(),      
    axis.line = element_line(color = "black", size = 1),  
    strip.text = element_text(face = "bold", size = 12)
  ) +
  labs(
    x = "Year",
    y = "Number of Surveys",
    fill = "Result"
  )


## Pic 3. Detection Rates  ---------------------------------------------


# Compute detection rates 
detection_rates <- detections %>%
  group_by(SURVEY, YEAR) %>% 
  mutate(total_surveys = sum(Count)) %>%
  ungroup() %>%
  filter(RESULT == "DETECTED") %>%
  mutate(detection_rate = Count / total_surveys)
detection_rates

ggplot(detection_rates, aes(x = YEAR, y = detection_rate, color = SURVEY, group = SURVEY)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Year",
    y = "Detection Rate",
    color = "Species"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black", size = 1)
  )






## Pic 4. Province ----------------------------------------------------------------

combined_data <- bind_rows(EAB_data_filtered, ALB_data_filtered, HWA_data_filtered)

### Detection count heatmap -------------------------------------------------

province_counts <- combined_data %>%
  group_by(SURVEY, YEAR, PROVINCE) %>%
  filter(RESULT == "DETECTED") %>%
  summarize(Count = n(), .groups = "drop")

# Create the heatmap for detection counts.
province_counts_hp <-  ggplot(province_counts, aes(x = factor(YEAR), y = PROVINCE, fill = Count)) +
  geom_tile() +  
  facet_wrap(~ SURVEY, scales = "free_y") +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(
    x = "Year",
    y = "Province",
    fill = "Detection Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    axis.line.x = element_line(color = "black", size = 1),
    axis.line.y = element_line(color = "black", size = 1),
    strip.text = element_text(face = "bold")
  )
province_counts_hp
# QC and ON had no detected EAB surveys in 2018?

### Detection Rate heatmap --------------------------------------------------


# calculate detection rates.
province_rates <- combined_data %>%
  group_by(SURVEY, YEAR, PROVINCE) %>%
  summarize(
    total_surveys = n(),
    detections = sum(RESULT == "DETECTED", na.rm = TRUE),
    detection_rate = detections / total_surveys,
    .groups = "drop"
  ) %>%
  filter(detection_rate > 0)
province_rates
# Create the heatmap for detection rates
province_rates_hp <- ggplot(province_rates, aes(x = factor(YEAR), y = PROVINCE, fill = detection_rate)) +
  geom_tile() +  # Outlines each cell
  facet_wrap(~ SURVEY, scales = "free_y") +
  scale_fill_gradient(low = "yellow", high = "red", labels = percent_format(accuracy = 1)) +
  labs(
    x = "Year",
    y = "Province",
    fill = "Detection Rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    axis.line.x = element_line(color = "black", size = 1),
    axis.line.y = element_line(color = "black", size = 1),
    strip.text = element_text(face = "bold")
  )
province_rates_hp


province_combined_plot <- province_counts_hp / province_rates_hp + plot_annotation(tag_levels = "A")
province_combined_plot # saved the pic, 1200 x 1000


## Pic 5. Community -----------------------------------------------------------------


combined_data_clean <- combined_data %>%
  filter(!is.na(COMMUNITY)) %>% 
  # Create a new combined label "Community (Province)"
  mutate(COMM_PROV = paste0(COMMUNITY, " (", PROVINCE, ")")) %>% 
  # Arrange so that communities are sorted by province, then community name
  arrange(PROVINCE, COMMUNITY) %>%
  # Convert COMM_PROV to a factor using the current order
  mutate(COMM_PROV = factor(COMM_PROV, levels = unique(COMM_PROV)))

head(combined_data_clean$COMM_PROV)



### Detection count heatmap -------------------------------------------------



community_counts <- combined_data_clean %>%
  filter(RESULT == "DETECTED") %>%
  group_by(SURVEY, YEAR, PROVINCE, COMM_PROV) %>%
  summarize(Count = n(), .groups = "drop") 


# Generate the heatmap with YEAR on the x-axis and the new COMM_PROV label on the y-axis
community_counts_hp <- ggplot(community_counts, aes(x = factor(YEAR), y = COMM_PROV, fill = Count)) +
  geom_tile() +  
  facet_wrap(~ SURVEY, scales = "free_y") +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(
    x = "Year",
    y = "Community",
    fill = "Detection Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),          # Remove default grid lines
    axis.line.x = element_line(color = "black", size = 1),
    axis.line.y = element_line(color = "black", size = 1),
    strip.text = element_text(face = "bold")
  )
community_counts_hp



### Detection Rate heatmap --------------------------------------------------


# Compute detection rate by grouping over SURVEY, YEAR, and COMMUNITY.
community_rates <- combined_data_clean %>%
  group_by(SURVEY, YEAR, COMM_PROV) %>%
  summarize(
    total_surveys = n(),
    detections = sum(RESULT == "DETECTED"),
    detection_rate = detections / total_surveys,
    .groups = "drop"
  ) %>% 
  filter(detection_rate > 0)
community_rates

# Create the heatmap with YEAR on x-axis and COMMUNITY on y-axis, faceting by SURVEY
community_rates_hp <- ggplot(community_rates, aes(x = factor(YEAR), y = COMM_PROV, fill = detection_rate)) +
  geom_tile() +  
  facet_wrap(~ SURVEY, scales = "free_y") +
  scale_fill_gradient(low = "yellow", high = "red", labels = percent_format(accuracy = 1)) +
  labs(
    x = "Year",
    y = "Community",
    fill = "Detection Rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(), 
    axis.line.x = element_line(color = "black", size = 1),
    axis.line.y = element_line(color = "black", size = 1),
    strip.text = element_text(face = "bold")
  )
community_rates_hp

community_combined_plot <- community_counts_hp / community_rates_hp + plot_annotation(tag_levels = "A")
community_combined_plot

