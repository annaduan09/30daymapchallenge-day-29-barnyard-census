#### 30 Day Map Challenge: 2023 
#### Day 1: Points
#### Author: Anna Duan, Fall 2023
#### Dataset: Agriculture Production in Pennsylvania, 1850
#### Source: PA_Historical_and_Museum_Commission via OpenDataPhilly.org

#### Setup ####
library(tidyverse)
library(sf)
library(tigris)
library(stringr)
library(viridis)
library(mapview)
library(ggimage)

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}


pa_county <- counties(state = "PA", year = "2020") %>%
  dplyr::select(County = NAME) %>%
  st_transform("EPSG:2272") 

dat <- read.csv("Agriculture_Production_Data_in_Pennsylvania_1850_census_PA_Historical_and_Museum_Commission_20231011.csv") %>%
  dplyr::select(County,
                Horses.Count, 
                Asses.and.Mules.Count, 
                Milch.Cows.Count, 
                Working.Oxen.Count, 
                Other.Cattle.Count,
                Sheep.Count, 
                Swine.Count) %>%
  group_by(County) %>%
  dplyr::summarize(horse = sum(Horses.Count, na.rm = T),
            ass_mule = sum(Asses.and.Mules.Count, na.rm = T),
            milch_cow = sum(Milch.Cows.Count, na.rm = T),
            working_oxen = sum(Working.Oxen.Count, na.rm = T),
            other_cattle = sum(Other.Cattle.Count, na.rm = T),
            sheep = sum(Sheep.Count, na.rm = T),
            swine = sum(Swine.Count, na.rm = T)) %>%
  dplyr::mutate(livestock = sum(ass_mule, milch_cow, working_oxen, other_cattle, sheep, swine)) %>%
  right_join(pa_county, by = "County")

# List of animal columns
animal_columns <- c("horse", "ass_mule", "milch_cow", "working_oxen", "other_cattle",
                    "sheep", "swine", "livestock")

# # Create new columns for animals per mile
# for (col in animal_columns) {
#   dat[paste0(col, "_per_sqmi")] <- dat[, col] / as.numeric(dat$area_mi)
# }



dat <- dat %>%
  st_as_sf() %>%
  st_centroid() %>%
  mutate(cow = sample(c("/Users/annaduan/Documents/GitHub/30-day-map-challenge-2023/29 population/image/cow.png"), size = 1, replace = TRUE)) %>%
  mutate(x = st_coordinates(geometry)[, 1], 
         y = st_coordinates(geometry)[, 2]) %>%
  st_drop_geometry() %>%
  dplyr::select(County, x, y, cow) %>%
  left_join(dat, by = "County") %>%
  st_as_sf()

#### Map ####
# cows
ggplot() +
  geom_sf(data = dat, fill = "lightgoldenrodyellow", color = "lemonchiffon2") +
  geom_sf(data = st_centroid(dat), shape = 21, color = "maroon", fill = "maroon", aes(size = milch_cow, alpha = milch_cow)) +
  scale_size(name = "",
             range = c(0, 80),
             breaks = c(5000, 10000, 15000, 20000),
             guide = "none") +
  scale_alpha(name = "",
            range = c(0, 0.6),
             breaks = c(5000, 10000, 15000, 20000),
            guide = "none") +
  theme_void() + theme(legend.position = c(1, 0.4))

# ass mule
ggplot() +
  geom_sf(data = dat, fill = "lightgoldenrodyellow", color = "lemonchiffon2") +
  geom_sf(data = st_centroid(dat), shape = 21, color = "maroon", fill = "maroon", aes(size = ass_mule, alpha = ass_mule)) +
  scale_size(name = "",
             range = c(0, 80),
             breaks = c(100, 200, 300, 400),
             guide = "none") +
  scale_alpha(name = "",
              range = c(0, 0.6),
              breaks = c(100, 200, 300, 400),
              guide = "none") +
  theme_void() + theme(legend.position = c(0.97, 0.4))

# sheep
ggplot() +
  geom_sf(data = dat, fill = "lightgoldenrodyellow", color = "lemonchiffon2") +
  geom_sf(data = st_centroid(dat), shape = 21, color = "maroon", fill = "maroon", aes(size = sheep, alpha = sheep)) +
  scale_size(name = "",
             range = c(0, 80),
             guide = "none") +
  scale_alpha(name = "",
              range = c(0, 0.6),
              guide = "none") +
  theme_void() + theme(legend.position = c(0.97, 0.4))


# horse
ggplot() +
  geom_sf(data = dat, fill = "lightgoldenrodyellow", color = "lemonchiffon2") +
  geom_sf(data = st_centroid(dat), shape = 21, color = "maroon", fill = "maroon", aes(size = swine, alpha = swine)) +
  scale_size(name = "",
             range = c(0, 80),
             guide = "none") +
  scale_alpha(name = "",
              range = c(0, 0.6),
              guide = "none") +
  theme_void() + theme(legend.position = c(0.97, 0.4))
