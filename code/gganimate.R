library(tidyverse)
library(readxl)
library(here)
library(magrittr)
library(scales)
library(readxl)
library(ggpubr)
library(ggeasy)
library(ggfortify)
library(ggbeeswarm)
library(ggforce)
library(ggrepel)
library(kableExtra)

# stat analysis
library(broom)
library(lme4)
library(performance)
library(car)
library(emmeans)
library(glmmTMB)
library(MASS)

library(gganimate)
library(av)

# set the default theme for ggplot as theme_bw
theme_set(theme_classic())


# imprt the data ----------------------------------------------------------

`tetras-exp1-1` <- readRDS("~/Documents/2023_MPSIII_salbutamol/data/R_objects/tetras-exp1-1.rds")
`tetras-exp1-2` <- readRDS("~/Documents/2023_MPSIII_salbutamol/data/R_objects/tetras-exp1-2.rds")
`distance_data-exp1-1` <- readRDS("~/Documents/2023_MPSIII_salbutamol/data/R_objects/distance_data-exp1-1.rds")
`distance_data-exp1-2` <- readRDS("~/Documents/2023_MPSIII_salbutamol/data/R_objects/distance_data-exp1-2.rds")


`distance_data-exp1-2` %>%
  dplyr::filter(BIN_NUM %in% seq(1:20)) %>%
  dplyr::filter(fish_id %in% seq(1:10)) %>%
  group_by(fish_id, BIN_NUM) %>%
  mutate(total_distance = sum(TOTAL_DISTANCE_IN_ZONE)) %>%
  ggplot(aes(x = BIN_NUM, y = total_distance, group = fish_id, colour = fish_id)) +
  geom_point() +
  geom_line()

# animati



v <-
  `distance_data-exp1-2` %>%
  dplyr::filter(BIN_NUM %in% 3550:3600) %>%
  dplyr::filter(fish_id %in% 13:20) %>%
  group_by(fish_id, BIN_NUM) %>%
  mutate(total_distance = sum(TOTAL_DISTANCE_IN_ZONE),
         geno_treat = paste0(genotype, "_", treatment)) %>%
  ggplot(aes(x = BIN_NUM, y = total_distance, colour = fish_id)) +
  #coord_cartesian(ylim = c(0,100)) +
  geom_line(alpha = 1,
            aes(group = fish_id)) +
  labs(x = "time", y = "distance travelled") +
  theme(legend.position = "none")+


  #geom_smooth(aes(group = geno_treat)) +
  transition_reveal(BIN_NUM)
  # Here comes the gganimate specific bits

  animate(v, renderer = ffmpeg_renderer(), height = 2, width = 3,
          units = "in",  res = 150)
