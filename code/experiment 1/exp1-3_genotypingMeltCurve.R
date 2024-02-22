# genotyping full experiment fish
library(tidyverse)
library(readxl)
library(magrittr)
library(scales)

# read in the data

files <- list.files(
  "data/exp1.3/genotyping_data/",
  full.names = TRUE
)

# melt cureve deriviities
dfs <-
  read_delim(
    grep(files, pattern = "derivativedata", value = T), # the file with derivitives
    delim = "\t",
    skip = 8) %>%
  pivot_longer(names_to = "Reading",
               values_to = "dF",
               starts_with("Reading")
  ) %>%
  dplyr::select(Position = `Well Location`, Reading, dF)

# temperature readings
temps <-
  read_delim(
    grep(files, pattern = "temperaturedata", value = T), # the file with temps
    delim = "\t",
    skip = 8) %>%
  pivot_longer(names_to = "Reading",
               values_to = "Temperature",
               starts_with("Reading")
               ) %>%
  dplyr::select(Position = `Well Location`, Reading, Temperature)

# fluor readings
fluors <-
  read_delim(
    grep(files, pattern = "normalizeddata", value = T), # the file with fluor
    delim = "\t",
    skip = 8) %>%
  pivot_longer(names_to = "Reading",
               values_to = "fluor",
               starts_with("Reading")
  ) %>%
  dplyr::select(Position = `Well Location`, Reading, fluor)

# join these together
data.meltcurve <-
  temps %>%
  left_join(dfs) %>%
  left_join(fluors)

# results file
data.results <-
  read_delim(
    grep(files, pattern = "results", value = T), # the file with derivitives
    delim = "\t",
    skip = 8) %>%
  dplyr::select(Position = Well, sample = `Sample Name`, Tm1, Tm2, Tm3) %>%
  mutate(
    group = case_when(
      Position %in% c(paste0("A", 1:3), paste0("H",11:12)) ~ "neg control",
      TRUE ~ "unknown")
  )


# metadata of this fish and ckeanup
meta <- read_excel("data/exp1.3/2024_Jan10_salbutamolexp1_3_meta.xlsx") %>%
  mutate(fish_id = as.character(fish_id),

         treatment = factor(treatment,
                            levels = c("untreated", "20 µM Salbutamol")),

         sex = as.factor(sex),

         behavBatch = as.factor(behavBatch),

         ymazeUnit = as.factor(ymazeUnit),

         ymazePosition = as.factor(ymazePosition)) %>%

  dplyr::select(fish_id, treatment, sex, PositionForGeno, everything())

# plot out the genotyping results
data.meltcurve %>%
  left_join(data.results) %>%
  ggplot(aes(x = Temperature, y = dF)) +
  geom_line(aes(group = Position,
                colour = group)) +
  scale_x_continuous(limits = c(78,86),
                     breaks = seq(70,90)) +
  scale_y_continuous(labels = comma)

# define the hets and homs
data.results %<>%
  mutate(
    genotype = case_when(

      group == "neg control" ~ NA,

      Tm1 > 82.5 ~ "het", # if the melt temp is higher than 83C, this is the wt band and so het

      between(Tm1, left = 80.5, 83) ~ "hom", # the lower Tm product refers to the hom band and so these would be hom

    )
  )

# repeat the plot
data.meltcurve %>%
  left_join(data.results) %>%
  ggplot(aes(x = Temperature, y = dF)) +
  geom_line(aes(group = Position,
                colour = genotype)) +
  scale_x_continuous(limits = c(78,86),
                     breaks = seq(70,90)) +
  scale_y_continuous(labels = comma)





# export data for the final document
meta %>%
  dplyr::select(-genotype) %>%
  left_join(
    data.results %>%
      dplyr::select(PositionForGeno = Position, genotype)
            ) %>%
  dplyr::select(1,2,3, genotype, everything()) %>%
  saveRDS("data/exp1.3/metadata_withGenotype.rds")

data.meltcurve %>%
  left_join(data.results) %>%
  saveRDS("data/exp1.3/genotyping_data/meltcurveWithGenotypes.rds")













