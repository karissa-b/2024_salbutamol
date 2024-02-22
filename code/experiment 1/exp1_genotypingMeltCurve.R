# genotyping full experiment fish
library(tidyverse)
library(readxl)
library(magrittr)
library(scales)

# read in the data

# melt cureve deriviities
dfs <- read_excel("data/exp1/genotyping_data/2023_0708_KB_melt_curve_genotyping_salbutamol_full_exp1_meltregionderivativedata.xls",
           skip = 6) %>%
  pivot_longer(names_to = "Reading",
               values_to = "dF",
               starts_with("Reading")
               ) %>%
  dplyr::select(Position = `Well Location`, Reading, dF)

# temperature readings
temps <- read_excel("data/exp1/genotyping_data/2023_0708_KB_melt_curve_genotyping_salbutamol_full_exp1_meltregiontemperaturedata.xls",
           skip = 6) %>%
  pivot_longer(names_to = "Reading",
               values_to = "Temperature",
               starts_with("Reading")
               ) %>%
  dplyr::select(Position = `Well Location`, Reading, Temperature)

# join these together
data.meltcurve <-
  dfs %>%
  left_join(temps)

# results file
data.results <- read_excel("data/exp1/genotyping_data/2023_0708_KB_melt_curve_genotyping_salbutamol_full_exp1_results.xls",
           skip = 6) %>%
  dplyr::select(Position = Well, sample = `Sample Name`, Tm1, Tm2, Tm3) %>%
  mutate(group = case_when(
    Position == "A1" ~ "neg control",
    Position == "A2" ~ "positive control",
    TRUE ~ "unknown"
  ))




# metadata of this fish and ckeanup
meta <- read_excel("data/exp1/2023_June30_salbutamolexp1_meta.xlsx") %>%
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

      Tm1 > 83 ~ "het", # if the melt temp is higher than 83C, this is the wt band and so het

      between(Tm1, left = 81, 83) ~ "hom", # the lower Tm product refers to the hom band and so these would be hom

      Position == "A1" ~ "neg control",
      Position == "A2" ~ "pos control"

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
  left_join(data.results %>%
              dplyr::select(PositionForGeno = Position, genotype)) %>%
  dplyr::select(1,2,3, genotype, everything()) %>%
  saveRDS("data/exp1/metadata_withGenotype.rds")

data.meltcurve %>%
  left_join(data.results) %>%
  saveRDS("data/exp1/genotyping_data/meltcurveWithGenotypes.rds")












