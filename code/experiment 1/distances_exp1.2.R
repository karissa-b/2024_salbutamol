# # IMPORT DATA -------------------------------------------------------------

meta <- readRDS("data/exp1.3/metadata_withGenotype.rds")

# define the filenames
file_list <- list.files("data/exp1.3/raw_data/distances",
                        pattern = "*.csv",
                        full.names = TRUE)

# make an object containing all the raw data files.

df <- tibble(data_file_distances = file_list) %>%  #Import data as a tibble with nested lists
  mutate(data = map(file_list, ~ read_csv(.x )),

         # cleanup the filename to match what is in the meta sheet
         data_file_distances = str_remove(data_file_distances,
                                          pattern = "data/exp1.3/raw_data/distances/"))

df

# convert to a tibble rather than a list
df %<>%
  unnest(data)

# cleanup the Arena zone columns to match the meta column
df <- df %>%
  gather(key = "temp", value = "value", starts_with("A")) %>%
  mutate(ymazePosition = str_remove(temp, pattern = "_Z[:digit:]"),
         ymazePosition = str_remove(ymazePosition, pattern = "A"),
         zone = str_remove(temp, pattern = "A*._"),
         zone = str_remove(zone, pattern = "Z")
  ) %>%
  dplyr::select(-temp, -TIME)  # unselect unnessary and problematic columns

df %<>%
left_join(meta,
            by = join_by(data_file_distances, ymazePosition)
  ) %>%
  pivot_wider( names_from = ENDPOINT,
               values_from = value)

df %>%
saveRDS("data/exp1.3/processed_data/distanceDataexp1-3.rds")

df %>%
  group_by(fish_id) %>%
  mutate(total_dist = sum(TOTAL_DISTANCE_IN_ZONE )) %>%
  dplyr::distinct(fish_id, .keep_all = TRUE) %>%
  dplyr::filter(genotype %in% c('het', 'hom'),
                sex %in% c("m", "f")) %>%
  ggplot(
    aes(x = genotype, y = total_dist)
    ) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(
    aes(colour = ymazePosition)
  ) +
  facet_wrap(~treatment+sex, nrow = 1)

bin_df <- tibble(bins10 = c(rep(1, 360),
                            rep(2, 360),
                            rep(3, 360),
                            rep(4, 360),
                            rep(5, 360),
                            rep(6, 360),
                            rep(7, 360),
                            rep(8, 360),
                            rep(9, 360),
                            rep(10, 360)
),
bins6 = c(rep(1, 600),
          rep(2, 600),
          rep(3, 600),
          rep(4, 600),
          rep(5, 600),
          rep(6, 600)
),
BIN_NUM = df$BIN_NUM %>% unique
)

df %>%
  left_join(bin_df) %>%
  group_by(fish_id, bins6) %>%
  mutate(total_distance = sum(TOTAL_DISTANCE_IN_ZONE)) %>%
  dplyr::distinct(bins6, .keep_all = T) %>%
  dplyr::filter(genotype %in% c('het', 'hom')) %>%
  ggplot(aes(x = bins6, y = total_distance, colour = treatment)) +
  geom_jitter(alpha = 0.75) +
  facet_wrap(~sex + genotype) +
  geom_smooth(aes(group = treatment),
              se = F) +
  geom_label(aes(label = fish_id),
             data = . %>%
               dplyr::filter(total_distance > 25000))
