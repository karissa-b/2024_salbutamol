# # IMPORT DATA -------------------------------------------------------------

# add the fish_ids to the metadata list
meta <- read_excel("data/pilot/7 days/06062023 salbutamol pilot fish metadata.xlsx",
                   sheet = "14 days") %>%

  # tidy up colnames
  mutate(fish_id = as.character(fish_id),
         treatment = factor(treatment, levels = c("untreated",
                                                  "20 µM Salbutamol",
                                                  "100 µM Salbutamol")),
         sex = as.factor(sex),
         ymazePosition = as.character(ymazePosition),
         genotype = factor(genotype, levels = c("het", "hom"))

  )

# define the filenames
file_list <- list.files("data/pilot/14 days/raw_data/distances",
           pattern = "*.csv",
           full.names = TRUE)

# make an object containing all the raw data files.

df <- tibble(data_file_distances = file_list) %>%  #Import data as a tibble with nested lists
  mutate(data = map(file_list, ~ read_csv(.x )),

         # cleanup the filename to match what is in the meta sheet
         data_file_distances = str_remove(data_file_distances, pattern = "data/pilot/14 days/raw_data/distances/"))


# converto to a tibble rather than a list
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
  dplyr::select(-temp, -TIME) %>% # unselect unnessary and problematic columns
  left_join(meta,
            by = join_by(data_file_distances, ymazePosition)
            ) %>%
  pivot_wider( names_from = ENDPOINT,
               values_from = value)

df %>%
  saveRDS("data/pilot/14 days/processed_data/distanceData14days.rds")

df %>%
  group_by(fish_id) %>%
  mutate(total_dist = sum(TOTAL_DISTANCE_IN_ZONE )) %>%
  dplyr::distinct(fish_id, .keep_all = TRUE) %>%
  dplyr::filter(genotype %in% c('het', 'hom')) %>%
  ggplot(aes(x = genotype, y = total_dist, colour = as.factor(`start time`))) +
  geom_boxplot() +
  geom_jitter()

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
  geom_smooth(aes(group = treatment),
              se = F) +
  geom_label(aes(label = fish_id),
             data = . %>%
               dplyr::filter(total_distance > 25000))
