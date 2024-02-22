`tetras-exp1-1` <- readRDS("~/Documents/2023_MPSIII_salbutamol/data/R_objects/tetras-exp1-1.rds")
`tetras-exp1-2` <- readRDS("~/Documents/2023_MPSIII_salbutamol/data/R_objects/tetras-exp1-2.rds")
`distance_data-exp1-1` <- readRDS("~/Documents/2023_MPSIII_salbutamol/data/R_objects/distance_data-exp1-1.rds")
`distance_data-exp1-2` <- readRDS("~/Documents/2023_MPSIII_salbutamol/data/R_objects/distance_data-exp1-2.rds")

bin_df <- tibble(
  # 10 min buins
  bins6 = c(rep(1, 600),
            rep(2, 600),
            rep(3, 600),
            rep(4, 600),
            rep(5, 600),
            rep(6, 600)
  ) %>% as.factor(),
  BIN_NUM = `distance_data-exp1-1` $BIN_NUM %>% unique)


fit.dists <-
  `distance_data-exp1-1` %>%
  left_join(bin_df) %>%
  group_by(fish_id, bins6) %>%
  mutate(total_distance = sum(TOTAL_DISTANCE_IN_ZONE)) %>%
  dplyr::distinct(bins6, .keep_all = T) %>%
  lmer(total_distance ~ (genotype*treatment*bins6*sex) + (1|behavBatch) +(1|fish_id),
       data = .)


fit.dists.2 <-
  `distance_data-exp1-2` %>%
  left_join(bin_df) %>%
  group_by(fish_id, bins6) %>%
  mutate(total_distance = sum(TOTAL_DISTANCE_IN_ZONE)) %>%
  dplyr::distinct(bins6, .keep_all = T) %>%
  lmer(total_distance ~ (genotype*treatment*bins6*sex) + (1|behavBatch) +(1|fish_id),
       data = .)

an.1 <- Anova(fit.dists)

pairwisePvals.1 <- emmeans(fit.dists, list(pairwise ~ genotype * treatment * sex), adjust = "tukey")

an.2 <- Anova(fit.dists.2)

pairwisePvals.2 <- emmeans(fit.dists.2, list(pairwise ~ genotype * treatment * sex), adjust = "tukey")

# geno treat sex
a <- print(emmeans(fit.dists, ~ genotype * treatment * sex), type = "response") %>%
  as_tibble() %>%
  ggplot(aes(x = treatment, y = emmean, colour = genotype)) +
  geom_col(aes(fill = genotype),
           alpha = 0.75,
           width = 0.75,
           position = position_dodge(width = 0.75)) +
  geom_errorbar(aes(ymin =  lower.CL, ymax = upper.CL),
                width = 0.5,
                position = position_dodge(width = 0.75)) +
  facet_wrap(~sex, nrow = 1) +
  scale_y_continuous(limits = c(0, 15000)) +
  labs(x = "Treatment",
       y = "Distance travelled in 1 h (cm)",
       title = "7 days on salbutamol")


b <- print(emmeans(fit.dists.2, ~ genotype * treatment * sex), type = "response") %>%
  as_tibble() %>%
  ggplot(aes(x = treatment, y = emmean, colour = genotype)) +
  geom_col(aes(fill = genotype),
           alpha = 0.75,
           width = 0.75,
           position = position_dodge(width = 0.75)) +
  geom_errorbar(aes(ymin =  lower.CL, ymax = upper.CL),
                width = 0.5,
                position = position_dodge(width = 0.75)) +
  facet_wrap(~sex, nrow = 1) +
  scale_y_continuous(limits = c(0, 15000)) +
  labs(x = "Treatment",
       y = "Distance travelled in 1 h (cm)",
       title = "7 days on salbutamol + 1 month no treatment ")


ggarrange(a, b,
          common.legend = T)
  ggsave("output/plots/genoTreatSex-exp-11-12.png",
         bg = "white",
         width = 18, height = 10,
         units = "cm", scale = 1.5)



# genoTreatBinSex ---------------------------------------------------------


print(emmeans(fit.dists, ~ genotype * treatment * bins6 * sex),
      type = "response") %>%
  as_tibble() %>%
  mutate(group = paste0(genotype, "_",
                        treatment, "_",
                        sex),
         genotreat = paste0(genotype, "_", treatment)) %>%
  dplyr::filter(genotreat != "het_20 µM Salbutamol") %>%
  mutate(exp = "7 days on salbutamol") %>%
  bind_rows(  print(emmeans(fit.dists.2, ~ genotype * treatment * bins6 * sex),
                    type = "response") %>%
                as_tibble() %>%
                mutate(group = paste0(genotype, "_",
                                      treatment, "_",
                                      sex),
                       genotreat = paste0(genotype, "_", treatment)) %>%
                dplyr::filter(genotreat != "het_20 µM Salbutamol") %>%
                mutate(exp = "7 days on salbutamol + 1 month no treatment")     )  %>%
  ggplot(aes(x = bins6, y = emmean, colour = genotreat)) +
  geom_point(aes(fill = group),
             show.legend = F,
             alpha = 0.75,
             position = position_dodge(width = 0.75)) +
  geom_errorbar(aes(ymin =  lower.CL,
                    ymax = upper.CL),
                position = position_dodge(0.75)) +
  geom_line(aes(group = group),
            position = position_dodge(0.75),
            show.legend = F) +
  facet_wrap(~sex + exp,
             nrow = 1,
             scales = "free_x") +
  labs(x = "Time interval in a Y-maze (10 mins)",
       y = "Distance travelled (cm)") +
    theme(legend.position = "bottom")
    ggsave("output/plots/genoTreatSexBin-exp-11-12.png",
           bg = "white",
           width = 18, height = 10,
           units = "cm", scale = 1.5)

emmeans(fit.dists, list(pairwise ~ genotype * treatment * sex), adjust = "tukey")


# tetras ------------------------------------------------------------------

#glm <-
  final_data %>%
  left_join(LR_Bias) %>%
  mutate(
    non_alts = total_turns - alts,
    bin = as.factor(bin)
  ) %>%
  glmmTMB(
    cbind(alts, non_alts) ~ (bin + genotype + treatment + sex)^3 + L_R_bias + (1|behavBatch) + (1|fish_id),
    family = betabinomial(),
    data = .
  )

Anova(glm) %>%
  as.data.frame() %>%
  dplyr::rename(pval = `Pr(>Chisq)`) %>%
  kable() %>%
  kable_styling(full_width = FALSE) %>%
  row_spec(row = 4, bold = TRUE)
# only hets
#
print(emmeans(fit.dists, ~ genotype * treatment * bins6 * sex),
      type = "response") %>%
  as_tibble() %>%
  mutate(group = paste0(genotype, "_",
                        treatment, "_",
                        sex),
         genotreat = paste0(genotype, "_", treatment)) %>%
  dplyr::filter(genotreat == "het_untreated") %>%

  mutate(exp = "7 days on salbutamol") %>%
  bind_rows(  print(emmeans(fit.dists.2, ~ genotype * treatment * bins6 * sex),
                    type = "response") %>%
                as_tibble() %>%
                mutate(group = paste0(genotype, "_",
                                      treatment, "_",
                                      sex),
                       genotreat = paste0(genotype, "_", treatment)) %>%
                dplyr::filter(genotreat != "het_20 µM Salbutamol") %>%
                mutate(exp = "7 days on salbutamol + 1 month no treatment")     )  %>%
  ggplot(aes(x = bins6, y = emmean, colour = genotreat)) +
  geom_point(aes(fill = group),
             show.legend = F,
             alpha = 0.75,
             position = position_dodge(width = 0.75)) +
  geom_errorbar(aes(ymin =  lower.CL,
                    ymax = upper.CL),
                position = position_dodge(0.75)) +
  geom_line(aes(group = group),
            position = position_dodge(0.75),
            show.legend = F) +
  facet_wrap(~sex,
             nrow = 1,
             scales = "free_x") +
  labs(x = "Time interval in a Y-maze (10 mins)",
       y = "Distance travelled (cm)") +
  theme(legend.position = "bottom")
