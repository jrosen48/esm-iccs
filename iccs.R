library(tidyverse)
library(brms)
library(sjstats)

d1 <- read_csv("imuscle-esm.csv")
d2 <- read_csv("scimo-esm.csv")
d3 <- read_csv("stemie-esm.csv")

# STEM-IE


m_b <- lmer(behavioral_engagement ~ 1 + (1|participant_ID) + (1|beep_ID_new) + (1|program_ID), data = d3)
m_a <- lmer(affective_engagement ~ 1 + (1|participant_ID) + (1|beep_ID_new) + (1|program_ID), data = d3)
m_c <- lmer(cognitive_engagement ~ 1 + (1|participant_ID) + (1|beep_ID_new) + (1|program_ID), data = d3)
# 
# l <- list(m_b, m_a, m_c)
# 
# f <- function(x) {
#     sjstats::icc(x) %>% 
#         as.data.frame() %>% 
#         rownames_to_column() %>% 
#         setNames(c("group", "ICC")) %>% 
#         spread(group, ICC)
# }
# 
# ICC_df <- map_df(l, f)
# ICC_df <- bind_cols(var = c("behavioral", "affective", "cognitive"),
#                     ICC_df)
# 
# m_bi <- lmer(behavioral_engagement ~ 1 + (longitudinal_signal_number|participant_ID) + (1|beep_ID_new) + (1|program_ID), data = stem_ie_merged)
# m_ai <- lmer(affective_engagement ~ 1 + (longitudinal_signal_number|participant_ID) + (1|beep_ID_new) + (1|program_ID), data = stem_ie_merged)
# m_ci <- lmer(cognitive_engagement ~ 1 + (longitudinal_signal_number|participant_ID) + (1|beep_ID_new) + (1|program_ID), data = stem_ie_merged)
# 
# l <- list(m_bi, m_ai, m_ci)
# 
# f <- function(x) {
#     sjstats::icc(x) %>% 
#         as.data.frame() %>% 
#         rownames_to_column() %>% 
#         setNames(c("group", "ICC")) %>% 
#         spread(group, ICC)
# }
# 
# ICC_dfi <- map_df(l, f)
# ICC_dfi <- bind_cols(var = c("behavioral", "affective", "cognitive"),
#                      ICC_df)
# 
# write_csv(ICC_df, here::here("Data", "iMUScLE", "stemie_icc.csv"))

# SciMo

m3b <- brm(beh_enga ~ 1 + (1|stud_ID) + (1|beep_ID) + (1|teacher_ID), data = scimo_merged)
m3c <- brm(cog_enga ~ 1 + (1|stud_ID) + (1|beep_ID) + (1|teacher_ID), data = scimo_merged)
m3a <- brm(aff_enga ~ 1 + (1|stud_ID) + (1|beep_ID) + (1|teacher_ID), data = d2)

m3aicc <- icc(m3a, posterior = TRUE, prob = .95)

f <- function(x) {
    b <- t(as.data.frame(attr(x, "hdi.icc")))
    d <- data.frame(var = rownames(b))
    d$point <- as.vector(x)
    d$lower <- as.vector(b[, 1])
    d$upper <- as.vector(b[, 2])
    d
}

x

as.vector(m3aicc)


# m_b <- lmer(beh_enga ~ 1 + (1|stud_ID) + (1|beep_ID) + (1|teacher_ID), data = scimo_merged)
# m_a <- lmer(aff_enga ~ 1 + (1|stud_ID) + (1|beep_ID) + (1|teacher_ID), data = scimo_merged)
# m_c <- lmer(cog_enga ~ 1 + (1|stud_ID) + (1|beep_ID) + (1|teacher_ID), data = scimo_merged)
# 
# l <- list(m_b, m_a, m_c)
# 
# f <- function(x) {
#     sjstats::icc(x) %>% 
#         as.data.frame() %>% 
#         rownames_to_column() %>% 
#         setNames(c("group", "ICC")) %>% 
#         spread(group, ICC)
# }
# 
# ICC_df <- map_df(l, f)
# ICC_df <- bind_cols(var = c("behavioral", "affective", "cognitive"),
#                     ICC_df)
# ICC_df
# write_csv(ICC_df, here::here("Data", "iMUScLE", "scimo_merged.csv"))

# iMUScLE

m_b <- lmer(beh_enga ~ 1 + (1|STUDID) + (1|beep_ID) + (1|TEACHID), data = imuscle_esm)
m_a <- lmer(aff_enga ~ 1 + (1|STUDID) + (1|beep_ID) + (1|TEACHID), data = imuscle_esm)
m_c <- lmer(cog_enga ~ 1 + (1|STUDID) + (1|beep_ID) + (1|TEACHID), data = imuscle_esm)



# icc <- read_csv("~/Documents/pse-iccs.csv")
# 
# icc %>% 
#     mutate(dataset = c(rep("iMUScLE", 3), rep("STEM-IE", 3), rep("Sci-Mo", 3))) %>% 
#     gather(key, val, -var, -dataset) %>% 
#     ggplot(aes(x = key, y = val, fill = var)) +
#     geom_col(position = "dodge") +
#     facet_wrap(~dataset) +
#     theme_bw() +
#     scale_fill_viridis_d("", option = "C", labels =  c("Affective", "Behavioral", "Cognitive")) +
#     scale_x_discrete("", labels = c("Class/Program", "Individual", "Moment")) +
#     coord_flip() +
#     xlab(NULL) +
#     ylab("Intra-class Correlation") +
#     theme(text = element_text(family = "Times", size = 14)) +
#     ggtitle("Sources of Variability for Engagement in Science") +
#     labs(caption = "Data at the momentary level was collected using an experience sampling method.")
# 
# ggsave('icc.png', width =  9, height = 7)

