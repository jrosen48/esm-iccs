library(tidyverse)

d1 <- read_csv("imuscle-esm.csv")
d2 <- read_csv("scimo-esm.csv")
d3 <- read_csv("stemie-esm.csv")

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

# STEM-IE


m_b <- lmer(behavioral_engagement ~ 1 + (1|participant_ID) + (1|beep_ID_new) + (1|program_ID), data = stem_ie_merged)
m_a <- lmer(affective_engagement ~ 1 + (1|participant_ID) + (1|beep_ID_new) + (1|program_ID), data = stem_ie_merged)
m_c <- lmer(cognitive_engagement ~ 1 + (1|participant_ID) + (1|beep_ID_new) + (1|program_ID), data = stem_ie_merged)

l <- list(m_b, m_a, m_c)

f <- function(x) {
    sjstats::icc(x) %>% 
        as.data.frame() %>% 
        rownames_to_column() %>% 
        setNames(c("group", "ICC")) %>% 
        spread(group, ICC)
}

ICC_df <- map_df(l, f)
ICC_df <- bind_cols(var = c("behavioral", "affective", "cognitive"),
                    ICC_df)

m_bi <- lmer(behavioral_engagement ~ 1 + (longitudinal_signal_number|participant_ID) + (1|beep_ID_new) + (1|program_ID), data = stem_ie_merged)
m_ai <- lmer(affective_engagement ~ 1 + (longitudinal_signal_number|participant_ID) + (1|beep_ID_new) + (1|program_ID), data = stem_ie_merged)
m_ci <- lmer(cognitive_engagement ~ 1 + (longitudinal_signal_number|participant_ID) + (1|beep_ID_new) + (1|program_ID), data = stem_ie_merged)

l <- list(m_bi, m_ai, m_ci)

f <- function(x) {
    sjstats::icc(x) %>% 
        as.data.frame() %>% 
        rownames_to_column() %>% 
        setNames(c("group", "ICC")) %>% 
        spread(group, ICC)
}

ICC_dfi <- map_df(l, f)
ICC_dfi <- bind_cols(var = c("behavioral", "affective", "cognitive"),
                     ICC_df)

write_csv(ICC_df, here::here("Data", "iMUScLE", "stemie_icc.csv"))

# SciMo

library(lme4)

scimo_merged$beep_ID <- str_c(scimo_merged$signal_value, scimo_merged$response_date, scimo_merged$teacher_ID)

library(brms)

m_bb <- brm(beh_enga ~ 1 + (1|stud_ID) + (1|beep_ID) + (1|teacher_ID), data = scimo_merged)

m_bb_icc <- icc(m_bb, posterior = TRUE, prob = .95)
as.vector(m_bb_icc)
attr(m_bb_icc, "hdi.icc")
attr(m_bb_icc, "hdi.icc")$icc_beep_ID
attr(m_bb_icc, "hdi.icc")$icc_stud_ID
attr(m_bb_icc, "hdi.icc")$icc_teacher_ID

m_b <- lmer(beh_enga ~ 1 + (1|stud_ID) + (1|beep_ID) + (1|teacher_ID), data = scimo_merged)
m_a <- lmer(aff_enga ~ 1 + (1|stud_ID) + (1|beep_ID) + (1|teacher_ID), data = scimo_merged)
m_c <- lmer(cog_enga ~ 1 + (1|stud_ID) + (1|beep_ID) + (1|teacher_ID), data = scimo_merged)

l <- list(m_b, m_a, m_c)

f <- function(x) {
    sjstats::icc(x) %>% 
        as.data.frame() %>% 
        rownames_to_column() %>% 
        setNames(c("group", "ICC")) %>% 
        spread(group, ICC)
}

ICC_df <- map_df(l, f)
ICC_df <- bind_cols(var = c("behavioral", "affective", "cognitive"),
                    ICC_df)
ICC_df
write_csv(ICC_df, here::here("Data", "iMUScLE", "scimo_merged.csv"))

# iMUScLE

library(lme4)

imuscle_esm$beep_ID <- str_c(imuscle_esm$PERIOD, imuscle_esm$time, imuscle_esm$Date, imuscle_esm$TEACHID)

write_csv(imuscle_esm,"~/documents/imuscle-esm.csv")


m_b <- lmer(beh_enga ~ 1 + (1|STUDID) + (1|beep_ID) + (1|TEACHID), data = imuscle_esm)
m_a <- lmer(aff_enga ~ 1 + (1|STUDID) + (1|beep_ID) + (1|TEACHID), data = imuscle_esm)
m_c <- lmer(cog_enga ~ 1 + (1|STUDID) + (1|beep_ID) + (1|TEACHID), data = imuscle_esm)

l <- list(m_b, m_a, m_c)

f <- function(x) {
    sjstats::icc(x) %>% 
        as.data.frame() %>% 
        rownames_to_column() %>% 
        setNames(c("group", "ICC")) %>% 
        spread(group, ICC)
}

ICC_df <- map_df(l, f)
ICC_df <- bind_cols(var = c("behavioral", "affective", "cognitive"),
                    ICC_df)
ICC_df

write_csv(ICC_df, here::here("Data", "iMUScLE", "imuscle_icc.csv"))
