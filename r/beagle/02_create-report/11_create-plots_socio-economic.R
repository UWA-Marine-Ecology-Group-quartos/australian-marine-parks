###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Socio-economic boat ramp survey responses
# Task:    Create socio-economic benchmark plots
# Author:  Claude Spencer
# Date:    October 2024
###

rm(list=ls())

# Load libraries
library(tidyverse)
library(CheckEM)
library(ggh4x)

# Set the study name
name <- "BeagleAMP"
park <- "beagle"

test <- readRDS(paste0("data/", park, "/tidy/National_KAP.rds")) %>%
  clean_names() %>%
  dplyr::select(cmwlth_support, cmwlth_impfish, cmwlth_impnonextr, cmwlth_impenv,
                cmwlth_awastated, cmwlth_awaname, mp, year) %>%
  dplyr::filter(mp %in% "Ngari Capes MP") %>%
  dplyr::mutate(cmwlth_awastated = if_else(cmwlth_awastated == 1, "Aware", "Unaware"),
                cmwlth_awaname = if_else(cmwlth_awaname == 1, "Aware", "Unaware")) %>%
  glimpse()

socdat <- function(data, columns){
  data %>%
    pivot_longer(cols = {{columns}},
                 names_to = "metric",
                 values_to = "value") %>%
    dplyr::filter(metric %in% "cmwlth_awaname" & value %in% "Aware" | # Because its 100% unaware its getting rid of the survey
                    metric %in% "cmwlth_awastated" & value %in% "Aware" |
                    metric %in% "cmwlth_impenv" & value %in% "Positive" |
                    metric %in% "cmwlth_impfish" & value %in% c("Positive", "No change") |
                    metric %in% "cmwlth_support" & value %in% "Supportive" |
                    metric %in% "cmwlth_impnonextr" & value %in% c("Somewhat increase", "Strongly increase")) %>%
    dplyr::group_by(mp, year, metric) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(total = sum(n),
                  perc = (n / total) * 100) %>%
    rowwise() %>%
    dplyr::mutate(conf.int = list(prop.test(n, total)$conf.int * 100)) %>%
    dplyr::mutate(lower_ci = conf.int[1],
                  upper_ci = conf.int[2]) %>%
    ungroup() %>%
    select(-c(conf.int, n, total)) %>%
    glimpse()
}

dat <- socdat(data = test, columns = c(cmwlth_support, cmwlth_impfish, cmwlth_impnonextr,
                                        cmwlth_impenv, cmwlth_awastated, cmwlth_awaname))

ggplot(data = dat, aes(x = year, y = perc)) +
  geom_line(linetype = "dashed") +
  scale_colour_identity() +
  geom_point() +
  geom_vline(xintercept = 2018.5, linetype = "dashed", linewidth = 0.3) +
  # geom_text(aes(x = x, y = y, label = subtitle), size = 3, fontface = "italic") +
  facet_wrap(~metric, ncol = 1) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                width = 0.2) +
  labs(x = "Year", y = "% of participants") +
  # scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(limits = c(2017, 2024)) +
  theme_classic()



dat <- read.csv(paste0("data/", park, "/tidy/socio-economic_monitoring_MN.csv")) %>%
  clean_names() %>%
  dplyr::filter(!metric %in% "Awarenes of AMPs nationally amongst South-west network residents") %>%
  dplyr::mutate(metric = case_when(metric %in% "Awarenes of an AMP in area" ~ "Awareness of the SwC or GMP",
                                   metric %in% "Correctly name an AMP" ~ "Correctly name the SwC or GMP",
                                   metric %in% "Supportive of AMP NPZ" ~ "Supportive of the NPZs in the SwC and GMP",
                                   metric %in% "AMP NPZ benefit environment" ~ "Perception that NPZs in the SwC and GMP benefit the marine environment",
                                   metric %in% "AMP NPZ negatively effect my fishing" ~ "Perception that NPZs in the SwC and GMP negatively impact recreational fishing"),
                subtitle = case_when(metric %in% "Awareness of the SwC or GMP" ~ "Knowledge",
                                     metric %in% "Correctly name the SwC or GMP" ~ "Knowledge",
                                     metric %in% "Supportive of the NPZs in the SwC and GMP" ~ "Attitudes",
                                     metric %in% "Perception that NPZs in the SwC and GMP benefit the marine environment" ~ "Attitudes",
                                     metric %in% "Perception that NPZs in the SwC and GMP negatively impact recreational fishing" ~ "Attitudes"),
                y = case_when(metric %in% "Perception that NPZs in the SwC and GMP negatively impact recreational fishing" ~ 5,
                              .default = 95),
                x = 2017.5) %>%
  glimpse()

trends <- dat %>%
  group_by(metric) %>%
  summarise(
    mean_2020 = mean(mean[year == 2020]),
    mean_2023 = mean(mean[year == 2023]),
    colour = case_when(
      mean_2023 > mean_2020 ~ "#0bb524",
      mean_2023 < mean_2020 ~ "#b5220b",
      TRUE ~ "#0bb524"
    )
  ) %>%
  dplyr::select(metric, colour)

plotdat <- dat %>%
  left_join(trends) %>%
  glimpse()

ggplot(data = dplyr::filter(plotdat, subtitle %in% "Knowledge"), aes(x = year, y = mean)) +
  geom_line(linetype = "dashed", aes(colour = colour)) +
  scale_colour_identity() +
  geom_point(aes(colour = colour)) +
  geom_vline(xintercept = 2018.5, linetype = "dashed", linewidth = 0.3) +
  # geom_text(aes(x = x, y = y, label = subtitle), size = 3, fontface = "italic") +
  facet_wrap(~metric, ncol = 1, scales = "free_y") +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci, colour = colour),
                width = 0.2) +
  labs(x = "Year", y = "% of participants") +
  # scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(limits = c(2017, 2024)) +
  theme_classic() +
  facetted_pos_scales(y = list(!metric %in% "Perception that NPZs in the SwC and GMP negatively impact recreational fishing" ~ scale_y_continuous(limits = c(0, 100)),
                               metric %in% "Perception that NPZs in the SwC and GMP negatively impact recreational fishing" ~ scale_y_reverse(limits = c(100, 0))))

ggsave(filename = paste0("plots/", park, "/socio-economic/control-plots_knowledge.png"), dpi = 300,
       units = "in", height = 3.6, width = 7, bg = "white")

ggplot(data = dplyr::filter(plotdat, subtitle %in% "Attitudes"), aes(x = year, y = mean)) +
  geom_line(linetype = "dashed", aes(colour = colour)) +
  scale_colour_identity() +
  geom_point(aes(colour = colour)) +
  geom_vline(xintercept = 2018.5, linetype = "dashed", linewidth = 0.3) +
  # geom_text(aes(x = x, y = y, label = subtitle), size = 3, fontface = "italic") +
  facet_wrap(~metric, ncol = 1, scales = "free_y") +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci, colour = colour),
                width = 0.2) +
  labs(x = "Year", y = "% of participants") +
  # scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(limits = c(2017, 2024)) +
  theme_classic() +
  facetted_pos_scales(y = list(!metric %in% "Perception that NPZs in the SwC and GMP negatively impact recreational fishing" ~ scale_y_continuous(limits = c(0, 100)),
                               metric %in% "Perception that NPZs in the SwC and GMP negatively impact recreational fishing" ~ scale_y_reverse(limits = c(100, 0))))

ggsave(filename = paste0("plots/", park, "/socio-economic/control-plots_attitudes.png"), dpi = 300,
       units = "in", height = 5.4, width = 7, bg = "white")

