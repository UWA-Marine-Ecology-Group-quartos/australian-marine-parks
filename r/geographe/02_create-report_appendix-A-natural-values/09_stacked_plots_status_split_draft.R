# Stacked plots

# Clear your environment
rm(list = ls())

# Set the study name
name <- "GeographeAMP"
park <- "geographe"

# Load libraries
library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(ggnewscale)
library(scales)
library(viridis)
library(patchwork)
library(tidyterra)
library(png)
library(lwgeom)
library(tidytext)
library(ggtext)
library(CheckEM)

theme_collapse<-theme(
  panel.grid.major=element_line(colour = "white"),
  panel.grid.minor=element_line(colour = "white", size = 0.25),
  plot.margin= grid::unit(c(0, 0, 0, 0), "in"))

theme.larger.text<-theme(
  strip.text.x = element_text(size = 5,angle = 0),
  strip.text.y = element_text(size = 5),
  axis.title.x=element_text(vjust=-0.0, size=10),
  axis.title.y=element_text(vjust=0.0,size=10),
  axis.text.x=element_text(size=8),
  axis.text.y=element_text(size=8),
  legend.title = element_text(family="TN",size=8),
  legend.text = element_text(family="TN",size=8))

pd <- position_dodge(width = 0.8)

# read in maxn
sti <- CheckEM::australia_life_history %>%
  clean_names() %>%
  dplyr::select(family, genus, species, rls_thermal_niche) %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  dplyr::distinct() %>%
  glimpse()

maxn <- readRDS(paste0("data/", park, "/raw/_count-with-zeros.RDS")) %>%
  mutate(year = year(date_time)) %>%
  left_join(sti) %>%
  select(year, sample, status, scientific_name, family, genus, species, count,
         rls_thermal_niche) %>%
  # dplyr::filter(!count > 200, # Remove some outliers
  #               # !sample %in% "779", ##HE what was 779?
  #               geoscience_roughness < 4) %>% # Remove outliers in roughness
  glimpse()

length(unique(maxn$sample)) * length(unique(maxn$scientific_name))

# workout mean maxn for each species ---
maxn.10 <- maxn %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  group_by(year, status, scientific) %>%
  summarise(
    maxn = mean(count, na.rm = TRUE),
    se   = sd(count, na.rm = TRUE) / sqrt(dplyr::n()),
    .groups = "drop") %>%
  # dplyr::filter(!scientific%in%c('Carangoides sp1', 'Unknown spp'))%>%
  group_by(year, scientific) %>%
  summarise(rank_maxn = sum(maxn, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  slice_max(order_by = rank_maxn, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(
    maxn %>%
      mutate(scientific = paste(genus, species, sep = " ")) %>%
      group_by(year, status, scientific) %>%
      summarise(
        maxn = mean(count, na.rm = TRUE),
        se   = sd(count, na.rm = TRUE) / sqrt(dplyr::n()),
        .groups = "drop"),
    by = c("year", "scientific")
  ) %>%
  left_join(sti) %>%
  glimpse()

sp14 <- maxn.10 %>% filter(year == 2014) %>% distinct(scientific) %>% pull(scientific)
sp24 <- maxn.10 %>% filter(year == 2024) %>% distinct(scientific) %>% pull(scientific)

unique_species <- union(
  setdiff(sp14, sp24),
  setdiff(sp24, sp14))

bar_maxn <- ggplot(
  maxn.10 %>%
    mutate(
      scientific_label = if_else(scientific %in% unique_species,
                                 paste0("**", scientific, "**"),
                                 scientific),
      status = factor(status, levels = c("Fished", "No-Take"))
    ),
  aes(x = reorder_within(scientific_label, rank_maxn, year), y = maxn, fill = status)
) +
  geom_col(position = pd, colour = "black", linewidth = 0.25) +
  geom_errorbar(
    aes(ymin = pmax(maxn - se, 0), ymax = maxn + se),
    width = 0.2,
    position = pd
  ) +
  coord_flip() +
  facet_wrap(~year, scales = "free_y") +
  scale_x_reordered() +
  scale_fill_manual(values = c("Fished" = "white", "No-Take" = "black")) +
  labs(
    x = "Species",
    y = expression(Average~abundance~(MaxN~per~BRUV)),
    fill = NULL) +
  theme_bw() +
  theme_collapse +
  theme(axis.text.y = element_markdown())

bar_maxn

ggsave(paste0("plots/", park, "/fish/", name, "_top_maxn_bar_plot.png"),
       plot = bar_maxn, height = 4, width = 9, dpi = 300, units = "in", bg = "white")


# Thermal Index stacked plot
cti.10 <- maxn %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  group_by(year, status, scientific) %>%
  summarise(
    maxn = mean(count, na.rm = TRUE),
    se   = sd(count, na.rm = TRUE) / sqrt(dplyr::n()),
    .groups = "drop") %>%
  # dplyr::filter(!scientific%in%c('Carangoides sp1', 'Unknown spp'))%>%
  left_join(sti) %>%
  filter(!is.na(rls_thermal_niche)) %>%
  group_by(year, scientific) %>%
  summarise(
    rank_maxn = sum(maxn, na.rm = TRUE),
    rls_thermal_niche = first(rls_thermal_niche),
    .groups = "drop") %>%
  group_by(year) %>%
  slice_max(order_by = rank_maxn, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(
    maxn %>%
      mutate(scientific = paste(genus, species, sep = " ")) %>%
      group_by(year, status, scientific) %>%
      summarise(
        maxn = mean(count, na.rm = TRUE),
        se   = sd(count, na.rm = TRUE) / sqrt(dplyr::n()),
        .groups = "drop") %>%
      left_join(sti) %>%
      filter(!is.na(rls_thermal_niche)),
    by = c("year", "scientific", "rls_thermal_niche")
  ) %>%
  glimpse()

sp.cti.14 <- cti.10 %>% filter(year == 2014) %>% distinct(scientific) %>% pull(scientific)
sp.cti.24 <- cti.10 %>% filter(year == 2024) %>% distinct(scientific) %>% pull(scientific)

unique_species_cti <- union(
  setdiff(sp.cti.14, sp.cti.24),
  setdiff(sp.cti.24, sp.cti.14))

log1p10_trans <- trans_new(
  name = "log10p1",
  transform = function(x) log10(x + 1),
  inverse   = function(x) 10^x - 1
)

# choose the centering statistic (mean is what you asked for)
mid_niche <- median(cti.10$rls_thermal_niche, na.rm = TRUE)

# global limits across both facets/years
niche_limits <- range(cti.10$rls_thermal_niche, na.rm = TRUE)

bar_cti <- ggplot(
  cti.10 %>%
    mutate(
      scientific_label = if_else(scientific %in% unique_species_cti,
                                 paste0("**", scientific, "**"),
                                 scientific),
      niche_lab = scales::number(rls_thermal_niche, accuracy = 0.01),
      status = factor(status, levels = c("Fished", "No-Take"))
    ),
  aes(
    x = reorder_within(scientific_label, rank_maxn, year),
    y = maxn,
    fill = rls_thermal_niche,
    alpha = status
  )
) +
  geom_col(position = pd, colour = "black", linewidth = 0.25) +
  geom_errorbar(
    aes(
      ymin = pmax(maxn - se, 0),
      ymax = maxn + se
    ),
    width = 0.2,
    position = pd
  ) +
  geom_text(
    data = cti.10 %>%
      distinct(year, scientific, rank_maxn, rls_thermal_niche) %>%
      mutate(
        scientific_label = if_else(scientific %in% unique_species_cti,
                                   paste0("**", scientific, "**"),
                                   scientific),
        niche_lab = scales::number(rls_thermal_niche, accuracy = 0.01)
      ),
    aes(
      x = reorder_within(scientific_label, rank_maxn, year),
      y = 16.5,
      label = niche_lab
    ),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3
  ) +
  coord_flip(clip = "off") +
  facet_wrap(~year, scales = "free_y") +
  scale_x_reordered() +
  scale_y_continuous(
    trans = log1p10_trans,
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_alpha_manual(values = c("Fished" = 0, "No-Take" = 1)) +
  # centre GREY at the mean thermal niche
  scale_fill_gradientn(
    colours = c("#2b83ba", "grey", "#d7191c"),
    values  = scales::rescale(c(niche_limits[1],
                                mid_niche,
                                niche_limits[2])),
    limits = niche_limits,
    na.value = "grey80"
  ) +
  guides(fill = "none", alpha = "none") +
  labs(
    x = "Species",
    y = expression(Log[10]~(Average~abundance~+~1))
  ) +
  theme_bw() +
  theme_collapse +
  theme(axis.text.y = element_markdown())

bar_cti

ggsave(paste0("plots/", park, "/fish/", name, "_top_maxn_cti_bar_plot.png"),
       plot = bar_cti, height = 4, width = 9, dpi = 300, units = "in", bg = "white")

# B20
# read in b20 species summaries (already mean + sd per year x species)
b20 <- readRDS(paste0("data/", park, "/tidy/", name, "_b20-species.rds")) %>%
  glimpse()

# top 10 b20 per year (2014 & 2024)
b20.10 <- b20 %>%
  group_by(year, status, scientific_name) %>%
  summarise(
    b20 = mean(b20, na.rm = TRUE),
    se  = sd(b20, na.rm = TRUE) / sqrt(dplyr::n()),
    .groups = "drop"
  ) %>%
  group_by(year, scientific_name) %>%
  summarise(rank_b20 = sum(b20, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  slice_max(order_by = rank_b20, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(
    b20 %>%
      group_by(year, status, scientific_name) %>%
      summarise(
        b20 = mean(b20, na.rm = TRUE),
        se  = sd(b20, na.rm = TRUE) / sqrt(dplyr::n()),
        .groups = "drop"
      ),
    by = c("year", "scientific_name")
  ) %>%
  glimpse()

# species unique to either year's top 10 (for bold labels)
sp14_b20 <- b20.10 %>% filter(year == 2014) %>% distinct(scientific_name) %>% pull(scientific_name)
sp24_b20 <- b20.10 %>% filter(year == 2024) %>% distinct(scientific_name) %>% pull(scientific_name)

unique_species_b20 <- union(
  setdiff(sp14_b20, sp24_b20),
  setdiff(sp24_b20, sp14_b20)
)

# plot
bar_b20 <- ggplot(
  b20.10 %>%
    mutate(
      scientific_label = if_else(scientific_name %in% unique_species_b20,
                                 paste0("**", scientific_name, "**"),
                                 scientific_name),
      status = factor(status, levels = c("Fished", "No-Take"))
    ),
  aes(x = reorder_within(scientific_label, rank_b20, year), y = b20, fill = status)
) +
  geom_col(position = pd, colour = "black", linewidth = 0.25) +
  geom_errorbar(
    aes(ymin = pmax(b20 - se, 0), ymax = b20 + se),
    width = 0.2,
    position = pd
  ) +
  coord_flip() +
  scale_y_log10() +   # <- log transform biomass axis
  facet_wrap(~year, scales = "free_y") +
  scale_x_reordered() +
  scale_fill_manual(values = c("Fished" = "white", "No-Take" = "black")) +
  labs(
    x = "Species",
    y = expression(Average~biomass~(B20~per~BRUV)),
    fill = NULL
  ) +
  theme_bw() +
  theme_collapse +
  theme(axis.text.y = element_markdown())

bar_b20

ggsave(paste0("plots/", park, "/fish/", name, "_top_b20_bar_plot.png"),
       plot = bar_b20, height = 4, width = 9, dpi = 300, units = "in", bg = "white")
