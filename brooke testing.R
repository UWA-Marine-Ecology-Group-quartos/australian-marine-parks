# Plots for dashboard

theme_shiny <- theme(axis.line = element_line(colour = "black"),
                     panel.background = element_blank(),
                     axis.title = element_text(size = 14),  # Adjust axis title size
                     axis.text = element_text(size = 12),   # Adjust axis text size
                     legend.title = element_text(size = 14),  # Adjust legend title size
                     legend.text = element_text(size = 12),   # Adjust legend text size
                     plot.title = element_text(size = 16)    # Adjust plot title size
)



# Greater than Lm carnivores
gg_lm <- ggplot(data = temporal_dat,
                aes(x = year, y = Lm, fill = zone))+
  geom_errorbar(data = temporal_dat,
                aes(ymin = Lm - Lm_se, ymax= Lm + Lm_se),
                width = 0.8, position = position_dodge(width = 0.6))+
  geom_point(size = 4, position = position_dodge(width = 0.6),
             stroke = 0.2, color = "black", alpha = 0.8, shape = 21)+
  # theme_classic() +
  scale_x_continuous(limits = c(2013, 2024),
                     breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
  geom_vline(xintercept = 2018, linetype = "dashed",color = "black",
             linewidth = 0.5,alpha = 0.5)+
  scale_fill_manual(values = c("Multiple Use Zone" = "#b9e6fb",
                               "Habitat Protection Zone" = "#fff8a3",
                               "National Park Zone" = "#7bbc63",
                               "Special Purpose Zone" = "#6daff4"),
                    name = "Australian Marine Parks") +
  labs(#title = "b)",
    x = "Year",
    y = ">Lm large bodied \ncarnivores") +
  theme_shiny
gg_lm

# plot year by community thermal index - plus a line for MPA gazetting time ---

gg_cti <- ggplot() +

  # SST needs turning back on after it is added to temporal_dat

  # geom_line(data = temporal_dat, aes(group = 1, x = year, y = sst.mean))+
  # geom_ribbon(data = temporal_dat,aes(group = 1, x = year, y = sst.mean,
  #                                      ymin = sst.mean - sd, ymax = sst.mean + sd),
  #             alpha = 0.2) +
  geom_errorbar(data = temporal_dat, aes(x = year, y = cti, ymin = cti - cti_se,
                                         ymax = cti + cti_se, fill = zone), # This has a warning but it plots wrong if you remove fill
                width = 0.8, position = position_dodge(width = 0.6))+
  geom_point(data = temporal_dat, aes(x = year, y = cti, fill = zone),size = 4,
             stroke = 0.2, color = "black", position = position_dodge(width = 0.6),
             alpha = 0.8, shape = 21)+
  # theme_classic() +
  # scale_y_continuous(limits = c(0, 8)) +
  scale_x_continuous(limits = c(2013, 2024),
                     breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black",
             size = 0.5, alpha = 0.5) +
  scale_fill_manual(values = c("Multiple Use Zone" = "#b9e6fb",
                               "Habitat Protection Zone" = "#fff8a3",
                               "National Park Zone" = "#7bbc63",
                               "Special Purpose Zone" = "#6daff4"),
                    name = "Australian Marine Parks") +
  labs(#title = "c)",
    x = "Year",
    y = "Community Temperature \nIndex") +
  theme_shiny
gg_cti

# plot year by species richness - plus a line for MPA gazetting time ---
gg_sr <- ggplot(data = temporal_dat, aes(x = year, y = richness, fill = zone)) +
  geom_errorbar(data = temporal_dat, aes(ymin = richness - richness_se,
                                         ymax = richness + richness_se),
                width = 0.8, position = position_dodge(width = 0.6)) +
  geom_point(size = 4, position = position_dodge(width = 0.6),
             stroke = 0.2, color = "black", alpha = 0.8, shape = 21) +
  # theme_classic() +
  scale_x_continuous(limits = c(2013, 2024),
                     breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black", linewidth = 0.5, alpha = 0.5) +
  scale_fill_manual(values = c("Multiple Use Zone" = "#b9e6fb",
                               "Habitat Protection Zone" = "#fff8a3",
                               "National Park Zone" = "#7bbc63",
                               "Special Purpose Zone" = "#6daff4"),
                    name = "Australian Marine Parks") +
  labs(#title = "a)",
    x = "Year",
    y = "Species richness") +
  theme_shiny
gg_sr

saveRDS(gg_cti, "inst/shiny/amp-dashboard/plots/geographe_gg_cti.RDS")
saveRDS(gg_lm, "inst/shiny/amp-dashboard/plots/geographe_gg_lm.RDS")
saveRDS(gg_sr, "inst/shiny/amp-dashboard/plots/geographe_gg_sr.RDS")

