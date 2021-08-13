library(ggplot2)
library(rmapshaper)
library(tidyr)
library(dplyr)
library(tidytext) # for reorder_within() and scale_x_reordered()
library(readr)
library(sf)
library(plotly)
library(bcmaps)
library(mapview)

# Runs after 'intersect_data' targets list

# Choose a couple of ecoregions to test with
ecoreg_sel <- c("NCM", "TOP")

left <- eco_bec[eco_bec$ecoregion_code %in% ecoreg_sel, ] %>%
  mutate(eco_var_area = st_area(.)) %>%
  group_by(ecoregion_code, ecoregion_name, zone, subzone, variant) %>%
  summarise(tot_area = as.numeric(sum(eco_var_area)), is_coverage = TRUE)

right <- pa_bec_eco[pa_bec_eco$ecoregion_code %in% ecoreg_sel, ] %>%
  mutate(pa_area = st_area(.)) %>%
  st_drop_geometry() %>%
  group_by(ecoregion_code, ecoregion_name, zone, subzone, variant, pa_type) %>%
  summarise(pa_area = as.numeric(sum(pa_area)))

tab_output <- st_drop_geometry(left) %>%
  left_join(right) %>%
  mutate(percent_prot = as.numeric(pa_area / tot_area * 100),
         percent_prot = ifelse(is.na(percent_prot), 0, percent_prot),
         bec_variant = gsub("NA$", "", paste0(zone, subzone, variant)))

(eco_rep_barplot <- ggplot(tab_output) +
  geom_col(aes(x = reorder_within(bec_variant, percent_prot, ecoregion_name, fun = sum), y = percent_prot, fill = pa_type)) +
  facet_grid(vars(ecoregion_name), scales = "free_y") +
  coord_flip() +
  scale_fill_discrete(na.translate = FALSE) +
  scale_x_reordered() +
  labs(fill = "Conserved Area Type", y = "Percent Conserved", x = "BEC Variant",
       title = paste0("Percent of BEC Variant x Ecoregion conserved")) +
  theme(axis.text.y = element_text(size = 8)))

comp_by_rep <- filter(tab_output, pa_type == "ppa") %>%
  group_by(ecoregion_name) %>%
  mutate(comp = tot_area / sum(tot_area) * 100)

bec_cols <- bec_colours()[unique(comp_by_rep$zone)]

(comp_by_rep_plot <- ggplot(comp_by_rep, aes(x = comp, y = percent_prot, colour = zone)) +
  geom_point(aes(text = bec_variant)) +
    scale_colour_manual(values = bec_cols, breaks = names(bec_cols)) +
  facet_wrap(vars(ecoregion_name)) +
  labs(title = "Percent of BEC variant conserved in PPAs vs composition in Ecoregions",
       x = "Percent variant composotion of ecoregion",
       y = "Percent of variant conserved in ecoregion") +
  theme_bw())

comp_by_rep_plotly <- ggplotly(comp_by_rep_plot, tooltip = "text")

sf_output <- ms_simplify(left, keep_shapes = TRUE) %>%
  left_join(
      pivot_wider(right, names_from = pa_type, values_from = pa_area)
    ) %>%
  mutate(percent_prot_ppa = replace_na(as.numeric(ppa / tot_area * 100), 0),
         percent_prot_oecm = replace_na(as.numeric(oecm / tot_area * 100), 0),
         percent_prot_total = percent_prot_ppa + percent_prot_oecm,
         percent_prot_total = ifelse(is.na(percent_prot_total), 0, percent_prot_total),
         bec_variant = gsub("NA$", "", paste0(zone, subzone, variant)))

eco_rep_map <- ggplot() +
  geom_sf(data = sf_output, mapping = aes(fill = percent_prot_total), colour = NA) +
  scale_fill_viridis_c() +
  labs(title = "Percent of BEC Variants conserved in Norther Columbia Mountains and\nThompson Okanagan Plateau ecoregions",
       fill = "percent of BEC variant\nconserved in ecoregion")

eco_var_rep_map_with_pa <- eco_rep_map +
  geom_sf(data = clean_pa, mapping = aes(colour = pa_type), alpha = 0.4, size = 0.5) +
  coord_sf(xlim = st_bbox(sf_output)[c(1,3)], ylim = st_bbox(sf_output)[c(2,4)]) +
  theme_minimal()

write_csv(tab_output, "out/ecoregion_variant_rep.csv")

ggsave("out/ecoregion_variant_rep_bar.png", plot = eco_rep_barplot, height = 10, width = 8)
ggsave("out/ecoregion_variant_rep_map.png", plot = eco_rep_map, height = 10, width = 10)
ggsave("out/eco_var_rep_map_with_pa.png", plot = eco_var_rep_map_with_pa, height = 10, width = 10, bg = "white")
ggsave("out/ecoregion_variant_rep_by_comp_scatter.png", plot = comp_by_rep_plot, height = 8, width = 10)
htmlwidgets::saveWidget(comp_by_rep_plotly, file = "out/comp_by_rep_plotly.html")

