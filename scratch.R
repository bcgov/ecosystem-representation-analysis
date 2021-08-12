library(ggplot2)
library(forcats)
library(rmapshaper)
library(tidyr)

# Runs after 'intersec_data' targets list

left <- eco_bec[eco_bec$ecoregion_code %in% unique(pa_bec_eco$ecoregion_code), ] %>%
  mutate(eco_var_area = st_area(.)) %>%
  group_by(ecoregion_name, zone, subzone, variant) %>%
  summarise(tot_area = as.numeric(sum(eco_var_area)), is_coverage = TRUE)

right <- pa_bec_eco %>%
  mutate(pa_area = st_area(.)) %>%
  st_drop_geometry() %>%
  group_by(ecoregion_name, zone, subzone, variant, pa_type) %>%
  summarise(pa_area = as.numeric(sum(pa_area)))

tab_output <- st_drop_geometry(left) %>%
  left_join(right) %>%
  mutate(percent_prot = as.numeric(pa_area / tot_area * 100),
         percent_prot = ifelse(is.na(percent_prot), 0, percent_prot),
         bec_variant = gsub("NA$", "", paste0(zone, subzone, variant)))

eco_rep_barplot <- ggplot(st_drop_geometry(output)) +
  geom_col(aes(x = fct_reorder(bec_variant, percent_prot, .fun = sum), y = percent_prot, fill = pa_type)) +
  coord_flip() +
  scale_fill_discrete(na.translate = FALSE) +
  labs(fill = "Conserved Area Type", y = "Percent Conserved", x = "BEC Variant",
       title = paste0("Percent of BEC Variants conserved in\n", output$ecoregion_name[1], " ecoregion"))

sf_output <- ms_simplify(left, keep_shapes = TRUE) %>%
  left_join(
      pivot_wider(right, names_from = pa_type, values_from = pa_area)
    ) %>%
  mutate(percent_prot_ppa = replace_na(as.numeric(ppa / tot_area * 100), 0),
         percent_prot_oecm = replace_na(as.numeric(oecm / tot_area * 100), 0),
         percent_prot_total = percent_prot_ppa + percent_prot_oecm,
         percent_prot_total = ifelse(is.na(percent_prot_total), 0, percent_prot_total),
         bec_variant = gsub("NA$", "", paste0(zone, subzone, variant)))

eco_rep_map <- ggplot(sf_output) +
  geom_sf(aes(fill = percent_prot_total), colour = NA) +
  scale_fill_viridis_c()

write_csv(tab_output, "out/ecoregion_variant_rep.csv")

ggsave("out/ecoregion_variant_rep_bar.png", plot = eco_rep_barplot)
ggsave("out/ecoregion_variant_rep_map.png", plot = eco_rep_map)

