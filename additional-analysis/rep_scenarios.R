
tar_load(c(pa_eco_bec_summary_wide, parks_removed, pa_bec_summary_wide))

geom_bc <- geom_sf(data = bc_bound(), fill = NA, size = 0.2)

eco_terrestrial <- st_intersection(bc_bound_hres(),ecoregions()) %>%
  st_make_valid()

geom_eco <-  geom_sf(data=eco_terrestrial, fill = NA, size = 0.5)


prov_eco_bec_summary_sf <- parks_removed %>%
  left_join(pa_eco_bec_summary_wide,
            by = c("ecoregion_name", "ecoregion_code", "zone", "subzone", "variant"))

rare_variants <- filter(pa_bec_summary_wide,
                        percent_comp_prov < quantile(percent_comp_prov, .05))

scenario_output<-prov_eco_bec_summary_sf %>%
  filter(percent_conserved_total < 17,
         (percent_comp_ecoregion > 1.25 | bec_variant %in% rare_variants$bec_variant)) %>%
  st_make_valid()

geojson_write(scenario_output, file="out/bec_scenario_17.gpkg")

bec_scenario <- ggplot() +
  geom_bc +
  geom_sf(
    data = scenario_output, aes(fill = percent_conserved_ppa), colour = NA) +
  scale_fill_viridis_c() +
  labs(title = "Underrepresented BEC variants x Ecoregions in B.C.\n PPAs & OECMs removed",
       caption = "Ecoregions*Variants with < 17% protected,\nwhere the variant makes up at least 1.25% of an ecoregion\nor is provincially rare (in the bottom 5% of variants)",
       fill = "Percent protected") +
  theme_minimal(base_size=26) +
  theme(legend.justification=c("center"),
        legend.position=c(0.8, 0.6)) +
  geom_eco
ggsave("out/maps/bec_17_print_ver.png", bec_scenario, width = 30.9, height = 40, dpi = 600, units="in")
ggsave("out/maps/bec_17_print_ver.pdf", bec_scenario, width = 34, height = 44, dpi = 600, units="in")
#geojson_write(scenario_output, file="out/bec_scenario_17.geojson")

biod<-biod <- read_csv("eco_communities_at_risk.csv") %>%
  rename_all(tolower) %>%
  mutate(eco_communities = gsub("/.*","", communities))

bec_at_risk<- unique(biod$eco_communities)

summary_info_bec_scenario <- scenario_output %>%
  mutate(bec_area=st_area(.),
         bec_area=set_units(bec_area, ha)) %>%
  st_set_geometry(NULL) %>%
  group_by(zone, subzone, variant, bec_variant) %>%
  summarise(ecoregion_variant_area = sum(bec_area),
            n_ecoregions=n()) %>%
  ungroup() %>%
  mutate(overall_area=sum(ecoregion_variant_area),
         perc_prov=((overall_area/100)/bcmaps::bc_area()),
         eco_at_risk = ifelse(bec_variant %in% bec_at_risk, TRUE, FALSE))

write.csv(summary_info_bec_scenario, "bec_scenario_summary_17-total-1.25-5.csv")

scenario_output<-prov_eco_bec_summary_sf %>%
  filter(percent_conserved_total < 17,
         (percent_comp_ecoregion > 1.25 | bec_variant %in% rare_variants$bec_variant)) %>%
  st_make_valid() %>%
  select(geometry) %>%
  summarise()

geojson_write(scenario_output, file="out/bec_scenario_17_mask.geojson")

tar_load(clean_pa)

dissolved_pa <- clean_pa %>%
  st_geometry() %>%
  summarise()

geojson_write(dissolved_pa, file="out/protected_area_mask.geojson")
#geojson_write(clean_pa, file="out/protected_area_full.geojson")


tar_load(ch_data)

dissolved_ch<- ch_data %>%
  select(common_name_english) %>%
  summarise()

geojson_write(dissolved_ch, file="out/ch_mask.geojson")
