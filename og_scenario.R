tar_load(c(pa_eco_bec_summary_wide, parks_removed, pa_bec_summary_wide))

og_tier_1 <- read_csv("c:/tmp/og-env-analysis/out/og_bec_summary_tier_1.csv")%>%
  mutate(variant = as.character(variant))

og_tier_2 <- read_csv("c:/tmp/og-env-analysis/out/og_bec_summary_tier_2.csv")%>%
  mutate(variant = as.character(variant))

geom_bc <- geom_sf(data = bc_bound(), fill = NA, size = 0.2)

eco_terrestrial <- st_intersection(bc_bound_hres(),ecoregions()) %>%
  st_make_valid()

geom_eco <-  geom_sf(data=eco_terrestrial, fill = NA, size = 0.5)

prov_eco_bec_summary_sf <- parks_removed %>%
  left_join(og_tier_1,
            by = c("ecoregion_name", "ecoregion_code", "zone", "subzone", "variant"))

rare_variants <- filter(pa_bec_summary_wide,
                        percent_comp_prov < quantile(percent_comp_prov, .05))

scenario_output<-prov_eco_bec_summary_sf %>%
  filter(tier_1_prot_perc < 17,
         (percent_comp_ecoregion > 1.25 | bec_variant %in% rare_variants$bec_variant)) %>%
  st_make_valid()

#geojson_write(scenario_output, file="out/bec_tier1_scenario.geojson")

bec_scenario <- ggplot() +
  geom_bc +
  geom_sf(data = scenario_output, aes(fill = tier_1_prot_perc), colour = NA) +
  scale_fill_viridis_c() +
  labs(#title = "Underrepresented BEC variants x Ecoregions\n in B.C. Parks and Protected Areas\n PPAs & OECMs removed",
    caption = "5 million ha scenario\nEcoregions*Variants with < 17% protected,\nwhere the variant makes up at least 1.25% of an ecoregion\nor is provincially rare (in the bottom 5% of variants)",
    fill = "Percent protected") +
  geom_eco+
  theme_minimal() +
  theme(legend.justification=c("center"),
        legend.position=c(0.8, 0.6))
bec_scenario
ggsave("out/maps/bec_tier1_scenario.png", bec_scenario, width = 9, height = 9, dpi = 300)
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

write.csv(summary_info_bec_scenario, "out/bec_tier1_scenario.csv")
