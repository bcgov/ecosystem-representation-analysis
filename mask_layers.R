
future_areas <- st_read("data/Master_RALCP.gdb", layer='RALCP_Projects', crs=3005)

future_areas <- future_areas %>%
    rename_all(tolower) %>%
    st_cast(to = "MULTIPOLYGON", warn = FALSE) %>%
    st_make_valid() %>%
    st_cast(to = "POLYGON", warn = FALSE)

geom_bc <- geom_sf(data = bc_bound(), fill = NA, size = 0.5)


ch_map<- ch_data %>%
  select(common_name_english) %>%
  summarise()


future_areas_map <- ggplot() +
  geom_sf(
    data = ch_data, fill = "skyblue") +
  geom_bc +
  #scale_fill_viridis_c() +
  #labs(#title = "Underrepresented BEC variants x Ecoregions\n in B.C. Parks and Protected Areas\n PPAs & OECMs removed",
    #caption = "Ecoregions*Variants with < 17% protected,\nwhere the variant makes up at least 1.25% of an ecoregion\nor is provincially rare (in the bottom 5% of variants)",
    #fill = "Percent protected") +
  theme_minimal()
  #theme(legend.justification=c("center"),
        #legend.position=c(0.8, 0.6)) +
  #geom_eco

future_areas_map
ggsave("out/maps/future_areas_map.png", future_areas_map, width = 9, height = 9, dpi = 300)
#geojson_write(scen
