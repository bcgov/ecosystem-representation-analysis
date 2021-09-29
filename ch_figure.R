
schedule_scale_og = c("Endangered"= "red", "Threatened"="orange")


species_data<- read_csv("out/data_summaries/prot-sp-critical-habitat.csv") %>%
  filter(g_rank == "G1" | g_rank =="G2?" | g_rank == "G1Q" | g_rank == "G1G2" | g_rank =="G2") %>%
  mutate(g_category = case_when(g_rank == "G1" ~ "G1",
                                g_rank == "G1Q" ~ "G1",
                                g_rank == "G1G2" ~ "G1G2",
                                g_rank == "G2" ~ "G2",
                                g_rank == "G2?"~ "G2")) %>%
  filter(pa_type =="ppa") %>%
  select(-X1) %>%
  distinct() %>%
  mutate(across(g_category, factor, levels=c("G1","G2","G1G2")))


bar1 <- ggplot(species_data,
               aes(x = perc_prot,
                   fct_reorder(common_name_english, perc_prot, .desc=FALSE),
                   fill = as.factor(schedule_status))) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.y = element_blank(),
        legend.position = c(0.7, 0.3),
        legend.title=element_blank()) +
  geom_bar(stat = "identity") +
  labs(x = "Critical Habitat in Protected Areas (%)",
       y = "Species Common Name") +
  scale_fill_manual(values = schedule_scale_og) +
  #scale_alpha_manual(name = "Protected Type", values = c("PPA"=1, "OECM"=0.5))+
  scale_x_continuous(expand = c(0,0)) # +
  #facet_grid(rows=vars(g_category), scales = "free_y", space = "free_y", drop=TRUE)
bar1

ggsave("out/global-responsibility-species.png", bar1, width = 7, height = 5, dpi = 300)
bar1
