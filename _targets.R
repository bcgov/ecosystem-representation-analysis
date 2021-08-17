# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

library(targets)
library(tarchetypes)
source("packages.R")
source("R/functions.R")
#tar_option_set(packages=c("dplyr", "tidyr", "readr", "purrr", "stringr", "ggplot2",
#                          "lubridate", "glue", "assertr", "sf", "bcmaps", "bcdata",
#                          "rmapshaper", "geojsonio", "ggiraph", "cowplot", "shiny",
#                          "knitr", "rmarkdown", "kableExtra", "tibble"),
#               imports=c("bcmaps", "bcdata"))

# load datasets ------------------------------------------------------------------------------------

load_data <- list(
  tar_target(pa_data, get_cpcad_bc_data()),
  tar_target(ecoregions, load_ecoregions()),
  tar_target(bec, load_bec())
)

# clean data --------------------------------------------------------------
clean_data <- list(
  tar_target(clean_pa, remove_overlaps(pa_data)),
  tar_target(clipped_bec, clip_to_bc_boundary(bec, simplify = TRUE)),
  tar_target(clipped_eco, clip_to_bc_boundary(ecoregions, simplify = TRUE))
)

# intersect data ----------------------------------------------------------
intersect_data <- list(
  tar_target(eco_bec, intersect_pa(ecoregions, bec)),
  tar_target(pa_eco_bec, intersect_pa(clean_pa, eco_bec))
)

# simplify spatial data  --------------------------------------------------
simplify_data <- list(
  tar_target(map_eco_background, simplify_background_map(clipped_eco, agg = c("ecoregion_code", "ecoregion_name"))),
  tar_target(map_bec_background, simplify_background_map(clipped_bec, agg = c("zone", "subzone", "variant"))),
  tar_target(map_eco_bec_background, simplify_background_map(eco_bec, agg = c("ecoregion_code", "ecoregion_name", "zone", "subzone", "variant")))
)

# analyze and prepare for visualization -----------------------------------
analyze_data <- list(
  tar_target(pa_eco_bec_summary,
             pa_eco_bec %>%
               mutate(area = as.numeric(st_area(.))) %>%
               st_drop_geometry() %>%
               group_by(ecoregion_name, ecoregion_code, zone, subzone, variant, pa_type) %>%
               summarise(area = sum(area))),
  tar_target(pa_eco_bec_summary_wide,
             pa_eco_bec_summary %>%
               pivot_wider(names_from = pa_type, values_from = area))
  # tar_target(map_eco_simp_results, join_attributes_to_map(map_eco_background, pa_eco_bec_summary_wide))
)

# supplemental bec zone plots ---------------------------------------------
plot_data <- list(
  tar_target(bec_plot_type, plot_by_bec_zone(pa_bec_sum)),
  tar_target(bec_plot_total, plot_bec_zone_totals(pa_bec_sum)),
  tar_target(bec_map_figure, bec_zone_map(map_bec)),
  tar_target(bc_button, create_bc_button())
)

# targets pipeline --------------------------------------------------------
list(
  load_data,
  clean_data,
  intersect_data,
  simplify_data,
  analyze_data
  # plot_data
  #...
  # tar_render(report, "eco_rep_report.Rmd")
)
#add list(,
#tar_targets() for each intermediate step of workflow)
