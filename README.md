<!-- README.md is generated from README.Rmd. Please edit that file -->

[![img](https://img.shields.io/badge/Lifecycle-Stable-97ca00)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

# Analysis of ecosystem representation in BC

The primary outputs of this analysis are:
1. A spatial file detailing the overall ecosystem representation using BEC variants by Ecoregion and by the Province
2. An example spatial file of underrepresented ecosystems using 17% protected as a threshold (by Ecoregion and Province)

## Key information for users

- Representation of BEC variants in the Protected Area system in B.C. was calculated by:
    - Parks and Protected Areas (PPAs) - column name: `percent_conserved_total`
    - Other Effective Area-Based Conservation Measures (OECMs, defined according to [Canadian Protected and Conserved Areas Database
    (CPCAD)](https://www.canada.ca/en/environment-climate-change/services/national-wildlife-areas/protected-conserved-areas-database.html)) - column  name: `percent_conserved_oecm`
    - Total Conserved = PPA + OECM - column name: `perecent_conserved_total`

- The linework between the BC boundary, ecoregions, and BEC variants do not exactly match creating sliver polygons. To reduce the number of slivers and the impact on the overall analysis, the distribution of a variant across two different ecoregions was calculated (`percent_var_in_eco`). 
    - For the underrepresented spatial output file, if a variant was <5% in an ecoregion, it was considered a sliver and removed from the representation analysis 

### Data

The analysis uses data from several sources. The data does not need to
be pre-downloaded to run the code.

-   [Canadian Protected and Conserved Areas Database
    (CPCAD)](https://www.canada.ca/en/environment-climate-change/services/national-wildlife-areas/protected-conserved-areas-database.html)
-   [Ecoregions - Ecoregion Ecosystem Classification of British
    Columbia](https://catalogue.data.gov.bc.ca/dataset/d00389e0-66da-4895-bd56-39a0dd64aa78)
    (Licence: [Open Government Licence - British
    Columbia](http://www2.gov.bc.ca/gov/content?id=A519A56BC2BF44E4A008B33FCF527F61))
-   [Biogeoclimatic Ecosystem Classification (BEC)
    Map](https://catalogue.data.gov.bc.ca/dataset/f358a53b-ffde-4830-a325-a5a03ff672c3)
    (Licence: [Open Government Licence - British
    Columbia](http://www2.gov.bc.ca/gov/content?id=A519A56BC2BF44E4A008B33FCF527F61))

### Targets Workflow

This project leverages the `targets` package, a pipeline toolkit for
data science projects in R. You can install `targets` from CRAN:

``` r
#install.packages("targets")
```

#Usage Run `targets::tar_make()` to run project. This will run all of
the analysis - no individual scripts are required.

### Required R packages

The packages used in this analysis are catalogued in `packages.R`. The
packages will be loaded automatically with `tar_make()` but some may
need to be installed prior to initiating the workflow.

## Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an
[issue](https://github.com/bcgov/protected-lands-and-waters-indicator/issues).

## How to Contribute

If you would like to contribute, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

## License

    Copyright 2016 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at 

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

This repository is maintained by [Environmental Reporting
BC](http://www2.gov.bc.ca/gov/content?id=FF80E0B985F245CEA62808414D78C41B).
Click [here](https://github.com/bcgov/EnvReportBC) for a complete list
of our repositories on GitHub.
