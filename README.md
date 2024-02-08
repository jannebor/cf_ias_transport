# What is the impact of accidentally transporting terrestrial alien species? A new Life Cycle Impact Assessment model

This repository provides code related to the article published in [Environmental Science & Technology](https://doi.org/10.1021/acs.est.3c08500).

![](Data/input/Artboard.png)

## Data Collection

**Note: Some data sets need to be downloaded individually from third-party sources for reproducing this study, otherwise skip part "Data Collection".**

### Species Data

-   A list of species, their Red List category, countries of occurrence, threats and threat severity were retrieved from the International Union for the Conservation of Nature (IUCN) [Red List of Threatened Species](https://www.iucnredlist.org/) via the R package [rredlist](https://rdrr.io/cran/rredlist/man/).

-   Native and introduced distribution data was retrieved for vascular plant species from the [Alien Species First Records Database (Version 2)](https://zenodo.org/records/4632335), [Plants of the World Online](https://powo.science.kew.org/), and [Borgelt et al. (2022)](https://www.nature.com/articles/s41597-022-01233-5).

R code for reproducing these steps (except manual downloads) can be found [here](https://github.com/jannebor/cf_ias_transport/blob/main/R/data_prep/4_species_data.R).

### Transportation data

-   Data on international trade between 1995-2019 was downloaded from the [BACI database](http://www.cepii.fr/DATA_DOWNLOAD/baci/doc/LegacyVersions.html) (legacy version February 2021, HS92). This data was converted and aggregated to obtain transported quantities of commodities between pairs of (importing & exporting) countries.

-   Monetary trade flows for the eriod 1870-2014 were retrieved from the [Correlates of War](https://correlatesofwar.org/data-sets/bilateral-trade/) project.

-   Distances between pairs of countries were calculated using [geospatial data](https://www.naturalearthdata.com/) and the R package [sf](https://r-spatial.github.io/sf/).

R code for reproducing these steps (except manual downloads) can be found [here](https://github.com/jannebor/cf_ias_transport/tree/main/R/data_prep).

## Characterization Factors

The characterization factor consists of Fate and Effect Factor and describes the exporting-country-specific impact on terrestrial biodiversity in the importing country as potentially disappeared fraction (PDF) of native species per kg of transported good between the bilateral trading partners.

**Characterization factor = EF × FF**

Code for reproducing Characterization Factor calculations can be found [here](https://github.com/jannebor/cf_ias_transport/tree/main/R/characterization_factor).

### Effect Factor

The effect factor describes the impact of introduced alien species on native terrestrial species as potentially disappeared fraction (PDF) of species due to a change in the alien species fraction (ASF). This can be calculated using the equation:

**Effect factor = 𝝙PDF / 𝝙ASF**

We used country-specific data on alien species presence and affected species to establish a stressor-response relationship.

Code for reproducing Effect Factor calculations can be found [here](https://github.com/jannebor/cf_ias_transport/tree/main/R/effect_factor).

### Fate Factor

The fate factor describes the change in alien species fraction in the importing country due to the transportation of commodities from exporting to importing country. All collected data on native & alien species presences and transported commodities was combined. Henceforth, we used generalized linear mixed effects modeling to analyze the impact of international trade on alien species relocations.

**Fate factor = 𝝙ASF / 𝝙kg**

Code for reproducing Fate Factor calculations can be found [here](https://github.com/jannebor/cf_ias_transport/tree/main/R/fate_factor).
