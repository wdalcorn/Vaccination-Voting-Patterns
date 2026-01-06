# COVID-19 Vaccination Rates and Political Partisanship

An analysis of county-level vaccination rates and their correlation with 2020 and 2024 presidential election results.

## Overview

Using CDC and MIT Election Lab data (3,021 counties), this project examines the stark COVID-19 vaccination gap that emerged between Democratic and Republican counties in early 2021 and persisted through 2023. The analysis includes statistical significance testing, time-series tracking, and geographic visualization.

## Key Findings

- By November 2021, Democratic counties averaged ~62% fully vaccinated while Republican counties reached only ~47%—a 15 percentage point gap
- Strong negative correlation (r = -0.58) between Republican vote share and vaccination rates in partisan strongholds
- The gap remained durable for 18+ months despite extended vaccine availability
- Correlation was 4x stronger in landslide counties (r = -0.58) vs competitive counties (r = -0.15)
- T-test confirmed statistically significant difference (p < 0.001)

## Data Sources

- **Vaccination data:** [CDC COVID-19 county-level vaccination rates](https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh) (January 2021 - May 2023) - download from CDC directly (file too large for GitHub)
- **Election data:** [MIT Election Lab county presidential returns](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ) (2020, 2024)

## Methods

- Linear regression modeling
- Welch's t-test for group comparison
- Correlation analysis by county competitiveness
- Time-series aggregation (monthly snapshots)
- Geographic joins using FIPS codes

## Tools

- R
- tidyverse (dplyr, ggplot2, stringr, lubridate)
- maps package for county-level choropleth visualization

## Visualizations

- County-level choropleth maps (2020 results, 2024 results, vaccination rates)
- Scatterplots with regression lines (overall and by county competitiveness)
- Time-series line chart (vaccination gap over 18 months)
- Distribution histograms by partisanship
- Box plots comparing Democratic vs Republican counties

## Files

```
/data
    countypres_2000-2024.csv    - MIT Election Lab county presidential returns
    (COVID data too large for GitHub - download from CDC link above)

/scripts
    PS8.R                       - Core analysis and visualizations
    extended_analysis.R         - Extended analysis with timeline and 2024 data
    statistical_tests.R         - Statistical testing (t-test, regression)
    final_analysis.R            - Final cleaned analysis script

/output
    poster.png                  - Academic poster summarizing findings
```

## Author

William Alcorn | Syracuse University | IST 421
