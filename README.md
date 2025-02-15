# **Ai Pollution in Latam**

[![Lifecycle:experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
![contributions welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

# Urban and Socioeconomic Disparities in PM2.5 Exposure Across 340 Latin American Cities

## Study description
Outdoor air pollution, particularly fine particulate matter (PM2.5), is a leading global health risk, with Latin American cities exhibiting some of the world's highest urban PM2.5 levels. Despite this, studies on neighborhood-level PM2.5 exposure and associated disparities in the region are limited. We conducted a cross-sectional ecological analysis of 53,041 neighborhoods across 340 cities in eight Latin American countries, leveraging the SALURBAL dataset. PM2.5 concentrations were derived from satellite data and linked to socioeconomic and urban characteristics. A multilevel model analyzed associations between neighborhood PM2.5 levels and factors such as greenness, population density, educational attainment, and proximity to city centers.  The median annual neighborhood PM2.5 concentration was 18.49 µg/m³, exceeding the 2021 WHO guideline of 5 µg/m³. Over 90% of the population (235 million residents) lived in neighborhoods exceeding pre-2021 WHO PM2.5 guidelines (10 µg/m³). Variability was greatest between cities (54.27% of total variance). Higher neighborhood PM2.5 levels were positively associated with higher educational attainment (β-coefficient=0.17, 95% CI 0.08 to 0.26), intersection density (β-coefficient=0.17, 95% CI 0.08 to 0.26), and city-level age (β-coefficient=1.45, 95% CI 0.58 to 2.33) but inversely related to population density (β-coefficient=-0.55, 95% CI -0.65 to -0.46), greenness (β-coefficient=-0.76, 95% CI -0.86 to -0.67), and distance from city centers (β-coefficient=-0.86, 95% CI -0.97 to -0.76). Urban neighborhoods with less greenness, higher educational attainment, and proximity to city centers experience elevated PM2.5 exposure, as well as older cities. Policies promoting greening and reducing fossil fuel emissions, especially in city cores, are critical for mitigating PM2.5-related health risks in Latin American cities.
![](https://github.com/healthinnovation/airpollution_latam/blob/main/output/figs/fig1.png)

## Repository structure

1. [Figures](https://github.com/healthinnovation/airpollution_latam/tree/main/output/figs) - Figures in the main text
    - Figure 1. Location of study cities (n=340) and city-level annual concentration of ambient PM2.5
    - Figure 2. Annual ambient PM2.5 within 53 041 urban neighbourhoods in Latin America.
    - Figure 3. Associations between neighborhood ambient PM2.5 concentration (µg/m³) and neighborhood-level and city-level features in 53,041 urban neighborhoods in Latin America.
    - Figure S1. Spearman correlation coefficients for all variables.
    - Figure S2. Model estimates stratified by City age tertiles
5. [Analysis](https://github.com/healthinnovation/airpollution_latam/tree/main) - Quarto in R language used to analyze and visualyze data results.
    - 00_package_data.qmd: Packages and innitital data manipulation
    - 01_exploratory_analysis.qmd: Code used for data exploring, analysis and visualization
