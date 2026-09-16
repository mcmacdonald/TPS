# TPS
This repository contains .R code to replicate geospatial analysis of non-residential motor vehicle thefts (MVTs) from Toronto Police Service (TPS). TPS makes open data available online: [https://www.tps.ca/data-maps/open-data/](URL)

I construct an artificial neural network (ANN) to predict variations in number of MVTs per 10,000 population across Toronto's 585 census tracts that includes: spatially weighted measures for MVTs, tract-level and spatially weighted measures for fatal and non-fatal shootings, population density, and measures that control for neighbourhood ecology.

Here is a summary of what each syntax file does:
1) download TPS incident-level crime data from Toronto Open Data
2) join together shootings and homicide incident data and plot spatial variations in fatal and non-fatal shootings
3) clean MVTs incident level data to ready it for analysis
4) download census tract data for Toronto, and create measures that control for variation in MVTs at the neighbourhood level of analysis
5) plot annual variations in MVTs and spatial autocorrelation in MVTs and shootings
6) construct and evaluate neural network that predicts variation in MVTs across census tracts

I've provided all the syntax files to replicate the analysis in the 'syntax' folder and the visualizations of the data in the 'viz' folder.

To replicate the analysis, run the .R syntax files in order of the naming convention of the files i.e., run the syntax file named '01.R' first, the file named '02.R' second, '03.R' third, etc.
