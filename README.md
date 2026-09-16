# TPS
This repository contains .R code to replicate geospatial analysis of non-residential motor vehicle thefts (MVTs) from Toronto Police Service (TPS). TPS makes open data available online: [https://www.tps.ca/data-maps/open-data/](URL)

The syntax downloads and prepares data to construct an artificial neural network (ANN) to try to predict variations in the number of MVTs per 10,000 population across Toronto's 585 census tracts. The training model includes spatially weighted measures for MVTs, tract-level and spatially weighted measures for fatal and non-fatal shootings, tract-level population density, and tract-level measures that control for neighbourhood ecology as predictors.

Here is a summary of what each syntax file does:

1) downloads TPS incident-level crime data from Toronto Open Data
2) joins together shootings and homicide incident data and plots spatial variations in fatal and non-fatal shootings
3) cleans MVTs incident-level data to prepare them for analysis
4) downloads census tract data for the City of Toronto, and constructs measures that control for variation in MVTs at the neighbourhood level of analysis
5) plots annual variations in MVTs and spatial autocorrelation for MVTs and fatal and non-fatal shootings
6) constructs and evaluates an ANN that predicts variation in MVTs across census tracts

I've provided all the syntax files to replicate the analysis in the 'syntax' folder and the visualizations of the data in the 'viz' folder.

To replicate the analysis, run the .R syntax files in order of the naming convention of the files i.e., run the syntax file named '01.R' first, the file named '02.R' second, '03.R' third, etc.
