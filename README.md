## Satellite tracking of Antarctic fur seal (Arctocephalus gazella)

This repository provides the R code that accompanies the article:

David March, Massimiliano Drago, Manel Gazo, Mariluz Parga, Diego Rita & Luis Cardona. Winter distribution of juvenile and sub‑adult male Antarctic fur seals (Arctocephalus gazella) along the western Antarctic Peninsula. Scientific Reports. https://doi.org/10.1038/s41598-021-01700-w


### Requirements
* R-studio with R >= 3.6.0


### Workflow


1. Run setup.R to create data paths and check that all required packages are installed
2. Place "tracking" (telemetry data) into "GitData/agazella/input".
3. Download GEBCO bathymetry and add into "GitData/agazella/input" under folder "bathymetry"
4. Run scripts in /cmems folder to download and post-processes environmental data. Check that path to adequate GEBCO map is defined in "04_derived_terrain.R". Main output are daily stacks of gridded environmental variables.
5. Process satellite tracking data by running "analysis/tracking/GAZ_main.R"
6. Habitat model "analysis/habitat_model/main_habitat.R"


### Structure of this repostitory

Folder          |  Description    
--------------- | -------------------
enviro          | scripts used to download and derive Environmental Data
analysis        | scripts used to analyse the data. Includes two main subfolders for the analyses of satellite tracks (`/tracking`) and the habitat model (`/habitat_model`)
plots           | scripts used to generate plots
scr             | scripts with custom functions


