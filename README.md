## Satellite tracking of Antarctic fur seal (*Arctocephalus gazella*)

This repository provides the R code that accompanies the article:

David March, Massimiliano Drago, Manel Gazo, Mariluz Parga, Diego Rita & Luis Cardona. Winter distribution of juvenile and sub‑adult male Antarctic fur seals (*Arctocephalus gazella*) along the western Antarctic Peninsula. Scientific Reports. https://doi.org/10.1038/s41598-021-01700-w


### Requirements
* R-studio with R >= 3.6.0


### Structure of this repostitory

Folder          |  Description    
--------------- | -------------------
enviro          | scripts used to download and derive Environmental Data
analysis        | scripts used to analyse the data. Includes two main subfolders for the analyses of satellite tracks (`/tracking`) and the habitat model (`/habitat_model`)
plots           | scripts used to generate plots
scr             | scripts with custom functions


### Genearal

1. Edit `setup.R` to create data paths and check that all required packages are installed
2. Place "tracking" (telemetry data) into "GitData/agazella/input".
3. Download GEBCO bathymetry and add into "GitData/agazella/input" under folder "bathymetry"
4. Run scripts in `/cmems` folder to download and post-processes environmental data. Check that path to adequate GEBCO map is defined in "04_derived_terrain.R". Main output are daily stacks of gridded environmental variables.
5. Process satellite tracking data by running `analysis/tracking/GAZ_main.R`
6. Habitat model `analysis/habitat_model/main_habitat.R`


### Data availability
Correspondence and requests for tracking data should be addressed to Lluis Cardona (luis.cardona at ub.edu)


### License
Copyright (c) 2020 David March  
Licensed under the [MIT license](https://github.com/dmarch/agazella/blob/master/LICENSE).


### Acknowledgements
This project was financed by grant CTM2017-83319-P from Ministerio de Ciencia, Innovación y Universidades (Spain) and supported by AEI/FEDER/UE. D. March acknowledges support from the European Union's Horizon 2020 research and innovation programme under the Marie Skłodowska-Curie grant agreement (no 794938), and the University of Exeter's Advanced Research Computing facilities at Penryn in carrying out this work. M. Drago acknowledges support from the Secretaria d'Universitats i Recerca, Generalitat de Catalunya (Spain) under the Beatriu de Pinós programme postdoctoral fellowship (2016 BP 00151). The authors acknowledge the members of the Spanish Army at the Gabriel de Castilla research station for assistance and logistic support during fieldwork in Deception Island. We thank two anonymous reviewers whose comments and suggestions contributed to improve and clarify this manuscript.