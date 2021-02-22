# agazella
Satellite tracking of Antarctic fur seal (Arctocephalus gazella)


# Workflow


1. Run setup.R to create data paths and check that all required packages are installed
2. Place "tracking" (telemetry data) into "GitData/agazella/input".
3. Download GEBCO bathymetry and add into "GitData/agazella/input" under folder "bathymetry"
4. Run scripts in /cmems folder to download and post-processes environmental data. Check that path to adequate GEBCO map is defined in "04_derived_terrain.R". Main output are daily stacks of gridded environmental variables.
5. Process satellite tracking data by running "analysis/tracking/GAZ_main.R"
6. Habitat model "analysis/habitat_model/main_habitat.R"




## Simulations

Hindell et al 2020: Locations at the animal’s home colony, and locations at known terrestrial resting sites, were fixed at the corresponding time and date in the simulated tracks, accurately simulating central place foraging behaviour.

We chose the number of simulations by plotting the relationship between the number of simulated tracks and the environmental space (rather than the geographic space) encompassed by the set of simulated tracks Tracking of marine predators to protect Southern Ocean ecosystems 4 in each case. This environmental space was characterised as the mean, standard deviation and variance of each of the 19 environmental covariates (described below) (e.g., Supplementary Figure S1). In a few cases the environmental space had not stabilized by 50 simulations but using more than 50 simulations in
the analyses that followed was often computationally infeasible