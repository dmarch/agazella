#--------------------------------------------------------------------------
# bootstrap_me        Bootstrap methods for MaxEnt
#--------------------------------------------------------------------------
# Similar to fit_sdm (replace by fit_me)
# Read observations
# Generate 50 different iterations (Hindel et al 2020)
# Sample half the data, with replacement, to fit each model.
# Only store the model (me_01, me_02, etc.)
# For each day: predict with each of the 50 models
# Similar approach for accessibility maps
# For each iteration, calculate habitat importance. In our case, we need to wait to have all predictions...
# We will need to store predictions to the calculate habitat importance. Save in specific folder.
# Uncertainty is the calculation of SD among the 50 predictions.
# Propagate by adding in quadrature http://ipl.physics.harvard.edu/wp-uploads/2013/03/PS3_Error_Propagation_sp13.pdf
# Overall uncertainty. Check temporal variability
