library(targets)
#library(tarchetypes)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "bookdown", "omxr"))

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
source("R/functions.R")


# Targets necessary to build your data and run your model
data_targets <- list(
  #read in Skims
  tar_target(skims_file, "data/skims.omx", format = "file"),
  tar_target(skims, readskims(skims_file)),
  
  # read in SE DATA
  tar_target(se_file, "data/households.csv", format = "file"), #need right file input
  tar_target(se_data, readCSV(se_file)),
  
  # read in trip rates
  tar_target(triprates_file, "data/households.csv", format = "file"), #need right file input
  tar_target(trip_rates, readCSV(triprates_file)),
  
  # read in mc / dc utilities
  tar_target(mc_coeff_file, "data/MC_coeff.csv", format = "file"),
  tar_target(mc_const_file, "data/MC_constants.csv", format = "file"),
  tar_target(dc_param_file, "data/DC_parameters.csv", format = "file"),
  tar_target(mc_coeff, readCSV(mc_coeff_file)),
  tar_target(mc_const, readCSV(mc_const_file)),
  tar_target(dc_param, readCSV(dc_param_file))
  
  # run trip gen
  
  # run mode choice logsum calculator
  
  # run destination choice calculator / compute destination choice
  
  # compute mode choice probability
)



# Targets necessary to build the book / article
book_targets <- list(
)



# run all targets
list(
  data_targets, 
  book_targets
)
