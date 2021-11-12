library(targets)
#library(tarchetypes)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "bookdown", "omxr", "nhts2017"))

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
source("R/functions.R")


# Targets necessary to build your data and run your model
data_targets <- list(
  #read in Skims
  tar_target(skims_file, "data/skims.omx", format = "file"),
  tar_target(skims, readskims(skims_file)),
  
  # household DATA
  tar_target(hh_file, "data/households.csv", format = "file"), 
  tar_target(hh, readCSV(hh_file)),
  tar_target(hh_clean, hh_data(hh)),

  # trip rates
  tar_target(nhts_hh, nhts_households),
  tar_target(nhts_trip, nhts_trips),
  tar_target(nhts_tripprod, nhts_2017(nhts_hh, nhts_trip)),
  tar_target(trips, tripprod(hh_clean, nhts_tripprod)),
  
  # read in mc / dc utilities
  tar_target(mc_coeff_file, "data/MC_coeff.csv", format = "file"),
  tar_target(mc_const_file, "data/MC_constants.csv", format = "file"),
  tar_target(dc_param_file, "data/DC_parameters.csv", format = "file"),
  tar_target(mc_coeff, readCSV(mc_coeff_file)),
  tar_target(mc_const, readCSV(mc_const_file)),
  tar_target(dc_param, readCSV(dc_param_file)),
  
  
  # run mode choice logsum calculator
  tar_target(HBW, mode <- "HBW"),
  tar_target(HBW_mc_logsum_skim, mc_logsum(HBW, skims, mc_coeff, mc_const)),
  tar_target(HBO, mode <- "HBO"),
  tar_target(HBO_mc_logsum_skim, mc_logsum(HBO, skims, mc_coeff, mc_const)),
  tar_target(NHB, mode <- "NHB"),
  tar_target(NHB_mc_logsum_skim, mc_logsum(NHB, skims, mc_coeff, mc_const)),
  
  # run destination choice calculator / compute destination choice
  
  # compute mode choice probability
  tar_target(HBW_probability, mc_probability(HBW_mc_logsum_skim)),
  tar_target(HBO_probability, mc_probability(HBO_mc_logsum_skim)),
  tar_target(NHB_probability, mc_probability(NHB_mc_logsum_skim))
)



# Targets necessary to build the book / article
book_targets <- list(
)



# run all targets
list(
  data_targets, 
  book_targets
)
