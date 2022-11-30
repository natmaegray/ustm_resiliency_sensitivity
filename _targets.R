library(targets)
#library(tarchetypes)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "bookdown", "omxr", "nhts2017", "rgdal", "sf", "ggthemes", "lhs", "foreign"))

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
source("R/functions.R")


# Targets necessary to build your data and run your model
data_targets <- list(
  #read in Skims
  tar_target(skims_file, "data/skims.omx", format = "file"),
  tar_target(skims, readskims(skims_file)),
  
  #read in Land Use
  tar_target(landuse_file, "data/land_use.csv", format = "file"),
  tar_target(land_use, readCSV(landuse_file)),
  
  # household DATA
  tar_target(hh_file, "data/households.csv", format = "file"), 
  tar_target(hh, readCSV(hh_file)),
  tar_target(hh_clean, hh_data(hh)),

  # trip rates
  tar_target(nhts_hh, nhts_households),
  tar_target(nhts_trip, nhts_trips),
  tar_target(nhts_tripprod, nhts_2017(nhts_hh, nhts_trip)),
  tar_target(trips, tripprod(hh_clean, nhts_tripprod)),
  
  # purposes
  tar_target(HBW, purpose <- "HBW"),
  tar_target(HBO, purpose <- "HBO"),
  tar_target(NHB, purpose <- "NHB"),
  
  # read in mc / dc utilities
  tar_target(mc_coeff_file, "data/MC_coeff.csv", format = "file"),
  tar_target(mc_const_file, "data/MC_constants.csv", format = "file"),
  tar_target(dc_coeff_file, "data/DESTCHOICE_PARAMETERS.DBF", format = "file"),
  tar_target(mc_coeff, readCSV(mc_coeff_file)),
  tar_target(mc_const, readCSV(mc_const_file)),
  tar_target(dc_coeff, as_tibble(foreign::read.dbf(dc_coeff_file, as.is = TRUE))),
  
  #generate MC coeffs
  tar_target(hbw_mc_coeff_lists_100, generate_mc_coeff(HBW, mc_coeff, mc_const, 100)),
  tar_target(hbo_mc_coeff_lists_100, generate_mc_coeff(HBO, mc_coeff, mc_const, 100)), 
  tar_target(nhb_mc_coeff_lists_100, generate_mc_coeff(NHB, mc_coeff, mc_const, 100)),
#  tar_target(hbw_mc_coeff_lists_600, generate_mc_coeff(HBW, mc_coeff, mc_const, 600)),
#  tar_target(hbo_mc_coeff_lists_600, generate_mc_coeff(HBO, mc_coeff, mc_const, 600)), 
#  tar_target(nhb_mc_coeff_lists_600, generate_mc_coeff(NHB, mc_coeff, mc_const, 600)),
  
  # DC coeff
  tar_target(hbw_dc_coeff_lists_100, generate_dc_coeff(HBW, dc_coeff, 100)),
  tar_target(hbo_dc_coeff_lists_100, generate_dc_coeff(HBO, dc_coeff, 100)), 
  tar_target(nhb_dc_coeff_lists_100, generate_dc_coeff(NHB, dc_coeff, 100)),
  
  #write out CSVs
#  tar_target(write_mc_lists, mcparam_csv(hbw_mc_coeff_lists_100, hbo_mc_coeff_lists_100, nhb_mc_coeff_lists_100, mc_coeff)),
#  tar_target(write_dc_lists, dcparam_csv(hbw_dc_coeff_lists_100, hbo_dc_coeff_lists_100, nhb_dc_coeff_lists_100, dc_coeff)),
  
  # run loop for logsums
  tar_target(hbw_full_loop_100, full_loop(skims, hbw_mc_coeff_lists_100, hbw_dc_coeff_lists_100, land_use, 100)),
  tar_target(hbo_full_loop_100, full_loop(skims, hbo_mc_coeff_lists_100, hbo_dc_coeff_lists_100, land_use, 100)),
  tar_target(nhb_full_loop_100, full_loop(skims, nhb_mc_coeff_lists_100, nhb_dc_coeff_lists_100, land_use, 100)),

  # pull tibbles
  tar_target(hbw_mclogsum_tibble,      pull_tibbles(hbw_full_loop_100, "ModeChoice_Logsum")),
  tar_target(hbw_dcutility_tibble,     pull_tibbles(hbw_full_loop_100, "Destination_Utility")),
  tar_target(hbw_dclogsum_tibble,      pull_tibbles(hbw_full_loop_100, "Destination_Logsum")),
  tar_target(hbw_mcprobability_tibble, pull_tibbles(hbw_full_loop_100, "ModeChoice_Probability")),
  tar_target(hbw_dcprobability_tibble, pull_tibbles(hbw_full_loop_100, "Destination_Probability")),
  
  tar_target(hbo_mclogsum_tibble,      pull_tibbles(hbo_full_loop_100, "ModeChoice_Logsum")),
  tar_target(hbo_dcutility_tibble,     pull_tibbles(hbo_full_loop_100, "Destination_Utility")),
  tar_target(hbo_dclogsum_tibble,      pull_tibbles(hbo_full_loop_100, "Destination_Logsum")),
  tar_target(hbo_mcprobability_tibble, pull_tibbles(hbo_full_loop_100, "ModeChoice_Probability")),
  tar_target(hbo_dcprobability_tibble, pull_tibbles(hbo_full_loop_100, "Destination_Probability")),

  tar_target(nhb_mclogsum_tibble,      pull_tibbles(nhb_full_loop_100, "ModeChoice_Logsum")),
  tar_target(nhb_dcutility_tibble,     pull_tibbles(nhb_full_loop_100, "Destination_Utility")),
  tar_target(nhb_dclogsum_tibble,      pull_tibbles(nhb_full_loop_100, "Destination_Logsum")),
  tar_target(nhb_mcprobability_tibble, pull_tibbles(nhb_full_loop_100, "ModeChoice_Probability")),
  tar_target(nhb_dcprobability_tibble, pull_tibbles(nhb_full_loop_100, "Destination_Probability")),
  
  # plots by purpose
  tar_target(hbw_plots, tibbleplots(hbw_mclogsum_tibble, hbw_dcutility_tibble, hbw_dclogsum_tibble, hbw_mcprobability_tibble, hbw_dcprobability_tibble)),
  tar_target(hbo_plots, tibbleplots(hbo_mclogsum_tibble, hbo_dcutility_tibble, hbo_dclogsum_tibble, hbo_mcprobability_tibble, hbo_dcprobability_tibble)),
  tar_target(nhb_plots, tibbleplots(nhb_mclogsum_tibble, nhb_dcutility_tibble, nhb_dclogsum_tibble, nhb_mcprobability_tibble, nhb_dcprobability_tibble)),


  tar_target(dummy, 2+2)
  
  )


# Targets necessary to build the book / article
book_targets <- list(
  tar_target(taz_file, "data/shapefile_taz.shp", format = "file"),
  tar_target(tazmap_data, tazmap(taz_file)),
  tar_target(map_plot, mapplot(tazmap_data))
)



# run all targets
list(
  data_targets, 
  book_targets
)
