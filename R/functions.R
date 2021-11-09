# This is where you write functions that can be called from 
# _targets
# 
# 

#read skims
readskims <- function(path){
  read_all_omx(path)
}

#read CSVs

# read in SE DATA, read in trip rates, read in mc / dc utilities
readCSV <- function(path){
  read_csv(path)
}


# run trip gen


# run mode choice logsum calculator


# run destination choice calculator / compute destination choice


# compute mode choice probability