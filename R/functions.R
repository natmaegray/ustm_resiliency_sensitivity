# This is where you write functions that can be called from 
# _targets
# 
# 

#read skims
readskims <- function(path){
  read_all_omx(path, names = c("DIST","SOV_TIME__AM", "DISTWALK", "WLK_LOC_WLK_TOTIVT__AM")) %>%
    mutate(auto = SOV_TIME__AM, nonmotor = DISTWALK*(5280*1/60*0.25) , transit = WLK_LOC_WLK_TOTIVT__AM/100 ) %>%
    select(c("origin", "destination","DIST", "auto", "nonmotor", "transit"))
}

#read CSVs
readCSV <- function(path){
  read_csv(path)
}

#organize hh data
hh_data <- function(households){
  households %>% 
    select(HHID, income, PERSONS, VEHICL, workers) %>%
    mutate(
      hhsize = ifelse(PERSONS > 4, 4, PERSONS) ,
      hhvehcnt = ifelse(VEHICL > 3, 3, VEHICL),
      wrkcount = ifelse(workers > 2, 2, workers)
    ) %>%
    select(HHID, income, hhsize, hhvehcnt, wrkcount)
}

#use nhts2017 data to get approximate trip productions
nhts_2017 <- function(nhts_households, nhts_trips){
  hh <- nhts_households %>% 
    filter(msasize == "02", !travday %in% c("01", "07")) %>%
    select(houseid, wthhfin, hhsize, hhvehcnt, numadlt, hhfaminc, wrkcount) %>%
    mutate(
      hhsize = ifelse(hhsize > 4, 4, hhsize),
      hhvehcnt = ifelse(hhvehcnt > 3, 3, hhvehcnt),
      wrkcount = ifelse(wrkcount > 2, 2, wrkcount)
    )
  trips <- nhts_trips %>% 
    filter(houseid %in% hh$houseid) %>%
    group_by(houseid, trippurp) %>%
    summarise(trips = n()) %>%
    pivot_wider(id_cols = houseid, names_from = trippurp, 
                values_from = trips, values_fill = 0)
  nato0 <- function(x) {ifelse(is.na(x), 0, x)}
  nhts_tripprod <- hh %>% 
    left_join(trips, by = "houseid") %>%
    mutate_at(vars(names(trips)), nato0) %>%
    select(c("hhsize", "hhvehcnt", "numadlt", "wthhfin", "wrkcount", "HBW","HBO", "NHB")) %>%
    group_by(hhsize, hhvehcnt, wrkcount) %>%
    summarise(HBW_mean = weighted.mean(HBW, wthhfin),
              HBO_mean = weighted.mean(HBO, wthhfin),
              NHB_mean = weighted.mean(NHB, wthhfin))
  nhts_tripprod
}

#trip gen
tripprod <- function(households, nhts_tripprod){
  nato0 <- function(x) {ifelse(is.na(x), 0, x)}

  households %>% 
    left_join(nhts_tripprod, by = c("hhsize", "hhvehcnt", "wrkcount"))
}

# generate mode choice coefficient lists
#' @param purpose HBW, HBO, or NHB
#' @param mc_coeff CSV for MC coefficients
#' @param mc_const CSV for MC constants
#' @param ndraws Number of draws
#' 
#' @examples 
#' mc_coeff <- list("ivtt" = -0.02, "cost" = -0.04)
#' mc_logsum("bike", skims, mc_coeff[[1]])

generate_mc_coeff <- function(purpose, mc_coeff, mc_const, ndraws){
  #original
  mc_coeff_purpose <- mc_coeff %>% select(c("Name", purpose))
  coeff_ivtt <- mc_coeff_purpose[[1,2]]
  coeff_cost <- mc_coeff_purpose[[5,2]]
  auto_cost <- mc_coeff_purpose[[19,2]]
  coeff_walk1 <- mc_coeff_purpose[[9,2]]   ### search and replace mode with purpose
  
  mc_const_mode <- mc_const %>% select(c("Name", purpose))
  k_nmot <- mc_const_mode[[3,2]]
  k_trn <- mc_const_mode[[2,2]]
  
  orig_coeffs <- list()
  orig_coeffs[[1]] <-  list("ivtt" = coeff_ivtt, 
                            "ccost" = coeff_cost, 
                            "autocost" = auto_cost, 
                            "walk1" = coeff_walk1, 
                            "knmot" = k_nmot, 
                            "k_trn" = k_trn)

  #rand
  rand_coeffs <-lapply(1:ndraws, function(i){
    list("ivtt" = rnorm(1, coeff_ivtt, abs(0.30*coeff_ivtt)), 
         "ccost" = rnorm(1, coeff_cost, abs(0.30*coeff_cost)), 
         "autocost" = rnorm(1, auto_cost, abs(0.30*auto_cost)), 
         "walk1" = rnorm(1, coeff_walk1, abs(0.30*coeff_walk1)), 
         "knmot" = rnorm(1, k_nmot, abs(0.30*k_nmot)), 
         "k_trn" = rnorm(1, k_trn, abs(0.30*k_trn)))
    })
  
  
  #lhs
  X <- randomLHS(ndraws, 6) 
  X[,1] <- -qunif(X[,1], -coeff_ivtt-0.30*-coeff_ivtt, -coeff_ivtt+0.30*-coeff_ivtt) 
  X[,2] <- -qunif(X[,2], -coeff_cost-0.30*-coeff_cost, -coeff_cost+0.30*-coeff_cost) 
  X[,3] <- qunif(X[,3], auto_cost-0.30*auto_cost, auto_cost+0.30*auto_cost) 
  X[,4] <- -qunif(X[,4], -coeff_walk1-0.30*-coeff_walk1, -coeff_walk1+0.30*-coeff_walk1)
  X[,5] <- qunif(X[,5], k_nmot-0.30*k_nmot, k_nmot+0.30*k_nmot) 
  X[,6] <- -qunif(X[,6], -k_trn-0.30*-k_trn, -k_trn+0.30*-k_trn)
  
  lhs_coeffs <- list()
  lhs_coeffs[[1]] <-  list("ivtt" = X[1,1], "ccost" = X[1,2], "autocost" = X[1,3], "walk1" = X[1,4], "knmot" = X[1,5], "k_trn" = X[1,6])
  lhs_coeffs[[2]] <-  list("ivtt" = X[2,1], "ccost" = X[2,2], "autocost" = X[2,3], "walk1" = X[2,4], "knmot" = X[2,5], "k_trn" = X[2,6])
  lhs_coeffs[[3]] <-  list("ivtt" = X[3,1], "ccost" = X[3,2], "autocost" = X[3,3], "walk1" = X[3,4], "knmot" = X[3,5], "k_trn" = X[3,6])
  lhs_coeffs[[4]] <-  list("ivtt" = X[4,1], "ccost" = X[4,2], "autocost" = X[4,3], "walk1" = X[4,4], "knmot" = X[4,5], "k_trn" = X[4,6])
  lhs_coeffs[[5]] <-  list("ivtt" = X[5,1], "ccost" = X[5,2], "autocost" = X[5,3], "walk1" = X[5,4], "knmot" = X[5,5], "k_trn" = X[5,6])
  
  mc_coeff_list <- list()
  mc_coeff_list[[1]] <- orig_coeffs
  mc_coeff_list[[2]] <- rand_coeffs
  mc_coeff_list[[3]] <- lhs_coeffs
  
  return(mc_coeff_list)
}




# run mode choice logsum calculator
#' @param mc_coeff A named list of parameter values
#' 
#' @examples 
#' mc_coeff <- list("ivtt" = -0.02, "cost" = -0.04)
#' mc_logsum("bike", skims, mc_coeff[[1]])

mc_logsum <- function(skims, coeff_list){

  mc_coeffs <- coeff_list
  
  coeff_ivtt <- mc_coeffs$ivtt
  coeff_cost <- mc_coeffs$ccost
  auto_cost <- mc_coeffs$autocost
  coeff_walk1 <- mc_coeffs$walk1
  k_nmot <- mc_coeffs$knmot
  k_trn <- mc_coeffs$k_trn

  mode_choice <- skims %>%
    mutate(drive_utility = (coeff_ivtt*auto)+(coeff_cost*auto_cost*DIST),
           nonmo_utility = (k_nmot + 20*(coeff_walk1*nonmotor)),
           trans_utility = k_trn + (coeff_ivtt*transit) 
      ) %>%
    mutate(ex_drive_util = exp(drive_utility),
           ex_nonmo_util = if_else(DIST > 2, 0, exp(nonmo_utility)),
           ex_trans_util = if_else(transit == 0, 0, exp(trans_utility))
      ) %>%
    mutate(denom = ex_drive_util + ex_nonmo_util + ex_trans_util,
           denom_util = if_else(denom == 0, 1, denom),
           logsum = log(denom_util)
      ) %>%
    select(c("origin", "destination", "ex_drive_util", "ex_nonmo_util", "ex_trans_util", "denom_util", "logsum" ))
}

# loop mc logsums
mc_logsum_loop <- function(skims, coeff_full){
  baselogsums <- mc_logsum(skims, coeff_full[[1]])
  montecarlologsums <- lapply(coeff_full[[2]], function(coeffs){
    mc_logsum(skims, coeffs)
  })
  latinhyperlogsums <- lapply(coeff_full[[3]], function(coeffs){
    mc_logsum(skims, coeffs)
  })
}


# run destination choice calculator / compute destination choice


  #employment to trip attraction

# compute mode choice probability
mc_probability <- function(mode_skims){
  mode_skims %>%
    mutate(drive_prob = ex_drive_util / denom_util,
           nonmo_prob = ex_nonmo_util / denom_util, 
           trans_prob = ex_trans_util / denom_util 
      ) %>%
    select(c("origin", "destination", "drive_prob", "nonmo_prob", "trans_prob"))
}

tazmap <- function(path){
  my_spdf <- readOGR(path)
  sf_object <- st_as_sf(my_spdf, plot = FALSE, fill = TRUE) %>%
    filter(TAZ1454 < 26) %>%
    select(c("geometry","TAZ1454") )
}


mapplot <- function(map_data){
  ggplot() + 
    geom_sf(data = map_data, size = 1, color = "black", fill = "grey") + 
    geom_sf_text(data = map_data, aes(label = TAZ1454)) +
    coord_sf() +
    theme_map()
}
