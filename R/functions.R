# This is where you write functions that can be called from 
# _targets
# 
# 

#read CSVs
readCSV <- function(path){
  read_csv(path)
}

#read skims -> gives travel times
readskims <- function(path){
  read_all_omx(path, names = c("DIST","SOV_TIME__AM", "DISTWALK", "WLK_LOC_WLK_TOTIVT__AM")) %>%
    mutate(auto = SOV_TIME__AM, nonmotor = DISTWALK*(5280*1/60*0.25) , transit = WLK_LOC_WLK_TOTIVT__AM/100 ) %>%
    select(c("origin", "destination","DIST", "auto", "nonmotor", "transit"))
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
  coeff_walk1 <- mc_coeff_purpose[[9,2]] 
  coeff_walk2 <- mc_coeff_purpose[[10,2]] 
  
  mc_const_purpose <- mc_const %>% select(c("Name", purpose))
  k_nmot <- mc_const_purpose[[3,2]]
  k_trn <- mc_const_purpose[[2,2]]
  
  orig_coeffs <- list()
  orig_coeffs[[1]] <-  list("ivtt" = coeff_ivtt, 
                            "ccost" = coeff_cost, 
                            "autocost" = auto_cost, 
                            "walk1" = coeff_walk1,
                            "walk2" = coeff_walk2)

  #rand
  rand_coeffs <-lapply(1:ndraws, function(i){
    list("ivtt" = rnorm(1, coeff_ivtt,    abs(0.10*coeff_ivtt)), 
         "ccost" = rnorm(1, coeff_cost,   abs(0.10*coeff_cost)), 
         "autocost" = rnorm(1, auto_cost, abs(0.10*auto_cost)), 
         "walk1" = rnorm(1, coeff_walk1,  abs(0.10*coeff_walk1)), 
         "walk2" = rnorm(1, coeff_walk2,  abs(0.10*coeff_walk2)))
    })
  
  
  #lhs
  X <- randomLHS(ndraws, 6) 
  X[,1] <- qnorm(X[,1], coeff_ivtt,  abs(0.10*coeff_ivtt)) 
  X[,2] <- qnorm(X[,2], coeff_cost,  abs(0.10*coeff_cost)) 
  X[,3] <- qnorm(X[,3], auto_cost,   abs(0.10*auto_cost)) 
  X[,4] <- qnorm(X[,4], coeff_walk1, abs(0.10*coeff_walk1))
  X[,5] <- qnorm(X[,5], coeff_walk2, abs(0.10*coeff_walk2))
  
  lhs_coeffs <-lapply(1:ndraws, function(i){
    list("ivtt" = X[i,1], 
         "ccost" = X[i,2], 
         "autocost" = X[i,3], 
         "walk1" = X[i,4],
         "walk2" = X[i,5])
    })
  
  mc_coeff_list <- list()
  mc_coeff_list[[1]] <- orig_coeffs
  mc_coeff_list[[2]] <- rand_coeffs
  mc_coeff_list[[3]] <- lhs_coeffs
  
  return(mc_coeff_list)
}

generate_dc_coeff <- function(purpose, dc_coeff, ndraws){
  #original
  dc_coeff_purpose <- dc_coeff %>% select(c("VAR", purpose))
  coeff_hh <- dc_coeff_purpose[[1,2]]
  oth_off <- dc_coeff_purpose[[2,2]]
  off_emp <- dc_coeff_purpose[[3,2]]
  oth_emp <- dc_coeff_purpose[[4,2]]
  ret_emp <- dc_coeff_purpose[[5,2]]  

  orig_coeffs <- list()
  orig_coeffs[[1]] <-  list("coeff_hh" = coeff_hh, 
                            "oth_off" = oth_off, 
                            "off_emp" = off_emp, 
                            "oth_emp" = oth_emp, 
                            "ret_emp" = ret_emp)
  ##random DC coeffs
  rand_coeffs <-lapply(1:ndraws, function(i){
    list("coeff_hh" = rnorm(1, coeff_hh,  abs(0.10*coeff_hh)), 
         "oth_off"  = rnorm(1, oth_off,   abs(0.10*oth_off)), 
         "off_emp"  = rnorm(1, off_emp,   abs(0.10*off_emp)), 
         "oth_emp"  = rnorm(1, oth_emp,   abs(0.10*oth_emp)), 
         "ret_emp"  = rnorm(1, ret_emp,   abs(0.10*ret_emp)))
  })
  
  #lhs
  X <- randomLHS(ndraws, 5) 
  X[,1] <- qnorm(X[,1], coeff_hh, abs(0.10*coeff_hh)) 
  X[,2] <- qnorm(X[,2], oth_off,  abs(0.10*oth_off)) 
  X[,3] <- qnorm(X[,3], off_emp,  abs(0.10*off_emp)) 
  X[,4] <- qnorm(X[,4], oth_emp,  abs(0.10*oth_emp))
  X[,5] <- qnorm(X[,5], ret_emp,  abs(0.10*ret_emp)) 

  lhs_coeffs <-lapply(1:ndraws, function(i){
    list("coeff_hh" = X[i,1], 
         "oth_off" = X[i,2], 
         "off_emp" = X[i,3], 
         "oth_emp" = X[i,4], 
         "ret_emp" = X[i,5] )
  })
  
  dc_coeff_list <- list()
  dc_coeff_list[[1]] <- orig_coeffs
  dc_coeff_list[[2]] <- rand_coeffs
  dc_coeff_list[[3]] <- lhs_coeffs
  
  return(dc_coeff_list)
}

# WRITE TO CSV - not using anymore
{

#mcparam_csv <- function(hbw, hbo, nhb, mccoeff){
#  hbw_df <- bind_rows(hbw)
#  hbo_df <- bind_rows(hbo)
#  nhb_df <- bind_rows(nhb)
#  
#  for(i in 1:100){
#  
#    folder <- "~/Desktop/param"
#    iter <- i
#    dir.create(file.path(folder, iter))
#    
#  mc_coeff_lhs <- mccoeff %>%
#    mutate(HBW = replace(HBW, Name == "CIVTT",    hbw_df[[i+101,1]]),
#           HBW = replace(HBW, Name == "CCOST",    hbw_df[[i+101,2]]),
#           HBW = replace(HBW, Name == "CWALK1",   hbw_df[[i+101,4]]),
#           HBW = replace(HBW, Name == "CWALK2",   hbw_df[[i+101,5]]),
#           HBW = replace(HBW, Name == "AUTOCOST", hbw_df[[i+101,3]]),
#           HBO = replace(HBO, Name == "CIVTT",    hbo_df[[i+101,1]]),
#           HBO = replace(HBO, Name == "CCOST",    hbo_df[[i+101,2]]),
#           HBO = replace(HBO, Name == "CWALK1",   hbo_df[[i+101,4]]),
#           HBO = replace(HBO, Name == "CWALK2",   hbo_df[[i+101,5]]), 
#           HBO = replace(HBO, Name == "AUTOCOST", hbo_df[[i+101,3]]),
#           NHB = replace(NHB, Name == "CIVTT",    nhb_df[[i+101,1]]),
#           NHB = replace(NHB, Name == "CCOST",    nhb_df[[i+101,2]]),
#           NHB = replace(NHB, Name == "CWALK1",   nhb_df[[i+101,4]]),
#           NHB = replace(NHB, Name == "CWALK2",   nhb_df[[i+101,5]]),
#           NHB = replace(NHB, Name == "AUTOCOST", nhb_df[[i+101,3]]))
#  
#  write_csv(mc_coeff_lhs , file = file.path(folder, iter, "MC_Coefficients.csv"), quote="none")
#  
#  2+3
#  }  
#  
#}
#
#dcparam_csv <- function(hbw, hbo, nhb, dccoeff){
#  hbw_df <- bind_rows(hbw)
#  hbo_df <- bind_rows(hbo)
#  nhb_df <- bind_rows(nhb)
#  
#  for(i in 1:100){
#    
#    folder <- "~/Desktop/param"
#    iter <- i
#    
#    dc_coeff_lhs <- dccoeff %>%
#      slice(1:(n()-1)) %>%
#      mutate(HBW = replace(HBW, VAR == "HH",      hbw_df[[i+1,1]]),
#             HBW = replace(HBW, VAR == "I(OTH_EMP + OFF_EMP)", hbw_df[[i+1,2]]),
#             HBW = replace(HBW, VAR == "OFF_EMP", hbw_df[[i+1,3]]),
#             HBW = replace(HBW, VAR == "OTH_EMP", hbw_df[[i+1,4]]),
#             HBW = replace(HBW, VAR == "RET_EMP", hbw_df[[i+1,5]]),
#             
#             HBO = replace(HBO, VAR == "HH",      hbo_df[[i+1,1]]),
#             HBO = replace(HBO, VAR == "I(OTH_EMP + OFF_EMP)", hbo_df[[i+1,2]]),
#             HBO = replace(HBO, VAR == "OFF_EMP", hbo_df[[i+1,3]]),
#             HBO = replace(HBO, VAR == "OTH_EMP", hbo_df[[i+1,4]]),
#             HBO = replace(HBO, VAR == "RET_EMP", hbo_df[[i+1,5]]),
#             
#             NHB = replace(NHB, VAR == "HH",      nhb_df[[i+1,1]]),
#             NHB = replace(NHB, VAR == "I(OTH_EMP + OFF_EMP)", nhb_df[[i+1,2]]),
#             NHB = replace(NHB, VAR == "OFF_EMP", nhb_df[[i+1,3]]),
#             NHB = replace(NHB, VAR == "OTH_EMP", nhb_df[[i+1,4]]),
#             NHB = replace(NHB, VAR == "RET_EMP", nhb_df[[i+1,5]])
#              )
#    
#    write_csv(dc_coeff_lhs , file = file.path(folder, iter, "DC_PARAM.csv"), quote="none")
#    
#  }  
#  
#}
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
  coeff_walk2 <- mc_coeffs$walk2
  k_nmot <- 1.7602
  k_trn <- -0.514

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


# run destination choice calculator / compute destination choice
dc_utility <- function(mc_logsum, dc_coeffs, land_use, skims){
  
  coeff_hh   <- dc_coeffs$coeff_hh 
  c_off_emp  <- dc_coeffs$off_emp
  c_oth_emp  <- dc_coeffs$oth_emp
  c_ret_emp  <- dc_coeffs$ret_emp
  c_lsum     <- 1
  
  tothh   <- land_use$TOTHH
  offiemp <- land_use$OTHEMPN - land_use$RETEMPN - land_use$AGREMPN - land_use$MWTEMPN
  retemp  <- land_use$RETEMPN
  allemp  <- land_use$TOTEMP
  
  sizeterm <- tibble(destination = 1:nrow(land_use),
                     sizeterm = coeff_hh*tothh  + c_off_emp*offiemp + c_ret_emp*retemp + c_oth_emp*(allemp - offiemp - retemp)) %>%
    mutate(log_sizeterm = ifelse(sizeterm > 0, log(sizeterm), 0))
  
  dc_utils <- mc_logsum %>%
    mutate(primary_impedance = c_lsum*logsum) %>%
    left_join(sizeterm, by = "destination") %>%
    mutate(dc_utility = primary_impedance + log_sizeterm) %>%
    select("origin", "destination", "dc_utility")

  dc_utils
  
}

dc_logsum <- function(dc_utils){
  dc_utils %>%
    group_by(origin) %>%
    summarise(dc_logsum = sum(exp(dc_utility)))
  
}

# compute mode choice probability
mc_probability <- function(mc_logsum){
  mc_logsum %>%
    mutate(drive_prob = ex_drive_util / denom_util,
           nonmo_prob = ex_nonmo_util / denom_util, 
           trans_prob = ex_trans_util / denom_util 
    ) %>%
    select(c("origin", "destination", "drive_prob", "nonmo_prob", "trans_prob"))
}

dc_probability <- function(dc_utils, dcls){
  dc_utils %>%
    left_join(dcls, by = "origin") %>%
    mutate(probability = exp(dc_utility) / dc_logsum)
}

#total_trips <- function(productions, dc_probability, mc_probability){
  # trips <- productions*probability*
#}


full_loop <- function(skims, mc_coeff_list_full, dc_coeffs_full, land_use, ndraws){
  
  functions <- function(mc_coeff, dc_coeffs){
    mc_logsum <- mc_logsum(skims, mc_coeff)
    dc_utils <- dc_utility(mc_logsum, dc_coeffs, land_use, skims)
    dc_logsum <- dc_logsum(dc_utils)
    mc_prob <- mc_probability(mc_logsum)
    dc_prob <- dc_probability(dc_utils, dc_logsum)
  
    iteration_list <- list(ModeChoice_Logsum = mc_logsum,
                         Destination_Utility = dc_utils,
                         Destination_Logsum = dc_logsum,
                         ModeChoice_Probability = mc_prob,
                         Destination_Probability = dc_prob)
  }
  
  baseiteration <- functions(mc_coeff_list_full[[1]][[1]], dc_coeffs_full[[1]][[1]])
  
  montecarloiterations <- list()
  for (i in 1:ndraws){
    new_element <- functions(mc_coeff_list_full[[2]][[i]], dc_coeffs_full[[2]][[i]])
    montecarloiterations[[length(montecarloiterations) + 1]] <- new_element} 
  
  latinhyperiterations <- list()
  for (i in 1:ndraws){
    new_element <- functions(mc_coeff_list_full[[3]][[i]], dc_coeffs_full[[3]][[i]])
    latinhyperiterations[[length(latinhyperiterations) + 1]] <- new_element}
  
  full_results <- list(BaseIteration = baseiteration,
       MonteCarloIterations = montecarloiterations,
       LatinHyperIterations = latinhyperiterations)
  
  full_results
}

pull_tibbles <- function(full_list, data_table){
  
  base <- full_list[["BaseIteration"]][[data_table]] %>% mutate(type = "base")
  
  montecarlo <- lapply(full_list[["MonteCarloIterations"]], function(x) x[[data_table]]) %>%
    bind_rows(.id = "draw") %>% mutate(type = "MC")
  
  latinhyper <- lapply(full_list[["LatinHyperIterations"]], function(x) x[[data_table]]) %>%
    bind_rows(.id = "draw") %>% mutate(type = "LHS")
  
  bind_rows(base, montecarlo, latinhyper) %>%
    mutate(draw = as.numeric(draw))
}



tibbleplots <- function(mclogsum_tibble, dcutility_tibble, dclogsum_tibble, mcprobability_tibble, dcprobability_tibble){
  
  mclogsum_tibble_chart <- mclogsum_tibble %>%
                          group_by(type, draw) %>%
                          summarise(mean = mean(logsum)) %>%
                          mutate(cumvar = cumvar(mean),
                                 cummean = cummean(mean)) %>%
               filter(type != "base") %>%
               ggplot() +
               aes(x = draw, y = cummean, ymin = cummean - 1.96*cumvar, ymax = cummean + 1.96*cumvar, colour = type, fill = type, group = type) +
               geom_ribbon(alpha = 0.2, colour = NA) +
               geom_line(size = 0.5) +
               #    ylim(0.025, 0.15) +
               labs(x = "Draw", 
                    y = "Cumulative Standard Deviation",
                    color = "method") +
               scale_color_hue(direction = 1) +
               theme_bw() +
               theme(legend.position = "bottom")
  
  dclogsum_chart <- dclogsum_tibble %>%
                   group_by(type, draw) %>%
                   summarise(mean = mean(dc_logsum)) %>%
    ggplot() +
    aes(x = "", y = mean, colour = type, group = type) +
    labs(x = "", 
         y = "Mean Destination Choice Logsum",
         color = "Method") +
    geom_boxplot(shape = "circle", fill = "#112446") +
    scale_color_hue(direction = 1) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  
  
  myplots <- list(MCLS_variation = mclogsum_tibble_chart,
                  DCLS_mean = dclogsum_chart)
  myplots
  
}



cumvar <- function (x, sd = TRUE) {
  x <- x - x[sample.int(length(x), 1)]  ## see Remark 2 below
  n <- seq_along(x)
  v <- (cumsum(x ^ 2) - cumsum(x) ^ 2 / n) / (n - 1)
  if (sd) v <- sqrt(v)
  v
}


process_stats <- function(meanlogsums){
  meanlogsums %>%
    group_by(type) %>%
    mutate(cumvar = cumvar(meanlogsum)) %>%
    mutate(cummean = cummean(meanlogsum))
  
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
