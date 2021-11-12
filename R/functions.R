# This is where you write functions that can be called from 
# _targets
# 
# 

#read skims
readskims <- function(path){
  read_all_omx(path) %>%
    mutate(auto = SOV_TIME__AM, nonmotor = WLK_LOC_WLK_TOTIVT__AM/100 , transit = WLK_COM_WLK_TOTIVT__AM/100 ) %>%
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
    summarise(HBW = mean(HBW),
              HBO = mean(HBO),
              NHB = mean(NHB))
  nhts_tripprod
}

#trip gen
tripprod <- function(households, nhts_tripprod){
  nato0 <- function(x) {ifelse(is.na(x), 0, x)}
  
  nhts_tripprod2 <- nhts_tripprod %>% 
    mutate("svw" = paste(hhsize, hhvehcnt, wrkcount))
  
  households2 <- households %>% 
    mutate("svw" = paste(hhsize, hhvehcnt, wrkcount)) %>%
    left_join(nhts_tripprod2, by = "svw") %>%
    select(-c("hhsize.y", "hhvehcnt.y", "wrkcount.y"))
  
  households2
}





# run mode choice logsum calculator
mc_logsum <- function(){
  #  HBW_util = 
  #  HBO_util = 
  #  NHB_util = 
}

# run destination choice calculator / compute destination choice

# compute mode choice probability