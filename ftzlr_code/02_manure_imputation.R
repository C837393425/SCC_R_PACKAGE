####################################################
## Activity Product 2024: Tier 2 Hawaii           ##
## Assign Manure to NRI points                    ##
## Lauren Hoskovec                                ##
## last updated: 6/20/2024                        ##
#################################################### 
rm(list = ls())
gc()

##################
## read in data ## -------------------------------------------------------------
##################

# paths
path_server <- "N:/Research/Ogle/Soil_C_N2O_natassess/1990-2023/Activity_Data_Imputations"
# Read NRI data
nri_sub <- readRDS(file = paste0(path_server, "/data/NRI23_all_with_irrigation_1979_2023.rds"))
# Filter data to only include records from Hawaii
nri_hawaii <- nri_sub %>% dplyr::filter(state_abbr == "HI")
# annual crops to impute on 
annual_crops <- c("Close/All other close grown", "Row/Corn for grain", "Row/Other veg/truck crops", 
                  "Row/Potatoes", "Row/Sorghum for grain")
# Filter the Hawaii NRI dataset to retain only records with annual crops
hawaii_annual <- nri_hawaii %>% dplyr::filter(chist_landuse %in% annual_crops)


# read in manure data 
crop_year_manure_pct <- readRDS("C:/Users/C837393425/Desktop/ftzlr_code/crop_year_manurere_pct.rds")
crop_year_manure_pct <- crop_year_manure_pct %>%
  rename(crop_year = year)
# nri_crop names for the vegetables 
crop_list <- unique(crop_year_manure_pct$nri_crop); crop_list
# dplyr::filter nri data 
nri_filtered <- hawaii_annual %>% 
  dplyr::filter(nri_crop %in% crop_list) %>%
  dplyr::mutate(manure_recv = 0) %>%
  dplyr::group_by(nri_crop, crop_year) %>%
  dplyr::mutate(xfact_wtd = xfact / sum(xfact) * 100) %>%
  ungroup() 






#########################################
## Select Points to Receive Manure ## --------------------------------------
#########################################

# do 6 imputations of assignment 
# because this is a random process 
# using the same pct area treated, 
# but the points assigned will differ by imputation 

# randomly order the data so that you get a new selection of points 
# with each imputation 

# start with 6 
n_imps <- 6
i = 1

for(i in 1:n_imps){
  
  print(paste("Imputation",i))
  
  # randomly order the points so sampling in order using cumsum(xfact_wtd) 
  # to reach predicted percentages gives a random sample
  
  set.seed(3321 + i)
  nri_filtered_arranged <- nri_filtered %>% arrange(sample(recordid2017))
  
  # Initialize empty data frame to store imputed manure data
  nri_manure_rec_df <- NULL
  
  # Record start time for timing performance
  st <- Sys.time()
  
  # Loop over years of interest (1979–2023)
  for(y in 1979:2023){
    
    # Filter NRI data for current year
    nri_year <- nri_filtered_arranged %>% dplyr::filter(crop_year == y)
    
    # Skip this year if there's no data
    if(nrow(nri_year) == 0){
      next
    }
    
    # do imputation
    nri_manure_rec_yr <- nri_year %>% 
      dplyr::select(recordid2017, crop_year, nri_crop, chist_landuse, ceap_region, xfact, xfact_wtd, manure_recv) %>% 
      
      # Join with estimated percent manured by crop and year
      left_join(crop_year_manure_pct %>% dplyr::filter(crop_year == y) %>% dplyr::select(nri_crop, crop_year, manure_pct), 
                by = c("nri_crop", "crop_year")) %>% 
      
      dplyr::rename(pct_est = manure_pct) %>%
      
      # Group by crop to enable cumsum calculations
      dplyr::group_by(nri_crop) %>% 
      arrange(nri_crop) %>% 
      
      # Calculate cumulative sum of weights (xfact)
      dplyr::mutate(cumsum_xfact = cumsum(xfact_wtd)) %>%
      
      # Identify the first record with the minimum weight within group
      dplyr::mutate(first_min = as.numeric(row_number() == first(which(xfact_wtd == min(xfact_wtd))))) %>% 
      
      # Flag cases where the estimated percentage is smaller than the minimum weight
      dplyr::mutate(too_small = case_when(pct_est < min(xfact_wtd) ~ 1, TRUE ~ 0)) %>%
      
      # Impute fertilizer for "too small" cases based on random draw
      dplyr::mutate(manure_impute = case_when(
        (first_min == 1) & (too_small == 1) & (runif(1) < pct_est / min(xfact_wtd)) ~ 1,
        TRUE ~ 0)) %>%
      
      # For regular cases, impute based on cumulative weights
      dplyr::mutate(manure_impute = case_when(
        (too_small == 0) & (cumsum_xfact <= pct_est + min(xfact_wtd)) ~ 1,
        TRUE ~ manure_impute)) %>%
      
      # Replace any NAs in manure_impute with 0
      dplyr::mutate(manure_impute = replace_na(manure_impute, 0)) %>%
      
      # Flag the rows where manure was imputed
      dplyr::mutate(flag = case_when(manure_impute == 1 ~ 1, TRUE ~ 0)) %>%
      
      # Select relevant columns for output
      dplyr::select(recordid2017, crop_year, nri_crop, chist_landuse, ceap_region, xfact, xfact_wtd, 
                    pct_est, manure_impute, manure_recv) %>%
      ungroup()
    
    # bind main crops and vegetables
    nri_manure_rec_year_bind <- nri_manure_rec_yr 
    
    # bind rows to finish the data frame 
    nri_manure_rec_df <- bind_rows(nri_manure_rec_df, nri_manure_rec_year_bind)
    
    # take out fert_recv to avoid confusion 
    nri_manure_rec_df <- nri_manure_rec_df %>% dplyr::select(-manure_recv)
    
  }
  
  # Record end time and print how long the imputation took
  en <- Sys.time()
  print(en - st) 
  
  saveRDS(nri_manure_rec_df, file = paste0("C:/Users/C837393425/Desktop/ftzlr_code/fert_nri_received_", i, ".rds"))
}

