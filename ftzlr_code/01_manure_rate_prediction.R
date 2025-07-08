#Necessary library
library(forecast)
library(zoo)
library(tidyverse)





                 # step 0: calculating total cropland area in Hawaii by annual crop and year from NRI data

# Read the full NRI dataset that includes irrigation data for 1979–2023
path_server <- "N:/Research/Ogle/Soil_C_N2O_natassess/1990-2023/Activity_Data_Imputations"
nri_all <- readRDS(file = paste0(path_server, "/data/NRI23_all_with_irrigation_1979_2023.rds"))

# Filter data to only include records from Hawaii
nri_hawaii <- nri_all %>% dplyr::filter(state_abbr == "HI")

# crops to impute on 
annual_crops <- c("Close/All other close grown", "Row/Corn for grain", "Row/Other veg/truck crops", 
                  "Row/Potatoes", "Row/Sorghum for grain")
perennial_crops <- c("Hay", "Other crop/Other-setaside etc", "Other crop/Summer fallow", "Pasture", 
                     "Rangeland", "Hort/Bush fruit", "Hort/Fruit", "Hort/Nut", 
                     "Hort/Other")

# Filter the Hawaii NRI dataset to retain only records with annual crops
hawaii_annual <- nri_hawaii %>% dplyr::filter(chist_landuse %in% annual_crops)
ha_treated_summary <- hawaii_annual %>%
  group_by(crop_year, nri_crop) %>%
  summarise(
    total_ha_treated = sum(xfact * 100 * 0.404686, na.rm = TRUE), # convert to acre then ha 
    .groups = "drop"
  )




               #Step 1: #First predict census manure level using erg manure data
                        #Then interpolate the missing census manure level data 

#load and manipulate data
data <- readRDS("N:/Research/Ogle/Soil_C_N2O_natassess/1990-2023/Activity_Data_Imputations/Hawaii/hawaii_manure_N_avail_census_ERG.rds")
erg_data <- data %>% dplyr::filter(source == "ERG")  #filtering ERG data
census_data <- data %>% dplyr::filter(source == "Census")  #filtering census data
combined <- inner_join(erg_data, census_data, by = "year", suffix = c("_erg", "_census"))  #merging erg and census data

# interpolate missing values at the beginning of ERG time series
full_years <- 1979:2023

# filter ERG data and rename variables
erg_full <- data %>% 
  filter(source == "ERG") %>% 
  select(year, avail_Nkg) %>% 
  rename(avail_Nkg_erg = avail_Nkg) %>%
  right_join(data.frame(year = full_years), by = "year") %>%
  arrange(year)

# Find the non-NA values
non_na_indices <- which(!is.na(erg_full$avail_Nkg_erg))

# Extract known x and y values
x_known <- erg_full$year[non_na_indices]
y_known <- erg_full$avail_Nkg_erg[non_na_indices]

# Fit a 2nd-degree polynomial model
model_poly <- lm(y_known ~ poly(x_known, 2))

# Identify missing indices before the first known point
missing_indices <- which(is.na(erg_full$avail_Nkg_erg))
early_missing_indices <- missing_indices[missing_indices < non_na_indices[1]]

# Predict using the polynomial model
erg_full$avail_Nkg_erg_interp <- erg_full$avail_Nkg_erg
erg_full$avail_Nkg_erg_interp[early_missing_indices] <- predict(
  model_poly,
  newdata = data.frame(x_known = erg_full$year[early_missing_indices])
)

# Fit time series model: predict census using ERG
fit <- auto.arima(combined$avail_Nkg_census, xreg = combined$avail_Nkg_erg)

# Predict missing census NKg using the time series model
erg_input <- erg_full$avail_Nkg_erg_interp
predicted_census <- forecast(fit, xreg = erg_input)$mean

census_pred_df <- data.frame(
  year = full_years,
  avail_Nkg_census_pred = as.numeric(predicted_census)
)


########################### QC plot for observed vs predicted census and ERG data

# Join observed and predicted values by year
census_compare_df <- census_pred_df %>%
  left_join(census_data %>% select(year, observed_census_Nkg = avail_Nkg), by = "year") %>%
  left_join(erg_data %>% select(year, observed_erg_Nkg = avail_Nkg), by = "year") %>%
  rename(
    predicted_census_Nkg = avail_Nkg_census_pred
  )

census_compare_long <- census_compare_df %>%
  pivot_longer(
    cols = c(predicted_census_Nkg, observed_census_Nkg, observed_erg_Nkg),
    names_to = "source",
    values_to = "N_kg"
  )

obs_pred_manure_N_plot <- ggplot(census_compare_long, aes(x = year, y = N_kg, color = source)) +
  # Plot predicted line
  geom_line(data = filter(census_compare_long, source == "predicted_census_Nkg"), size = 1.2) +
  geom_point(data = filter(census_compare_long, source == "predicted_census_Nkg"), size = 3) +
  
  # Plot observed census points
  geom_point(data = filter(census_compare_long, source == "observed_census_Nkg"), 
             shape = 17, size = 4) +
  
  # Plot observed ERG points (optional)
  geom_point(data = filter(census_compare_long, source == "observed_erg_Nkg"), 
             shape = 15, size = 2.5) +
  
  # Custom colors and labels
  scale_color_manual(
    values = c(
      "predicted_census_Nkg" = "blue", 
      "observed_census_Nkg" = "darkgreen",
      "observed_erg_Nkg" = "red"
    ),
    labels = c(
      "predicted_census_Nkg" = "Predicted (Census Model)",
      "observed_census_Nkg" = "Observed (Census)",
      "observed_erg_Nkg" = "Observed (ERG)"
    )
  ) +
  
  labs(
    title = "Comparison of Observed vs Predicted Manure N (kg)",
    x = "Year",
    y = "Manure N (kg)",
    color = "Source"
  ) +
  theme_bw(base_size = 14)

ggsave("outputs/plots/census_comparison_plot.png", plot = obs_pred_manure_N_plot, width = 10, height = 6, dpi = 300)
##############################

# Replace predicted values with actual where known
census_known <- census_data %>% select(year, avail_Nkg) %>% rename(avail_Nkg_census_pred = avail_Nkg)

census_full <- left_join(census_pred_df, census_known, by = "year") %>%
  mutate(avail_Nkg_final = ifelse(!is.na(avail_Nkg_census_pred.y), 
                                  avail_Nkg_census_pred.y, 
                                  avail_Nkg_census_pred.x)) %>%
  select(year, avail_Nkg_final)

# Comment: I guess interpolation of ERG time series should be done before census time series prediction



                 # Step 2: # Calculate Crop Wise avg manure N rate estimate and percentage across all states and years 

# hawaii annual crops
annual_crops <- c("Close/All other close grown", "Row/Corn", "Row/Other veg", 
                  "Row/Potatoes", "Row/Sorghum")

# Compute total hectares and crop share of Row/Corn and Row/Sorghum in 2005 from Hawaii annual crops dataset
area_proprotion <- hawaii_annual %>%
  filter(crop_year == 2005, nri_crop %in% c("Row/Corn", "Row/Sorghum")) %>%
  group_by(nri_crop) %>%
  summarise(total_ha = sum(xfact * 100 * 0.404686, na.rm = TRUE)) %>%
  mutate(avg_pct = total_ha*100 / sum(total_ha))

# Load the avg rates data
avg_rates <-  readRDS("N:/Research/Ogle/Soil_C_N2O_natassess/1990-2023/Activity_Data_Imputations/data/manure_arms_1996_2010.rds")

# Average Nrate_est across all states/years for each hawaii annual crop
avg_cropwise_summary <- avg_rates %>%
  filter(nri_crop %in% annual_crops) %>%
  group_by(nri_crop) %>%
  summarize(
    avg_Nrate_lb_acre = mean(Nrate_est, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(avg_pct = area_proprotion$avg_pct[1])     

Sorghum_row <- tibble(
  nri_crop = "Row/Sorghum",
  avg_Nrate_lb_acre = avg_cropwise_summary$avg_Nrate_lb_acre,
  avg_pct = area_proprotion$avg_pct[2]  
)

avg_cropwise_summary <- bind_rows(avg_cropwise_summary, Sorghum_row)

# Convert lb/acre to kg/ha (1 lb/acre = 1.12085 kg/ha)
avg_cropwise_summary <- avg_cropwise_summary %>%
  mutate(avg_Nrate_kg_ha = avg_Nrate_lb_acre * 1.12085) %>%
  select(-c("avg_Nrate_lb_acre"))


                     # Step 3: Calculating the total amount of manure nitrogen by crop and year.

# Cross-join years with crop info
manure_allocation <- census_full %>%
  crossing(avg_cropwise_summary) %>%
  mutate(
    manure_Nkg_to_crop = avail_Nkg_final * (avg_pct/100),  # distributed N per crop
    ha_treated = manure_Nkg_to_crop / avg_Nrate_kg_ha  # hectares treated
  ) %>% arrange(nri_crop) 

########
#QC :Ensure total imputed manure N (kg) across all Hawaii crops does not exceed census-available manure N by year

# Summarize total imputed manure N by year across all crops
total_imputed_by_year <- manure_allocation %>%
  group_by(year) %>%
  summarise(total_imputed_Nkg = sum(manure_Nkg_to_crop, na.rm = TRUE))

# Join with census manure N availability
qc_check <- total_imputed_by_year %>%
  left_join(census_full, by = "year") %>%
  mutate(
    difference = avail_Nkg_final - total_imputed_Nkg,
    exceeds_census = total_imputed_Nkg > avail_Nkg_final
  )

# View any problematic years
qc_check %>% filter(exceeds_census == TRUE)
###########

# Adding extra NRI crops
manure_allocation_updated <- tibble(
  nri_crop = rep(c("Row/Corn", "Row/Sorghum", "Row/Potatoes", "Row/Other veg", "Close/All other close grown"), each = 45),
  year = rep(1979:2023, times = 5),
  manure_N_to_crop = c(
    manure_allocation$manure_Nkg_to_crop,  # For Corn/sorghum
    rep(0, 45 * 3)                        # 0 for remaining 3 crops
  ),
  ha_treated = c(
    manure_allocation$ha_treated,        # For Corn/Sorghum
    rep(0, 45 * 3)                        # 0 for remaining 3 crops
  )
)




# Step 4: taking ha applying manure over total cropland hectares in Hawaii to get percent area applying manure 

# Rename to clarify denominator
nri_cropland_summary <- ha_treated_summary %>%
  rename(year = crop_year, total_crop_ha = total_ha_treated)

# Join manure allocation with total cropland area
manure_pct_by_crop_year <- manure_allocation_updated %>%
  left_join(nri_cropland_summary, by = c("year", "nri_crop")) %>%
  mutate(
    manure_pct = (ha_treated / total_crop_ha) * 100
  ) %>%
  mutate(
    manure_pct = case_when(
      is.na(manure_pct) ~ 0,
      total_crop_ha < ha_treated ~ 100,
      TRUE ~ manure_pct
    )
  )

# Saving crop and yearwise manure application rate data
saveRDS(manure_pct_by_crop_year, "crop_year_manurere_pct.rds")

