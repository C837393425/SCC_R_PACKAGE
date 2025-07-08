library(ggplot2)
library(dplyr)
library(scales)

# Output PDF file
pdf("C:/Users/C837393425/Desktop/ftzlr_code/manure_qc_plots.pdf", width = 10, height = 6)

# Loop over 6 repetitions
for (i in 1:6) {
  # Read the RDS file
  file_path <- paste0("C:/Users/C837393425/Desktop/ftzlr_code/fert_nri_received_", i, ".rds")
  nri_manure_rec_df <- readRDS(file_path)
  
  # Calculate percentage of manure applied per crop_year and crop
  manure_pct_df <- nri_manure_rec_df %>%
    group_by(crop_year, nri_crop) %>%
    summarize(
      pct_manure = mean(manure_impute == 1, na.rm = TRUE) * 100,
      .groups = "drop"
    )
  
  # Create the plot
  p <- ggplot(manure_pct_df, aes(x = crop_year, y = pct_manure, color = nri_crop)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    scale_y_continuous(labels = percent_format(scale = 1)) +
    labs(
      title = paste("QC Plot", i, "- % of Records Getting Manure by Crop Over Time"),
      x = "Crop Year",
      y = "Percent Getting Manure",
      color = "NRI Crop"
    ) +
    theme_bw(base_size = 14) 
  
  # Print the plot into the PDF
  print(p)
}

# Close the PDF device
dev.off()
