
library(tidyverse)
library(gridExtra)


#line_plot_function_state: 
#Function to plot the average annual predictions of a given variable by year and state

#Objective: 
# This function computes the weighted average of a specified variable across two datasets (two daycent outputs),
# and creates a time series plot for each state showing the weighted averages over the years.

# Input: 
#df_final: A data frame containing the data with columns including year, state_name, variable_name.x (from first input data (i.e. 491)), variable_name.y (from first input data (i.e. 513)),, and area_ha.
#variable_name: A string representing the name of the variable 
#unit: A string representing the unit of the variable (e.g. ton/ha, cm).

# Output: 
# A list of ggplot objects, each representing a plot for a state showing the time series of weighted averages for the two datasets.
#'@noRd

line_plot_function_state <- function(df_final, variable_name, unit, df1_lab, df2_lab, same_axis) {
  
  # Compute the weighted average by year and state
  df_combined <- df_final %>%
    group_by(state_name, year) %>%
    summarise(
      !!paste0("avg_", variable_name, ".x") := sum(.data[[paste0(variable_name, ".x")]] * area_ha) / sum(area_ha),
      !!paste0("avg_", variable_name, ".y") := sum(.data[[paste0(variable_name, ".y")]] * area_ha) / sum(area_ha),
      .groups = "drop"
    )
  
  # Prepare the combined data for plotting
  df_combined1 <- df_combined %>%
    select(year, state_name,
           avg_variable_df1 = !!paste0("avg_", variable_name, ".x"),
           avg_variable_df2 = !!paste0("avg_", variable_name, ".y"))
  
  # Get axis limits if same_axis = TRUE
  if (same_axis) {
    y_min <- min(c(df_combined1$avg_variable_df1, df_combined1$avg_variable_df2), na.rm = TRUE)
    y_max <- max(c(df_combined1$avg_variable_df1, df_combined1$avg_variable_df2), na.rm = TRUE)
    y_limits <- c(y_min, y_max)
  } else {
    y_limits <- NULL
  }
  
  # Precompute number of points and total area per state
  state_stats <- df_final %>%
    group_by(state_name) %>%
    summarise(
      n_points = n(),
      total_area = sum(area_ha, na.rm = TRUE),
      .groups = "drop"
    )
  
  
  states <- sort(unique(df_combined1$state_name))
  plot_list <- list()
  
  for (i in 1:length(states)) {
    
    state = states[i]
    state_data <- df_combined1 %>% filter(state_name == state)
    
    # Retrieve stats
    stats <- state_stats %>% filter(state_name == state)
    n_pts <- stats$n_points
    area_sum <- stats$total_area
    
    p <- ggplot(state_data, aes(x = year)) +
      geom_line(aes(y = avg_variable_df1, color = df1_lab)) +
      geom_line(aes(y = avg_variable_df2, color = df2_lab)) +
      geom_point(aes(y = avg_variable_df1, color = df1_lab)) +
      geom_point(aes(y = avg_variable_df2, color = df2_lab)) +
      labs(
        title = paste("Year-wise Average", variable_name,unit, "for", state),
        subtitle = paste("Data points:", n_pts, "| Total area (ha):", round(area_sum, 1)),
        x = "Year", 
        y = paste("Average Annual \n", variable_name, unit, "Predictions"),
        color = "Prediction Source"
      ) +
      theme_bw() +
      theme(legend.position = "bottom") +
      (if (!is.null(y_limits)) coord_cartesian(ylim = y_limits) else NULL)
    
    plot_list[[i]] <- p
  }
  
  return(plot_list)
}








#line_plot_function_national: 
#Function to plot the average annual national predictions of a given variable by year 

#Objective: 
# This function computes the weighted average of a specified variable across two datasets (two daycent outputs),
# and creates a time series plot for whole nation showing the weighted averages over the years.

# Input: 
#df_final: A data frame containing the data with columns including year, state_name, variable_name.x (from first input data (i.e. 491)), variable_name.y (from first input data (i.e. 513)),, and area_ha.
#variable_name: A string representing the name of the variable 
#unit: A string representing the unit of the variable (e.g. ton/ha, cm).

# Output: 
# A list of ggplot objects, each representing a plot showing the time series of weighted averages for the two datasets.
#'@noRd


line_plot_function_national <- function(df_final, variable_name, unit, df1_lab, df2_lab, same_axis) {
  
  # Compute the nationwide weighted average by year
  df_combined <- df_final %>%
    group_by(year) %>%
    summarise(
      !!paste0("avg_", variable_name, ".x") := sum(.data[[paste0(variable_name, ".x")]] * area_ha, na.rm = TRUE) / sum(area_ha, na.rm = TRUE),
      !!paste0("avg_", variable_name, ".y") := sum(.data[[paste0(variable_name, ".y")]] * area_ha, na.rm = TRUE) / sum(area_ha, na.rm = TRUE),
      n_points = n(),
      total_area = sum(area_ha, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Get axis limits if same_axis = TRUE
  if (same_axis) {
    y_min <- min(c(df_combined[[paste0("avg_", variable_name, ".x")]],
                   df_combined[[paste0("avg_", variable_name, ".y")]]), na.rm = TRUE)
    y_max <- max(c(df_combined[[paste0("avg_", variable_name, ".x")]],
                   df_combined[[paste0("avg_", variable_name, ".y")]]), na.rm = TRUE)
    y_limits <- c(y_min, y_max)
  } else {
    y_limits <- NULL
  }
  
  # Plot nationwide average
  p <- ggplot(df_combined, aes(x = year)) +
    geom_line(aes(y = .data[[paste0("avg_", variable_name, ".x")]], color = df1_lab)) +
    geom_line(aes(y = .data[[paste0("avg_", variable_name, ".y")]], color = df2_lab)) +
    geom_point(aes(y = .data[[paste0("avg_", variable_name, ".x")]], color = df1_lab)) +
    geom_point(aes(y = .data[[paste0("avg_", variable_name, ".y")]], color = df2_lab)) +
    labs(
      title = paste("Nationwide Average", variable_name, unit, "over Years"),
      subtitle = paste("Total points:", sum(df_combined$n_points), "| Total area (ha):", round(sum(df_combined$total_area), 1)),
      x = "Year", 
      y = paste("Average Annual", variable_name, unit, "Predictions"),
      color = "Prediction Source"
    ) +
    theme_bw() +
    theme(legend.position = "bottom") +
    (if (!is.null(y_limits)) coord_cartesian(ylim = y_limits) else NULL)
  
  return(list(p))  # Return as list for compatibility
}














# violin_plot_function_state: Create state-level violin plots for a given variable
#
# Objective:
# This function generates violin plots to visualize the distribution of model predictions 
# (from two data sources) for a specific variable across U.S. states. The plots are grouped 
# into user-defined year bins (e.g., 5-year periods) and annotated with total area and 
# number of data points for each state.
#
# Input:
# - df_final: Merged dataframe containing predictions from two models along with year, state_name, and area_ha.
# - variable_name: Name of the variable to plot (e.g., "SOMSC").
# - unit: Unit of the variable (e.g., "Mg/ha").
# - df1_lab, df2_lab: Labels for the two models/datasets used in the legend.
# - violin_plot_bin: Width of the time bin (in years) for grouping years (e.g., 5 for 5-year bins).
# - same_axis: Logical; if TRUE, all plots use the same y-axis scale across states.
#
# Output:
# - Returns a list of `ggplot` violin plots, one per state, with state-level subtitle stats 
#   (number of points and total area).
#' @noRd


violin_plot_function_state <- function(df_final, variable_name, unit, df1_lab, df2_lab, violin_plot_bin, same_axis) {
  
  # Prepare long-format data
  df_long <- df_final %>%
    select(year, state_name, area_ha,
           value_df1 = !!sym(paste0(variable_name, ".x")),
           value_df2 = !!sym(paste0(variable_name, ".y"))) %>%
    pivot_longer(cols = starts_with("value_df"), names_to = "Model", values_to = variable_name) %>%
    mutate(
      Model = recode(Model,
                     "value_df1" = df1_lab,
                     "value_df2" = df2_lab),
      # Create 5-year bins
      year_group = cut(
        year,
        breaks = c(seq(min(year), max(year) - 1, by = violin_plot_bin), max(year)),  # dynamic + final bin
        include.lowest = TRUE,
        right = TRUE,
        dig.lab = 4
      )
    )
  
  
  # Get axis limits if same_axis = TRUE
  y_limits <- if (same_axis) {
    range(df_long[[variable_name]], na.rm = TRUE)
  } else {
    NULL
  }
  
  # Compute stats: number of points and area per state
  state_stats <- df_final %>%
    group_by(state_name) %>%
    summarise(
      n_points = n(),
      total_area = sum(area_ha, na.rm = TRUE),
      .groups = "drop"
    )
  
  states <- sort(unique(df_long$state_name))
  plot_list <- list()
  
  for (i in 1:length(states)) {
    state <- states[i]
    state_data <- df_long %>% filter(state_name == state)
    
    stats <- state_stats %>% filter(state_name == state)
    n_pts <- stats$n_points
    area_sum <- stats$total_area
    
    p <- ggplot(state_data, aes(x = year_group, y = .data[[variable_name]], fill = Model)) +
      geom_violin(position = position_dodge(width = 0.8), trim = FALSE, alpha = 0.6) +
      labs(
        title = paste("Violin Plot of", variable_name, "by", violin_plot_bin, "-Year Bins and Model:", state),
        subtitle = paste("Data points:", n_pts, "| Total area (ha):", round(area_sum, 1)),
        x = paste("Year Group (",violin_plot_bin, "-year bins)"), 
        y = paste(variable_name, unit), 
        fill = "Model"
      ) +
      theme_bw() +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) + 
      (if (!is.null(y_limits)) coord_cartesian(ylim = y_limits) else NULL)
    
    plot_list[[i]] <- p
  }
  
  return(plot_list)
}














# violin_plot_function_national: Create a nationwide aggregated violin plot for a given variable
#
# Objective:
# This function generates a violin plot showing the distribution of a selected variable 
# (from two model predictions) across the entire nation. The data are grouped into 
# user-specified year bins (e.g., 5-year periods), with plot annotations for total data 
# points and area.
#
# Input:
# - df_final: Merged dataframe containing predictions from two models along with year, state_name, and area_ha.
# - variable_name: Name of the variable to be visualized (e.g., "SOMSC").
# - unit: Unit of the variable (e.g., "Mg/ha").
# - df1_lab, df2_lab: Labels for the two models/datasets used in the legend.
# - violin_plot_bin: Width of the time bin (in years) for grouping years (e.g., 5 for 5-year bins).
# - same_axis: Logical; if TRUE, the y-axis range is fixed to match across states/national plots.
#
# Output:
# - A list containing a single `ggplot` object (for compatibility with plotting workflows).
#   The plot shows national-level violin distributions grouped by year bins and model.
#' @noRd
#' 
violin_plot_function_national <- function(df_final, variable_name, unit, df1_lab, df2_lab, violin_plot_bin, same_axis) {
  
  # Pivot longer
  df_long <- df_final %>%
    select(year, state_name, area_ha,
           value_df1 = !!sym(paste0(variable_name, ".x")),
           value_df2 = !!sym(paste0(variable_name, ".y"))) %>%
    pivot_longer(cols = starts_with("value_df"), names_to = "Model", values_to = variable_name) %>%
    mutate(
      Model = recode(Model,
                     "value_df1" = df1_lab,
                     "value_df2" = df2_lab),
      # Create 5-year bins
      year_group = cut(
        year,
        breaks = c(seq(min(year), max(year) - 1, by = violin_plot_bin), max(year)),  # dynamic + final bin
        include.lowest = TRUE,
        right = TRUE,
        dig.lab = 4
      )
    )
  
  # Get axis limits if same_axis = TRUE
  y_limits <- if (same_axis) {
    range(df_long[[variable_name]], na.rm = TRUE)
  } else {
    NULL
  }
  
  # Compute stats: number of points and area in nation
  national_stats <- df_final %>%
    summarise(
      n_pts = n(),
      total_area = sum(area_ha, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Plot nationwide data
  p <- ggplot(df_long, aes(x = year_group, y = .data[[variable_name]], fill = Model)) +
    geom_violin(position = position_dodge(width = 0.8), trim = FALSE, alpha = 0.6) +
    labs(
      title = paste("Nationwide Violin Plot of", variable_name, "by", violin_plot_bin, "-Year Bins and Model"),
      subtitle = paste("Data points:", national_stats$n_pts, "| Total area (ha):", round(national_stats$total_area, 1)),
      x = paste("Year Group (",violin_plot_bin, "-year bins)"), 
      y = paste(variable_name, unit), 
      fill = "Model"
    ) +
    theme_bw() +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) + 
    (if (!is.null(y_limits)) coord_cartesian(ylim = y_limits) else NULL)
  
  return(list(p))  # Return as list for compatibility
}















# scatter_plot_function: Function to create a scatter plot comparing two variables from two datasets
#
# Objective: 
# This function creates a scatter plot to compare the predicted values of a given variable between two datasets,
# and calculates the Pearson correlation coefficient between the two variables.
#
# Input: 
# - df_scatter_plot: A data frame containing the two variables to be compared.
#   - variable_df1: Predicted values of the variable from the first dataset (e.g., DayCent_491).
#   - variable_df2: Predicted values of the variable from the second dataset (e.g., DayCent_513).
# - variable_name: A string representing the name of the variable.
# - unit: A string representing the unit of the variable.
#
# Output: 
# - A ggplot object representing the scatter plot comparing the two datasets' predicted values.
#'@noRd




scatter_plot_function <- function(df_scatter_plot, variable_name, unit, df1_lab, df2_lab) {
  
  num_points <- nrow(df_scatter_plot)
  total_area_ha <- sum(df_scatter_plot$area_ha, na.rm = TRUE)
  
  cor_value <- cor(df_scatter_plot$variable_df1, df_scatter_plot$variable_df2, 
                   use = "complete.obs", method = "pearson")
  
  ggplot(df_scatter_plot, aes(x = variable_df1, y = variable_df2)) +
    geom_point(size = 1) +  # Scatter plot points
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +  # y = x line
    labs(x = paste("Predicted Annual \n", variable_name, unit, "_", df1_lab), 
         y = paste("Predicted Annual \n", variable_name, unit, "_", df2_lab) 
    ) +
    # annotate("text", 
    #          x = min(df_scatter_plot$variable_df1, na.rm = TRUE), 
    #          y = max(df_scatter_plot$variable_df2, na.rm = TRUE), 
    #          label = paste("Correlation:", round(cor_value, 3)), 
    #          hjust = 0, vjust = 1, size = 5, color = "red") +
    # theme_bw(base_size = 10)  # Apply a clean theme
    
    annotate("text", 
             x = min(df_scatter_plot$variable_df1, na.rm = TRUE), 
             y = max(df_scatter_plot$variable_df2, na.rm = TRUE), 
             label = paste0("Correlation: ", round(cor_value, 3), 
                            "\nN = ", num_points, 
                            "\nTotal_area = ", round(total_area_ha, 0), "(ha)"), 
             hjust = 0, vjust = 1, size = 5, color = "red") +
    theme_bw(base_size = 10)  # Apply a clean theme
}








# print_violin_line_plots_per_page: Function to print multiple line plots on each page
#
# Objective: 
# This function arranges and displays a list of line plots across multiple pages, showing a specified number of plots per page.
#
# Input: 
# - plot_list: A list containing ggplot objects (line plots) that you want to display.
# - plots_per_page: An integer specifying how many plots should be shown on each page.
#
# Output: 
# - The function will print the plots in pages with the specified number of plots per page.
#'@noRd


print_violin_line_plots_per_page <- function(plot_list,  plots_per_page) {
  
  plot_list <- unlist(plot_list, recursive = FALSE)
  num_pages <- ceiling(length(plot_list) / plots_per_page)
  
  for (i in seq_len(num_pages)) {
    start_idx <- (i - 1) * plots_per_page + 1
    end_idx <- min(i * plots_per_page, length(plot_list))
    
    grid.arrange(grobs = plot_list[start_idx:end_idx])
  }
}











# print_save_scatter_plots_per_page: Function to print and save multiple scatter plots across pages
#
# Objective:
# This function arranges and saves a list of scatter plots across multiple pages, showing a specified number of plots per page.
# Each page is saved as a PNG file in the specified output directory.
#
# Input:
# - plot_list: A list containing ggplot scatter plot objects to be saved.
# - plots_per_page: An integer specifying how many plots should be shown on each page.
# - output_dir: The directory where the plots will be saved as PNG files.
#
# Output:
# - The function saves PNG files of scatter plot pages in the specified directory.
#'@noRd


print_save_scatter_plots_per_page <- function(plot_list, plots_per_page, output_dir, scatter_plot_file_name) {
  
  num_pages <- ceiling(length(plot_list) / plots_per_page)
  
  for (i in seq_len(num_pages)) {
    start_idx <- (i - 1) * plots_per_page + 1
    end_idx <- min(i * plots_per_page, length(plot_list))
    
    # Arrange multiple plots
    combined_plot <- grid.arrange(grobs = plot_list[start_idx:end_idx], ncol = 2)
    
    # Define full file path
    file_name <- file.path(output_dir, paste0(scatter_plot_file_name, "_", i, ".png"))
    
    # Save each page as a PNG with lower quality
    ggsave(file_name, combined_plot, width = 8.27, height = 9.69, dpi = 100) # Adjust width, height, and dpi for lower quality
  }
}











# plots_pdf: Function to generate and save time series or violin plots in a PDF
#
# Objective:
# This function creates either line or violin plots for a list of variables from two datasets, 
# at either the state or national level, and arranges the plots across multiple pages in a single PDF file.
# The function handles both raw and difference variables (e.g., SOMSC_diff), merges with metadata,
# applies unit conversions, and uses appropriate plotting functions based on user input.
#
# Input:
# - df1, df2: Data frames containing prediction data with at least 'recordid2017' and 'year' columns.
# - nri_all_info: Data frame containing metadata, including 'recordid2017', 'year', 'state_name', and 'area_ha'.
# - variables: A character vector of variable names to plot (e.g., "SOMSC", "SOMSC_diff").
# - variable_units: A character vector of units corresponding to the variables.
# - no_of_plots_per_page: Integer indicating the number of plots to include on each page of the PDF.
# - output_dir: Directory path where the final PDF will be saved.
# - df1_lab, df2_lab: Labels for df1 and df2 to use in plot legends.
# - same_axis: Logical value indicating whether both data sources should be plotted with the same y-axis scale.
# - line_plot_file_name_state: Filename (without extension) for state-level line plot PDF. Only used if applicable.
# - line_plot_file_name_national: Filename for national-level line plot PDF.
# - violin_plot_file_name_state: Filename for state-level violin plot PDF.
# - violin_plot_file_name_national: Filename for national-level violin plot PDF.
# - level: Either "state" or "national", indicating the geographic aggregation for the plots.
# - plot_type: Either "line" or "violin", indicating the type of plot to generate.
# - violin_plot_bin: Logical (or bin size) used to control binning behavior for violin plots (optional).
#
# Output:
# - A multi-page PDF saved in the specified directory containing the requested plots for all variables.
#' @noRd







plots_pdf <- function(df1, df2, nri_all_info, variables, variable_units, 
                      no_of_plots_per_page, output_dir, df1_lab, df2_lab, same_axis, 
                      line_plot_file_name_state = FALSE, line_plot_file_name_national = FALSE,       
                      violin_plot_file_name_national = FALSE, violin_plot_file_name_state = FALSE,  
                      level, plot_type, violin_plot_bin = FALSE) {
  
  # Create an empty list for plots
  plot_list <- list()
  
  # Loop through the variables
  for (i in seq_along(variables)) {
    
    variable_name <- variables[i]
    unit <- variable_units[i]
    
    if (!all(c("recordid2017", "year") %in% colnames(df1)) ||
        !all(c("recordid2017", "year") %in% colnames(df2))) {
      stop("Error: Both 'df1' and 'df2' must contain 'recordid2017' and 'year' columns.")
    }
    
    # Prepare filtered data
    filtered_df1 <- df1
    filtered_df2 <- df2
    
    if (grepl("_diff$", variable_name)) {
      base_variable <- str_remove(variable_name, "_diff$")
      
      if (!(base_variable %in% colnames(df1)) || !(base_variable %in% colnames(df2))) {
        stop(paste("Error: base variable", base_variable, "is not found in both df1 and df2."))
      }
      
      filtered_df1 <- df1 %>%
        select(recordid2017, year, all_of(base_variable)) %>%
        mutate(across(all_of(base_variable), as.numeric)) %>%
        arrange(recordid2017, year) %>%
        group_by(recordid2017) %>%
        mutate(!!variable_name := !!sym(base_variable) - lag(!!sym(base_variable))) %>%
        ungroup() %>%
        filter(!is.na(!!sym(variable_name))) %>%
        select(recordid2017, year, all_of(variable_name))
      
      filtered_df2 <- df2 %>%
        select(recordid2017, year, all_of(base_variable)) %>%
        mutate(across(all_of(base_variable), as.numeric)) %>%
        arrange(recordid2017, year) %>%
        group_by(recordid2017) %>%
        mutate(!!variable_name := !!sym(base_variable) - lag(!!sym(base_variable))) %>%
        ungroup() %>%
        filter(!is.na(!!sym(variable_name))) %>%
        select(recordid2017, year, all_of(variable_name))
      
    } else {
      if (!(variable_name %in% colnames(df1)) || !(variable_name %in% colnames(df2))) {
        stop(paste("Error: Variable", variable_name, "is not found in df1 or df2."))
      }
      
      filtered_df1 <- df1 %>% select(recordid2017, year, all_of(variable_name))
      filtered_df2 <- df2 %>% select(recordid2017, year, all_of(variable_name))
    }
    
    if (!all(c("recordid2017", "year", "state_name") %in% colnames(nri_all_info))) {
      stop("Error: 'nri_all_info' must contain 'recordid2017', 'year', and 'state_name' columns.")
    }
    
    # Merge and join
    df_merged <- merge(filtered_df1, filtered_df2, by = c("recordid2017", "year"), all = TRUE)
    df_final <- left_join(df_merged, nri_all_info, by = c("recordid2017", "year"))
    
    if (!"area_ha" %in% colnames(df_final)) {
      stop("Error: 'area_ha' column is missing in the final dataframe.")
    }
    
    # Unit conversion
    df_final[[paste0(variable_name, ".x")]] <- df_final[[paste0(variable_name, ".x")]] * 0.01
    df_final[[paste0(variable_name, ".y")]] <- df_final[[paste0(variable_name, ".y")]] * 0.01
    
    # Plot selection logic
    if (level == "state" && plot_type == "line") {
      plot <- line_plot_function_state(df_final, variable_name, unit, df1_lab, df2_lab, same_axis)
      plot_pdf_name <- line_plot_file_name_state
    } else if (level == "national" && plot_type == "line") {
      plot <- line_plot_function_national(df_final, variable_name, unit, df1_lab, df2_lab, same_axis)
      plot_pdf_name <- line_plot_file_name_national
    } else if (level == "state" && plot_type == "violin") {
      plot <- violin_plot_function_state(df_final, variable_name, unit, df1_lab, df2_lab, violin_plot_bin = violin_plot_bin, same_axis)
      plot_pdf_name <- violin_plot_file_name_state
    } else if (level == "national" && plot_type == "violin") {
      plot <- violin_plot_function_national(df_final, variable_name, unit, df1_lab, df2_lab, violin_plot_bin = violin_plot_bin, same_axis)
      plot_pdf_name <- violin_plot_file_name_national
    } else {
      stop("Invalid combination of 'level' and 'plot_type'")
    }
    
    plot_list[[i]] <- plot
  }
  
  # Save plots to PDF
  pdf_path <- file.path(output_dir, paste0(plot_pdf_name, ".pdf"))
  pdf(pdf_path, width = 14, height = 14)
  print_violin_line_plots_per_page(plot_list, no_of_plots_per_page)
  dev.off()
}










# Function to create and save scatter plots

# Input:
# - df1: A data frame containing the first set of data (e.g., model 1 predictions).
# - df2: A data frame containing the second set of data (e.g., model 2 predictions).
# variables: A vector of variable names to create scatter plots for.
# variable_units: A vector of corresponding units for each variable
# no_of_plots_per_page: The number of plots to display per page in the output image
# output_dir: The directory where the resulting scatter plots should be saved

# Output:
# Saves scatter plots as images in the specified output directory
#'@noRd


create_scatter_plots_image <- function(df1, df2, variables, variable_units, no_of_plots_per_page, output_dir, df1_lab, df2_lab, scatter_plot_file_name) {
  
  # Create an empty list for line plots
  scatter_plot_list <- list() 
  
  # Loop through the variables
  for (i in 1:length(variables)) {
    
    # Check if the input data frames have the necessary columns
    if (!all(c("recordid2017", "year") %in% colnames(df1))) {
      stop("Error: 'df1' must contain 'recordid2017' and 'year' columns.")
    }
    if (!all(c("recordid2017", "year") %in% colnames(df2))) {
      stop("Error: 'df2' must contain 'recordid2017' and 'year' columns.")
    }
    
    variable_name <- variables[i]
    unit <- variable_units[i]
    
    # Ensure filtered_df1 and filtered_df2 are initialized before any condition
    filtered_df1 <- df1
    filtered_df2 <- df2
    
    # Handle cases where the variable name ends with "_diff" (e.g., somsc_diff)
    if (grepl("_diff$", variable_name)) {
      base_variable <- str_remove(variable_name, "_diff$")
      
      # Check if base_variable exists in all dataframes
      if (!base_variable %in% colnames(df1)) {
        stop(paste("Error: base variable", base_variable, "is not found in df1."))
      }
      if (!base_variable %in% colnames(df2)) {
        stop(paste("Error: base variable", base_variable, "is not found in df2."))
      }
      
      # Process df1
      filtered_df1 <- df1 %>%
        select(recordid2017, year, all_of(base_variable)) %>% # Select relevant columns
        mutate(across(all_of(base_variable), as.numeric)) %>% # Ensure base_variable is numeric
        arrange(recordid2017, year) %>% # Ensure sorting
        group_by(recordid2017) %>% # Group by recordid2017
        mutate(!!variable_name := !!sym(base_variable) - lag(!!sym(base_variable))) %>% # Compute lag difference
        ungroup() %>%
        filter(!is.na(!!sym(variable_name))) %>% # Remove rows where somsc_diff is NA (year = 1979)
        select(-all_of(base_variable)) # Drop the original base_variable column
      
      # Process df2 similarly
      filtered_df2 <- df2 %>%
        select(recordid2017, year, all_of(base_variable)) %>% # Select relevant columns
        mutate(across(all_of(base_variable), as.numeric)) %>% # Ensure base_variable is numeric
        arrange(recordid2017, year) %>% # Ensure sorting
        group_by(recordid2017) %>% # Group by recordid2017
        mutate(!!variable_name := !!sym(base_variable) - lag(!!sym(base_variable))) %>% # Compute lag difference
        ungroup() %>%
        filter(!is.na(!!sym(variable_name))) %>% # Remove rows where somsc_diff is NA (year = 1979)
        select(-all_of(base_variable)) # Drop the original base_variable column
      
    } else {
      # If the variable does not have "_diff", select it directly from all dataframes
      if (!variable_name %in% colnames(filtered_df1)) {
        stop(paste("Error: Variable", variable_name, "is not found in df1."))
      }
      if (!variable_name %in% colnames(filtered_df2)) {
        stop(paste("Error: Variable", variable_name, "is not found in df2."))
      }
      
      filtered_df1 <- filtered_df1 %>% select(recordid2017, year, all_of(variable_name))
      filtered_df2 <- filtered_df2 %>% select(recordid2017, year, all_of(variable_name))
    }
    
    
    # Check if nri_all_info has the necessary columns for merging
    if (!all(c("recordid2017", "year", "state_name") %in% colnames(nri_all_info))) {
      stop("Error: 'nri_all_info' must contain 'recordid2017' and 'year' columns for merging.")
    }
    
    # Merge datasets by 'recordid2017' and 'year'
    #df_final <- merge(filtered_df1, filtered_df2, by = c("recordid2017", "year"), all = TRUE)
    
    df_merged <- merge(filtered_df1, filtered_df2, by = c("recordid2017", "year"), all = TRUE)
    df_final <- left_join(df_merged, nri_all_info, by = c("recordid2017", "year"))
    
    # Convert units from g/m² to ton/ha (1 g/m² = 10 kg/ha = 0.01 t/ha)
    df_final[[paste0(variable_name, ".x")]] <- df_final[[paste0(variable_name, ".x")]] * 0.01
    df_final[[paste0(variable_name, ".y")]] <- df_final[[paste0(variable_name, ".y")]] * 0.01
    
    # Create a combined dataset for scatter plot
    # df_scatter_plot <- data.frame(
    #   variable_df1 = df_final[[paste0(variable_name, ".x")]],
    #   variable_df2 = df_final[[paste0(variable_name, ".y")]]
    # )
    # 
    df_scatter_plot <- df_final %>% mutate(
      variable_df1 = df_final[[paste0(variable_name, ".x")]],
      variable_df2 = df_final[[paste0(variable_name, ".y")]]
    )
    
    plot_scatter <- scatter_plot_function(df_scatter_plot, variable_name, unit, df1_lab, df2_lab)
    scatter_plot_list[[i]] = plot_scatter
    
  }
  
  #saving scatter plots
  print_save_scatter_plots_per_page(scatter_plot_list, no_of_plots_per_page, output_dir, scatter_plot_file_name)
}









#Comparison Plots for Quality Control (QC)
# 
# Generates comparison plots based on specified variables and plot types.
# (please make sure to install ggplot2, dplyr, gridExtra install in your computer)
#'
# @param df1 First data frame (e.g., model 1 predictions).
# @param df2 Second data frame (e.g., model 2 predictions).
# @param nri_all_info Data frame containing additional information such as state_name, recordid2017, and year. Ensure these columns are present in the dataset.
# @param variables A vector of variable names for which plots should be created. To generate lag difference plots, append "_diff" to the variable name (e.g., "somc_diff").
# @param variable_units A vector of corresponding units (e.g., "(ton/ha)", "(cm)") for each variable. Note: g/m² has already been converted to ton/ha. Include brackets in units.
# @param plot_types A vector specifying the plot type for each variable ("line", "scatter", or "all").
# @param no_of_plots_per_page Maximum number of plots to display per page.
# @param output_dir Directory where output plots will be saved.
# 
# @return The function generates and saves line plots as a PDF and scatter plots as image files.
#





create_QC_comparison_plots <- function(df1, df2, nri_all_info, variables, variable_units, plot_types, 
                                       no_of_plots_per_page, output_dir, df1_lab, df2_lab, same_axis, 
                                       line_plot_file_name_state = FALSE, line_plot_file_name_national = FALSE, 
                                       scatter_plot_file_name = FALSE, violin_plot_file_name_state = FALSE, 
                                       violin_plot_file_name_national = FALSE, violin_plot_bin = FALSE) {
  
  # Initialize lists to hold the variables and units grouped by plot type
  line_vars_state <- list()
  line_vars_national <- list()
  violin_vars_state <- list()
  violin_vars_national <- list()
  scatter_vars <- list()
  all_vars <- list()
  
  for (i in 1:length(variables)) {
    variable_name <- variables[i]
    unit <- variable_units[i]
    plot_type <- plot_types[i]
    
    # Check if plot_type is valid (not NA or missing)
    if (is.na(plot_type) || !(plot_type %in% c("line_state", "line_national", "violin_state", "violin_national", "scatter", "all"))) {
      warning(paste("Invalid or missing plot type for variable:", variable_name))
      next  # Skip the current iteration if plot_type is invalid
    }
    
    # Group by plot type
    if (plot_type == "line_state") {
      line_vars_state <- append(line_vars_state, list(list(variable_name = variable_name, unit = unit)))
    } else if (plot_type == "line_national") {
      line_vars_national <- append(line_vars_national, list(list(variable_name = variable_name, unit = unit)))
    } else if (plot_type == "violin_state") {
      violin_vars_state <- append(violin_vars_state, list(list(variable_name = variable_name, unit = unit)))
    } else if (plot_type == "violin_national") {
      violin_vars_national <- append(violin_vars_national, list(list(variable_name = variable_name, unit = unit)))
    } else if (plot_type == "scatter") {
      scatter_vars <- append(scatter_vars, list(list(variable_name = variable_name, unit = unit)))
    } else if (plot_type == "all") {
      all_vars <- append(all_vars, list(list(variable_name = variable_name, unit = unit)))
    }
  }
  
  
  #combining all variables list with scatter and line plot list
  line_vars_state_merged <- c(line_vars_state, all_vars)
  line_vars_national_merged <- c(line_vars_national, all_vars)
  violin_vars_state_merged <- c(violin_vars_state, all_vars)
  violin_vars_national_merged <- c(violin_vars_national, all_vars)
  scatter_vars_merged <- c(scatter_vars, all_vars)
  
  # Generate statewise line plots for grouped variables
  if (length(line_vars_state_merged) > 0) {
    variables <- sapply(line_vars_state_merged, function(x) x$variable_name)
    units <- sapply(line_vars_state_merged, function(x) x$unit)
    #create_line_
    plots_pdf(df1, df2, nri_all_info, variables, units, 
              no_of_plots_per_page, output_dir, df1_lab, df2_lab, same_axis, 
              line_plot_file_name_state = line_plot_file_name_state, level = "state", plot_type = "line")
  }
  
  
  # Generate national line plots for grouped variables
  if (length(line_vars_national_merged) > 0) {
    variables <- sapply(line_vars_national_merged, function(x) x$variable_name)
    units <- sapply(line_vars_national_merged, function(x) x$unit)
    plots_pdf(df1, df2, nri_all_info, variables, units, 
              no_of_plots_per_page, output_dir, df1_lab, df2_lab, same_axis, 
              line_plot_file_name_national = line_plot_file_name_national, level = "national", plot_type = "line")
  }
  
  
  # Generate statewise violin plots for grouped variables
  if (length(violin_vars_state_merged) > 0) {
    variables <- sapply(violin_vars_state_merged, function(x) x$variable_name)
    units <- sapply(violin_vars_state_merged, function(x) x$unit)
    plots_pdf(df1, df2, nri_all_info, variables, units, 
              no_of_plots_per_page, output_dir, df1_lab, df2_lab, same_axis, 
              violin_plot_file_name_state = violin_plot_file_name_state, level = "state", plot_type = "violin",
              violin_plot_bin = violin_plot_bin)
  }
  
  
  # Generate national violin plots for grouped variables
  if (length(violin_vars_national_merged) > 0) {
    variables <- sapply(violin_vars_national_merged, function(x) x$variable_name)
    units <- sapply(violin_vars_national_merged, function(x) x$unit)
    plots_pdf(df1, df2, nri_all_info, variables, units, 
              no_of_plots_per_page, output_dir, df1_lab, df2_lab, same_axis, 
              violin_plot_file_name_national = violin_plot_file_name_national, level = "national", plot_type = "violin",
              violin_plot_bin = violin_plot_bin)
  }
  
  # Generate scatter plots for grouped variables
  if (length(scatter_vars_merged) > 0) {
    scatter_variables <- sapply( scatter_vars_merged, function(x) x$variable_name)
    scatter_units <- sapply( scatter_vars_merged, function(x) x$unit)
    create_scatter_plots_image(df1, df2, scatter_variables, scatter_units, 
                               no_of_plots_per_page, output_dir, df1_lab, df2_lab, scatter_plot_file_name)
  }
  
  
}



