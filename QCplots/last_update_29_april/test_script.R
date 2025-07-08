#load library
library(DBI) #this library used for pulling data from datbase
library(RMySQL) #this library used for pulling data from datbase
library(tidyverse)
library(gridExtra) #these library needs to be loaded for running our package


#reading input dataset
my_cnf_file <- "C:/Users/C837393425/.my.cnf" #add your cnf
trillium    <- "trillium.nrel.colostate.edu"
dbName      <- "inv2022_output"
tblName1    <- "INV2022_OUT_NRI_Yearly_Iter1_rev491_COMP_FEB2024"
tblName2    <- "INV2022_OUT_NRI_Yearly_Iter1_rev513_COMP_FEB2024"

db_conn <- dbConnect(RMySQL::MySQL(), 
                     host = trillium, 
                     dbname = dbName, 
                     default.file = my_cnf_file)

#Reading Daycent 491 outputs
query_491 <- paste("SELECT * FROM", tblName1)  # Adjust query if needed
df_491 <- dbGetQuery(db_conn, query_491)

#Readuing Daycent 513  outputs
query_513 <- paste("SELECT * FROM", tblName2)  # Adjust query if needed
df_513 <- dbGetQuery(db_conn, query_513)
 
#site info reading 
dbName_nri      <- "nri2017"
db_conn_nri <- dbConnect(RMySQL::MySQL(), 
                         host = trillium, 
                         dbname = dbName_nri, 
                         default.file = my_cnf_file)

tblName_nri    <- "LandArea_Combined_1979_2022"
query_nri <- paste("SELECT * FROM", tblName_nri)  
nri_point_info <- dbGetQuery(db_conn_nri, query_nri)

tblName_site_info <- "INV_lookup_point_site_info"
query_site <- paste("SELECT * FROM", tblName_site_info)  
site_info <- dbGetQuery(db_conn_nri, query_site)

nri_all_info <- nri_point_info %>%
  left_join(site_info %>% select(recordid2017, state_name), by = "recordid2017")


source("create_QC_comparison_plots.R")

#Example
variables <- c("somsc", "n2oflux", "strmac1")
variable_units <- c("(ton/ha)", "(ton/ha)", "(ton/ha)")
plot_types <- c("all", "all", "all")   #you need to select plot_types "line_state", "line_national", "violin_state", "violin_national", "scatter", "all"
output_dir = "C:/Users/C837393425/Desktop/QCplots/last_update_29_april"
highest_no_of_plots_per_page = 6

df1_lab = "run1"
df2_lab = "run2"
same_axis = TRUE
line_plot_file_name_state = "state_line_plots"
line_plot_file_name_national = "national_line_plots"
violin_plot_file_name_state = "state_violin_plots"
violin_plot_file_name_national = "national_violin_plots"
scatter_plot_file_name = "scatter_plots"
violin_plot_bin = 6

create_QC_comparison_plots(df1 = df_491, 
                 df2 = df_513, 
                 nri_all_info = nri_all_info, 
                 variables = variables, 
                 variable_units = variable_units,
                 plot_types = plot_types,
                 no_of_plots_per_page = highest_no_of_plots_per_page,
                 output_dir = output_dir,
                 df1_lab = df1_lab, df2_lab = df2_lab, same_axis = same_axis, line_plot_file_name_national = line_plot_file_name_national,
                 line_plot_file_name_state = line_plot_file_name_state, scatter_plot_file_name = scatter_plot_file_name,
                 violin_plot_file_name_state = violin_plot_file_name_state, violin_plot_file_name_national = violin_plot_file_name_national,
                 violin_plot_bin = violin_plot_bin)



