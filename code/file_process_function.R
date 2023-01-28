# Script to read in and process LPS files
# Files output according to "HBuckland.prf" on Beckman Coulter LPS
# Hannah Buckland
# 21/01/2022

library(tidyverse)
library(data.table)
library(gridExtra)
library(ggthemes)
library(patchwork)


LPSprocess <- function(LPStxt){
  options(scipen=999) # turn of scientific notation
  
  #### Read in text files and extract the key data in different stages ####

  LPS_file <- LPStxt
  
  # Read in the file and extract the statistics calculated by the LPS
  LPS_stats <- read.table(file=LPS_file,
                          sep="\t",
                          skip=21,
                          nrows = 13,
                          fill=TRUE,
                          stringsAsFactors = FALSE)
  
  LPS_stats <- data.frame(statistic=LPS_stats$V1,value=LPS_stats$V2) # exclude remainder of file
  
  LPS_stats <- LPS_stats %>%
    mutate(statistic = str_remove(LPS_stats$statistic, "[:]")) %>% # get rid of spurious colons %>%
    mutate(value = as.numeric(value))
  
  # Set up data frame of percentiles for plotting on cumulative plot
  LPS_percentiles <- data.frame(x=as.numeric(LPS_stats$value[11:13]),
                                y=c(10,50,90),
                                label = paste(as.character(LPS_stats$statistic[11:13]),"=",LPS_stats$value[11:13]))

  # Read in volume percentages for GSD from raw files
  LPS_data <- read.table(file=LPS_file,
                         sep="\t",
                         skip=66, # 66 works for November files
                         nrows = 28,
                         fill=TRUE,
                         stringsAsFactors = FALSE)
  
  LPS_data <- data.frame(lower_um=LPS_data$V1,
                         cumul.vol_lessthan = LPS_data$V2,
                         cumul.vol_greatthan = LPS_data$V3,
                         vol_perc = LPS_data$V4)
  
  # Read in interpolated data for plotting smooth cumulative distributions
  LPS_inter <- read.table(file=LPS_file,
                          sep="\t",
                          skip=100,
                          fill=TRUE,
                          stringsAsFactors = FALSE)
  
  
  LPS_inter <- data.frame(lower_um = LPS_inter$V1,
                           middle_um = LPS_inter$V2,
                           upper_um = LPS_inter$V3,
                           vol_perc = LPS_inter$V4,
                          cumul.vol_lessthan = LPS_inter$V5,
                          cumul.vol_greatthan = LPS_inter$V6,
                          number_perc = LPS_inter$V7)
  
  # Change to numeric and push down vol_perc column (reads in as difference)
  LPS_data <- LPS_data %>%
    mutate(vol_perc=lag(vol_perc)) 
  
  # set first vol_perc as cumulative value
  LPS_data$vol_perc[1] <-   LPS_data$cumul.vol_lessthan[1]
  
  
  # Set up phi column by mutating particle diameter and rounding to nearest 0.5
  LPS_phi <- LPS_data %>%
    mutate(lower_phi = round((-log((lower_um/1000),2))/0.5)*0.5)
  
  #### Produce plots of LPS outputs ####

  # Smooth cumulative distribution with percentiles marked and annotated with stats
  cumulative <- ggplot(LPS_inter) +
    geom_path(aes(x=lower_um,
                  y=cumul.vol_lessthan)) +
    geom_point(data=LPS_percentiles,
               aes(x=x,y=y)) +
    geom_label(data=LPS_percentiles,
               aes(x=x,y=y,label=label),nudge_x = log(3,10),nudge_y=-2) +
    annotation_custom(tableGrob(LPS_stats[1:10,],
                                rows=NULL,
                                theme=ttheme_default(base_size = 10)), 
                      xmin=log(0.001,10), xmax=log(100,10), ymin=40, ymax=90) +
    scale_x_log10() +
    ylab("Cumulative Volume %") +
    xlab("Grain size (µm)") +
    theme_LPS()
  
  # Histogram of half phi sieve intervals
  phiscale <- ggplot(LPS_phi) +
    geom_col(aes(x=lower_phi,
                 y=vol_perc)) +
    scale_x_reverse() +
    ylab("Volume %") +
    xlab("Grainsize (phi)") +
    theme_LPS()
  
  sidebyside <- cumulative + phiscale
  
  #out <- list(LPS_phi=LPS_phi, LPS_inter=LPS_inter)
  out <- LPS_phi
  # From function return the stats, the phi scale data and the plots
  # return(list(stats=LPS_stats,
  #             phi_data=LPS_phi,
  #             plot=sidebyside))
  
  # When batch processing lots of data only output interpolated data
  return(out)
}

