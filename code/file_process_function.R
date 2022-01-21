# Script to read in and process LPS files
# Files output according to "HBuckland.prf" on Beckman Coulter LPS
# Hannah Buckland
# 21/01/2022

library(tidyverse)
library(data.table)

LPS_file <- "data/20220120/CITEST_CI_Rom_12.txt"

LPS_data <- read.table(file=LPS_file,
                       sep="\t",
                       skip=95,
                       fill=TRUE)
