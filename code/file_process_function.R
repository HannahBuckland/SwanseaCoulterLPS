# Script to read in and process LPS files
# Files output according to "HBuckland.prf" on Beckman Coulter LPS
# Hannah Buckland
# 21/01/2022

library(tidyverse)
library(data.table)

LPS_file <- "data/20220120/CITEST_CI_Rom_12.txt"

LPS_data <- read.table(file=LPS_file,
                       sep="\t",
                       skip=66,
                       fill=TRUE)

LPS_data <- LPS_data[1:28,] %>%
  mutate_if(is.character,as.numeric)

colnames(LPS_data) <- c("lower_um",
                        "cumul.vol_lessthan",
                        "cumul.vol_greatthan",
                        "vol_perc")

LPS_phi <- LPS_data %>%
  mutate(lower_phi = round(-log((lower_um/1000),2),1))
    

ggplot(LPS_data) +
  geom_path(aes(x=lower_um,
                y=cumul.vol_lessthan)) +
  scale_x_log10()

ggplot(LPS_phi) +
  geom_col(aes(x=lower_phi,
                y=vol_perc)) +
  scale_x_reverse()
