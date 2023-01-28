# File to process IODP LPS data and save to useful format
# Hannah Buckland
# 19/01/2023

library(data.table)
library(stringr)
library(tidyverse)

# Read in data processing and plotting functions
source(here::here("code","file_process_function.R"))
source(here::here("code","theme_function.R"))


day1IODP <- list.files(here::here("data","20221109"),
                        pattern = glob2rx("*STV*.txt*"),
                        full.names = TRUE)

day1sstd <- list.files(here::here("data","20221109"),
                       pattern = glob2rx("*MazPB*.txt*"),
                       full.names = TRUE)

day2IODP <- list.files(here::here("data","20221118"),
                        pattern = glob2rx("*STV*.txt*"),
                        full.names = TRUE)

day2sstd <- list.files(here::here("data","20221118"),
                       pattern = glob2rx("*MazPB*.txt*"),
                       full.names = TRUE)

IODP_files_Nov <- c(day1IODP,day2IODP)

IODP_NovNames <- tstrsplit(IODP_files_Nov,"STV")[[2]]
IODP_NovSubsample <- tstrsplit(IODP_NovNames,"_")[[2]]

IODP_NovNames <- tstrsplit(IODP_NovNames,"-")[[1]]
IODP_NovNames <- tstrsplit(IODP_NovNames,"_")[[1]]

IODP_NovSubsample <- tstrsplit(IODP_NovSubsample,".txt")[[1]]

IODP_NovNames <- paste0("STV",IODP_NovNames,"_",IODP_NovSubsample)

LPS_processed <- lapply(IODP_files_Nov,
                        FUN = LPSprocess)

names(LPS_processed) <- IODP_NovNames

all_phi <- rbindlist(LPS_processed,
                     idcol = TRUE)

all_phi_process <- all_phi %>%
  mutate(STV_numb = tstrsplit(.id,"_")[[1]],
         analysis_numb = as.character(tstrsplit(.id,"_")[[2]])) 

average_STV <- all_phi_process %>%
  group_by(STV_numb,lower_phi) %>%
  summarise_at(vars(vol_perc), list(vol_perc = mean)) %>%
  mutate(vol_perc = round(vol_perc,2))

average_wide <- pivot_wider(average_STV,
                            id_cols=lower_phi,
                            names_from = STV_numb,
                            values_from = vol_perc) 

phiscale <- ggplot(all_phi_process) +
  geom_col(aes(x=lower_phi,
               y=vol_perc,
               fill = analysis_numb),
           colour = NA,
           alpha = 0.5,
           position = "identity") +
  geom_col(data = average_STV,
           aes(x=lower_phi,
               y=vol_perc),
           colour = "black",
           fill = NA) +
  scale_x_reverse() +
  ylab("Volume %") +
  xlab("Grainsize (phi)") +
  theme_LPS() +
  facet_wrap(~STV_numb)

phiscale
