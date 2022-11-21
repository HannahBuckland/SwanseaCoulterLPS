# File to process IODP LPS data and save to useful format
# Hannah Buckland
# 25/02/2022

library(data.table)
library(stringr)

# Read in data processing and plotting functions
source(here::here("code","file_process_function.R"))
source(here::here("code","theme_function.R"))


day1files <- list.files(here::here("data","20220223"),
                        pattern = glob2rx("*IODP*.txt*"),
                        full.names = TRUE)

day2files <- list.files(here::here("data","20220224"),
                        pattern = glob2rx("*IODP*.txt*"),
                        full.names = TRUE)

day3files <- list.files(here::here("data","20220802"),
                        pattern = glob2rx("*STV*.txt*"),
                        full.names = TRUE)

IODP_files_Feb <- c(day1files,day2files,day3files)

IODP_names <- tstrsplit(IODP_files_Feb,"STV")[[2]]
IODP_names <- tstrsplit(IODP_names,".txt")[[1]]
IODP_names <- paste0("STV",IODP_names)

LPS_processed <- lapply(IODP_files_Feb,
                        FUN = LPSprocess)

names(LPS_processed) <- IODP_names

all_phi <- rbindlist(LPS_processed,
                     idcol = TRUE)

all_phi_process <- all_phi %>%
  mutate(STV_numb = tstrsplit(.id,"_")[[1]],
         analysis_numb = as.character(tstrsplit(.id,"_")[[2]])) 

cumulative <- ggplot(all_phi_process) +
  geom_path(aes(x=lower_um,
                y=vol_perc)) +
  scale_x_log10() +
  ylab("Cumulative Volume %") +
  xlab("Grain size (µm)") +
  facet_wrap(~STV_numb) +
  theme_LPS() 



