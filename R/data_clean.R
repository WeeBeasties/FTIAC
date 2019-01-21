library(dataMaid)
library(tidyverse)
library(readxl)

first_dataset <- read_xls("data_raw/Fall_FT_FTIAC_201008-201408.xls")
second_dataset <- read_xls("data_raw/Fall_FT_FTIAC_201508-201808.xls")
combined <- rbind(first_dataset,second_dataset)
combined <- combined %>% filter(STAT_ORIG=="MI")                # Drop foreign and out of state
combined <- filter(combined, COLLEGE != "CP")                   # Drop CP
combined$COLLEGE[combined$COLLEGE == "AH"] <- "HP"              # Allied Health now called Health Professions
combined$TERM <- as.numeric(substr(combined$TERM,1,4))		# Trim semester off of terms to just years
write_csv(combined, "data_processed/FTIAC_data.csv")               # Save the munged data

makeCodebook(combined, file="data_processed/FTIAC_data_codebook.pdf", reportTitle="FTIAC_data.csv Codebook", replace = TRUE)


