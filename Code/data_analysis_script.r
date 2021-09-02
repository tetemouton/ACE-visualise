library(ggplot2)
library(magrittr)
library(tidyverse)
library(grid)
library(lubridate)
library(data.table)


  theme_set(theme_bw())

  dirpth <- "C:/GitHub/ACE-visualise"

  ez.convert <- read.csv(file = paste0(dirpth, "/Data/EZ_Converter.csv"), stringsAsFactors = FALSE, header = TRUE)

  ace.ez <- read.csv(file = paste0(dirpth, "/Data/ACE_EEZ.csv"), stringsAsFactors = FALSE, header = TRUE)

  ace.ez$ez_alt <- ez.convert$Mod[match(ace.ez$ez_id, ez.convert$All)]
  ace.ez$ez_nm <- ez.convert$Full[match(ace.ez$ez_id, ez.convert$All)]

  ace.tab <- ace.ez %>% group_by(Year = yy, EEZ = ez_id, EZnm = ez_nm) %>% summarise(Days = sum(days), ALBc = sum(alb_c), BETc = sum(bet_c), YFTc = sum(yft_c))

  write.csv(ace.tab, file = paste0(dirpth, "/Data/ACE_EEZ_YR.csv"), row.names = FALSE)











