library(ggplot2)
library(magrittr)
library(tidyverse)
library(grid)
library(lubridate)
library(data.table)


  theme_set(theme_bw())

  dirpth <- "C:/GitHub/ACE-visualise"

  #ez.convert <- read.csv(file = paste0(dirpth, "/Data/EZ_Converter.csv"), stringsAsFactors = FALSE, header = TRUE)

  ace.ez <- read.csv(file = paste0(dirpth, "/Data/ACE_EEZ_YR.csv"), stringsAsFactors = FALSE, header = TRUE)


  kp.ez <- c("CK","GU","PF","TV","TO","WS")

















