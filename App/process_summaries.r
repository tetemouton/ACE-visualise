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

  low.yr <- 2015
  high.yr <- 2019

  foc.spp <- "ALB"
  
  dat.sum <- ace.ez %>% filter(EEZ %in% kp.ez, between(Year, low.yr, high.yr)) %>% mutate(ALBday = ALBc/Days, BETday = BETc/Days, YFTday = YFTc/Days) %>%
                        group_by(EEZ, EZnm) %>% summarise(avDays = mean(Days), avALB = mean(ALBc), avBET = mean(BETc), avYFT = mean(YFTc),
                                                          avALBDay = mean(ALBday), avBETDay = mean(BETday), avYFTDay = mean(YFTday))

  
  cat.sum <- dat.sum %>% select(EEZ, EZnm, TOTc = paste0("av", foc.spp)) %>% ungroup() %>% mutate(Pcat = TOTc/sum(TOTc))
  
  rat.sum <- dat.sum %>% select(EEZ, EZnm, MTpDay = paste0("av", foc.spp, "Day")) %>% mutate(DayMt = 1/MTpDay)











