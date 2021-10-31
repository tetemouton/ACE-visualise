library(ggplot2)
library(magrittr)
library(tidyverse)
library(grid)
library(lubridate)
library(data.table)


  theme_set(theme_bw())

  dirpth <- "C:/Catch_Prediction"
  
  ez.convert <- read.csv(file = paste0(dirpth, "/Data/EZ_Converter.csv"), stringsAsFactors = FALSE, header = TRUE)

  ace.ez <- read.csv(file = paste0(dirpth, "/Data/ACE_EEZ.csv"), stringsAsFactors = FALSE, header = TRUE)

  load(paste0(dirpth, "/Data/OPLL_EEZ.RData"), verbose = TRUE) # This is object op.ez
  load(paste0(dirpth, "/Data/OPLL_EEZ_TRIPS.RData"), verbose = TRUE) # This is object tr.ez
  
  op.ez <- left_join(op.ez, tr.ez, by = "trip_id")
  
  load(paste0(dirpth, "/Data/VMS_EEZ.RData"), verbose = TRUE) # This is object vms.ez

  cnt = "VU"


  ls.cnt <- filter(op.ez, ez_id == cnt)
  ace.cnt <- filter(ace.ez, ez_id == cnt, between(yy, 2010, 2020))
  
  

  mon.dat <- ls.cnt %>% group_by(year, month) %>%
               summarise(Nset = length(unique(set_id)), Thook = sum(hook), ALBc = sum(alb_c), BETc = sum(bet_c), YFTc = sum(yft_c)) %>%
               group_by(year) %>% mutate(cumSET = cumsum(Nset), cumTH = cumsum(Thook), cumALB = cumsum(ALBc), cumBET = cumsum(BETc), cumYFT = cumsum(YFTc),
                                         pSET = cumsum(Nset)/sum(Nset), pTH = cumsum(Thook)/sum(Thook), pALB = cumsum(ALBc)/sum(ALBc), pBET = cumsum(BETc)/sum(BETc), pYFT = cumsum(YFTc)/sum(YFTc))

  
  ace.sum <- ace.cnt %>% group_by(year = yy) %>% summarise(Days = sum(days), ALBc = sum(alb_c), BETc = sum(bet_c), YFTc = sum(yft_c))
  ace.sum$month = runif(dim(ace.sum)[1], 12.3, 12.45)
  
  
  
  
  comb.dat <- select(mon.dat, year, month, ALBc = cumALB, BETc = cumBET, YFTc = cumYFT, Days = cumSET)

  tmp.dat <- data.frame(year = unique(comb.dat$year), month = 0, ALBc = 0, BETc = 0, YFTc = 0, Days = 0)
  
  comb.dat <- rbind(tmp.dat, comb.dat, ace.sum)
  
  
  windows(4000,3000)
    pl <- ggplot(mon.dat, aes(x = month, y = cumSET, colour = factor(year))) + geom_line(size = 1.5) + geom_point(size = 1)
    print(pl)
  
  windows(4000,3000)
    pl <- ggplot(mon.dat, aes(x = month, y = cumTH/1000000, colour = factor(year))) + geom_line(size = 1.5) + geom_point(size = 1)
    print(pl)
  
  windows(4000,3000)
    pl <- ggplot(mon.dat, aes(x = month, y = cumALB, colour = factor(year))) + geom_line(size = 1.5) + geom_point(size = 1)
    print(pl)
    
    windows(4000,3000)
    pl <- ggplot() + geom_line(data = comb.dat, aes(x = month, y = ALBc, colour = factor(year)), size = 1.3, linetype = "dotted") +
                 geom_line(data = comb.dat[comb.dat$month <= 12,], aes(x = month, y = ALBc, colour = factor(year)), size = 1.5) +
                 geom_point(data = ace.sum, aes(x = month, y = ALBc, colour = factor(year)), size = 4) +
                 scale_colour_brewer(palette = "Paired") + scale_x_continuous(breaks = seq(1,12,1), labels = month.abb[1:12]) +
                 theme(legend.position = c(0.1,0.8), legend.title = element_blank())
    print(pl)
    
   windows(4000,3000)
    pl <- ggplot(mon.dat, aes(x = month, y = pYFT, colour = factor(year))) + geom_line(size = 1.5) + geom_point(size = 1)
    print(pl)
  
  
  
  windows(4000,3000)
    pl <- ggplot() + geom_line(data = mon.dat, aes(x = month, y = pSET), size = 1.5, colour = alpha("blue", 0.5)) +
                 geom_line(data = mon.dat, aes(x = month, y = pYFT), size = 1.5) + facet_wrap(~ factor(year), ncol = 3)
    print(pl)
  
  
  windows(4000,3000)
    pl <- ggplot() + geom_line(data = mon.dat, aes(x = month, y = pSET, group = year), size = 0.8, colour = alpha("blue", 0.5)) +
                 geom_line(data = mon.dat, aes(x = month, y = pYFT, group = year), size = 0.8) #+ facet_wrap(~ factor(year), ncol = 3)
    print(pl)
    

  windows(4000,3000)
    pl <- ggplot() + geom_line(data = mon.dat, aes(x = month, y = pYFT - pSET, group = year), size = 0.8, colour = alpha("blue", 0.5))
    print(pl)
  
  
    
    
# Predictor from other years
    
    
    pre.dat <- mon.dat %>% mutate(rat.cumALB = cumALB/lag(cumALB))
    
    
    yr.rng <- 2010:2020


    ref.yr <- 2017
    
    mn.kp <- 5
    
    tmp.base <- pre.dat %>% filter(year == ref.yr)
    
    predict.mths = function(mn.kp = 1){
    

    
    yr.diff <- yr.rng[-(yr.rng == ref.yr)]
    
    predict.df = function(grp = 1){
    
      yr.chx <- yr.diff[grp]
    
      tmp <- data.frame(year = rep(yr.chx,13), mon = 0:12, obs.alb = NA, pre.alb = NA, grp = yr.chx, mn.kp)
    
      tmp.scalar <- pre.dat[pre.dat$year == yr.chx,]$rat.cumALB
    
      tmp$obs.alb[1:(mn.kp+1)] <- c(0,tmp.base$cumALB[1:mn.kp])
    
      tmp$pre.alb[1:(mn.kp+1)] <- tmp$obs.alb[1:(mn.kp+1)]
    
      for(i in (mn.kp+2):13) tmp$pre.alb[i] <- tmp$pre.alb[i-1]*tmp.scalar[i-1]
    
      return(tmp)
    }
    
    str.grp <- lapply(1:(length(yr.rng)-1), predict.df)
    str.grp.rb <- rbindlist(str.grp)
    
    return(str.grp.rb)
    }
    
    str.mn <- lapply(1:11, predict.mths)
    str.mn.rb <- rbindlist(str.mn)
    str.mn.rb$mth <- month.abb[str.mn.rb$mn.kp]
    
    
    
    # windows(3000,2000)
    #   pl <- ggplot() + geom_line(data = tmp.base, aes(x = month, y = cumALB), colour = alpha("red", 0.5), size = 1.5) +
    #                    geom_line(data = filter(str.grp.rb, year == min(str.grp.rb$year)), aes(x = mon, y = obs.alb), size = 1.5) +
    #                    geom_line(data = str.grp.rb, aes(x = mon, y = pre.alb, group = grp), colour = grey(0.5))
    #   print(pl)
    # dev.off()
    
    
    windows(4000,3000)
      pl <- ggplot() + geom_line(data = tmp.base, aes(x = month, y = cumALB), colour = alpha("red", 0.5), size = 1.5) +
                       geom_line(data = str.mn.rb, aes(x = mon, y = pre.alb, group = grp), colour = grey(0.5)) +
                       geom_line(data = filter(str.mn.rb, year == min(str.mn.rb$year)), aes(x = mon, y = obs.alb), size = 1.2) +
                       scale_x_continuous(breaks = seq(0,12,1), labels = c("",month.abb)) +
                       facet_wrap(~ factor(mth, levels = month.abb), ncol = 3)
                       #facet_wrap(~ factor(mn.kp, levels = rev(1:11)), ncol = 3)
      print(pl)
    dev.off()
    
    
    
    
    
    
  # Comparative summaries
    
    
    
    ace.ez$ez_alt <- ez.convert$Mod[match(ace.ez$ez_id, ez.convert$All)]
    
    op.ez$ez_alt <- ez.convert$Mod[match(op.ez$ez_id, ez.convert$All)]

    vms.ez$ez_alt <- ez.convert$Mod[match(vms.ez$ez_id, ez.convert$All)]
    
    
    #all.ez <- data.frame(eez = sort(unique(c(op.ez$ez_id,ace.ez$ez_id))))
    all.ez <- data.frame(EEZ = sort(unique(c(op.ez$ez_alt,ace.ez$ez_alt))))
    
    # Remove "IW"
    
    
    full.dat <- expand(all.ez, EEZ, Year = 2010:2020)
    
  
    ez.sum <- ace.ez %>% filter(ez_id != "IW", between(yy, 2010, 2020)) %>% group_by(Year = yy, EEZ = ez_alt) %>%
                         summarise(Days = sum(days), ALBc = sum(alb_c), BETc = sum(bet_c), YFTc = sum(yft_c)) %>%
                         mutate(Sets = NA, Srce = "ACE")
    
    op.sum <- op.ez %>% filter(ez_id != "IW", between(year, 2010, 2020)) %>% group_by(Year = year, EEZ = ez_alt) %>%
                        summarise(Sets = length(unique(set_id)), ALBc = sum(alb_c), BETc = sum(bet_c), YFTc = sum(yft_c)) %>%
                        mutate(Days = NA, Srce = "LogSht")
    
    vms.sum <- vms.ez %>% filter(ez_id != "IW", ez_alt %in% all.ez$EEZ, between(year, 2010, 2020)) %>% group_by(Year = year, EEZ = ez_alt) %>%
                          summarise(Days = sum(days_vms), ALBc = NA, BETc = NA, YFTc = NA) %>%
                          mutate(Sets = NA, Srce = "VMS")
    
    ez.sum.fl <- left_join(full.dat, ez.sum, by = c("EEZ", "Year"))
    
    op.sum.fl <- left_join(full.dat, op.sum, by = c("EEZ", "Year"))
    
    vms.sum.fl <- left_join(full.dat, vms.sum, by = c("EEZ", "Year"))
    
    
    all.sum <- rbind(ez.sum.fl, op.sum.fl, vms.sum)
    
    
    

    
    tmp <- filter(all.sum, EEZ == "TV")
    
    
    
    windows(4000,3000)
    pl <- ggplot(tmp, aes(x = Year, y = Days, colour = Srce)) + geom_line(size = 1.5)
    print(pl)
    
    
    windows(4000,3000)
    pl <- ggplot() + geom_line(data = tmp, aes(x = Year, y = Days, colour = Srce), size = 1.5) +
            geom_line(data = tmp, aes(x = Year, y = Sets, colour = Srce), size = 1.5)
    print(pl)
    
    windows(4000,3000)
    pl <- ggplot(tmp, aes(x = Year, y = ALBc, colour = Srce)) + geom_line(size = 1.5)
    print(pl)

    
    windows(4000,3000)
    pl <- ggplot(tmp, aes(x = Year, y = BETc, colour = Srce)) + geom_line(size = 1.5)
    print(pl)
    
    
    windows(4000,3000)
    pl <- ggplot(tmp, aes(x = Year, y = YFTc, colour = Srce)) + geom_line(size = 1.5)
    print(pl)
    
    
    
    
    
    
    
    # Vessel summaries
    vms.ves.min <- 5
    
    vms.ves <- vms.ez %>% filter(ez_alt != "IW", !is.na(ez_alt), between(year, 2010, 2020)) %>% group_by(year, ez_alt, vessel_id) %>% summarise(Days = sum(days)) %>%
                          filter(Days >= vms.ves.min) %>% group_by(year, ez_alt) %>% summarise(Nves = n()) %>% mutate(Srce = "VMS")
    

    windows(4000,2500)
    pl <- ggplot(filter(vms.ves, year == 2020), aes(x = ez_alt, y = Nves)) + geom_bar(stat = "identity")
    print(pl)
    dev.off()
    

    op.ves.min <- 5
    
    op.ves <- op.ez %>% filter(ez_alt != "IW", !is.na(ez_alt), between(year, 2010, 2020)) %>% group_by(year, ez_alt, boat_id) %>% summarise(Sets = n()) %>% filter(Sets >= op.ves.min) %>%
                        group_by(year, ez_alt) %>% summarise(Nves = n()) %>% mutate(Srce = "LogSht")
    
    
    full.ves <- rbind(vms.ves, op.ves)
    

    windows(5000,5000)
      pl <- ggplot(filter(full.ves, year >= 2018), aes(x = ez_alt, y = Nves, fill = Srce)) + geom_bar(stat = "identity", position = "dodge") +
                   facet_wrap(~ year, ncol = 1) +
                   theme(legend.position = "top")
      print(pl)
    dev.off()

    windows(5000,5000)
      pl <- ggplot(filter(full.ves, year >= 2018, ez_alt != "HS"), aes(x = ez_alt, y = Nves, fill = Srce)) + geom_bar(stat = "identity", position = "dodge") +
                   facet_wrap(~ year, ncol = 1) +
                   theme(legend.position = "top")
      print(pl)
    dev.off()
    
    
    
    
    
    

    
    
    
    
    
                                