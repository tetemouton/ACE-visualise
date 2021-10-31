library(RODBC)

  dat.pth <- "C:/Catch_Prediction/Data/"


  channellog <- odbcDriverConnect("DSN=Visual FoxPro Database;UID=;PWD=;
                                  SourceDB=\\\\corp.spc.int\\shared\\FAME\\NC_NOU\\OFP\\db1\\tuna_dbs\\Log_dbs\\DBF\\logsheet.dbc;
                                  SourceType=DBC;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=Yes;Deleted=Yes;")


  ace.ez <- sqlQuery(channellog, query = "SELECT * FROM a_yb_ez WHERE gr_id = 'L' AND yy > 1999", max = 0, stringsAsFactors = FALSE)
  # ace.ez <- sqlQuery(channellog, query = "SELECT yy, gr_id, ez_id, flag_id, in_arch, sum(alb_c), sum(bet_c), sum(yft_c) FROM a_yb_ez
  #                                         WHERE gr_id = 'L' AND yy > 1999 GROUP BY yy, gr_id, ez_id, flag_id, in_arch",
  #                    max = 0, stringsAsFactors = FALSE)

  write.csv(ace.ez, file = paste0(dat.pth, "ACE_EEZ.csv"), row.names = FALSE)



  vms.ez <- sqlQuery(channellog, query = "SELECT *, year(vms_date) AS year, month(vms_date) AS month, day(vms_date) AS day
                     FROM l_vms_days WHERE year(vms_date) > 2009", max = 0, stringsAsFactors = FALSE)

  save(vms.ez, file = paste0(dat.pth, "VMS_EEZ.RData"))


  op.ez <- sqlQuery(channellog, query = "SELECT *, year(logdate) AS year, month(logdate) AS month, day(logdate) AS day,
                    INT(IIF(RIGHT(lon,1)='W',360-(VAL(LEFT(lon,3))+(VAL(SUBSTR(lon,4,2))/60)),VAL(LEFT(lon,3))+(VAL(SUBSTR(lon,4,2))/60)))+0.5 as lond,
                    IIF(RIGHT(lat,1)='S',-1,1)*(VAL(LEFT(lat,2))+0.5) as latd
                    FROM l_set WHERE not dup_r AND year(logdate) <= 2020 AND year(logdate) >= 2010", max = 0)


  
  save(op.ez, file = paste0(dat.pth, "OPLL_EEZ.RData"))


  tr.ez <- sqlQuery(channellog, query = "SELECT * FROM l_trip", max = 0)

  save(tr.ez, file = paste0(dat.pth, "OPLL_EEZ_TRIPS.RData"))









  
  
  
  
  
  
  
  