# Run using 32bit R and ensure all appropriate drivers are installed
library(RODBC)

  dat.pth <- "C:/GitHub/ACE-visualise/Data/"


  channellog <- odbcDriverConnect("DSN=Visual FoxPro Database;UID=;PWD=;
                                  SourceDB=\\\\corp.spc.int\\shared\\FAME\\NC_NOU\\OFP\\db1\\tuna_dbs\\Log_dbs\\DBF\\logsheet.dbc;
                                  SourceType=DBC;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=Yes;Deleted=Yes;")


  ace.ez <- sqlQuery(channellog, query = "SELECT * FROM a_yb_ez WHERE gr_id = 'L' AND yy > 1999", max = 0, stringsAsFactors = FALSE)

  write.csv(ace.ez, file = paste0(dat.pth, "ACE_EEZ.csv"), row.names = FALSE)























