library(shiny)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(shinyWidgets)
library(shinyMatrix)
library(magrittr)
library(grid)
library(gridExtra)
library(rnaturalearth)
library(rnaturalearthdata)
library(plotly)
library(sf)
library(rgeos)
library(data.table)


  # https://smspc-apps.shinyapps.io/ACE-visualise/


  theme_set(theme_bw())


  ace.ez <- read.csv(file = "./Data/ACE_EEZ_YR.csv", stringsAsFactors = FALSE, header = TRUE)
  ace.ez.flg <- read.csv(file = "./Data/ACE_EEZ_YR_FLG.csv", stringsAsFactors = FALSE, header = TRUE)

  low.yr <- 2015
  high.yr <- 2019

  foc.spp <- "ALB"

  latest.yr <- 2020
# 
# 
# cnt.nms <- data.frame(sht.nm = c("all.cnt","HS","AS","AU","BZ","CA","CN","CK","EC","SV","EU","FM","FJ","GU","ID","JP","KI","KR","MH","NR","NC","NZ","NU",
#                                  "MP","PW","PG","PH","PF","WS","SB","TW","TK","TO","TV","US","VN","VU","WF"),
#                       lng.nm = c("All","High Seas","American Samoa","Australia","Belize","Canada","China","Cook Islands","Ecuador","El Salvador","European Union","FSM","Fiji","Guam","Indonesia","Japan",
#                                  "Kiribati","Korea","Marshall Islands","Nauru","New Caledonia","New Zealand (Godzone)","Niue","Nthrn Mariana Is.",
#                                  "Palau","PNG","Philippines","Polynesie Francais","Samoa","Solomon Is.","Taiwan","Tokelau","Tonga","Tuvalu","USA",
#                                  "Vietnam","Vanuatu","Wallis/Futuna"))
# 
# gr.cols <- data.frame(sht.nm = c("S","L","P","T","Z"),
#                       grcols = c("royalblue2","darkseagreen2","peachpuff","tomato","lightgoldenrod2"))
# 
# # Load data and assign EEZ's to higher level countries
# acedat <- read.table(file="./Data/ACE_BY_FLAG.TXT", header = TRUE, sep = ",") %>%
#   rename(Year = yy) %>%
#   mutate(tot_mt = alb_mt + bet_mt + skj_mt + yft_mt,
#          ez_ag = ifelse(eez %in% c("AU","NF"), "AU", eez),
#          ez_ag = ifelse(ez_ag %in% c("GL","KI","LN","PX"), "KI", ez_ag),
#          ez_ag = ifelse(ez_ag %in% c("NC","MA"), "NC", ez_ag),
#          ez_ag = ifelse(ez_ag %in% c("HB","HW","JT","JV","PY","WK"), "US", ez_ag),
#          ez_ag = ifelse(ez_ag %in% c("H4","H5","I0","I1","I2","I3","I4","I5","I6","I7","I8","I9","IW"), "HS", ez_ag),
#          flag = ifelse(flag %in% c("ES","PT"), "EU", flag))



# mapdat <- read.table(file="./Data/AGGREGATE_RAISED_5X5_S-L-P-T-Z.TXT", header = TRUE, sep = ",")
# 
# world <- ne_countries(scale = "medium", returnclass = "sf")
# 
# mappl <- mapdat %>% mutate(tot_mt = alb_mt + bet_mt + skj_mt + yft_mt) %>% group_by(lond, latd) %>% summarise(Catch = sum(tot_mt))

#cnt.keep <- read.csv(file="./EEZs/Data/PacCountries.csv", header=TRUE)

# eez <- st_read("./EEZs/Data/World_EEZ_Files/World_EEZ_v10_2018_0_360.shp")
# 
# pac.eez <- eez[eez$Territory1 %in% cnt.keep$Ctry,]

#____________________________________________________________________________________________________________
# User interface

 
 # selectInput("spp", "Select the Species of interest:",
 #             c("All" = "all.cnt", "High Seas" = "HS",
 #               "American Samoa" = "AS", "Australia" = "AU", "Belize" = "BZ", "Canada" = "CA", "China" = "CN", "Cook Islands" = "CK", "Ecuador" = "EC",
 #               "El Salvador" = "SV", "European Union" = "EU", "FSM" = "FM", "Fiji" = "FJ", "Guam" = "GU", "Indonesia" = "ID",
 #               "Japan" = "JP", "Kiribati" = "KI", "Korea" = "KR", "Marshall Islands" = "MH", "Nauru" = "NR", "New Caledonia" = "NC",
 #               "New Zealand (Godzone)" = "NZ", "Niue" = "NU", "Nthrn Mariana Is." = "MP", "Palau" = "PW", "PNG" = "PG", "Philippines" = "PH",
 #               "Polynesie Francais" = "PF", "Samoa" = "WS", "Solomon Is." = "SB", "Taiwan" = "TW", "Tokelau" = "TK", "Tonga" = "TO",
 #               "Tuvalu" = "TV", "USA" = "US", "Vietnam" = "VN", "Vanuatu" = "VU", "Wallis/Futuna" = "WF")),
 
 
 
 
ui <- navbarPage(
  
  
  
  title = "ACE - Visualisation",
  tabPanel("Catch tables",
           sidebarLayout(
             sidebarPanel(width = 3,
                          
                          tags$head(tags$style(HTML("
                                 .multicol { 
                                   height: 110px;
                                   -webkit-column-count: 4; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 4;    /* Firefox */ 
                                   column-count: 4; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 }
                                 .checkbox-inline { 
                    margin-left: 0px;
                    margin-right: 10px;
          }
         .checkbox-inline+.checkbox-inline {
                    margin-left: 0px;
                    margin-right: 10px;
          }
          "))),
                          selectInput("spp", "Select the Species of interest:",
                                      c("Albacore" = "ALBc", "Bigeye" = "BETc", "Yellowfin" = "YFTc")),
                          br(),
                          
                          sliderInput("sliderrng", "Choose reference time period",  min = 1990, max = latest.yr, value = c(latest.yr - 4,latest.yr),
                                      width = "400px", ticks = TRUE, sep = ""),
                          br(),
                          
                          list(tags$div(align = 'left', 
                                        class = 'multicol', 
                                        checkboxGroupInput(inputId  = "ffa.cnts", label = "Select FFA zones:",
                                                           c("Australia " = "AU", "Cook Isl. " = "CK", "Fiji      " = "FJ", "FSM       " = "FM",
                                                             "Kiribati  " = "KI", "Marshalls " = "MH", "Nauru     " = "NR", "Niue      " = "NU", 
                                                             "New Zld   " = "NZ",  "PNG       " = "PG", "Palau     " = "PW",
                                                             "Solomons  " = "SB", "Tokelau  " = "TK", "Tonga     " = "TO",
                                                             "Tuvalu    " = "TV", "Vanuatu   " = "VU", "Samoa     " = "WS"),
                                                           selected = c("AU","CK","FJ","FM","KI","MH","NR","NU", 
                                                                        "NZ","PG","PW","SB","TK","TO","TV","VU","WS"), inline = TRUE))), 
                          br(),
                          br(),
                          list(tags$div(align = 'left', 
                                        class = 'multicol', 
                                        checkboxGroupInput(inputId  = "us.cnts", label = "Select US zones:",
                                                           c("A Samoa" = "AS","Hawaii" = "HW","Johnson" = "JT","Jarvis" = "JV","Palmyra" = "PY","Wake" = "WK"),
                                                           selected = c("AS","HW","JT","JV","PY","WK"), inline = TRUE))), 
                          br(),
                          checkboxInput("aggHS", label = "Aggregate high seas areas?", value = FALSE),
                          br(),
                          list(tags$div(align = 'left', 
                                        class = 'multicol', 
                                        checkboxGroupInput(inputId  = "hs.cnts", label = "Select high seas zones:",
                                                           c("Int. Wt 1" = "I1","Int. Wt 2" = "I2","Int. Wt 3" = "I3","Int. Wt 4" = "I4","Int. Wt 5" = "I5",
                                                             "Int. Wt 6" = "I6","Int. Wt 7" = "I7","Int. Wt 8" = "I8","Int. Wt 9" = "I9",
                                                             "HS. Zn 4" = "H4","HS. Zn 5" = "H5"),
                                                           selected = c("I1","I2","I3","I4","I5","I6","I7","I8","I9","H4","H5"), inline = TRUE))), 
                          br(),
                          list(tags$div(align = 'left', 
                                        class = 'multicol', 
                                        checkboxGroupInput(inputId  = "oth.cnts", label = "Select Other zones:",
                                                           c("China" = "CN", "Indonesia" = "ID", "Japan" = "JP", "New Cal." = "NC", "French Pol" = "PF",
                                                             "Philippines" = "PH", "Taiwan" = "TW", "Vietnam" = "VN", "Wallis/Futuna" = "WF"),
                                                           selected = c("CN","ID","JP","NC","PF","PH","TW","VN","WF"), inline = TRUE))), 
                          
             #                tags$style(
             #                  HTML('
             # .multicol { 
             # -webkit-column-count: 3; /* Chrome, Safari, Opera */ 
             # -moz-column-count: 3;    /* Firefox */ 
             # column-count: 3; 
             # -moz-column-fill: auto;
             # -column-fill: auto;
             # }
             # ')
             #               
             #              ),  
  #                         tags$style("
  #     .checkbox { /* checkbox is a div class*/
  #       line-height: 30px;
  #       margin-bottom: 40px; /*set the margin, so boxes don't overlap*/
  #     }
  #     input[type='checkbox']{ /* style for checkboxes */
  #       width: 20px; /*Desired width*/
  #       height: 20px; /*Desired height*/
  #       line-height: 20px; 
  #     }
  #     span { 
  #         margin-left: 15px;  /*set the margin, so boxes don't overlap labels*/
  #         line-height: 15px; 
  #     }
  # "),

                          
                          # selectInput("cnt", "Countries to keep in table:",
                          #             c("Fishing in your EEZ" = "EEZ",
                          #               "Fishing by your flagged/charter vessels" = "flag")),
                          # checkboxGroupInput("grs", "Fishing methods to display:",
                          #                    c("Purse Seine (S)" = "S", "Longline (L)" = "L", "Pole and Line (P)" = "P", "Troll (T)" = "T", "Other gears (Z)" = "Z"),
                          #                    selected = c("S","L","P","T","Z")),
                          # br(),
                          # # selectInput("spatial.ext", "Choose the region:",
                          #             c("WCPO - WCPFC convention area" = "wcpo",
                          #               "Whole of Pacific" = "full.pacific")),
                          # br(),
                          # prettyCheckboxGroup(inputId = "checkgroup1",
                          #                     label = "Click me",
                          #                     choices = c("Australia " = "AU", "Cook Isl. " = "CK", "Fiji      " = "FJ", "FSM       " = "FM",
                          #                                 "Kiribati  " = "KI", "Marshalls " = "MH", "Nauru     " = "NR", "Niue      " = "NU", 
                          #                                 "New Zld   " = "NZ",  "PNG       " = "PG", "Palau     " = "PW",
                          #                                 "Solomons  " = "SB", "Tokelau  " = "TK", "Tonga     " = "TO",
                          #                                 "Tuvalu    " = "TV", "Vanuatu   " = "VU", "Samoa     " = "WS"), inline = TRUE),
                          # checkboxGroupInput("ffa.cnts", "FFA zones to keep in table:",
                          #                    c("Australia " = "AU", "Cook Isl. " = "CK", "Fiji      " = "FJ", "FSM       " = "FM",
                          #                      "Kiribati  " = "KI", "Marshalls " = "MH", "Nauru     " = "NR", "Niue      " = "NU", 
                          #                      "New Zld   " = "NZ",  "PNG       " = "PG", "Palau     " = "PW",
                          #                      "Solomons  " = "SB", "Tokelau  " = "TK", "Tonga     " = "TO",
                          #                      "Tuvalu    " = "TV", "Vanuatu   " = "VU", "Samoa     " = "WS"),
                          #                    selected = c("AU","CK","FJ","FM","KI","MH","NR","NU", 
                          #                                 "NZ","PG","PW","SB","TK","TO","TV","VU","WS"), inline = TRUE),
                          # checkboxGroupInput("us.cnts", "US zones to keep in table:",
                          #                    c("Am Samoa" = "AS","Hawaii" = "HW","Johnson" = "JT","Jarvis" = "JV","Palmyra" = "PY","Wake" = "WK"),
                          #                    selected = c("AS","HW","JT","JV","PY","WK"), inline = TRUE),
                          # checkboxGroupInput("hs.cnts", "High seas zones to keep in table:",
                          #                    c("Int. Wt 1" = "I1","Int. Wt 2" = "I2","Int. Wt 3" = "I3","Int. Wt 4" = "I4","Int. Wt 5" = "I5",
                          #                      "Int. Wt 6" = "I6","Int. Wt 7" = "I7","Int. Wt 8" = "I8","Int. Wt 9" = "I9",
                          #                      "HS. Zone 4" = "H4","HS. Zone 5" = "H5"),
                          #                    selected = c("I1","I2","I3","I4","I5","I6","I7","I8","I9","H4","H5"), inline = TRUE),
                          # checkboxGroupInput("oth.cnts", "Other zones to keep in table:",
                          #                    c("China" = "CN", "Indonesia" = "ID", "Japan" = "JP", "New Caledonia" = "NC", "French Pol" = "PF",
                          #                      "Philippines" = "PH", "Taiwan" = "TW", "Vietnam" = "VN", "Wallis/Futuna" = "WF"),
                          #                    selected = c("CN","ID","JP","NC","PF","PH","TW","VN","WF"), inline = TRUE),
                          # br(),
                          br()
             ),
             mainPanel(
               #fluidRow(column(9, plotOutput("TotCatch")), column(3, plotlyOutput("Gear.pie"))),
               br(),
               br(),
               #plotOutput("SelCatch"),
               fluidRow(column(6, dataTableOutput("CatTab.DT")), column(6, dataTableOutput("CatTab.grp.DT"))),
               
               #plotlyOutput("PlotlyPlot")
             )
           )
   ),
  # tags$head(
  #   tags$style(
  #     HTML(
  #       ".checkbox-inline {
  #                   margin-left: 0px;
  #                   margin-right: 5px;
  #         }
  #        .checkbox-inline+.checkbox-inline {
  #                   margin-left: 0px;
  #                   margin-right: 5px;
  #         }
  #       "
  #     )
  #   )
  # ),
  # tags$head(tags$style(HTML("
  #                           .multicol .shiny-options-group{
  #                           -webkit-column-count: 3; /* Chrome, Safari, Opera */
  #                           -moz-column-count: 3;    /* Firefox */
  #                           column-count: 3;
  #                           -moz-column-fill: balanced;
  #                           -column-fill: balanced;
  #                           }
  #                           .checkbox{
  #                           margin-top: 0px !important;
  #                           -webkit-margin-after: 0px !important; 
  #                           }
  #                           "))),
  tabPanel("Catch output",
           sidebarLayout(
             sidebarPanel(width = 1,

                          # Add download button
                          downloadButton("downloadData", "Download")
                          
             ),
           mainPanel(
             tableOutput("CatTab.yr")
           ))),
  
  tabPanel("Converter",
           sidebarLayout(
             sidebarPanel(width = 3,
                          selectInput("metric", "Catch species to convert from:",
                                      c("Albacore" = "ALBc", "Bigeye" = "BETc", "Yellowfin" = "YFTc")),
                          br(),
                          list(tags$div(align = 'left', 
                                        class = 'multicol', 
                                        checkboxGroupInput(inputId  = "flags", label = "Select Other zones:",
                                                           c("China" = "CN", "Indonesia" = "ID", "Japan" = "JP", "Korea" = "KR",
                                                             "Taiwan" = "TW","USA" = "US"),
                                                           selected = c("CN"), inline = TRUE)))
                          
             ),
           mainPanel(
             fluidRow(column(6, dataTableOutput("Conv.CatTab.DT.1")), column(6, dataTableOutput("Conv.CatTab.DT.2")))
           ))),
  
  tabPanel("Test plot",
           sidebarLayout(
             sidebarPanel(width = 1,
                          
                          # Add download button
                          downloadButton("downloadPlot", "Download")
                          
             ),
           mainPanel(
             plotOutput("GrpCatchPlt")
           )
           )),
  
   tabPanel("Zone map",
            mainPanel(
              img(src = "Pacific_EEZs.png", height = 708, width = 1065)
            ))

)
  
  # cnt.cols <- c("slategray4", "steelblue3")
  # 
  # pl2 <- ggplot(plot.dat()[[4]], aes(x = "", y = Catch, fill = Category)) +
  #               geom_bar(width = 1, stat = "identity", color = "white") +
  #               coord_polar("y", start = 0) +
  #               scale_fill_manual(values = cnt.cols) +
  #               geom_text(aes(y = Catch, label = round(CatP*100, 1)), color = "black", size = 9)+
  #               #scale_fill_manual(values = mycols) +
  #               theme_void() +
  #               theme(legend.title = element_blank())
  # 
  # grid.arrange(pl1, pl2, ncol = 2, widths = c(3,1))



#____________________________________________________________________________________________________________
# The server


server <- function(input, output) {
  
  
  scen.scl2 <- reactive({
    if(input$scenarios == "TRPOvr"){
      sclr <- 0.0275
    } else {
      sclr <- NA
    }
  })
  
  scen.isLL <- reactive({
    if(input$scenarios == "TRPLLon"){
      sclr <- NA
    } else {
      sclr <- "Apply.to.troll"
    }
  })
  
  
  #____________________________________________________________________________________________________________
  # Do the total catch calculations  
  
  
  cnt.dat  <- reactive({
  
    c(input$ffa.cnts,input$us.cnts,input$hs.cnts,input$oth.cnts)
    
  })
  
  # plot.dat <- reactive({
  #   
  #   if(is.null(input$grs)){
  #     
  #     acedat.gr <- acedat
  #     
  #   } else{
  #     
  #     acedat.gr <- acedat %>% filter(gear %in% c(input$grs))
  #     
  #   }
  #   
  #   if(input$sum.type == "EEZ"){
  #     
  #     acedat.gr %<>% mutate(resp.var = ez_ag)
  #     
  #   } else{
  #     
  #     acedat.gr %<>% mutate(resp.var = flag)
  #     
  #   }
  #   
  #   tmpdat <- acedat.gr %>% select(input$species) %>% mutate(TotCat = rowSums(.))
  #   
  #   
  #   if(input$country == "all.cnt"){
  #     acepl <- acedat.gr %>% mutate(Category = "Other", TotCat = tmpdat$TotCat)
  #     acepl$Category <- factor(acepl$Category, levels = "Other")
  #   } else{
  #     
  #     acepl <- acedat.gr %>% mutate(Category = ifelse(resp.var == input$country, cnt.nms$lng.nm[cnt.nms$sht.nm == input$country], "Other"), TotCat = tmpdat$TotCat)
  #     acepl$Category <- factor(acepl$Category, levels = c("Other", cnt.nms$lng.nm[cnt.nms$sht.nm == input$country]))
  #   }
  #   
  #   acepl.yr <- acepl %>% group_by(Year, Category) %>% summarise(Catch = sum(TotCat))
  #   
  #   ace.pc.gr <- acepl %>% filter(Year >= input$sliderrng[1], Year <= input$sliderrng[2]) %>% group_by(Gear = gear) %>%
  #     summarise(Catch = sum(TotCat)) %>% mutate(CatP = Catch/length(input$sliderrng[1]:input$sliderrng[2]))
  #   
  #   ace.pc.cnt <- acepl %>% filter(Year >= input$sliderrng[1], Year <= input$sliderrng[2]) %>% group_by(Category) %>%
  #     summarise(Catch = sum(TotCat)) %>% mutate(CatP = Catch/length(input$sliderrng[1]:input$sliderrng[2]))
  #   
  #   if(input$country == "all.cnt"){
  #     selpl <- acepl %>% group_by(Year) %>% summarise(Catch = sum(TotCat))
  #   } else{
  #     
  #     if(input$country %in% acepl$resp.var){
  #       selpl <- filter(acepl, Category == cnt.nms$lng.nm[cnt.nms$sht.nm == input$country]) %>% group_by(Year) %>% summarise(Catch = sum(TotCat))
  #       tmp.yr <- data.frame(Year = 1990:latest.yr)
  #       selpl <- left_join(tmp.yr, selpl, by = "Year")
  #     } else{
  #       selpl <- data.frame(Year = 1990:latest.yr, Catch = 0)
  #     }
  #   }
  #   
  #   dat <- list(acepl.yr, ace.pc.gr, selpl, ace.pc.cnt)
  #   
  # })
  # 
  # 
  # #____________________________________________________________________________________________________________
  # # Plot total longline calculations  
  # 
  # output$TotCatch <- renderPlot({
  #   
  #   # if(length(unique(plot.dat()[[1]])) > 1){
  #   #   cnt.cols <- c("slategray4", "steelblue3")
  #   # } else{
  #   #   cnt.cols <- c("slategray4")
  #   # }
  #   cnt.cols <- c("slategray4", "magenta2")
  #   
  #   mu.catch <- plot.dat()[[1]] %>% filter(Year >= input$sliderrng[1], Year <= input$sliderrng[2]) %>% summarise(Catch = sum(Catch))
  #   
  #   pl1 <- ggplot(plot.dat()[[1]], aes(x = as.character(Year), y = Catch/1000)) +
  #     geom_rect(aes(xmin = length(1990:latest.yr) - (latest.yr - input$sliderrng[1]) - 0.5,
  #                   xmax = length(1990:latest.yr) - (latest.yr - input$sliderrng[2]) + 0.5,
  #                   ymin = -Inf, ymax = Inf), fill = "palegreen1") +
  #     geom_bar(aes(fill = Category), stat = "identity", width = 0.8) +
  #     xlab("Year") + ylab("Catch (1,000's mt)") + guides(fill = guide_legend(nrow = 1)) +
  #     scale_y_continuous(limits = c(0,3000)) +
  #     scale_x_discrete(breaks = seq(1, latest.yr, by = 1)) +
  #     scale_fill_manual(values = cnt.cols) +
  #     theme_few() +
  #     theme(axis.text.x = element_text(size = 11, angle = 90), axis.text.y = element_text(size = 14), axis.title = element_text(size = 18),
  #           legend.position = "top", legend.title = element_blank(), legend.text = element_text(size = 12), legend.key.size = unit(0.8, "cm"),
  #           legend.spacing.x = unit(0.2, "cm"), axis.title.x = element_blank(), panel.border = element_blank(),
  #           axis.line = element_line(colour = "grey"))
  #   
  #   print(pl1)
  #   
  #   # pl.cols <- gr.cols$grcols[match(unique(plot.dat()[[2]]$Gear),gr.cols$sht.nm)]
  #   # 
  #   # pl2 <- ggplot(plot.dat()[[2]], aes(x = "", y = Catch, fill = Gear)) +
  #   #               geom_bar(width = 1, stat = "identity", color = "white") +
  #   #               coord_polar("y", start = 0) +
  #   #               #geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  #   #               scale_fill_manual(values = pl.cols) +
  #   #               theme_void()
  #   # 
  #   # grid.arrange(pl1, pl2, ncol = 2, widths = c(3,1))
  #   
  # })
  # 
  # output$SelCatch <- renderPlot({
  #   
  #   pl1 <- ggplot(plot.dat()[[3]], aes(x = as.character(Year), y = Catch/1000)) +
  #     geom_rect(aes(xmin = length(1990:latest.yr) - (latest.yr - input$sliderrng[1]) - 0.5,
  #                   xmax = length(1990:latest.yr) - (latest.yr - input$sliderrng[2]) + 0.5,
  #                   ymin = -Inf, ymax = Inf), fill = "palegreen1") +
  #     geom_bar(stat = "identity", fill = "slategray4", width = 0.8) +
  #     xlab("Year") + ylab("Catch (1,000's mt)") +
  #     scale_x_discrete(breaks = seq(1, latest.yr, by = 1)) +
  #     theme_few() +
  #     theme(axis.text.x = element_text(size = 11, angle = 90), axis.text.y = element_text(size = 14), axis.title = element_text(size = 18),
  #           legend.position = "top", legend.title = element_blank(), legend.text = element_text(size = 12), legend.key.size = unit(0.8, "cm"),
  #           legend.spacing.x = unit(0.2, "cm"), axis.title.x = element_blank(), panel.border = element_blank(),
  #           axis.line = element_line(colour = "grey"))
  #   
  #   print(pl1)
  #   
  #   # cnt.cols <- c("slategray4", "steelblue3")
  #   # 
  #   # pl2 <- ggplot(plot.dat()[[4]], aes(x = "", y = Catch, fill = Category)) +
  #   #               geom_bar(width = 1, stat = "identity", color = "white") +
  #   #               coord_polar("y", start = 0) +
  #   #               scale_fill_manual(values = cnt.cols) +
  #   #               geom_text(aes(y = Catch, label = round(CatP*100, 1)), color = "black", size = 9)+
  #   #               #scale_fill_manual(values = mycols) +
  #   #               theme_void() +
  #   #               theme(legend.title = element_blank())
  #   # 
  #   # grid.arrange(pl1, pl2, ncol = 2, widths = c(3,1))
  #   
  # })

  CatTab <- reactive({

    kp.ez <- cnt.dat()
    tab.dat <- ace.ez %>% ungroup()
    tab.dat$res <- tab.dat[,input$spp]
    
    Nyrs <- length(input$sliderrng[1]:input$sliderrng[2])
    
    tab <- tab.dat %>% filter(EEZ %in% kp.ez, Year >= input$sliderrng[1], Year <= input$sliderrng[2]) %>% group_by(Zone = EZnm) %>%
                       summarise(Days = round(sum(Days)/Nyrs), Catch = round(sum(res)/Nyrs)) %>% mutate(Percent = round(Catch/sum(Catch)*100)) 
    
    
    
    if(input$aggHS){
      
      tab <- tab.dat %>% filter(EEZ %in% kp.ez, Year >= input$sliderrng[1], Year <= input$sliderrng[2]) %>% 
          mutate(EZnm = ifelse(EZnm %in% c("Int1","Int2","Int3","Int4","Int5","Int6","Int7","Int8","Int9","HS4","HS5"), "High seas", EZnm)) %>%
          group_by(Zone = EZnm) %>%
          summarise(Days = round(sum(Days)/Nyrs), Catch = round(sum(res)/Nyrs)) %>% mutate(Percent = round(Catch/sum(Catch)*100)) 
        
    } else{
      
      tab <- tab.dat %>% filter(EEZ %in% kp.ez, Year >= input$sliderrng[1], Year <= input$sliderrng[2]) %>% group_by(Zone = EZnm) %>%
                         summarise(Days = round(sum(Days)/Nyrs), Catch = round(sum(res)/Nyrs)) %>% mutate(Percent = round(Catch/sum(Catch)*100)) 
    }

  })
  
  output$CatTab.DT <- renderDataTable(CatTab(), options = list(pageLength = 50))

  
  CatTab.grp <- reactive({
    
    kp.ez <- cnt.dat()
    tab.dat <- ace.ez %>% ungroup()
    tab.dat$res <- tab.dat[,input$spp]
    
    Nyrs <- length(input$sliderrng[1]:input$sliderrng[2])
    
    tab <- tab.dat %>% filter(EEZ %in% kp.ez, Year >= input$sliderrng[1], Year <= input$sliderrng[2]) %>% group_by(Zone = Grp) %>%
                       summarise(Days = round(sum(Days)/Nyrs), Catch = round(sum(res)/Nyrs)) %>% mutate(Percent = round(Catch/sum(Catch)*100)) %>%
                       arrange(desc(Catch))
    
  })

  output$CatTab.grp.DT <- renderDataTable(CatTab.grp(), options = list(pageLength = 50))
  
  
  GrpCatchPlt.pl <- function(){
    
    pl1 <- ggplot(CatTab.grp(), aes(x = Zone, y = Catch)) +
      geom_bar(stat = "identity", fill = "slategray4", width = 0.8) +
      xlab("") + ylab("Catch mt") +
      theme_few()
    print(pl1)
    
  }
  
  
  output$GrpCatchPlt <- renderPlot({
    
    GrpCatchPlt.pl()
      
  })
  

  output$downloadPlot <- downloadHandler(
    filename = "Zone_Catches.png",
    content = function(file) {
      png(file)
      print(GrpCatchPlt.pl())
      dev.off()
    })  
  
  
  
  CatTab.yr.dat <- reactive({
    
    kp.ez <- cnt.dat()
    tab.dat <- ace.ez %>% ungroup()
    tab.dat$res <- tab.dat[,input$spp]
    #tab.dat$str <- tab.dat[,input$str.type] 
    
    tab <- tab.dat %>% filter(EEZ %in% kp.ez, Year >= input$sliderrng[1], Year <= input$sliderrng[2]) %>% group_by(Zone = EZnm, Year) %>%
                       summarise(Days = round(sum(Days)), Catch = round(sum(res)))
    
    out.tab <- tab %>% pivot_wider(-Catch, names_from = Year, values_from = Days)
    out.tab[is.na(out.tab)] <- 0
    
    out.tab
    
  })
  
  
  output$CatTab.yr <- renderTable({
    
    CatTab.yr.dat()
    
  })
  
  
  
  output$downloadData <- downloadHandler(
    filename = function(){"Catch_Data.csv"}, 
    content = function(fname){
      write.csv(CatTab.yr.dat(), fname, row.names = FALSE)
    }
  )
  
  
  
  
  
  
  
  
  
  Conv.CatTab.1 <- reactive({
    
    kp.ez <- cnt.dat()
    tab.dat <- ace.ez %>% ungroup()
    tab.dat$res <- tab.dat[,input$metric]
    
    Nyrs <- length(input$sliderrng[1]:input$sliderrng[2])
    
    # Remove the following tab calcs plus those in the other catch tabs throughout
    tab <- tab.dat %>% filter(EEZ %in% kp.ez, Year >= input$sliderrng[1], Year <= input$sliderrng[2]) %>% group_by(Zone = EZnm) %>%
                       summarise(Days = round(sum(Days)/Nyrs), Catch = sum(res)/Nyrs, ALB.cat = sum(ALBc)/Nyrs, BET.cat = sum(BETc)/Nyrs, YFT.cat = sum(YFTc)/Nyrs) %>%
                       mutate(ALB.conv = round(ALB.cat/Catch, 2), BET.conv = round(BET.cat/Catch, 2), YFT.conv = round(YFT.cat/Catch, 2), Catch = round(Catch)) %>%
                       select(Zone, Days, Catch, ALB.conv, BET.conv, YFT.conv)
    
    
    if(input$aggHS){
      
      tab <- tab.dat %>% filter(EEZ %in% kp.ez, Year >= input$sliderrng[1], Year <= input$sliderrng[2]) %>% 
                         mutate(EZnm = ifelse(EZnm %in% c("Int1","Int2","Int3","Int4","Int5","Int6","Int7","Int8","Int9","HS4","HS5"), "High seas", EZnm)) %>%
                         group_by(Zone = EZnm) %>%
                         summarise(Days = round(sum(Days)/Nyrs), Catch = sum(res)/Nyrs, ALB.cat = sum(ALBc)/Nyrs, BET.cat = sum(BETc)/Nyrs, YFT.cat = sum(YFTc)/Nyrs) %>%
                         mutate(ALB.conv = round(ALB.cat/Catch, 2), BET.conv = round(BET.cat/Catch, 2), YFT.conv = round(YFT.cat/Catch, 2), Catch = round(Catch)) %>%
                         select(Zone, Days, Catch, ALB.conv, BET.conv, YFT.conv)
      
    } else{
      
      tab <- tab.dat %>% filter(EEZ %in% kp.ez, Year >= input$sliderrng[1], Year <= input$sliderrng[2]) %>% group_by(Zone = EZnm) %>%
                         summarise(Days = round(sum(Days)/Nyrs), Catch = sum(res)/Nyrs, ALB.cat = sum(ALBc)/Nyrs, BET.cat = sum(BETc)/Nyrs, YFT.cat = sum(YFTc)/Nyrs) %>%
                         mutate(ALB.conv = round(ALB.cat/Catch, 2), BET.conv = round(BET.cat/Catch, 2), YFT.conv = round(YFT.cat/Catch, 2), Catch = round(Catch)) %>%
                         select(Zone, Days, Catch, ALB.conv, BET.conv, YFT.conv)
    }
    
  })
  
  output$Conv.CatTab.DT.1 <- renderDataTable(Conv.CatTab.1(), options = list(pageLength = 50))
  
  
  
  
  
  
  Conv.CatTab.2 <- reactive({
    
    kp.ez <- cnt.dat()
    tab.dat <- ace.ez.flg %>% ungroup()
    tab.dat$res <- tab.dat[,input$metric]
    
    Nyrs <- length(input$sliderrng[1]:input$sliderrng[2])
    
    
    # Remove the following tab calcs plus those in the other catch tabs throughout
    tab <- tab.dat %>% filter(EEZ %in% kp.ez, Year >= input$sliderrng[1], Year <= input$sliderrng[2], Flag %in% input$flags) %>% group_by(Zone = EZnm) %>%
                       summarise(Days = round(sum(Days)/Nyrs), Catch = sum(res)/Nyrs, ALB.cat = sum(ALBc)/Nyrs, BET.cat = sum(BETc)/Nyrs, YFT.cat = sum(YFTc)/Nyrs) %>%
                       mutate(ALB.conv = round(ALB.cat/Catch, 2), BET.conv = round(BET.cat/Catch, 2), YFT.conv = round(YFT.cat/Catch, 2), Catch = round(Catch)) %>%
                       select(Zone, Days, Catch, ALB.conv, BET.conv, YFT.conv)
    
    
    if(input$aggHS){
      
      tab <- tab.dat %>% filter(EEZ %in% kp.ez, Year >= input$sliderrng[1], Year <= input$sliderrng[2], Flag %in% input$flags) %>% 
                         mutate(EZnm = ifelse(EZnm %in% c("Int1","Int2","Int3","Int4","Int5","Int6","Int7","Int8","Int9","HS4","HS5"), "High seas", EZnm)) %>%
                         group_by(Zone = EZnm) %>%
                         summarise(Days = round(sum(Days)/Nyrs), Catch = sum(res)/Nyrs, ALB.cat = sum(ALBc)/Nyrs, BET.cat = sum(BETc)/Nyrs, YFT.cat = sum(YFTc)/Nyrs) %>%
                         mutate(ALB.conv = round(ALB.cat/Catch, 2), BET.conv = round(BET.cat/Catch, 2), YFT.conv = round(YFT.cat/Catch, 2), Catch = round(Catch)) %>%
                         select(Zone, Days, Catch, ALB.conv, BET.conv, YFT.conv)
      
    } else{
      
      tab <- tab.dat %>% filter(EEZ %in% kp.ez, Year >= input$sliderrng[1], Year <= input$sliderrng[2], Flag %in% input$flags) %>% group_by(Zone = EZnm) %>%
                         summarise(Days = round(sum(Days)/Nyrs), Catch = sum(res)/Nyrs, ALB.cat = sum(ALBc)/Nyrs, BET.cat = sum(BETc)/Nyrs, YFT.cat = sum(YFTc)/Nyrs) %>%
                         mutate(ALB.conv = round(ALB.cat/Catch, 2), BET.conv = round(BET.cat/Catch, 2), YFT.conv = round(YFT.cat/Catch, 2), Catch = round(Catch)) %>%
                         select(Zone, Days, Catch, ALB.conv, BET.conv, YFT.conv)
    }
    
    tab.mu <- data.frame(Zone = "Total", Days = sum(tab$Days), Catch = sum(tab$Catch), ALB.conv = round(weighted.mean(tab$ALB.conv, tab$Catch),2),
                         BET.conv = round(weighted.mean(tab$BET.conv, tab$Catch),2), YFT.conv = round(weighted.mean(tab$YFT.conv, tab$Catch),2))

    tab <- rbind(tab, tab.mu)
    
  })
  
  output$Conv.CatTab.DT.2 <- renderDataTable(Conv.CatTab.2(), options = list(pageLength = 50))
  
  
}

shinyApp(ui = ui, server = server)




























