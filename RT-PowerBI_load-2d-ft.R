# This requires that a R-wells.xlsx exists in working directory
# Do not forget to change the epsg at line 3373 to the appropriate CRS for input data
# Output is set to NAD27 Lat Long




WD <- tk_choose.dir(getwd(), "Choose a suitable folder")
setwd(WD)


read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# set Conversion CRS
epsg <- 32039 # WGS state plane 32039 -Texas Central, 32020 -North Dakota North, 26753 - North Colorado

# Set projection
s <- 68 + 180
e <- s - 90
  
side <- 0
edge <- 284


#If error : Error in paste(V1 = character(0), V2 = character(0), V3 = character(0),  : 
#                        non-string argument to internal 'paste' 

# run once to build
spot_load <- function(){
options( warn = -1 )



# Microseismic Load -------------------------------------------------------


print("loading microseismic")
FiltersMSM <- matrix(c("EVENTS", "EVENTS.csv", "All files", "*"),
                     4, 2, byrow = TRUE)
# MSM_data <- read_csv(tk_choose.files(caption = "Select MSM export file",
#                                      multi = FALSE, filters = FiltersMSM),
#                      col_names = TRUE)

MSM_data <- read_csv("Trentino Pad - Final DASMSM_EVENTS.csv")#, col_types = cols(date_time = col_datetime(format = "%m/%d/%Y %H:%M:%S")))
#create and read correct Date+Time values
MSM_time <- as.POSIXct(paste(MSM_data$"Acquisition Date (Local)",
                             MSM_data$"Acquisition Time (Local)"),
                       format="%m/%d/%Y %H:%M:%S")
MSM_data <- add_column(MSM_data, MSM_time)
rm(MSM_time)

MSM_data <- MSM_data %>% separate("Stage",
                                  c("Well", "Stage"),
                                  sep = " - Stage ",
                                  remove = TRUE,
                                  convert = FALSE)
# delete columns
#write.csv(MSM_data, file = "tmp.csv")
MSM_data <- MSM_data[-c(1:2,7:12,22:27,29:34)]
glimpse(MSM_data)
well_list <- unique(MSM_data$`Well`)

# Well Load ---------------------------------------------------------------


print("loading wells")
newcolnames <- c("Perforations","MD (ft)","TVD (ft)","E/W Relative Local Tangent Plane (ft)",
                 "N/S Relative Local Tangent Plane (ft)","TVDSS (ft)","Easting (ft)",
                 "Northing (ft)","Extent","Stage","KB","Well", "Latitude", "Longitude")
 
wellsheets <- read_excel_allsheets("R-Wells.xlsx")
wellnames <- names(wellsheets)
# write.xlsx(Frac1, 'frac1.xlsx')
if("Frac1" %in% wellnames){
  Frac1<-data.frame(wellsheets$Frac1)
  colnames(Frac1) <- newcolnames
  Frac1 <- Frac1 %>% separate("Stage",
                                    c("STG", "Stage"),
                                    sep = "Stage ",
                                    remove = TRUE,
                                    convert = FALSE)
  Frac1 <- Frac1[-c(4,5,9,10)]
}

if("Frac2" %in% wellnames){
  Frac2<-data.frame(wellsheets$Frac2)
  colnames(Frac2) <- newcolnames
  Frac2 <- Frac2 %>% separate("Stage",
                              c("STG", "Stage"),
                              sep = "Stage ",
                              remove = TRUE,
                              convert = FALSE)
  Frac2 <- Frac2[-c(4,5,9,10)]
}

if("Frac3" %in% wellnames){
  Frac3<-data.frame(wellsheets$Frac3)
  colnames(Frac3) <- newcolnames
  Frac3 <- Frac3 %>% separate("Stage",
                              c("STG", "Stage"),
                              sep = "Stage ",
                              remove = TRUE,
                              convert = FALSE)
  Frac3 <- Frac3[-c(4,5,9,10)]
}

if("Frac4" %in% wellnames){
  Frac4<-data.frame(wellsheets$Frac4)
  colnames(Frac4) <- newcolnames
  Frac4 <- Frac4 %>% separate("Stage",
                              c("STG", "Stage"),
                              sep = "Stage ",
                              remove = TRUE,
                              convert = FALSE)
  Frac4 <- Frac4[-c(4,5,9,10)]
}

if("Frac5" %in% wellnames){
  Frac5<-data.frame(wellsheets$Frac5)
  colnames(Frac5) <- newcolnames
  Frac5 <- Frac5 %>% separate("Stage",
                              c("STG", "Stage"),
                              sep = "Stage ",
                              remove = TRUE,
                              convert = FALSE)
  Frac5 <- Frac5[-c(4,5,9,10)]
}

if("Frac6" %in% wellnames){
  Frac6<-data.frame(wellsheets$Frac6)
  colnames(Frac6) <- newcolnames
  Frac6 <- Frac6 %>% separate("Stage",
                              c("STG", "Stage"),
                              sep = "Stage ",
                              remove = TRUE,
                              convert = FALSE)
  Frac6 <- Frac6[-c(4,5,9,10)]
}

if("Frac7" %in% wellnames){
  Frac7<-data.frame(wellsheets$Frac7)
  colnames(Frac7) <- newcolnames
  Frac7 <- Frac7 %>% separate("Stage",
                              c("STG", "Stage"),
                              sep = "Stage ",
                              remove = TRUE,
                              convert = FALSE)
  Frac7 <- Frac7[-c(4,5,9,10)]
}

if("Frac8" %in% wellnames){
  Frac8<-data.frame(wellsheets$Frac8)
  colnames(Frac8) <- newcolnames
  Frac8 <- Frac8 %>% separate("Stage",
                              c("STG", "Stage"),
                              sep = "Stage ",
                              remove = TRUE,
                              convert = FALSE)
  Frac8 <- Frac8[-c(4,5,9,10)]
}

if("Frac9" %in% wellnames){
  Frac9<-data.frame(wellsheets$Frac9)
  colnames(Frac9) <- newcolnames
  Frac9 <- Frac9 %>% separate("Stage",
                              c("STG", "Stage"),
                              sep = "Stage ",
                              remove = TRUE,
                              convert = FALSE)
  Frac9 <- Frac9[-c(4,5,9,10)]
}

if("Frac10" %in% wellnames){
  Frac10<-data.frame(wellsheets$Frac10)
  colnames(Frac10) <- newcolnames
  Frac10 <- Frac10 %>% separate("Stage",
                              c("STG", "Stage"),
                              sep = "Stage ",
                              remove = TRUE,
                              convert = FALSE)
  Frac10 <- Frac10[-c(4,5,9,10)]
}

if("Frac11" %in% wellnames){
  Frac11<-data.frame(wellsheets$Frac11)
  colnames(Frac11) <- newcolnames
  Frac11 <- Frac11 %>% separate("Stage",
                              c("STG", "Stage"),
                              sep = "Stage ",
                              remove = TRUE,
                              convert = FALSE)
  Frac11 <- Frac11[-c(4,5,9,10)]
}

if("Frac12" %in% wellnames){
  Frac12<-data.frame(wellsheets$Frac12)
  colnames(Frac12) <- newcolnames
  Frac12 <- Frac12 %>% separate("Stage",
                              c("STG", "Stage"),
                              sep = "Stage ",
                              remove = TRUE,
                              convert = FALSE)
  Frac12 <- Frac12[-c(4,5,9,10)]
}

if("Frac13" %in% wellnames){
  Frac13<-data.frame(wellsheets$Frac13)
  colnames(Frac13) <- newcolnames
  Frac13 <- Frac13 %>% separate("Stage",
                              c("STG", "Stage"),
                              sep = "Stage ",
                              remove = TRUE,
                              convert = FALSE)
  Frac13 <- Frac13[-c(4,5,9,10)]
}

if("Frac14" %in% wellnames){
  Frac14<-data.frame(wellsheets$Frac14)
  colnames(Frac14) <- newcolnames
  Frac14 <- Frac14 %>% separate("Stage",
                                c("STG", "Stage"),
                                sep = "Stage ",
                                remove = TRUE,
                                convert = FALSE)
  Frac14 <- Frac14[-c(4,5,9,10)]
}

if("Frac15" %in% wellnames){
  Frac15<-data.frame(wellsheets$Frac15)
  colnames(Frac15) <- newcolnames
  Frac15 <- Frac15 %>% separate("Stage",
                                c("STG", "Stage"),
                                sep = "Stage ",
                                remove = TRUE,
                                convert = FALSE)
  Frac15 <- Frac15[-c(4,5,9,10)]
}

if("Frac16" %in% wellnames){
  Frac16<-data.frame(wellsheets$Frac16)
  colnames(Frac16) <- newcolnames
  Frac16 <- Frac16 %>% separate("Stage",
                                c("STG", "Stage"),
                                sep = "Stage ",
                                remove = TRUE,
                                convert = FALSE)
  Frac16 <- Frac16[-c(4,5,9,10)]
}

if("Frac17" %in% wellnames){
  Frac17<-data.frame(wellsheets$Frac17)
  colnames(Frac17) <- newcolnames
  Frac17 <- Frac17 %>% separate("Stage",
                                c("STG", "Stage"),
                                sep = "Stage ",
                                remove = TRUE,
                                convert = FALSE)
  Frac17 <- Frac17[-c(4,5,9,10)]
}

if("Frac18" %in% wellnames){
  Frac18<-data.frame(wellsheets$Frac18)
  colnames(Frac18) <- newcolnames
  Frac18 <- Frac18 %>% separate("Stage",
                                c("STG", "Stage"),
                                sep = "Stage ",
                                remove = TRUE,
                                convert = FALSE)
  Frac18 <- Frac18[-c(4,5,9,10)]
}

if("Frac19" %in% wellnames){
  Frac19<-data.frame(wellsheets$Frac19)
  colnames(Frac19) <- newcolnames
  Frac19 <- Frac19 %>% separate("Stage",
                                c("STG", "Stage"),
                                sep = "Stage ",
                                remove = TRUE,
                                convert = FALSE)
  Frac19 <- Frac19[-c(4,5,9,10)]
}

if("Frac20" %in% wellnames){
  Frac20<-data.frame(wellsheets$Frac20)
  colnames(Frac20) <- newcolnames
  Frac20 <- Frac20 %>% separate("Stage",
                                c("STG", "Stage"),
                                sep = "Stage ",
                                remove = TRUE,
                                convert = FALSE)
  Frac20 <- Frac20[-c(4,5,9,10)]
}

if("Frac21" %in% wellnames){
  Frac21<-data.frame(wellsheets$Frac21)
  colnames(Frac21) <- newcolnames
  Frac21 <- Frac21 %>% separate("Stage",
                                c("STG", "Stage"),
                                sep = "Stage ",
                                remove = TRUE,
                                convert = FALSE)
  Frac21 <- Frac21[-c(4,5,9,10)]
}

if("Frac22" %in% wellnames){
  Frac22<-data.frame(wellsheets$Frac22)
  colnames(Frac22) <- newcolnames
  Frac22 <- Frac22 %>% separate("Stage",
                                c("STG", "Stage"),
                                sep = "Stage ",
                                remove = TRUE,
                                convert = FALSE)
  Frac22 <- Frac22[-c(4,5,9,10)]
}

if("Frac23" %in% wellnames){
  Frac23<-data.frame(wellsheets$Frac23)
  colnames(Frac23) <- newcolnames
  Frac23 <- Frac23 %>% separate("Stage",
                                c("STG", "Stage"),
                                sep = "Stage ",
                                remove = TRUE,
                                convert = FALSE)
  Frac23 <- Frac23[-c(4,5,9,10)]
}

# Perf KBs
if("Frac1" %in% wellnames){
  Frac1_KB <- Frac1$KB[1]
}

if("Frac2" %in% wellnames){
  Frac2_KB <- Frac2$KB[1]
}

if("Frac3" %in% wellnames){
  Frac3_KB <- Frac3$KB[1]
}

if("Frac4" %in% wellnames){
  Frac4_KB <- Frac4$KB[1]
}

if("Frac5" %in% wellnames){
  Frac5_KB <- Frac5$KB[1]
}

if("Frac6" %in% wellnames){
  Frac6_KB <- Frac6$KB[1]
}

if("Frac7" %in% wellnames){
  Frac7_KB <- Frac7$KB[1]
}

if("Frac8" %in% wellnames){
  Frac8_KB <- Frac8$KB[1]
}

if("Frac9" %in% wellnames){
  Frac9_KB <- Frac9$KB[1]
}

if("Frac10" %in% wellnames){
  Frac10_KB <- Frac10$KB[1]
}

if("Frac11" %in% wellnames){
  Frac11_KB <- Frac11$KB[1]
}

if("Frac12" %in% wellnames){
  Frac12_KB <- Frac12$KB[1]
}

if("Frac13" %in% wellnames){
  Frac13_KB <- Frac13$KB[1]
}

if("Frac14" %in% wellnames){
  Frac14_KB <- Frac14$KB[1]
}

if("Frac15" %in% wellnames){
  Frac15_KB <- Frac15$KB[1]
}

if("Frac16" %in% wellnames){
  Frac16_KB <- Frac16$KB[1]
}

if("Frac17" %in% wellnames){
  Frac17_KB <- Frac17$KB[1]
}

if("Frac18" %in% wellnames){
  Frac18_KB <- Frac18$KB[1]
}

if("Frac19" %in% wellnames){
  Frac19_KB <- Frac19$KB[1]
}

if("Frac20" %in% wellnames){
  Frac20_KB <- Frac20$KB[1]
}

if("Frac21" %in% wellnames){
  Frac21_KB <- Frac21$KB[1]
}

if("Frac22" %in% wellnames){
  Frac22_KB <- Frac22$KB[1]
}

if("Frac23" %in% wellnames){
  Frac23_KB <- Frac23$KB[1]
}


print("loading wells")
#write.csv(Frac1, file = "Frac1.csv")

# Lateral Distance --------------------------------------------------------



print("getting event to heel distances")
if("Frac1" %in% wellnames){
  Frac1a<-data.frame(Frac1$Well,Frac1$Stage,Frac1$`Easting (ft)`,Frac1$`Northing (ft)`,Frac1$`TVD (ft)`,Frac1$`MD (ft)`)
  Frac1a<-Frac1a %>%slice(which.min(Frac1$`MD (ft)`))
  Frac1b<-data.frame(`Well` = Frac1a$Frac1.Well,Stage = Frac1a$Frac1.Stage,Frac1a$Frac1..Easting..ft..,
                     Frac1a$Frac1..Northing..ft..,Frac1a$Frac1..TVD..ft..,Frac1a$Frac1..MD..ft..)
  Frac1 <- Frac1 %>% right_join(Frac1b, by=c("Well"))
}

if("Frac2" %in% wellnames){
  Frac2a<-data.frame(Frac2$Well,Frac2$Stage,Frac2$`Easting (ft)`,Frac2$`Northing (ft)`,Frac2$`TVD (ft)`,Frac2$`MD (ft)`)
  Frac2a<-Frac2a %>%slice(which.min(Frac2$`MD (ft)`))
  Frac2b<-data.frame(`Well` = Frac2a$Frac2.Well,Stage = Frac2a$Frac2.Stage,Frac2a$Frac2..Easting..ft..,
                     Frac2a$Frac2..Northing..ft..,Frac2a$Frac2..TVD..ft..,Frac2a$Frac2..MD..ft..)
  Frac2 <- Frac2 %>% right_join(Frac2b, by=c("Well"))
}

if("Frac3" %in% wellnames){
  Frac3a<-data.frame(Frac3$Well,Frac3$Stage,Frac3$`Easting (ft)`,Frac3$`Northing (ft)`,Frac3$`TVD (ft)`,Frac3$`MD (ft)`)
  Frac3a<-Frac3a %>%slice(which.min(Frac3$`MD (ft)`))
  Frac3b<-data.frame(`Well` = Frac3a$Frac3.Well,Stage = Frac3a$Frac3.Stage,Frac3a$Frac3..Easting..ft..,
                     Frac3a$Frac3..Northing..ft..,Frac3a$Frac3..TVD..ft..,Frac3a$Frac3..MD..ft..)
  Frac3 <- Frac3 %>% right_join(Frac3b, by=c("Well"))
}

if("Frac4" %in% wellnames){
  Frac4a<-data.frame(Frac4$Well,Frac4$Stage,Frac4$`Easting (ft)`,Frac4$`Northing (ft)`,Frac4$`TVD (ft)`,Frac4$`MD (ft)`)
  Frac4a<-Frac4a %>%slice(which.min(Frac4$`MD (ft)`))
  Frac4b<-data.frame(`Well` = Frac4a$Frac4.Well,Stage = Frac4a$Frac4.Stage,Frac4a$Frac4..Easting..ft..,
                     Frac4a$Frac4..Northing..ft..,Frac4a$Frac4..TVD..ft..,Frac4a$Frac4..MD..ft..)
  Frac4 <- Frac4 %>% right_join(Frac4b, by=c("Well"))
}

if("Frac5" %in% wellnames){
  Frac5a<-data.frame(Frac5$Well,Frac5$Stage,Frac5$`Easting (ft)`,Frac5$`Northing (ft)`,Frac5$`TVD (ft)`,Frac5$`MD (ft)`)
  Frac5a<-Frac5a %>%slice(which.min(Frac5$`MD (ft)`))
  Frac5b<-data.frame(`Well` = Frac5a$Frac5.Well,Stage = Frac5a$Frac5.Stage,Frac5a$Frac5..Easting..ft..,
                     Frac5a$Frac5..Northing..ft..,Frac5a$Frac5..TVD..ft..,Frac5a$Frac5..MD..ft..)
  Frac5 <- Frac5 %>% right_join(Frac5b, by=c("Well"))
}

if("Frac6" %in% wellnames){
  Frac6a<-data.frame(Frac6$Well,Frac6$Stage,Frac6$`Easting (ft)`,Frac6$`Northing (ft)`,Frac6$`TVD (ft)`,Frac6$`MD (ft)`)
  Frac6a<-Frac6a %>%slice(which.min(Frac6$`MD (ft)`))
  Frac6b<-data.frame(`Well` = Frac6a$Frac6.Well,Stage = Frac6a$Frac6.Stage,Frac6a$Frac6..Easting..ft..,
                     Frac6a$Frac6..Northing..ft..,Frac6a$Frac6..TVD..ft..,Frac6a$Frac6..MD..ft..)
  Frac6 <- Frac6 %>% right_join(Frac6b, by=c("Well"))
}

if("Frac7" %in% wellnames){
  Frac7a<-data.frame(Frac7$Well,Frac7$Stage,Frac7$`Easting (ft)`,Frac7$`Northing (ft)`,Frac7$`TVD (ft)`,Frac7$`MD (ft)`)
  Frac7a<-Frac7a %>%slice(which.min(Frac7$`MD (ft)`))
  Frac7b<-data.frame(`Well` = Frac7a$Frac7.Well,Stage = Frac7a$Frac7.Stage,Frac7a$Frac7..Easting..ft..,
                     Frac7a$Frac7..Northing..ft..,Frac7a$Frac7..TVD..ft..,Frac7a$Frac7..MD..ft..)
  Frac7 <- Frac7 %>% right_join(Frac7b, by=c("Well"))
}

if("Frac8" %in% wellnames){
  Frac8a<-data.frame(Frac8$Well,Frac8$Stage,Frac8$`Easting (ft)`,Frac8$`Northing (ft)`,Frac8$`TVD (ft)`,Frac8$`MD (ft)`)
  Frac8a<-Frac8a %>%slice(which.min(Frac8$`MD (ft)`))
  Frac8b<-data.frame(`Well` = Frac8a$Frac8.Well,Stage = Frac8a$Frac8.Stage,Frac8a$Frac8..Easting..ft..,
                     Frac8a$Frac8..Northing..ft..,Frac8a$Frac8..TVD..ft..,Frac8a$Frac8..MD..ft..)
  Frac8 <- Frac8 %>% right_join(Frac8b, by=c("Well"))
}

if("Frac9" %in% wellnames){
  Frac9a<-data.frame(Frac9$Well,Frac9$Stage,Frac9$`Easting (ft)`,Frac9$`Northing (ft)`,Frac9$`TVD (ft)`,Frac9$`MD (ft)`)
  Frac9a<-Frac9a %>%slice(which.min(Frac9$`MD (ft)`))
  Frac9b<-data.frame(`Well` = Frac9a$Frac9.Well,Stage = Frac9a$Frac9.Stage,Frac9a$Frac9..Easting..ft..,
                     Frac9a$Frac9..Northing..ft..,Frac9a$Frac9..TVD..ft..,Frac9a$Frac9..MD..ft..)
  Frac9 <- Frac9 %>% right_join(Frac9b, by=c("Well"))
}

if("Frac10" %in% wellnames){
  Frac10a<-data.frame(Frac10$Well,Frac10$Stage,Frac10$`Easting (ft)`,Frac10$`Northing (ft)`,Frac10$`TVD (ft)`,Frac10$`MD (ft)`)
  Frac10a<-Frac10a %>%slice(which.min(Frac10$`MD (ft)`))
  Frac10b<-data.frame(`Well` = Frac10a$Frac10.Well,Stage = Frac10a$Frac10.Stage,Frac10a$Frac10..Easting..ft..,
                     Frac10a$Frac10..Northing..ft..,Frac10a$Frac10..TVD..ft..,Frac10a$Frac10..MD..ft..)
  Frac10 <- Frac10 %>% right_join(Frac10b, by=c("Well"))
}

if("Frac11" %in% wellnames){
  Frac11a<-data.frame(Frac11$Well,Frac11$Stage,Frac11$`Easting (ft)`,Frac11$`Northing (ft)`,Frac11$`TVD (ft)`,Frac11$`MD (ft)`)
  Frac11a<-Frac11a %>%slice(which.min(Frac11$`MD (ft)`))
  Frac11b<-data.frame(`Well` = Frac11a$Frac11.Well,Stage = Frac11a$Frac11.Stage,Frac11a$Frac11..Easting..ft..,
                      Frac11a$Frac11..Northing..ft..,Frac11a$Frac11..TVD..ft..,Frac11a$Frac11..MD..ft..)
  Frac11 <- Frac11 %>% right_join(Frac11b, by=c("Well"))
}

if("Frac12" %in% wellnames){
  Frac12a<-data.frame(Frac12$Well,Frac12$Stage,Frac12$`Easting (ft)`,Frac12$`Northing (ft)`,Frac12$`TVD (ft)`,Frac12$`MD (ft)`)
  Frac12a<-Frac12a %>%slice(which.min(Frac12$`MD (ft)`))
  Frac12b<-data.frame(`Well` = Frac12a$Frac12.Well,Stage = Frac12a$Frac12.Stage,Frac12a$Frac12..Easting..ft..,
                      Frac12a$Frac12..Northing..ft..,Frac12a$Frac12..TVD..ft..,Frac12a$Frac12..MD..ft..)
  Frac12 <- Frac12 %>% right_join(Frac12b, by=c("Well"))
}

if("Frac13" %in% wellnames){
  Frac13a<-data.frame(Frac13$Well,Frac13$Stage,Frac13$`Easting (ft)`,Frac13$`Northing (ft)`,Frac13$`TVD (ft)`,Frac13$`MD (ft)`)
  Frac13a<-Frac13a %>%slice(which.min(Frac13$`MD (ft)`))
  Frac13b<-data.frame(`Well` = Frac13a$Frac13.Well,Stage = Frac13a$Frac13.Stage,Frac13a$Frac13..Easting..ft..,
                      Frac13a$Frac13..Northing..ft..,Frac13a$Frac13..TVD..ft..,Frac13a$Frac13..MD..ft..)
  Frac13 <- Frac13 %>% right_join(Frac13b, by=c("Well"))
}

if("Frac14" %in% wellnames){
  Frac14a<-data.frame(Frac14$Well,Frac14$Stage,Frac14$`Easting (ft)`,Frac14$`Northing (ft)`,Frac14$`TVD (ft)`,Frac14$`MD (ft)`)
  Frac14a<-Frac14a %>%slice(which.min(Frac14$`MD (ft)`))
  Frac14b<-data.frame(`Well` = Frac14a$Frac14.Well,Stage = Frac14a$Frac14.Stage,Frac14a$Frac14..Easting..ft..,
                      Frac14a$Frac14..Northing..ft..,Frac14a$Frac14..TVD..ft..,Frac14a$Frac14..MD..ft..)
  Frac14 <- Frac14 %>% right_join(Frac14b, by=c("Well"))
}

if("Frac15" %in% wellnames){
  Frac15a<-data.frame(Frac15$Well,Frac15$Stage,Frac15$`Easting (ft)`,Frac15$`Northing (ft)`,Frac15$`TVD (ft)`,Frac15$`MD (ft)`)
  Frac15a<-Frac15a %>%slice(which.min(Frac15$`MD (ft)`))
  Frac15b<-data.frame(`Well` = Frac15a$Frac15.Well,Stage = Frac15a$Frac15.Stage,Frac15a$Frac15..Easting..ft..,
                      Frac15a$Frac15..Northing..ft..,Frac15a$Frac15..TVD..ft..,Frac15a$Frac15..MD..ft..)
  Frac15 <- Frac15 %>% right_join(Frac15b, by=c("Well"))
}

if("Frac16" %in% wellnames){
  Frac16a<-data.frame(Frac16$Well,Frac16$Stage,Frac16$`Easting (ft)`,Frac16$`Northing (ft)`,Frac16$`TVD (ft)`,Frac16$`MD (ft)`)
  Frac16a<-Frac16a %>%slice(which.min(Frac16$`MD (ft)`))
  Frac16b<-data.frame(`Well` = Frac16a$Frac16.Well,Stage = Frac16a$Frac16.Stage,Frac16a$Frac16..Easting..ft..,
                      Frac16a$Frac16..Northing..ft..,Frac16a$Frac16..TVD..ft..,Frac16a$Frac16..MD..ft..)
  Frac16 <- Frac16 %>% right_join(Frac16b, by=c("Well"))
}

if("Frac17" %in% wellnames){
  Frac17a<-data.frame(Frac17$Well,Frac17$Stage,Frac17$`Easting (ft)`,Frac17$`Northing (ft)`,Frac17$`TVD (ft)`,Frac17$`MD (ft)`)
  Frac17a<-Frac17a %>%slice(which.min(Frac17$`MD (ft)`))
  Frac17b<-data.frame(`Well` = Frac17a$Frac17.Well,Stage = Frac17a$Frac17.Stage,Frac17a$Frac17..Easting..ft..,
                      Frac17a$Frac17..Northing..ft..,Frac17a$Frac17..TVD..ft..,Frac17a$Frac17..MD..ft..)
  Frac17 <- Frac17 %>% right_join(Frac17b, by=c("Well"))
}

if("Frac18" %in% wellnames){
  Frac18a<-data.frame(Frac18$Well,Frac18$Stage,Frac18$`Easting (ft)`,Frac18$`Northing (ft)`,Frac18$`TVD (ft)`,Frac18$`MD (ft)`)
  Frac18a<-Frac18a %>%slice(which.min(Frac18$`MD (ft)`))
  Frac18b<-data.frame(`Well` = Frac18a$Frac18.Well,Stage = Frac18a$Frac18.Stage,Frac18a$Frac18..Easting..ft..,
                      Frac18a$Frac18..Northing..ft..,Frac18a$Frac18..TVD..ft..,Frac18a$Frac18..MD..ft..)
  Frac18 <- Frac18 %>% right_join(Frac18b, by=c("Well"))
}

if("Frac19" %in% wellnames){
  Frac19a<-data.frame(Frac19$Well,Frac19$Stage,Frac19$`Easting (ft)`,Frac19$`Northing (ft)`,Frac19$`TVD (ft)`,Frac19$`MD (ft)`)
  Frac19a<-Frac19a %>%slice(which.min(Frac19$`MD (ft)`))
  Frac19b<-data.frame(`Well` = Frac19a$Frac19.Well,Stage = Frac19a$Frac19.Stage,Frac19a$Frac19..Easting..ft..,
                      Frac19a$Frac19..Northing..ft..,Frac19a$Frac19..TVD..ft..,Frac19a$Frac19..MD..ft..)
  Frac19 <- Frac19 %>% right_join(Frac19b, by=c("Well"))
}

if("Frac20" %in% wellnames){
  Frac20a<-data.frame(Frac20$Well,Frac20$Stage,Frac20$`Easting (ft)`,Frac20$`Northing (ft)`,Frac20$`TVD (ft)`,Frac20$`MD (ft)`)
  Frac20a<-Frac20a %>%slice(which.min(Frac20$`MD (ft)`))
  Frac20b<-data.frame(`Well` = Frac20a$Frac20.Well,Stage = Frac20a$Frac20.Stage,Frac20a$Frac20..Easting..ft..,
                      Frac20a$Frac20..Northing..ft..,Frac20a$Frac20..TVD..ft..,Frac20a$Frac20..MD..ft..)
  Frac20 <- Frac20 %>% right_join(Frac20b, by=c("Well"))
}

if("Frac21" %in% wellnames){
  Frac21a<-data.frame(Frac21$Well,Frac21$Stage,Frac21$`Easting (ft)`,Frac21$`Northing (ft)`,Frac21$`TVD (ft)`,Frac21$`MD (ft)`)
  Frac21a<-Frac21a %>%slice(which.min(Frac21$`MD (ft)`))
  Frac21b<-data.frame(`Well` = Frac21a$Frac21.Well,Stage = Frac21a$Frac21.Stage,Frac21a$Frac21..Easting..ft..,
                      Frac21a$Frac21..Northing..ft..,Frac21a$Frac21..TVD..ft..,Frac21a$Frac21..MD..ft..)
  Frac21 <- Frac21 %>% right_join(Frac21b, by=c("Well"))
}

if("Frac22" %in% wellnames){
  Frac22a<-data.frame(Frac22$Well,Frac22$Stage,Frac22$`Easting (ft)`,Frac22$`Northing (ft)`,Frac22$`TVD (ft)`,Frac22$`MD (ft)`)
  Frac22a<-Frac22a %>%slice(which.min(Frac22$`MD (ft)`))
  Frac22b<-data.frame(`Well` = Frac22a$Frac22.Well,Stage = Frac22a$Frac22.Stage,Frac22a$Frac22..Easting..ft..,
                      Frac22a$Frac22..Northing..ft..,Frac22a$Frac22..TVD..ft..,Frac22a$Frac22..MD..ft..)
  Frac22 <- Frac22 %>% right_join(Frac22b, by=c("Well"))
}

if("Frac23" %in% wellnames){
  Frac23a<-data.frame(Frac23$Well,Frac23$Stage,Frac23$`Easting (ft)`,Frac23$`Northing (ft)`,Frac23$`TVD (ft)`,Frac23$`MD (ft)`)
  Frac23a<-Frac23a %>%slice(which.min(Frac23$`MD (ft)`))
  Frac23b<-data.frame(`Well` = Frac23a$Frac23.Well,Stage = Frac23a$Frac23.Stage,Frac23a$Frac23..Easting..ft..,
                      Frac23a$Frac23..Northing..ft..,Frac23a$Frac23..TVD..ft..,Frac23a$Frac23..MD..ft..)
  Frac23 <- Frac23 %>% right_join(Frac23b, by=c("Well"))
}

print("getting average perfs")


# Get Averages: Perfs -----------------------------------------------------



#Well 1
if("Frac1" %in% wellnames){
  
  TVD1<-Frac1 %>%
    group_by(Well,Stage.x)%>%
    summarize(`TVD (ft)` = mean(`TVD (ft)`))
  names(TVD1)[names(TVD1) == 'Stage.x'] <- 'Stage'
  names(TVD1)[names(TVD1) == "TVD (ft)"] <- "TVD (ft)1"
  EAST1<-Frac1 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Easting (ft)` = mean(`Easting (ft)`))
  names(EAST1)[names(EAST1) == 'Stage.x'] <- 'Stage'
  names(EAST1)[names(EAST1) == "Easting (ft)"] <- "Easting (ft)1"
  NORTH1<-Frac1 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Northing (ft)` = mean(`Northing (ft)`))
  names(NORTH1)[names(NORTH1) == 'Stage.x'] <- 'Stage'
  names(NORTH1)[names(NORTH1) == "Northing (ft)"] <- "Northing (ft)1"
  MD1<-Frac1 %>%
    group_by(Well,Stage.x)%>%
    summarize(`MD (ft)` = mean(`MD (ft)`))
  names(MD1)[names(MD1) == 'Stage.x'] <- 'Stage'
  names(MD1)[names(MD1) == "MD (ft)"] <- "MD (ft)1"
  
  TVD1h<-Frac1 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac1a.Frac1..TVD..ft.. = mean(Frac1a.Frac1..TVD..ft..))
  names(TVD1h)[names(TVD1h) == 'Stage.x'] <- 'Stage'
  EAST1h<-Frac1 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac1a.Frac1..Easting..ft.. = mean(Frac1a.Frac1..Easting..ft..))
  names(EAST1h)[names(EAST1h) == 'Stage.x'] <- 'Stage'
  NORTH1h<-Frac1 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac1a.Frac1..Northing..ft.. = mean(Frac1a.Frac1..Northing..ft..))
  names(NORTH1h)[names(NORTH1h) == 'Stage.x'] <- 'Stage'
  MD1h<-Frac1 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac1a.Frac1..MD..ft.. = mean(Frac1a.Frac1..MD..ft..))
  names(MD1h)[names(MD1h) == 'Stage.x'] <- 'Stage'
}

if("Frac2" %in% wellnames){
  
  TVD2<-Frac2 %>%
    group_by(Well,Stage.x)%>%
    summarize(`TVD (ft)` = mean(`TVD (ft)`))
  names(TVD2)[names(TVD2) == 'Stage.x'] <- 'Stage'
  names(TVD2)[names(TVD2) == "TVD (ft)"] <- "TVD (ft)2"
  EAST2<-Frac2 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Easting (ft)` = mean(`Easting (ft)`))
  names(EAST2)[names(EAST2) == 'Stage.x'] <- 'Stage'
  names(EAST2)[names(EAST2) == "Easting (ft)"] <- "Easting (ft)2"
  NORTH2<-Frac2 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Northing (ft)` = mean(`Northing (ft)`))
  names(NORTH2)[names(NORTH2) == 'Stage.x'] <- 'Stage'
  names(NORTH2)[names(NORTH2) == "Northing (ft)"] <- "Northing (ft)2"
  MD2<-Frac2 %>%
    group_by(Well,Stage.x)%>%
    summarize(`MD (ft)` = mean(`MD (ft)`))
  names(MD2)[names(MD2) == 'Stage.x'] <- 'Stage'
  names(MD2)[names(MD2) == "MD (ft)"] <- "MD (ft)2"
  
  TVD2h<-Frac2 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac2a.Frac2..TVD..ft.. = mean(Frac2a.Frac2..TVD..ft..))
  names(TVD2h)[names(TVD2h) == 'Stage.x'] <- 'Stage'
  EAST2h<-Frac2 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac2a.Frac2..Easting..ft.. = mean(Frac2a.Frac2..Easting..ft..))
  names(EAST2h)[names(EAST2h) == 'Stage.x'] <- 'Stage'
  NORTH2h<-Frac2 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac2a.Frac2..Northing..ft.. = mean(Frac2a.Frac2..Northing..ft..))
  names(NORTH2h)[names(NORTH2h) == 'Stage.x'] <- 'Stage'
  MD2h<-Frac2 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac2a.Frac2..MD..ft.. = mean(Frac2a.Frac2..MD..ft..))
  names(MD2h)[names(MD2h) == 'Stage.x'] <- 'Stage'
}

if("Frac3" %in% wellnames){
  
  TVD3<-Frac3 %>%
    group_by(Well,Stage.x)%>%
    summarize(`TVD (ft)` = mean(`TVD (ft)`))
  names(TVD3)[names(TVD3) == 'Stage.x'] <- 'Stage'
  names(TVD3)[names(TVD3) == "TVD (ft)"] <- "TVD (ft)3"
  EAST3<-Frac3 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Easting (ft)` = mean(`Easting (ft)`))
  names(EAST3)[names(EAST3) == 'Stage.x'] <- 'Stage'
  names(EAST3)[names(EAST3) == "Easting (ft)"] <- "Easting (ft)3"
  NORTH3<-Frac3 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Northing (ft)` = mean(`Northing (ft)`))
  names(NORTH3)[names(NORTH3) == 'Stage.x'] <- 'Stage'
  names(NORTH3)[names(NORTH3) == "Northing (ft)"] <- "Northing (ft)3"
  MD3<-Frac3 %>%
    group_by(Well,Stage.x)%>%
    summarize(`MD (ft)` = mean(`MD (ft)`))
  names(MD3)[names(MD3) == 'Stage.x'] <- 'Stage'
  names(MD3)[names(MD3) == "MD (ft)"] <- "MD (ft)3"
  
  TVD3h<-Frac3 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac3a.Frac3..TVD..ft.. = mean(Frac3a.Frac3..TVD..ft..))
  names(TVD3h)[names(TVD3h) == 'Stage.x'] <- 'Stage'
  EAST3h<-Frac3 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac3a.Frac3..Easting..ft.. = mean(Frac3a.Frac3..Easting..ft..))
  names(EAST3h)[names(EAST3h) == 'Stage.x'] <- 'Stage'
  NORTH3h<-Frac3 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac3a.Frac3..Northing..ft.. = mean(Frac3a.Frac3..Northing..ft..))
  names(NORTH3h)[names(NORTH3h) == 'Stage.x'] <- 'Stage'
  MD3h<-Frac3 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac3a.Frac3..MD..ft.. = mean(Frac3a.Frac3..MD..ft..))
  names(MD3h)[names(MD3h) == 'Stage.x'] <- 'Stage'
}

if("Frac4" %in% wellnames){
  
  TVD4<-Frac4 %>%
    group_by(Well,Stage.x)%>%
    summarize(`TVD (ft)` = mean(`TVD (ft)`))
  names(TVD4)[names(TVD4) == 'Stage.x'] <- 'Stage'
  names(TVD4)[names(TVD4) == "TVD (ft)"] <- "TVD (ft)4"
  EAST4<-Frac4 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Easting (ft)` = mean(`Easting (ft)`))
  names(EAST4)[names(EAST4) == 'Stage.x'] <- 'Stage'
  names(EAST4)[names(EAST4) == "Easting (ft)"] <- "Easting (ft)4"
  NORTH4<-Frac4 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Northing (ft)` = mean(`Northing (ft)`))
  names(NORTH4)[names(NORTH4) == 'Stage.x'] <- 'Stage'
  names(NORTH4)[names(NORTH4) == "Northing (ft)"] <- "Northing (ft)4"
  MD4<-Frac4 %>%
    group_by(Well,Stage.x)%>%
    summarize(`MD (ft)` = mean(`MD (ft)`))
  names(MD4)[names(MD4) == 'Stage.x'] <- 'Stage'
  names(MD4)[names(MD4) == "MD (ft)"] <- "MD (ft)4"
  
  TVD4h<-Frac4 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac4a.Frac4..TVD..ft.. = mean(Frac4a.Frac4..TVD..ft..))
  names(TVD4h)[names(TVD4h) == 'Stage.x'] <- 'Stage'
  EAST4h<-Frac4 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac4a.Frac4..Easting..ft.. = mean(Frac4a.Frac4..Easting..ft..))
  names(EAST4h)[names(EAST4h) == 'Stage.x'] <- 'Stage'
  NORTH4h<-Frac4 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac4a.Frac4..Northing..ft.. = mean(Frac4a.Frac4..Northing..ft..))
  names(NORTH4h)[names(NORTH4h) == 'Stage.x'] <- 'Stage'
  MD4h<-Frac4 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac4a.Frac4..MD..ft.. = mean(Frac4a.Frac4..MD..ft..))
  names(MD4h)[names(MD4h) == 'Stage.x'] <- 'Stage'
}

if("Frac5" %in% wellnames){
  
  TVD5<-Frac5 %>%
    group_by(Well,Stage.x)%>%
    summarize(`TVD (ft)` = mean(`TVD (ft)`))
  names(TVD5)[names(TVD5) == 'Stage.x'] <- 'Stage'
  names(TVD5)[names(TVD5) == "TVD (ft)"] <- "TVD (ft)5"
  EAST5<-Frac5 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Easting (ft)` = mean(`Easting (ft)`))
  names(EAST5)[names(EAST5) == 'Stage.x'] <- 'Stage'
  names(EAST5)[names(EAST5) == "Easting (ft)"] <- "Easting (ft)5"
  NORTH5<-Frac5 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Northing (ft)` = mean(`Northing (ft)`))
  names(NORTH5)[names(NORTH5) == 'Stage.x'] <- 'Stage'
  names(NORTH5)[names(NORTH5) == "Northing (ft)"] <- "Northing (ft)5"
  MD5<-Frac5 %>%
    group_by(Well,Stage.x)%>%
    summarize(`MD (ft)` = mean(`MD (ft)`))
  names(MD5)[names(MD5) == 'Stage.x'] <- 'Stage'
  names(MD5)[names(MD5) == "MD (ft)"] <- "MD (ft)5"
  
  TVD5h<-Frac5 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac5a.Frac5..TVD..ft.. = mean(Frac5a.Frac5..TVD..ft..))
  names(TVD5h)[names(TVD5h) == 'Stage.x'] <- 'Stage'
  EAST5h<-Frac5 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac5a.Frac5..Easting..ft.. = mean(Frac5a.Frac5..Easting..ft..))
  names(EAST5h)[names(EAST5h) == 'Stage.x'] <- 'Stage'
  NORTH5h<-Frac5 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac5a.Frac5..Northing..ft.. = mean(Frac5a.Frac5..Northing..ft..))
  names(NORTH5h)[names(NORTH5h) == 'Stage.x'] <- 'Stage'
  MD5h<-Frac5 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac5a.Frac5..MD..ft.. = mean(Frac5a.Frac5..MD..ft..))
  names(MD5h)[names(MD5h) == 'Stage.x'] <- 'Stage'
}

if("Frac6" %in% wellnames){
  
  TVD6<-Frac6 %>%
    group_by(Well,Stage.x)%>%
    summarize(`TVD (ft)` = mean(`TVD (ft)`))
  names(TVD6)[names(TVD6) == 'Stage.x'] <- 'Stage'
  names(TVD6)[names(TVD6) == "TVD (ft)"] <- "TVD (ft)6"
  EAST6<-Frac6 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Easting (ft)` = mean(`Easting (ft)`))
  names(EAST6)[names(EAST6) == 'Stage.x'] <- 'Stage'
  names(EAST6)[names(EAST6) == "Easting (ft)"] <- "Easting (ft)6"
  NORTH6<-Frac6 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Northing (ft)` = mean(`Northing (ft)`))
  names(NORTH6)[names(NORTH6) == 'Stage.x'] <- 'Stage'
  names(NORTH6)[names(NORTH6) == "Northing (ft)"] <- "Northing (ft)6"
  MD6<-Frac6 %>%
    group_by(Well,Stage.x)%>%
    summarize(`MD (ft)` = mean(`MD (ft)`))
  names(MD6)[names(MD6) == 'Stage.x'] <- 'Stage'
  names(MD6)[names(MD6) == "MD (ft)"] <- "MD (ft)6"
  
  TVD6h<-Frac6 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac6a.Frac6..TVD..ft.. = mean(Frac6a.Frac6..TVD..ft..))
  names(TVD6h)[names(TVD6h) == 'Stage.x'] <- 'Stage'
  EAST6h<-Frac6 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac6a.Frac6..Easting..ft.. = mean(Frac6a.Frac6..Easting..ft..))
  names(EAST6h)[names(EAST6h) == 'Stage.x'] <- 'Stage'
  NORTH6h<-Frac6 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac6a.Frac6..Northing..ft.. = mean(Frac6a.Frac6..Northing..ft..))
  names(NORTH6h)[names(NORTH6h) == 'Stage.x'] <- 'Stage'
  MD6h<-Frac6 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac6a.Frac6..MD..ft.. = mean(Frac6a.Frac6..MD..ft..))
  names(MD6h)[names(MD6h) == 'Stage.x'] <- 'Stage'
}

if("Frac7" %in% wellnames){
  
  TVD7<-Frac7 %>%
    group_by(Well,Stage.x)%>%
    summarize(`TVD (ft)` = mean(`TVD (ft)`))
  names(TVD7)[names(TVD7) == 'Stage.x'] <- 'Stage'
  names(TVD7)[names(TVD7) == "TVD (ft)"] <- "TVD (ft)7"
  EAST7<-Frac7 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Easting (ft)` = mean(`Easting (ft)`))
  names(EAST7)[names(EAST7) == 'Stage.x'] <- 'Stage'
  names(EAST7)[names(EAST7) == "Easting (ft)"] <- "Easting (ft)7"
  NORTH7<-Frac7 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Northing (ft)` = mean(`Northing (ft)`))
  names(NORTH7)[names(NORTH7) == 'Stage.x'] <- 'Stage'
  names(NORTH7)[names(NORTH7) == "Northing (ft)"] <- "Northing (ft)7"
  MD7<-Frac7 %>%
    group_by(Well,Stage.x)%>%
    summarize(`MD (ft)` = mean(`MD (ft)`))
  names(MD7)[names(MD7) == 'Stage.x'] <- 'Stage'
  names(MD7)[names(MD7) == "MD (ft)"] <- "MD (ft)7"
  
  TVD7h<-Frac7 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac7a.Frac7..TVD..ft.. = mean(Frac7a.Frac7..TVD..ft..))
  names(TVD7h)[names(TVD7h) == 'Stage.x'] <- 'Stage'
  EAST7h<-Frac7 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac7a.Frac7..Easting..ft.. = mean(Frac7a.Frac7..Easting..ft..))
  names(EAST7h)[names(EAST7h) == 'Stage.x'] <- 'Stage'
  NORTH7h<-Frac7 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac7a.Frac7..Northing..ft.. = mean(Frac7a.Frac7..Northing..ft..))
  names(NORTH7h)[names(NORTH7h) == 'Stage.x'] <- 'Stage'
  MD7h<-Frac7 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac7a.Frac7..MD..ft.. = mean(Frac7a.Frac7..MD..ft..))
  names(MD7h)[names(MD7h) == 'Stage.x'] <- 'Stage'
}

if("Frac8" %in% wellnames){
  
  TVD8<-Frac8 %>%
    group_by(Well,Stage.x)%>%
    summarize(`TVD (ft)` = mean(`TVD (ft)`))
  names(TVD8)[names(TVD8) == 'Stage.x'] <- 'Stage'
  names(TVD8)[names(TVD8) == "TVD (ft)"] <- "TVD (ft)8"
  EAST8<-Frac8 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Easting (ft)` = mean(`Easting (ft)`))
  names(EAST8)[names(EAST8) == 'Stage.x'] <- 'Stage'
  names(EAST8)[names(EAST8) == "Easting (ft)"] <- "Easting (ft)8"
  NORTH8<-Frac8 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Northing (ft)` = mean(`Northing (ft)`))
  names(NORTH8)[names(NORTH8) == 'Stage.x'] <- 'Stage'
  names(NORTH8)[names(NORTH8) == "Northing (ft)"] <- "Northing (ft)8"
  MD8<-Frac8 %>%
    group_by(Well,Stage.x)%>%
    summarize(`MD (ft)` = mean(`MD (ft)`))
  names(MD8)[names(MD8) == 'Stage.x'] <- 'Stage'
  names(MD8)[names(MD8) == "MD (ft)"] <- "MD (ft)8"
  
  TVD8h<-Frac8 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac8a.Frac8..TVD..ft.. = mean(Frac8a.Frac8..TVD..ft..))
  names(TVD8h)[names(TVD8h) == 'Stage.x'] <- 'Stage'
  EAST8h<-Frac8 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac8a.Frac8..Easting..ft.. = mean(Frac8a.Frac8..Easting..ft..))
  names(EAST8h)[names(EAST8h) == 'Stage.x'] <- 'Stage'
  NORTH8h<-Frac8 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac8a.Frac8..Northing..ft.. = mean(Frac8a.Frac8..Northing..ft..))
  names(NORTH8h)[names(NORTH8h) == 'Stage.x'] <- 'Stage'
  MD8h<-Frac8 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac8a.Frac8..MD..ft.. = mean(Frac8a.Frac8..MD..ft..))
  names(MD8h)[names(MD8h) == 'Stage.x'] <- 'Stage'
}

if("Frac9" %in% wellnames){
  
  TVD9<-Frac9 %>%
    group_by(Well,Stage.x)%>%
    summarize(`TVD (ft)` = mean(`TVD (ft)`))
  names(TVD9)[names(TVD9) == 'Stage.x'] <- 'Stage'
  names(TVD9)[names(TVD9) == "TVD (ft)"] <- "TVD (ft)9"
  EAST9<-Frac9 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Easting (ft)` = mean(`Easting (ft)`))
  names(EAST9)[names(EAST9) == 'Stage.x'] <- 'Stage'
  names(EAST9)[names(EAST9) == "Easting (ft)"] <- "Easting (ft)9"
  NORTH9<-Frac9 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Northing (ft)` = mean(`Northing (ft)`))
  names(NORTH9)[names(NORTH9) == 'Stage.x'] <- 'Stage'
  names(NORTH9)[names(NORTH9) == "Northing (ft)"] <- "Northing (ft)9"
  MD9<-Frac9 %>%
    group_by(Well,Stage.x)%>%
    summarize(`MD (ft)` = mean(`MD (ft)`))
  names(MD9)[names(MD9) == 'Stage.x'] <- 'Stage'
  names(MD9)[names(MD9) == "MD (ft)"] <- "MD (ft)9"
  
  TVD9h<-Frac9 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac9a.Frac9..TVD..ft.. = mean(Frac9a.Frac9..TVD..ft..))
  names(TVD9h)[names(TVD9h) == 'Stage.x'] <- 'Stage'
  EAST9h<-Frac9 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac9a.Frac9..Easting..ft.. = mean(Frac9a.Frac9..Easting..ft..))
  names(EAST9h)[names(EAST9h) == 'Stage.x'] <- 'Stage'
  NORTH9h<-Frac9 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac9a.Frac9..Northing..ft.. = mean(Frac9a.Frac9..Northing..ft..))
  names(NORTH9h)[names(NORTH9h) == 'Stage.x'] <- 'Stage'
  MD9h<-Frac9 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac9a.Frac9..MD..ft.. = mean(Frac9a.Frac9..MD..ft..))
  names(MD9h)[names(MD9h) == 'Stage.x'] <- 'Stage'
}

if("Frac10" %in% wellnames){
  
  TVD10<-Frac10 %>%
    group_by(Well,Stage.x)%>%
    summarize(`TVD (ft)` = mean(`TVD (ft)`))
  names(TVD10)[names(TVD10) == 'Stage.x'] <- 'Stage'
  names(TVD10)[names(TVD10) == "TVD (ft)"] <- "TVD (ft)10"
  EAST10<-Frac10 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Easting (ft)` = mean(`Easting (ft)`))
  names(EAST10)[names(EAST10) == 'Stage.x'] <- 'Stage'
  names(EAST10)[names(EAST10) == "Easting (ft)"] <- "Easting (ft)10"
  NORTH10<-Frac10 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Northing (ft)` = mean(`Northing (ft)`))
  names(NORTH10)[names(NORTH10) == 'Stage.x'] <- 'Stage'
  names(NORTH10)[names(NORTH10) == "Northing (ft)"] <- "Northing (ft)10"
  MD10<-Frac10 %>%
    group_by(Well,Stage.x)%>%
    summarize(`MD (ft)` = mean(`MD (ft)`))
  names(MD10)[names(MD10) == 'Stage.x'] <- 'Stage'
  names(MD10)[names(MD10) == "MD (ft)"] <- "MD (ft)10"
  
  TVD10h<-Frac10 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac10a.Frac10..TVD..ft.. = mean(Frac10a.Frac10..TVD..ft..))
  names(TVD10h)[names(TVD10h) == 'Stage.x'] <- 'Stage'
  EAST10h<-Frac10 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac10a.Frac10..Easting..ft.. = mean(Frac10a.Frac10..Easting..ft..))
  names(EAST10h)[names(EAST10h) == 'Stage.x'] <- 'Stage'
  NORTH10h<-Frac10 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac10a.Frac10..Northing..ft.. = mean(Frac10a.Frac10..Northing..ft..))
  names(NORTH10h)[names(NORTH10h) == 'Stage.x'] <- 'Stage'
  MD10h<-Frac10 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac10a.Frac10..MD..ft.. = mean(Frac10a.Frac10..MD..ft..))
  names(MD10h)[names(MD10h) == 'Stage.x'] <- 'Stage'
}

if("Frac11" %in% wellnames){
  
  TVD11<-Frac11 %>%
    group_by(Well,Stage.x)%>%
    summarize(`TVD (ft)` = mean(`TVD (ft)`))
  names(TVD11)[names(TVD11) == 'Stage.x'] <- 'Stage'
  names(TVD11)[names(TVD11) == "TVD (ft)"] <- "TVD (ft)11"
  EAST11<-Frac11 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Easting (ft)` = mean(`Easting (ft)`))
  names(EAST11)[names(EAST11) == 'Stage.x'] <- 'Stage'
  names(EAST11)[names(EAST11) == "Easting (ft)"] <- "Easting (ft)11"
  NORTH11<-Frac11 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Northing (ft)` = mean(`Northing (ft)`))
  names(NORTH11)[names(NORTH11) == 'Stage.x'] <- 'Stage'
  names(NORTH11)[names(NORTH11) == "Northing (ft)"] <- "Northing (ft)11"
  MD11<-Frac11 %>%
    group_by(Well,Stage.x)%>%
    summarize(`MD (ft)` = mean(`MD (ft)`))
  names(MD11)[names(MD11) == 'Stage.x'] <- 'Stage'
  names(MD11)[names(MD11) == "MD (ft)"] <- "MD (ft)11"
  
  TVD11h<-Frac11 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac11a.Frac11..TVD..ft.. = mean(Frac11a.Frac11..TVD..ft..))
  names(TVD11h)[names(TVD11h) == 'Stage.x'] <- 'Stage'
  EAST11h<-Frac11 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac11a.Frac11..Easting..ft.. = mean(Frac11a.Frac11..Easting..ft..))
  names(EAST11h)[names(EAST11h) == 'Stage.x'] <- 'Stage'
  NORTH11h<-Frac11 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac11a.Frac11..Northing..ft.. = mean(Frac11a.Frac11..Northing..ft..))
  names(NORTH11h)[names(NORTH11h) == 'Stage.x'] <- 'Stage'
  MD11h<-Frac11 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac11a.Frac11..MD..ft.. = mean(Frac11a.Frac11..MD..ft..))
  names(MD11h)[names(MD11h) == 'Stage.x'] <- 'Stage'
}

if("Frac12" %in% wellnames){
  
  TVD12<-Frac12 %>%
    group_by(Well,Stage.x)%>%
    summarize(`TVD (ft)` = mean(`TVD (ft)`))
  names(TVD12)[names(TVD12) == 'Stage.x'] <- 'Stage'
  names(TVD12)[names(TVD12) == "TVD (ft)"] <- "TVD (ft)12"
  EAST12<-Frac12 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Easting (ft)` = mean(`Easting (ft)`))
  names(EAST12)[names(EAST12) == 'Stage.x'] <- 'Stage'
  names(EAST12)[names(EAST12) == "Easting (ft)"] <- "Easting (ft)12"
  NORTH12<-Frac12 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Northing (ft)` = mean(`Northing (ft)`))
  names(NORTH12)[names(NORTH12) == 'Stage.x'] <- 'Stage'
  names(NORTH12)[names(NORTH12) == "Northing (ft)"] <- "Northing (ft)12"
  MD12<-Frac12 %>%
    group_by(Well,Stage.x)%>%
    summarize(`MD (ft)` = mean(`MD (ft)`))
  names(MD12)[names(MD12) == 'Stage.x'] <- 'Stage'
  names(MD12)[names(MD12) == "MD (ft)"] <- "MD (ft)12"
  
  TVD12h<-Frac12 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac12a.Frac12..TVD..ft.. = mean(Frac12a.Frac12..TVD..ft..))
  names(TVD12h)[names(TVD12h) == 'Stage.x'] <- 'Stage'
  EAST12h<-Frac12 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac12a.Frac12..Easting..ft.. = mean(Frac12a.Frac12..Easting..ft..))
  names(EAST12h)[names(EAST12h) == 'Stage.x'] <- 'Stage'
  NORTH12h<-Frac12 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac12a.Frac12..Northing..ft.. = mean(Frac12a.Frac12..Northing..ft..))
  names(NORTH12h)[names(NORTH12h) == 'Stage.x'] <- 'Stage'
  MD12h<-Frac12 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac12a.Frac12..MD..ft.. = mean(Frac12a.Frac12..MD..ft..))
  names(MD12h)[names(MD12h) == 'Stage.x'] <- 'Stage'
}

if("Frac13" %in% wellnames){
  
  TVD13<-Frac13 %>%
    group_by(Well,Stage.x)%>%
    summarize(`TVD (ft)` = mean(`TVD (ft)`))
  names(TVD13)[names(TVD13) == 'Stage.x'] <- 'Stage'
  names(TVD13)[names(TVD13) == "TVD (ft)"] <- "TVD (ft)13"
  EAST13<-Frac13 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Easting (ft)` = mean(`Easting (ft)`))
  names(EAST13)[names(EAST13) == 'Stage.x'] <- 'Stage'
  names(EAST13)[names(EAST13) == "Easting (ft)"] <- "Easting (ft)13"
  NORTH13<-Frac13 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Northing (ft)` = mean(`Northing (ft)`))
  names(NORTH13)[names(NORTH13) == 'Stage.x'] <- 'Stage'
  names(NORTH13)[names(NORTH13) == "Northing (ft)"] <- "Northing (ft)13"
  MD13<-Frac13 %>%
    group_by(Well,Stage.x)%>%
    summarize(`MD (ft)` = mean(`MD (ft)`))
  names(MD13)[names(MD13) == 'Stage.x'] <- 'Stage'
  names(MD13)[names(MD13) == "MD (ft)"] <- "MD (ft)13"
  
  TVD13h<-Frac13 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac13a.Frac13..TVD..ft.. = mean(Frac13a.Frac13..TVD..ft..))
  names(TVD13h)[names(TVD13h) == 'Stage.x'] <- 'Stage'
  EAST13h<-Frac13 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac13a.Frac13..Easting..ft.. = mean(Frac13a.Frac13..Easting..ft..))
  names(EAST13h)[names(EAST13h) == 'Stage.x'] <- 'Stage'
  NORTH13h<-Frac13 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac13a.Frac13..Northing..ft.. = mean(Frac13a.Frac13..Northing..ft..))
  names(NORTH13h)[names(NORTH13h) == 'Stage.x'] <- 'Stage'
  MD13h<-Frac13 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac13a.Frac13..MD..ft.. = mean(Frac13a.Frac13..MD..ft..))
  names(MD13h)[names(MD13h) == 'Stage.x'] <- 'Stage'
}

if("Frac14" %in% wellnames){
  
  TVD14<-Frac14 %>%
    group_by(Well,Stage.x)%>%
    summarize(`TVD (ft)` = mean(`TVD (ft)`))
  names(TVD14)[names(TVD14) == 'Stage.x'] <- 'Stage'
  names(TVD14)[names(TVD14) == "TVD (ft)"] <- "TVD (ft)14"
  EAST14<-Frac14 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Easting (ft)` = mean(`Easting (ft)`))
  names(EAST14)[names(EAST14) == 'Stage.x'] <- 'Stage'
  names(EAST14)[names(EAST14) == "Easting (ft)"] <- "Easting (ft)14"
  NORTH14<-Frac14 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Northing (ft)` = mean(`Northing (ft)`))
  names(NORTH14)[names(NORTH14) == 'Stage.x'] <- 'Stage'
  names(NORTH14)[names(NORTH14) == "Northing (ft)"] <- "Northing (ft)14"
  MD14<-Frac14 %>%
    group_by(Well,Stage.x)%>%
    summarize(`MD (ft)` = mean(`MD (ft)`))
  names(MD14)[names(MD14) == 'Stage.x'] <- 'Stage'
  names(MD14)[names(MD14) == "MD (ft)"] <- "MD (ft)14"
  
  TVD14h<-Frac14 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac14a.Frac14..TVD..ft.. = mean(Frac14a.Frac14..TVD..ft..))
  names(TVD14h)[names(TVD14h) == 'Stage.x'] <- 'Stage'
  EAST14h<-Frac14 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac14a.Frac14..Easting..ft.. = mean(Frac14a.Frac14..Easting..ft..))
  names(EAST14h)[names(EAST14h) == 'Stage.x'] <- 'Stage'
  NORTH14h<-Frac14 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac14a.Frac14..Northing..ft.. = mean(Frac14a.Frac14..Northing..ft..))
  names(NORTH14h)[names(NORTH14h) == 'Stage.x'] <- 'Stage'
  MD14h<-Frac14 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac14a.Frac14..MD..ft.. = mean(Frac14a.Frac14..MD..ft..))
  names(MD14h)[names(MD14h) == 'Stage.x'] <- 'Stage'
}

if("Frac15" %in% wellnames){
  
  TVD15<-Frac15 %>%
    group_by(Well,Stage.x)%>%
    summarize(`TVD (ft)` = mean(`TVD (ft)`))
  names(TVD15)[names(TVD15) == 'Stage.x'] <- 'Stage'
  names(TVD15)[names(TVD15) == "TVD (ft)"] <- "TVD (ft)15"
  EAST15<-Frac15 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Easting (ft)` = mean(`Easting (ft)`))
  names(EAST15)[names(EAST15) == 'Stage.x'] <- 'Stage'
  names(EAST15)[names(EAST15) == "Easting (ft)"] <- "Easting (ft)15"
  NORTH15<-Frac15 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Northing (ft)` = mean(`Northing (ft)`))
  names(NORTH15)[names(NORTH15) == 'Stage.x'] <- 'Stage'
  names(NORTH15)[names(NORTH15) == "Northing (ft)"] <- "Northing (ft)15"
  MD15<-Frac15 %>%
    group_by(Well,Stage.x)%>%
    summarize(`MD (ft)` = mean(`MD (ft)`))
  names(MD15)[names(MD15) == 'Stage.x'] <- 'Stage'
  names(MD15)[names(MD15) == "MD (ft)"] <- "MD (ft)15"
  
  TVD15h<-Frac15 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac15a.Frac15..TVD..ft.. = mean(Frac15a.Frac15..TVD..ft..))
  names(TVD15h)[names(TVD15h) == 'Stage.x'] <- 'Stage'
  EAST15h<-Frac15 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac15a.Frac15..Easting..ft.. = mean(Frac15a.Frac15..Easting..ft..))
  names(EAST15h)[names(EAST15h) == 'Stage.x'] <- 'Stage'
  NORTH15h<-Frac15 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac15a.Frac15..Northing..ft.. = mean(Frac15a.Frac15..Northing..ft..))
  names(NORTH15h)[names(NORTH15h) == 'Stage.x'] <- 'Stage'
  MD15h<-Frac15 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac15a.Frac15..MD..ft.. = mean(Frac15a.Frac15..MD..ft..))
  names(MD15h)[names(MD15h) == 'Stage.x'] <- 'Stage'
}

if("Frac16" %in% wellnames){
  
  TVD16<-Frac16 %>%
    group_by(Well,Stage.x)%>%
    summarize(`TVD (ft)` = mean(`TVD (ft)`))
  names(TVD16)[names(TVD16) == 'Stage.x'] <- 'Stage'
  names(TVD16)[names(TVD16) == "TVD (ft)"] <- "TVD (ft)16"
  EAST16<-Frac16 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Easting (ft)` = mean(`Easting (ft)`))
  names(EAST16)[names(EAST16) == 'Stage.x'] <- 'Stage'
  names(EAST16)[names(EAST16) == "Easting (ft)"] <- "Easting (ft)16"
  NORTH16<-Frac16 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Northing (ft)` = mean(`Northing (ft)`))
  names(NORTH16)[names(NORTH16) == 'Stage.x'] <- 'Stage'
  names(NORTH16)[names(NORTH16) == "Northing (ft)"] <- "Northing (ft)16"
  MD16<-Frac16 %>%
    group_by(Well,Stage.x)%>%
    summarize(`MD (ft)` = mean(`MD (ft)`))
  names(MD16)[names(MD16) == 'Stage.x'] <- 'Stage'
  names(MD16)[names(MD16) == "MD (ft)"] <- "MD (ft)16"
  
  TVD16h<-Frac16 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac16a.Frac16..TVD..ft.. = mean(Frac16a.Frac16..TVD..ft..))
  names(TVD16h)[names(TVD16h) == 'Stage.x'] <- 'Stage'
  EAST16h<-Frac16 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac16a.Frac16..Easting..ft.. = mean(Frac16a.Frac16..Easting..ft..))
  names(EAST16h)[names(EAST16h) == 'Stage.x'] <- 'Stage'
  NORTH16h<-Frac16 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac16a.Frac16..Northing..ft.. = mean(Frac16a.Frac16..Northing..ft..))
  names(NORTH16h)[names(NORTH16h) == 'Stage.x'] <- 'Stage'
  MD16h<-Frac16 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac16a.Frac16..MD..ft.. = mean(Frac16a.Frac16..MD..ft..))
  names(MD16h)[names(MD16h) == 'Stage.x'] <- 'Stage'
}

if("Frac17" %in% wellnames){
  
  TVD17<-Frac17 %>%
    group_by(Well,Stage.x)%>%
    summarize(`TVD (ft)` = mean(`TVD (ft)`))
  names(TVD17)[names(TVD17) == 'Stage.x'] <- 'Stage'
  names(TVD17)[names(TVD17) == "TVD (ft)"] <- "TVD (ft)17"
  EAST17<-Frac17 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Easting (ft)` = mean(`Easting (ft)`))
  names(EAST17)[names(EAST17) == 'Stage.x'] <- 'Stage'
  names(EAST17)[names(EAST17) == "Easting (ft)"] <- "Easting (ft)17"
  NORTH17<-Frac17 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Northing (ft)` = mean(`Northing (ft)`))
  names(NORTH17)[names(NORTH17) == 'Stage.x'] <- 'Stage'
  names(NORTH17)[names(NORTH17) == "Northing (ft)"] <- "Northing (ft)17"
  MD17<-Frac17 %>%
    group_by(Well,Stage.x)%>%
    summarize(`MD (ft)` = mean(`MD (ft)`))
  names(MD17)[names(MD17) == 'Stage.x'] <- 'Stage'
  names(MD17)[names(MD17) == "MD (ft)"] <- "MD (ft)17"
  
  TVD17h<-Frac17 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac17a.Frac17..TVD..ft.. = mean(Frac17a.Frac17..TVD..ft..))
  names(TVD17h)[names(TVD17h) == 'Stage.x'] <- 'Stage'
  EAST17h<-Frac17 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac17a.Frac17..Easting..ft.. = mean(Frac17a.Frac17..Easting..ft..))
  names(EAST17h)[names(EAST17h) == 'Stage.x'] <- 'Stage'
  NORTH17h<-Frac17 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac17a.Frac17..Northing..ft.. = mean(Frac17a.Frac17..Northing..ft..))
  names(NORTH17h)[names(NORTH17h) == 'Stage.x'] <- 'Stage'
  MD17h<-Frac17 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac17a.Frac17..MD..ft.. = mean(Frac17a.Frac17..MD..ft..))
  names(MD17h)[names(MD17h) == 'Stage.x'] <- 'Stage'
}

if("Frac18" %in% wellnames){
  
  TVD18<-Frac18 %>%
    group_by(Well,Stage.x)%>%
    summarize(`TVD (ft)` = mean(`TVD (ft)`))
  names(TVD18)[names(TVD18) == 'Stage.x'] <- 'Stage'
  names(TVD18)[names(TVD18) == "TVD (ft)"] <- "TVD (ft)18"
  EAST18<-Frac18 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Easting (ft)` = mean(`Easting (ft)`))
  names(EAST18)[names(EAST18) == 'Stage.x'] <- 'Stage'
  names(EAST18)[names(EAST18) == "Easting (ft)"] <- "Easting (ft)18"
  NORTH18<-Frac18 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Northing (ft)` = mean(`Northing (ft)`))
  names(NORTH18)[names(NORTH18) == 'Stage.x'] <- 'Stage'
  names(NORTH18)[names(NORTH18) == "Northing (ft)"] <- "Northing (ft)18"
  MD18<-Frac18 %>%
    group_by(Well,Stage.x)%>%
    summarize(`MD (ft)` = mean(`MD (ft)`))
  names(MD18)[names(MD18) == 'Stage.x'] <- 'Stage'
  names(MD18)[names(MD18) == "MD (ft)"] <- "MD (ft)18"
  
  TVD18h<-Frac18 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac18a.Frac18..TVD..ft.. = mean(Frac18a.Frac18..TVD..ft..))
  names(TVD18h)[names(TVD18h) == 'Stage.x'] <- 'Stage'
  EAST18h<-Frac18 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac18a.Frac18..Easting..ft.. = mean(Frac18a.Frac18..Easting..ft..))
  names(EAST18h)[names(EAST18h) == 'Stage.x'] <- 'Stage'
  NORTH18h<-Frac18 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac18a.Frac18..Northing..ft.. = mean(Frac18a.Frac18..Northing..ft..))
  names(NORTH18h)[names(NORTH18h) == 'Stage.x'] <- 'Stage'
  MD18h<-Frac18 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac18a.Frac18..MD..ft.. = mean(Frac18a.Frac18..MD..ft..))
  names(MD18h)[names(MD18h) == 'Stage.x'] <- 'Stage'
}

if("Frac19" %in% wellnames){
  
  TVD19<-Frac19 %>%
    group_by(Well,Stage.x)%>%
    summarize(`TVD (ft)` = mean(`TVD (ft)`))
  names(TVD19)[names(TVD19) == 'Stage.x'] <- 'Stage'
  names(TVD19)[names(TVD19) == "TVD (ft)"] <- "TVD (ft)19"
  EAST19<-Frac19 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Easting (ft)` = mean(`Easting (ft)`))
  names(EAST19)[names(EAST19) == 'Stage.x'] <- 'Stage'
  names(EAST19)[names(EAST19) == "Easting (ft)"] <- "Easting (ft)19"
  NORTH19<-Frac19 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Northing (ft)` = mean(`Northing (ft)`))
  names(NORTH19)[names(NORTH19) == 'Stage.x'] <- 'Stage'
  names(NORTH19)[names(NORTH19) == "Northing (ft)"] <- "Northing (ft)19"
  MD19<-Frac19 %>%
    group_by(Well,Stage.x)%>%
    summarize(`MD (ft)` = mean(`MD (ft)`))
  names(MD19)[names(MD19) == 'Stage.x'] <- 'Stage'
  names(MD19)[names(MD19) == "MD (ft)"] <- "MD (ft)19"
  
  TVD19h<-Frac19 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac19a.Frac19..TVD..ft.. = mean(Frac19a.Frac19..TVD..ft..))
  names(TVD19h)[names(TVD19h) == 'Stage.x'] <- 'Stage'
  EAST19h<-Frac19 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac19a.Frac19..Easting..ft.. = mean(Frac19a.Frac19..Easting..ft..))
  names(EAST19h)[names(EAST19h) == 'Stage.x'] <- 'Stage'
  NORTH19h<-Frac19 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac19a.Frac19..Northing..ft.. = mean(Frac19a.Frac19..Northing..ft..))
  names(NORTH19h)[names(NORTH19h) == 'Stage.x'] <- 'Stage'
  MD19h<-Frac19 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac19a.Frac19..MD..ft.. = mean(Frac19a.Frac19..MD..ft..))
  names(MD19h)[names(MD19h) == 'Stage.x'] <- 'Stage'
}

if("Frac20" %in% wellnames){
  
  TVD20<-Frac20 %>%
    group_by(Well,Stage.x)%>%
    summarize(`TVD (ft)` = mean(`TVD (ft)`))
  names(TVD20)[names(TVD20) == 'Stage.x'] <- 'Stage'
  names(TVD20)[names(TVD20) == "TVD (ft)"] <- "TVD (ft)20"
  EAST20<-Frac20 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Easting (ft)` = mean(`Easting (ft)`))
  names(EAST20)[names(EAST20) == 'Stage.x'] <- 'Stage'
  names(EAST20)[names(EAST20) == "Easting (ft)"] <- "Easting (ft)20"
  NORTH20<-Frac20 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Northing (ft)` = mean(`Northing (ft)`))
  names(NORTH20)[names(NORTH20) == 'Stage.x'] <- 'Stage'
  names(NORTH20)[names(NORTH20) == "Northing (ft)"] <- "Northing (ft)20"
  MD20<-Frac20 %>%
    group_by(Well,Stage.x)%>%
    summarize(`MD (ft)` = mean(`MD (ft)`))
  names(MD20)[names(MD20) == 'Stage.x'] <- 'Stage'
  names(MD20)[names(MD20) == "MD (ft)"] <- "MD (ft)20"
  
  TVD20h<-Frac20 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac20a.Frac20..TVD..ft.. = mean(Frac20a.Frac20..TVD..ft..))
  names(TVD20h)[names(TVD20h) == 'Stage.x'] <- 'Stage'
  EAST20h<-Frac20 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac20a.Frac20..Easting..ft.. = mean(Frac20a.Frac20..Easting..ft..))
  names(EAST20h)[names(EAST20h) == 'Stage.x'] <- 'Stage'
  NORTH20h<-Frac20 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac20a.Frac20..Northing..ft.. = mean(Frac20a.Frac20..Northing..ft..))
  names(NORTH20h)[names(NORTH20h) == 'Stage.x'] <- 'Stage'
  MD20h<-Frac20 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac20a.Frac20..MD..ft.. = mean(Frac20a.Frac20..MD..ft..))
  names(MD20h)[names(MD20h) == 'Stage.x'] <- 'Stage'
}

if("Frac21" %in% wellnames){
  
  TVD21<-Frac21 %>%
    group_by(Well,Stage.x)%>%
    summarize(`TVD (ft)` = mean(`TVD (ft)`))
  names(TVD21)[names(TVD21) == 'Stage.x'] <- 'Stage'
  names(TVD21)[names(TVD21) == "TVD (ft)"] <- "TVD (ft)21"
  EAST21<-Frac21 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Easting (ft)` = mean(`Easting (ft)`))
  names(EAST21)[names(EAST21) == 'Stage.x'] <- 'Stage'
  names(EAST21)[names(EAST21) == "Easting (ft)"] <- "Easting (ft)21"
  NORTH21<-Frac21 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Northing (ft)` = mean(`Northing (ft)`))
  names(NORTH21)[names(NORTH21) == 'Stage.x'] <- 'Stage'
  names(NORTH21)[names(NORTH21) == "Northing (ft)"] <- "Northing (ft)21"
  MD21<-Frac21 %>%
    group_by(Well,Stage.x)%>%
    summarize(`MD (ft)` = mean(`MD (ft)`))
  names(MD21)[names(MD21) == 'Stage.x'] <- 'Stage'
  names(MD21)[names(MD21) == "MD (ft)"] <- "MD (ft)21"
  
  TVD21h<-Frac21 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac21a.Frac21..TVD..ft.. = mean(Frac21a.Frac21..TVD..ft..))
  names(TVD21h)[names(TVD21h) == 'Stage.x'] <- 'Stage'
  EAST21h<-Frac21 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac21a.Frac21..Easting..ft.. = mean(Frac21a.Frac21..Easting..ft..))
  names(EAST21h)[names(EAST21h) == 'Stage.x'] <- 'Stage'
  NORTH21h<-Frac21 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac21a.Frac21..Northing..ft.. = mean(Frac21a.Frac21..Northing..ft..))
  names(NORTH21h)[names(NORTH21h) == 'Stage.x'] <- 'Stage'
  MD21h<-Frac21 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac21a.Frac21..MD..ft.. = mean(Frac21a.Frac21..MD..ft..))
  names(MD21h)[names(MD21h) == 'Stage.x'] <- 'Stage'
}

if("Frac22" %in% wellnames){
  
  TVD22<-Frac22 %>%
    group_by(Well,Stage.x)%>%
    summarize(`TVD (ft)` = mean(`TVD (ft)`))
  names(TVD22)[names(TVD22) == 'Stage.x'] <- 'Stage'
  names(TVD22)[names(TVD22) == "TVD (ft)"] <- "TVD (ft)22"
  EAST22<-Frac22 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Easting (ft)` = mean(`Easting (ft)`))
  names(EAST22)[names(EAST22) == 'Stage.x'] <- 'Stage'
  names(EAST22)[names(EAST22) == "Easting (ft)"] <- "Easting (ft)22"
  NORTH22<-Frac22 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Northing (ft)` = mean(`Northing (ft)`))
  names(NORTH22)[names(NORTH22) == 'Stage.x'] <- 'Stage'
  names(NORTH22)[names(NORTH22) == "Northing (ft)"] <- "Northing (ft)22"
  MD22<-Frac22 %>%
    group_by(Well,Stage.x)%>%
    summarize(`MD (ft)` = mean(`MD (ft)`))
  names(MD22)[names(MD22) == 'Stage.x'] <- 'Stage'
  names(MD22)[names(MD22) == "MD (ft)"] <- "MD (ft)22"
  
  TVD22h<-Frac22 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac22a.Frac22..TVD..ft.. = mean(Frac22a.Frac22..TVD..ft..))
  names(TVD22h)[names(TVD22h) == 'Stage.x'] <- 'Stage'
  EAST22h<-Frac22 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac22a.Frac22..Easting..ft.. = mean(Frac22a.Frac22..Easting..ft..))
  names(EAST22h)[names(EAST22h) == 'Stage.x'] <- 'Stage'
  NORTH22h<-Frac22 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac22a.Frac22..Northing..ft.. = mean(Frac22a.Frac22..Northing..ft..))
  names(NORTH22h)[names(NORTH22h) == 'Stage.x'] <- 'Stage'
  MD22h<-Frac22 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac22a.Frac22..MD..ft.. = mean(Frac22a.Frac22..MD..ft..))
  names(MD22h)[names(MD22h) == 'Stage.x'] <- 'Stage'
}

if("Frac23" %in% wellnames){
  
  TVD23<-Frac23 %>%
    group_by(Well,Stage.x)%>%
    summarize(`TVD (ft)` = mean(`TVD (ft)`))
  names(TVD23)[names(TVD23) == 'Stage.x'] <- 'Stage'
  names(TVD23)[names(TVD23) == "TVD (ft)"] <- "TVD (ft)23"
  EAST23<-Frac23 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Easting (ft)` = mean(`Easting (ft)`))
  names(EAST23)[names(EAST23) == 'Stage.x'] <- 'Stage'
  names(EAST23)[names(EAST23) == "Easting (ft)"] <- "Easting (ft)23"
  NORTH23<-Frac23 %>%
    group_by(Well,Stage.x)%>%
    summarize(`Northing (ft)` = mean(`Northing (ft)`))
  names(NORTH23)[names(NORTH23) == 'Stage.x'] <- 'Stage'
  names(NORTH23)[names(NORTH23) == "Northing (ft)"] <- "Northing (ft)23"
  MD23<-Frac23 %>%
    group_by(Well,Stage.x)%>%
    summarize(`MD (ft)` = mean(`MD (ft)`))
  names(MD23)[names(MD23) == 'Stage.x'] <- 'Stage'
  names(MD23)[names(MD23) == "MD (ft)"] <- "MD (ft)23"
  
  TVD23h<-Frac23 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac23a.Frac23..TVD..ft.. = mean(Frac23a.Frac23..TVD..ft..))
  names(TVD23h)[names(TVD23h) == 'Stage.x'] <- 'Stage'
  EAST23h<-Frac23 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac23a.Frac23..Easting..ft.. = mean(Frac23a.Frac23..Easting..ft..))
  names(EAST23h)[names(EAST23h) == 'Stage.x'] <- 'Stage'
  NORTH23h<-Frac23 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac23a.Frac23..Northing..ft.. = mean(Frac23a.Frac23..Northing..ft..))
  names(NORTH23h)[names(NORTH23h) == 'Stage.x'] <- 'Stage'
  MD23h<-Frac23 %>%
    group_by(Well,Stage.x)%>%
    summarize(Frac23a.Frac23..MD..ft.. = mean(Frac23a.Frac23..MD..ft..))
  names(MD23h)[names(MD23h) == 'Stage.x'] <- 'Stage'
}



# Get Perf Distances ------------------------------------------------------



print("Getting Perf Distances")
if("Frac1" %in% wellnames){
  colnames(Frac1)[7] <- "Stage"
  Frac1 <- Frac1  %>% mutate_at('Stage',as.numeric)
  p1 <- MSM_data %>% right_join(EAST1, by=c("Well","Stage"))
  p2 <- p1 %>% right_join(NORTH1, by=c("Well","Stage"))
  p3 <- p2 %>% right_join(TVD1, by=c("Well","Stage"))
  p4 <- p3 %>% right_join(MD1, by=c("Well","Stage"))
  e1 <- p4$`Easting ft (Abs)`
  e2 <- p4$`Easting (ft)1`
  n1 <- p4$`Northing ft (Abs)`
  n2 <- p4$`Northing (ft)1`
  d1 <- p4$`Depth ft (TVD)`
  d2 <- p4$`TVD (ft)1`
  p5 <- p4 %>% mutate("Distance From Perfs" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p6 <- p5 %>% right_join(EAST1h, by=c("Well","Stage"))
  p7 <- p6 %>% right_join(NORTH1h, by=c("Well","Stage"))
  p8 <- p7 %>% right_join(TVD1h, by=c("Well","Stage"))
  p9 <- p8 %>% right_join(MD1h, by=c("Well","Stage"))
  e1 <- p9$`Easting ft (Abs)`
  e2 <- p9$Frac1a.Frac1..Easting..ft..
  n1 <- p9$`Northing ft (Abs)`
  n2 <- p9$Frac1a.Frac1..Northing..ft..
  d1 <- p9$`Depth ft (TVD)`
  d2 <- p9$Frac1a.Frac1..TVD..ft..
  p10 <- p9 %>% mutate("Distance From Heel" = sqrt(((e1- e2)^2)+((n1- n2)^2)+((d1- d2)^2)))
  #Single Treatment
  perf_heel <- p10 %>% arrange(p10$`Event File`)
  
}
#write.csv(p4, file = "p4.csv")
#perf_heel<-p10
if("Frac2" %in% wellnames){
  colnames(Frac2)[7] <- "Stage"
  p11 <- MSM_data %>% right_join(EAST2, by=c("Well","Stage"))
  p12 <- p11 %>% right_join(NORTH2, by=c("Well","Stage"))
  p13 <- p12 %>% right_join(TVD2, by=c("Well","Stage"))
  p14 <- p13 %>% right_join(MD2, by=c("Well","Stage")) 
  e1 <- p14$`Easting ft (Abs)`
  e2 <- p14$`Easting (ft)2`
  n1 <- p14$`Northing ft (Abs)`
  n2 <- p14$`Northing (ft)2`
  d1 <- p14$`Depth ft (TVD)`
  d2 <- p14$`TVD (ft)2`
  p15 <- p14 %>% mutate("Distance From Perfs" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p16 <- p15 %>% right_join(EAST2h, by=c("Well","Stage"))
  p17 <- p16 %>% right_join(NORTH2h, by=c("Well","Stage"))
  p18 <- p17 %>% right_join(TVD2h, by=c("Well","Stage"))
  p19 <- p18 %>% right_join(MD2h, by=c("Well","Stage"))
  e1 <- p19$`Easting ft (Abs)`
  e2 <- p19$Frac2a.Frac2..Easting..ft..
  n1 <- p19$`Northing ft (Abs)`
  n2 <- p19$Frac2a.Frac2..Northing..ft..
  d1 <- p19$`Depth ft (TVD)`
  d2 <- p19$Frac2a.Frac2..TVD..ft..
  p20 <- p19 %>% mutate("Distance From Heel" = sqrt(((e1- e2)^2)+((n1- n2)^2)+((d1- d2)^2)))
  #Two Wells
  p20 <- p20 %>% arrange(p20$`Event File`)
  perf_heel<-merge(p10,p20, all=TRUE)

  
}
if("Frac3" %in% wellnames){
  colnames(Frac3)[7] <- "Stage"
  p21 <- MSM_data %>% right_join(EAST3, by=c("Well","Stage"))
  p22 <- p21 %>% right_join(NORTH3, by=c("Well","Stage"))
  p23 <- p22 %>% right_join(TVD3, by=c("Well","Stage"))
  p24 <- p23 %>% right_join(MD3, by=c("Well","Stage"))
  e1 <- p24$`Easting ft (Abs)`
  e2 <- p24$`Easting (ft)3`
  n1 <- p24$`Northing ft (Abs)`
  n2 <- p24$`Northing (ft)3`
  d1 <- p24$`Depth ft (TVD)`
  d2 <- p24$`TVD (ft)3`
  p25 <- p24 %>% mutate("Distance From Perfs" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p26 <- p25 %>% right_join(EAST3h, by=c("Well","Stage"))
  p27 <- p26 %>% right_join(NORTH3h, by=c("Well","Stage"))
  p28 <- p27 %>% right_join(TVD3h, by=c("Well","Stage"))
  p29 <- p28 %>% right_join(MD3h, by=c("Well","Stage"))
  e1 <- p29$`Easting ft (Abs)`
  e2 <- p29$Frac3a.Frac3..Easting..ft..
  n1 <- p29$`Northing ft (Abs)`
  n2 <- p29$Frac3a.Frac3..Northing..ft..
  d1 <- p29$`Depth ft (TVD)`
  d2 <- p29$Frac3a.Frac3..TVD..ft..
  p30 <- p29 %>% mutate("Distance From Heel" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p30 <- p30 %>% arrange(p30$`Event File`)
  perf_heel<-merge(perf_heel,p30, all=TRUE)
}
if("Frac4" %in% wellnames){
  colnames(Frac4)[4] <- "Stage"
  p31 <- MSM_data %>% right_join(EAST4, by=c("Well","Stage"))
  p32 <- p31 %>% right_join(NORTH4, by=c("Well","Stage"))
  p33 <- p32 %>% right_join(TVD4, by=c("Well","Stage"))
  p34 <- p33 %>% right_join(MD4, by=c("Well","Stage"))
  e1 <- p34$`Easting ft (Abs)`
  e2 <- p34$`Easting (ft)4`
  n1 <- p34$`Northing ft (Abs)`
  n2 <- p34$`Northing (ft)4`
  d1 <- p34$`Depth ft (TVD)`
  d2 <- p34$`TVD (ft)4`
  p35 <- p34 %>% mutate("Distance From Perfs" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p36 <- p35 %>% right_join(EAST4h, by=c("Well","Stage"))
  p37 <- p36 %>% right_join(NORTH4h, by=c("Well","Stage"))
  p38 <- p37 %>% right_join(TVD4h, by=c("Well","Stage"))
  p39 <- p38 %>% right_join(MD4h, by=c("Well","Stage"))
  e1 <- p39$`Easting ft (Abs)`
  e2 <- p39$Frac4a.Frac4..Easting..ft..
  n1 <- p39$`Northing ft (Abs)`
  n2 <- p39$Frac4a.Frac4..Northing..ft..
  d1 <- p39$`Depth ft (TVD)`
  d2 <- p39$Frac4a.Frac4..TVD..ft..
  p40 <- p39 %>% mutate("Distance From Heel" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p40 <- p40 %>% arrange(p40$`Event File`)
  perf_heel<-merge(perf_heel,p40, all=TRUE)
}
if("Frac5" %in% wellnames){
  p41 <- MSM_data %>% right_join(EAST5, by=c("Well","Stage"))
  p42 <- p41 %>% right_join(NORTH5, by=c("Well","Stage"))
  p43 <- p42 %>% right_join(TVD5, by=c("Well","Stage"))
  p44 <- p43 %>% right_join(MD5, by=c("Well","Stage"))
  e1 <- p44$`Easting ft (Abs)`
  e2 <- p44$`Easting (ft)5`
  n1 <- p44$`Northing ft (Abs)`
  n2 <- p44$`Northing (ft)5`
  d1 <- p44$`Depth ft (TVD)`
  d2 <- p44$`TVD (ft)5`
  p45 <- p44 %>% mutate("Distance From Perfs" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p46 <- p45 %>% right_join(EAST5h, by=c("Well","Stage"))
  p47 <- p46 %>% right_join(NORTH5h, by=c("Well","Stage"))
  p48 <- p47 %>% right_join(TVD5h, by=c("Well","Stage"))
  p49 <- p48 %>% right_join(MD5h, by=c("Well","Stage"))
  e1 <- p49$`Easting ft (Abs)`
  e2 <- p49$Frac5a.Frac5..Easting..ft..
  n1 <- p49$`Northing ft (Abs)`
  n2 <- p49$Frac5a.Frac5..Northing..ft..
  d1 <- p49$`Depth ft (TVD)`
  d2 <- p49$Frac5a.Frac5..TVD..ft..
  p50 <- p49 %>% mutate("Distance From Heel" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p50 <- p50 %>% arrange(p50$`Event File`)
  perf_heel<-merge(perf_heel,p50, all=TRUE)
}
if("Frac6" %in% wellnames){
  p51 <- MSM_data %>% right_join(EAST6, by=c("Well","Stage"))
  p52 <- p51 %>% right_join(NORTH6, by=c("Well","Stage"))
  p53 <- p52 %>% right_join(TVD6, by=c("Well","Stage"))
  p54 <- p53 %>% right_join(MD6, by=c("Well","Stage"))
  e1 <- p54$`Easting ft (Abs)`
  e2 <- p54$`Easting (ft)6`
  n1 <- p54$`Northing ft (Abs)`
  n2 <- p54$`Northing (ft)6`
  d1 <- p54$`Depth ft (TVD)`
  d2 <- p54$`TVD (ft)6`
  p55 <- p54 %>% mutate("Distance From Perfs" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p56 <- p55 %>% right_join(EAST6h, by=c("Well","Stage"))
  p57 <- p56 %>% right_join(NORTH6h, by=c("Well","Stage"))
  p58 <- p57 %>% right_join(TVD6h, by=c("Well","Stage"))
  p59 <- p58 %>% right_join(MD6h, by=c("Well","Stage"))
  e1 <- p59$`Easting ft (Abs)`
  e2 <- p59$Frac6a.Frac6..Easting..ft..
  n1 <- p59$`Northing ft (Abs)`
  n2 <- p59$Frac6a.Frac6..Northing..ft..
  d1 <- p59$`Depth ft (TVD)`
  d2 <- p59$Frac6a.Frac6..TVD..ft..
  p60 <- p59 %>% mutate("Distance From Heel" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p60 <- p60 %>% arrange(p60$`Event File`)
  perf_heel<-merge(perf_heel,p60, all=TRUE)
}
if("Frac7" %in% wellnames){
  p61 <- MSM_data %>% right_join(EAST7, by=c("Well","Stage"))
  p62 <- p61 %>% right_join(NORTH7, by=c("Well","Stage"))
  p63 <- p62 %>% right_join(TVD7, by=c("Well","Stage"))
  p64 <- p63 %>% right_join(MD7, by=c("Well","Stage"))
  e1 <- p64$`Easting ft (Abs)`
  e2 <- p64$`Easting (ft)7`
  n1 <- p64$`Northing ft (Abs)`
  n2 <- p64$`Northing (ft)7`
  d1 <- p64$`Depth ft (TVD)`
  d2 <- p64$`TVD (ft)7`
  p65 <- p64 %>% mutate("Distance From Perfs" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p66 <- p65 %>% right_join(EAST7h, by=c("Well","Stage"))
  p67 <- p66 %>% right_join(NORTH7h, by=c("Well","Stage"))
  p68 <- p67 %>% right_join(TVD7h, by=c("Well","Stage"))
  p69 <- p68 %>% right_join(MD7h, by=c("Well","Stage"))
  e1 <- p69$`Easting ft (Abs)`
  e2 <- p69$Frac7a.Frac7..Easting..ft..
  n1 <- p69$`Northing ft (Abs)`
  n2 <- p69$Frac7a.Frac7..Northing..ft..
  d1 <- p69$`Depth ft (TVD)`
  d2 <- p69$Frac7a.Frac7..TVD..ft..
  p70 <- p69 %>% mutate("Distance From Heel" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p70 <- p70 %>% arrange(p70$`Event File`)
  perf_heel<-merge(perf_heel,p70, all=TRUE)
}
if("Frac8" %in% wellnames){
  p71 <- MSM_data %>% right_join(EAST8, by=c("Well","Stage"))
  p72 <- p71 %>% right_join(NORTH8, by=c("Well","Stage"))
  p73 <- p72 %>% right_join(TVD8, by=c("Well","Stage"))
  p74 <- p73 %>% right_join(MD8, by=c("Well","Stage"))
  e1 <- p74$`Easting ft (Abs)`
  e2 <- p74$`Easting (ft)8`
  n1 <- p74$`Northing ft (Abs)`
  n2 <- p74$`Northing (ft)8`
  d1 <- p74$`Depth ft (TVD)`
  d2 <- p74$`TVD (ft)8`
  p75 <- p74 %>% mutate("Distance From Perfs" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p76 <- p75 %>% right_join(EAST8h, by=c("Well","Stage"))
  p77 <- p76 %>% right_join(NORTH8h, by=c("Well","Stage"))
  p78 <- p77 %>% right_join(TVD8h, by=c("Well","Stage"))
  p79 <- p78 %>% right_join(MD8h, by=c("Well","Stage"))
  e1 <- p79$`Easting ft (Abs)`
  e2 <- p79$Frac8a.Frac8..Easting..ft..
  n1 <- p79$`Northing ft (Abs)`
  n2 <- p79$Frac8a.Frac8..Northing..ft..
  d1 <- p79$`Depth ft (TVD)`
  d2 <- p79$Frac8a.Frac8..TVD..ft..
  p80 <- p79 %>% mutate("Distance From Heel" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p80 <- p80 %>% arrange(p80$`Event File`)
  perf_heel<-merge(perf_heel,p80, all=TRUE)
}
if("Frac9" %in% wellnames){
  p81 <- MSM_data %>% right_join(EAST9, by=c("Well","Stage"))
  p82 <- p81 %>% right_join(NORTH9, by=c("Well","Stage"))
  p83 <- p82 %>% right_join(TVD9, by=c("Well","Stage"))
  p84 <- p83 %>% right_join(MD9, by=c("Well","Stage"))
  e1 <- p84$`Easting ft (Abs)`
  e2 <- p84$`Easting (ft)9`
  n1 <- p84$`Northing ft (Abs)`
  n2 <- p84$`Northing (ft)9`
  d1 <- p84$`Depth ft (TVD)`
  d2 <- p84$`TVD (ft)9`
  p85 <- p84 %>% mutate("Distance From Perfs" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p86 <- p85 %>% right_join(EAST9h, by=c("Well","Stage"))
  p87 <- p86 %>% right_join(NORTH9h, by=c("Well","Stage"))
  p88 <- p87 %>% right_join(TVD9h, by=c("Well","Stage"))
  p89 <- p88 %>% right_join(MD9h, by=c("Well","Stage"))
  e1 <- p89$`Easting ft (Abs)`
  e2 <- p89$Frac9a.Frac9..Easting..ft..
  n1 <- p89$`Northing ft (Abs)`
  n2 <- p89$Frac9a.Frac9..Northing..ft..
  d1 <- p89$`Depth ft (TVD)`
  d2 <- p89$Frac9a.Frac9..TVD..ft..
  p90 <- p89 %>% mutate("Distance From Heel" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p90 <- p90 %>% arrange(p90$`Event File`)
  perf_heel<-merge(perf_heel,p90, all=TRUE)
}
if("Frac10" %in% wellnames){
  p91 <- MSM_data %>% right_join(EAST10, by=c("Well","Stage"))
  p92 <- p91 %>% right_join(NORTH10, by=c("Well","Stage"))
  p93 <- p92 %>% right_join(TVD10, by=c("Well","Stage"))
  p94 <- p93 %>% right_join(MD10, by=c("Well","Stage"))
  e1 <- p94$`Easting ft (Abs)`
  e2 <- p94$`Easting (ft)10`
  n1 <- p94$`Northing ft (Abs)`
  n2 <- p94$`Northing (ft)10`
  d1 <- p94$`Depth ft (TVD)`
  d2 <- p94$`TVD (ft)10`
  p95 <- p94 %>% mutate("Distance From Perfs" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p96 <- p95 %>% right_join(EAST10h, by=c("Well","Stage"))
  p97 <- p96 %>% right_join(NORTH10h, by=c("Well","Stage"))
  p98 <- p97 %>% right_join(TVD10h, by=c("Well","Stage"))
  p99 <- p98 %>% right_join(MD10h, by=c("Well","Stage"))
  e1 <- p99$`Easting ft (Abs)`
  e2 <- p99$Frac10a.Frac10..Easting..ft..
  n1 <- p99$`Northing ft (Abs)`
  n2 <- p99$Frac10a.Frac10..Northing..ft..
  d1 <- p99$`Depth ft (TVD)`
  d2 <- p99$Frac10a.Frac10..TVD..ft..
  p100 <- p99 %>% mutate("Distance From Heel" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p100 <- p100 %>% arrange(p100$`Event File`)
  perf_heel<-merge(perf_heel,p100, all=TRUE)
}
if("Frac11" %in% wellnames){
  p101 <- MSM_data %>% right_join(EAST11, by=c("Well","Stage"))
  p102 <- p101 %>% right_join(NORTH11, by=c("Well","Stage"))
  p103 <- p102 %>% right_join(TVD11, by=c("Well","Stage"))
  p104 <- p103 %>% right_join(MD11, by=c("Well","Stage"))
  e1 <- p104$`Easting ft (Abs)`
  e2 <- p104$`Easting (ft)11`
  n1 <- p104$`Northing ft (Abs)`
  n2 <- p104$`Northing (ft)11`
  d1 <- p104$`Depth ft (TVD)`
  d2 <- p104$`TVD (ft)11`
  p105 <- p104 %>% mutate("Distance From Perfs" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p106 <- p105 %>% right_join(EAST11h, by=c("Well","Stage"))
  p107 <- p106 %>% right_join(NORTH11h, by=c("Well","Stage"))
  p108 <- p107 %>% right_join(TVD11h, by=c("Well","Stage"))
  p109 <- p108 %>% right_join(MD11h, by=c("Well","Stage"))
  e1 <- p109$`Easting ft (Abs)`
  e2 <- p109$Frac11a.Frac11..Easting..ft..
  n1 <- p109$`Northing ft (Abs)`
  n2 <- p109$Frac11a.Frac11..Northing..ft..
  d1 <- p109$`Depth ft (TVD)`
  d2 <- p109$Frac11a.Frac11..TVD..ft..
  p110 <- p109 %>% mutate("Distance From Heel" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p110 <- p110 %>% arrange(p110$`Event File`)
  perf_heel<-merge(perf_heel,p110, all=TRUE)
}
if("Frac12" %in% wellnames){
  p111 <- MSM_data %>% right_join(EAST12, by=c("Well","Stage"))
  p112 <- p111 %>% right_join(NORTH12, by=c("Well","Stage"))
  p113 <- p112 %>% right_join(TVD12, by=c("Well","Stage"))
  p114 <- p113 %>% right_join(MD12, by=c("Well","Stage"))
  e1 <- p114$`Easting ft (Abs)`
  e2 <- p114$`Easting (ft)12`
  n1 <- p114$`Northing ft (Abs)`
  n2 <- p114$`Northing (ft)12`
  d1 <- p114$`Depth ft (TVD)`
  d2 <- p114$`TVD (ft)12`
  p115 <- p114 %>% mutate("Distance From Perfs" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p116 <- p115 %>% right_join(EAST12h, by=c("Well","Stage"))
  p117 <- p116 %>% right_join(NORTH12h, by=c("Well","Stage"))
  p118 <- p117 %>% right_join(TVD12h, by=c("Well","Stage"))
  p119 <- p118 %>% right_join(MD12h, by=c("Well","Stage"))
  e1 <- p119$`Easting ft (Abs)`
  e2 <- p119$Frac12a.Frac12..Easting..ft..
  n1 <- p119$`Northing ft (Abs)`
  n2 <- p119$Frac12a.Frac12..Northing..ft..
  d1 <- p119$`Depth ft (TVD)`
  d2 <- p119$Frac12a.Frac12..TVD..ft..
  p120 <- p119 %>% mutate("Distance From Heel" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p120 <- p120 %>% arrange(p120$`Event File`)
  perf_heel<-merge(perf_heel,p120, all=TRUE)
}
if("Frac13" %in% wellnames){
  p121 <- MSM_data %>% right_join(EAST13, by=c("Well","Stage"))
  p122 <- p121 %>% right_join(NORTH13, by=c("Well","Stage"))
  p123 <- p122 %>% right_join(TVD13, by=c("Well","Stage"))
  p124 <- p123 %>% right_join(MD13, by=c("Well","Stage"))
  e1 <- p124$`Easting ft (Abs)`
  e2 <- p124$`Easting (ft)13`
  n1 <- p124$`Northing ft (Abs)`
  n2 <- p124$`Northing (ft)13`
  d1 <- p124$`Depth ft (TVD)`
  d2 <- p124$`TVD (ft)13`
  p125 <- p124 %>% mutate("Distance From Perfs" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p126 <- p125 %>% right_join(EAST13h, by=c("Well","Stage"))
  p127 <- p126 %>% right_join(NORTH13h, by=c("Well","Stage"))
  p128 <- p127 %>% right_join(TVD13h, by=c("Well","Stage"))
  p129 <- p128 %>% right_join(MD13h, by=c("Well","Stage"))
  e1 <- p129$`Easting ft (Abs)`
  e2 <- p129$Frac13a.Frac13..Easting..ft..
  n1 <- p129$`Northing ft (Abs)`
  n2 <- p129$Frac13a.Frac13..Northing..ft..
  d1 <- p129$`Depth ft (TVD)`
  d2 <- p129$Frac13a.Frac13..TVD..ft..
  p130 <- p129 %>% mutate("Distance From Heel" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p130 <- p130 %>% arrange(p130$`Event File`)
  perf_heel<-merge(perf_heel,p130, all=TRUE)
}
if("Frac14" %in% wellnames){
  p131 <- MSM_data %>% right_join(EAST14, by=c("Well","Stage"))
  p132 <- p131 %>% right_join(NORTH14, by=c("Well","Stage"))
  p133 <- p132 %>% right_join(TVD14, by=c("Well","Stage"))
  p134 <- p133 %>% right_join(MD14, by=c("Well","Stage"))
  e1 <- p134$`Easting ft (Abs)`
  e2 <- p134$`Easting (ft)14`
  n1 <- p134$`Northing ft (Abs)`
  n2 <- p134$`Northing (ft)14`
  d1 <- p134$`Depth ft (TVD)`
  d2 <- p134$`TVD (ft)14`
  p135 <- p134 %>% mutate("Distance From Perfs" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p136 <- p135 %>% right_join(EAST14h, by=c("Well","Stage"))
  p137 <- p136 %>% right_join(NORTH14h, by=c("Well","Stage"))
  p138 <- p137 %>% right_join(TVD14h, by=c("Well","Stage"))
  p139 <- p138 %>% right_join(MD14h, by=c("Well","Stage"))
  e1 <- p139$`Easting ft (Abs)`
  e2 <- p139$Frac14a.Frac14..Easting..ft..
  n1 <- p139$`Northing ft (Abs)`
  n2 <- p139$Frac14a.Frac14..Northing..ft..
  d1 <- p139$`Depth ft (TVD)`
  d2 <- p139$Frac14a.Frac14..TVD..ft..
  p140 <- p139 %>% mutate("Distance From Heel" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p140 <- p140 %>% arrange(p140$`Event File`)
  perf_heel<-merge(perf_heel,p140, all=TRUE)
}
if("Frac15" %in% wellnames){
  p141 <- MSM_data %>% right_join(EAST15, by=c("Well","Stage"))
  p142 <- p141 %>% right_join(NORTH15, by=c("Well","Stage"))
  p143 <- p142 %>% right_join(TVD15, by=c("Well","Stage"))
  p144 <- p143 %>% right_join(MD15, by=c("Well","Stage"))
  e1 <- p144$`Easting ft (Abs)`
  e2 <- p144$`Easting (ft)15`
  n1 <- p144$`Northing ft (Abs)`
  n2 <- p144$`Northing (ft)15`
  d1 <- p144$`Depth ft (TVD)`
  d2 <- p144$`TVD (ft)15`
  p145 <- p144 %>% mutate("Distance From Perfs" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p146 <- p145 %>% right_join(EAST15h, by=c("Well","Stage"))
  p147 <- p146 %>% right_join(NORTH15h, by=c("Well","Stage"))
  p148 <- p147 %>% right_join(TVD15h, by=c("Well","Stage"))
  p149 <- p148 %>% right_join(MD15h, by=c("Well","Stage"))
  e1 <- p149$`Easting ft (Abs)`
  e2 <- p149$Frac15a.Frac15..Easting..ft..
  n1 <- p149$`Northing ft (Abs)`
  n2 <- p149$Frac15a.Frac15..Northing..ft..
  d1 <- p149$`Depth ft (TVD)`
  d2 <- p149$Frac15a.Frac15..TVD..ft..
  p150 <- p149 %>% mutate("Distance From Heel" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p150 <- p150 %>% arrange(p150$`Event File`)
  perf_heel<-merge(perf_heel,p150, all=TRUE)
}
if("Frac16" %in% wellnames){
  p151 <- MSM_data %>% right_join(EAST16, by=c("Well","Stage"))
  p152 <- p151 %>% right_join(NORTH16, by=c("Well","Stage"))
  p153 <- p152 %>% right_join(TVD16, by=c("Well","Stage"))
  p154 <- p153 %>% right_join(MD16, by=c("Well","Stage"))
  e1 <- p154$`Easting ft (Abs)`
  e2 <- p154$`Easting (ft)16`
  n1 <- p154$`Northing ft (Abs)`
  n2 <- p154$`Northing (ft)16`
  d1 <- p154$`Depth ft (TVD)`
  d2 <- p154$`TVD (ft)16`
  p155 <- p154 %>% mutate("Distance From Perfs" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p156 <- p155 %>% right_join(EAST16h, by=c("Well","Stage"))
  p157 <- p156 %>% right_join(NORTH16h, by=c("Well","Stage"))
  p158 <- p157 %>% right_join(TVD16h, by=c("Well","Stage"))
  p159 <- p158 %>% right_join(MD16h, by=c("Well","Stage"))
  e1 <- p159$`Easting ft (Abs)`
  e2 <- p159$Frac16a.Frac16..Easting..ft..
  n1 <- p159$`Northing ft (Abs)`
  n2 <- p159$Frac16a.Frac16..Northing..ft..
  d1 <- p159$`Depth ft (TVD)`
  d2 <- p159$Frac16a.Frac16..TVD..ft..
  p160 <- p159 %>% mutate("Distance From Heel" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p160 <- p160 %>% arrange(p160$`Event File`)
  perf_heel<-merge(perf_heel,p160, all=TRUE)
}
if("Frac17" %in% wellnames){
  p161 <- MSM_data %>% right_join(EAST17, by=c("Well","Stage"))
  p162 <- p161 %>% right_join(NORTH17, by=c("Well","Stage"))
  p163 <- p162 %>% right_join(TVD17, by=c("Well","Stage"))
  p164 <- p163 %>% right_join(MD17, by=c("Well","Stage"))
  e1 <- p164$`Easting ft (Abs)`
  e2 <- p164$`Easting (ft)17`
  n1 <- p164$`Northing ft (Abs)`
  n2 <- p164$`Northing (ft)17`
  d1 <- p164$`Depth ft (TVD)`
  d2 <- p164$`TVD (ft)17`
  p165 <- p164 %>% mutate("Distance From Perfs" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p166 <- p165 %>% right_join(EAST17h, by=c("Well","Stage"))
  p167 <- p166 %>% right_join(NORTH17h, by=c("Well","Stage"))
  p168 <- p167 %>% right_join(TVD17h, by=c("Well","Stage"))
  p169 <- p168 %>% right_join(MD17h, by=c("Well","Stage"))
  e1 <- p169$`Easting ft (Abs)`
  e2 <- p169$Frac17a.Frac17..Easting..ft..
  n1 <- p169$`Northing ft (Abs)`
  n2 <- p169$Frac17a.Frac17..Northing..ft..
  d1 <- p169$`Depth ft (TVD)`
  d2 <- p169$Frac17a.Frac17..TVD..ft..
  p170 <- p169 %>% mutate("Distance From Heel" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p170 <- p170 %>% arrange(p170$`Event File`)
  perf_heel<-merge(perf_heel,p170, all=TRUE)
}
if("Frac18" %in% wellnames){
  p171 <- MSM_data %>% right_join(EAST18, by=c("Well","Stage"))
  p172 <- p171 %>% right_join(NORTH18, by=c("Well","Stage"))
  p173 <- p172 %>% right_join(TVD18, by=c("Well","Stage"))
  p174 <- p173 %>% right_join(MD18, by=c("Well","Stage"))
  e1 <- p174$`Easting ft (Abs)`
  e2 <- p174$`Easting (ft)18`
  n1 <- p174$`Northing ft (Abs)`
  n2 <- p174$`Northing (ft)18`
  d1 <- p174$`Depth ft (TVD)`
  d2 <- p174$`TVD (ft)18`
  p175 <- p174 %>% mutate("Distance From Perfs" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p176 <- p175 %>% right_join(EAST18h, by=c("Well","Stage"))
  p177 <- p176 %>% right_join(NORTH18h, by=c("Well","Stage"))
  p178 <- p177 %>% right_join(TVD18h, by=c("Well","Stage"))
  p179 <- p178 %>% right_join(MD18h, by=c("Well","Stage"))
  e1 <- p179$`Easting ft (Abs)`
  e2 <- p179$Frac18a.Frac18..Easting..ft..
  n1 <- p179$`Northing ft (Abs)`
  n2 <- p179$Frac18a.Frac18..Northing..ft..
  d1 <- p179$`Depth ft (TVD)`
  d2 <- p179$Frac18a.Frac18..TVD..ft..
  p180 <- p179 %>% mutate("Distance From Heel" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p180 <- p180 %>% arrange(p180$`Event File`)
  perf_heel<-merge(perf_heel,p180, all=TRUE)
}
if("Frac19" %in% wellnames){
  p181 <- MSM_data %>% right_join(EAST19, by=c("Well","Stage"))
  p182 <- p181 %>% right_join(NORTH19, by=c("Well","Stage"))
  p183 <- p182 %>% right_join(TVD19, by=c("Well","Stage"))
  p184 <- p183 %>% right_join(MD19, by=c("Well","Stage"))
  e1 <- p184$`Easting ft (Abs)`
  e2 <- p184$`Easting (ft)19`
  n1 <- p184$`Northing ft (Abs)`
  n2 <- p184$`Northing (ft)19`
  d1 <- p184$`Depth ft (TVD)`
  d2 <- p184$`TVD (ft)19`
  p185 <- p184 %>% mutate("Distance From Perfs" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p186 <- p185 %>% right_join(EAST19h, by=c("Well","Stage"))
  p187 <- p186 %>% right_join(NORTH19h, by=c("Well","Stage"))
  p188 <- p187 %>% right_join(TVD19h, by=c("Well","Stage"))
  p189 <- p188 %>% right_join(MD19h, by=c("Well","Stage"))
  e1 <- p189$`Easting ft (Abs)`
  e2 <- p189$Frac19a.Frac19..Easting..ft..
  n1 <- p189$`Northing ft (Abs)`
  n2 <- p189$Frac19a.Frac19..Northing..ft..
  d1 <- p189$`Depth ft (TVD)`
  d2 <- p189$Frac19a.Frac19..TVD..ft..
  p190 <- p189 %>% mutate("Distance From Heel" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p190 <- p190 %>% arrange(p190$`Event File`)
  perf_heel<-merge(perf_heel,p190, all=TRUE)
}
if("Frac20" %in% wellnames){
  p191 <- MSM_data %>% right_join(EAST20, by=c("Well","Stage"))
  p192 <- p191 %>% right_join(NORTH20, by=c("Well","Stage"))
  p193 <- p192 %>% right_join(TVD20, by=c("Well","Stage"))
  p194 <- p193 %>% right_join(MD20, by=c("Well","Stage"))
  e1 <- p194$`Easting ft (Abs)`
  e2 <- p194$`Easting (ft)20`
  n1 <- p194$`Northing ft (Abs)`
  n2 <- p194$`Northing (ft)20`
  d1 <- p194$`Depth ft (TVD)`
  d2 <- p194$`TVD (ft)20`
  p195 <- p194 %>% mutate("Distance From Perfs" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p196 <- p195 %>% right_join(EAST20h, by=c("Well","Stage"))
  p197 <- p196 %>% right_join(NORTH20h, by=c("Well","Stage"))
  p198 <- p197 %>% right_join(TVD20h, by=c("Well","Stage"))
  p199 <- p198 %>% right_join(MD20h, by=c("Well","Stage"))
  e1 <- p199$`Easting ft (Abs)`
  e2 <- p199$Frac20a.Frac20..Easting..ft..
  n1 <- p199$`Northing ft (Abs)`
  n2 <- p199$Frac20a.Frac20..Northing..ft..
  d1 <- p199$`Depth ft (TVD)`
  d2 <- p199$Frac20a.Frac20..TVD..ft..
  p200 <- p199 %>% mutate("Distance From Heel" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p200 <- p200 %>% arrange(p200$`Event File`)
  perf_heel<-merge(perf_heel,p200, all=TRUE)
}
if("Frac21" %in% wellnames){
  p201 <- MSM_data %>% right_join(EAST21, by=c("Well","Stage"))
  p202 <- p201 %>% right_join(NORTH21, by=c("Well","Stage"))
  p203 <- p202 %>% right_join(TVD21, by=c("Well","Stage"))
  p204 <- p203 %>% right_join(MD21, by=c("Well","Stage"))
  e1 <- p204$`Easting ft (Abs)`
  e2 <- p204$`Easting (ft)21`
  n1 <- p204$`Northing ft (Abs)`
  n2 <- p204$`Northing (ft)21`
  d1 <- p204$`Depth ft (TVD)`
  d2 <- p204$`TVD (ft)21`
  p205 <- p204 %>% mutate("Distance From Perfs" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p206 <- p205 %>% right_join(EAST21h, by=c("Well","Stage"))
  p207 <- p206 %>% right_join(NORTH21h, by=c("Well","Stage"))
  p208 <- p207 %>% right_join(TVD21h, by=c("Well","Stage"))
  p209 <- p208 %>% right_join(MD21h, by=c("Well","Stage"))
  e1 <- p209$`Easting ft (Abs)`
  e2 <- p209$Frac21a.Frac21..Easting..ft..
  n1 <- p209$`Northing ft (Abs)`
  n2 <- p209$Frac21a.Frac21..Northing..ft..
  d1 <- p209$`Depth ft (TVD)`
  d2 <- p209$Frac21a.Frac21..TVD..ft..
  p210 <- p209 %>% mutate("Distance From Heel" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p210 <- p210 %>% arrange(p210$`Event File`)
  perf_heel<-merge(perf_heel,p210, all=TRUE)
}
if("Frac22" %in% wellnames){
  p211 <- MSM_data %>% right_join(EAST22, by=c("Well","Stage"))
  p212 <- p211 %>% right_join(NORTH22, by=c("Well","Stage"))
  p213 <- p212 %>% right_join(TVD22, by=c("Well","Stage"))
  p214 <- p213 %>% right_join(MD22, by=c("Well","Stage"))
  e1 <- p214$`Easting ft (Abs)`
  e2 <- p214$`Easting (ft)22`
  n1 <- p214$`Northing ft (Abs)`
  n2 <- p214$`Northing (ft)22`
  d1 <- p214$`Depth ft (TVD)`
  d2 <- p214$`TVD (ft)22`
  p215 <- p214 %>% mutate("Distance From Perfs" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p216 <- p215 %>% right_join(EAST22h, by=c("Well","Stage"))
  p217 <- p216 %>% right_join(NORTH22h, by=c("Well","Stage"))
  p218 <- p217 %>% right_join(TVD22h, by=c("Well","Stage"))
  p219 <- p218 %>% right_join(MD22h, by=c("Well","Stage"))
  e1 <- p219$`Easting ft (Abs)`
  e2 <- p219$Frac22a.Frac22..Easting..ft..
  n1 <- p219$`Northing ft (Abs)`
  n2 <- p219$Frac22a.Frac22..Northing..ft..
  d1 <- p219$`Depth ft (TVD)`
  d2 <- p219$Frac22a.Frac22..TVD..ft..
  p220 <- p219 %>% mutate("Distance From Heel" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p220 <- p220 %>% arrange(p220$`Event File`)
  perf_heel<-merge(perf_heel,p220, all=TRUE)
}
if("Frac23" %in% wellnames){
  p221 <- MSM_data %>% right_join(EAST23, by=c("Well","Stage"))
  p222 <- p221 %>% right_join(NORTH23, by=c("Well","Stage"))
  p223 <- p222 %>% right_join(TVD23, by=c("Well","Stage"))
  p224 <- p223 %>% right_join(MD23, by=c("Well","Stage"))
  e1 <- p224$`Easting ft (Abs)`
  e2 <- p224$`Easting (ft)23`
  n1 <- p224$`Northing ft (Abs)`
  n2 <- p224$`Northing (ft)23`
  d1 <- p224$`Depth ft (TVD)`
  d2 <- p224$`TVD (ft)23`
  p225 <- p224 %>% mutate("Distance From Perfs" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p226 <- p225 %>% right_join(EAST23h, by=c("Well","Stage"))
  p227 <- p226 %>% right_join(NORTH23h, by=c("Well","Stage"))
  p228 <- p227 %>% right_join(TVD23h, by=c("Well","Stage"))
  p229 <- p228 %>% right_join(MD23h, by=c("Well","Stage"))
  e1 <- p229$`Easting ft (Abs)`
  e2 <- p229$Frac23a.Frac23..Easting..ft..
  n1 <- p229$`Northing ft (Abs)`
  n2 <- p229$Frac23a.Frac23..Northing..ft..
  d1 <- p229$`Depth ft (TVD)`
  d2 <- p229$Frac23a.Frac23..TVD..ft..
  p230 <- p229 %>% mutate("Distance From Heel" = sqrt(((e1-e2)^2)+((n1-n2)^2)+((d1-d2)^2)))
  p230 <- p230 %>% arrange(p230$`Event File`)
  perf_heel<-merge(perf_heel,p230, all=TRUE)
}



perf_heel <- perf_heel %>% arrange(perf_heel$`Event File`)
write.csv(perf_heel, file = "perf_heel.csv")

# #pop up explorer
# if (interactive() && .Platform$OS.type == "windows")
#   WD <- choose.dir(getwd(), "Choose a suitable folder")
# #set selected directory
# setwd(WD)



# Get Mid Tool Position ---------------------------------------------------


print("getting mid tool position")
#Get Averages: Tools
#Array 1
newcolnames <- c("Tool Array","Tool Level","MD (ft)","TVD (ft)","E/W Relative Local Tangent Plane (ft)","N/S Relative Local Tangent Plane (ft)","TVDSS (ft)",
                 "Easting (ft)","Northing (ft)","Well","Stage", "Latitude","Longitude")

if("Obs1" %in% wellnames){
  Obs1<-data.frame(wellsheets$Obs1)
  colnames(Obs1) <- newcolnames
  Obs1 <- Obs1 %>% separate("Stage",
                                c("STG", "Stage"),
                                sep = "Stage ",
                                remove = TRUE,
                                convert = FALSE)
  Obs1 <- Obs1[-c(5,6,11)]
}

if("Obs2" %in% wellnames){
  Obs2<-data.frame(wellsheets$Obs2)
  colnames(Obs2) <- newcolnames
  Obs2 <- Obs2 %>% separate("Stage",
                            c("STG", "Stage"),
                            sep = "Stage ",
                            remove = TRUE,
                            convert = FALSE)
  Obs2 <- Obs2[-c(5,6,11)]
}

if("Obs3" %in% wellnames){
  Obs3<-data.frame(wellsheets$Obs3)
  colnames(Obs3) <- newcolnames
  Obs3 <- Obs3 %>% separate("Stage",
                            c("STG", "Stage"),
                            sep = "Stage ",
                            remove = TRUE,
                            convert = FALSE)
  Obs3 <- Obs3[-c(5,6,11)]
}

if("Obs4" %in% wellnames){
  Obs4<-data.frame(wellsheets$Obs4)
  colnames(Obs1) <- newcolnames
  Obs4 <- Obs4 %>% separate("Stage",
                            c("STG", "Stage"),
                            sep = "Stage ",
                            remove = TRUE,
                            convert = FALSE)
  Obs4 <- Obs4[-c(5,6,11)]
}


# Get Tool Dist -----------------------------------------------------------


print("Getting mid tool position")
if("Obs1" %in% wellnames){
  TVDo1<-Obs1 %>%
    group_by(Well,Stage)%>%
    summarize(`TVD (ft)` = mean(`TVD (ft)`))
  names(TVDo1)[names(TVDo1) == "TVD (ft)"] <- "TVD (ft)1"
  EASTo1<-Obs1 %>%
    group_by(Well,Stage)%>%
    summarize(`Easting (ft)` = mean(`Easting (ft)`))
  names(EASTo1)[names(EASTo1) == "Easting (ft)"] <- "Easting (ft)1"
  NORTHo1<-Obs1 %>%
    group_by(Well,Stage)%>%
    summarize(`Northing (ft)` = mean(`Northing (ft)`))
  names(NORTHo1)[names(NORTHo1) == "Northing (ft)"] <- "Northing (ft)1"
  TVDSSo1<-Obs1 %>%
    group_by(Well,Stage)%>%
    summarize(`TVDSS (ft)` = mean(`TVDSS (ft)`))
  names(TVDSSo1)[names(TVDSSo1) == "TVDSS (ft)"] <- "TVDSS (ft)1"
}
#Array 2
if("Obs2" %in% wellnames){
  TVDo2<-Obs2 %>%
    group_by(Well,Stage)%>%
    summarize(`TVD (ft)` = mean(`TVD (ft)`))
  names(TVDo2)[names(TVDo2) == "TVD (ft)"] <- "TVD (ft)2"
  EASTo2<-Obs2 %>%
    group_by(Well,Stage)%>%
    summarize(`Easting (ft)` = mean(`Easting (ft)`))
  names(EASTo2)[names(EASTo2) == "Easting (ft)"] <- "Easting (ft)2"
  NORTHo2<-Obs2 %>%
    group_by(Well,Stage)%>%
    summarize(`Northing (ft)` = mean(`Northing (ft)`))
  names(NORTHo2)[names(NORTHo2) == "Northing (ft)"] <- "Northing (ft)2"
  TVDSSo2<-Obs2 %>%
    group_by(Well,Stage)%>%
    summarize(`TVDSS (ft)` = mean(`TVDSS (ft)`))
  names(TVDSSo2)[names(TVDSSo2) == "TVDSS (ft)"] <- "TVDSS (ft)2"
}
#Array 3
if("Obs3" %in% wellnames){
  TVDo3<-Obs3 %>%
    group_by(Well,Stage)%>%
    summarize(`TVD (ft)` = mean(`TVD (ft)`))
  names(TVDo3)[names(TVDo3) == "TVD (ft)"] <- "TVD (ft)3"
  EASTo3<-Obs3 %>%
    group_by(Well,Stage)%>%
    summarize(`Easting (ft)` = mean(`Easting (ft)`))
  names(EASTo3)[names(EASTo3) == "Easting (ft)"] <- "Easting (ft)3"
  NORTHo3<-Obs3 %>%
    group_by(Well,Stage)%>%
    summarize(`Northing (ft)` = mean(`Northing (ft)`))
  names(NORTHo3)[names(NORTHo3) == "Northing (ft)"] <- "Northing (ft)3"
  TVDSSo3<-Obs3 %>%
    group_by(Well,Stage)%>%
    summarize(`TVDSS (ft)` = mean(`TVDSS (ft)`))
  names(TVDSSo3)[names(TVDSSo3) == "TVDSS (ft)"] <- "TVDSS (ft)3"
}
#Array 4
if("Obs4" %in% wellnames){
  TVDo4<-Obs4 %>%
    group_by(Well,Stage)%>%
    summarize(`TVD (ft)` = mean(`TVD (ft)`))
  names(TVDo4)[names(TVDo3) == "TVD (ft)"] <- "TVD (ft)4"
  EASTo4<-Obs4 %>%
    group_by(Well,Stage)%>%
    summarize(`Easting (ft)` = mean(`Easting (ft)`))
  names(EASTo4)[names(EASTo4) == "Easting (ft)"] <- "Easting (ft)4"
  NORTHo4<-Obs4 %>%
    group_by(Well,Stage)%>%
    summarize(`Northing (ft)` = mean(`Northing (ft)`))
  names(NORTHo4)[names(NORTHo4) == "Northing (ft)"] <- "Northing (ft)4"
  TVDSSo4<-Obs4 %>%
    group_by(Well,Stage)%>%
    summarize(`TVDSS (ft)` = mean(`TVDSS (ft)`))
  names(TVDSSo4)[names(TVDSSo3) == "TVDSS (ft)"] <- "TVDSS (ft)4"
}
print("getting even to mid tool distances")
#Array 1


#######Write Tool Distance###########
if("Obs1" %in% wellnames){
  t1 <- perf_heel %>% left_join(EASTo1, by=c("Well","Stage"))
  t2 <- t1 %>% left_join(NORTHo1, by=c("Well","Stage"))
  t3 <- t2 %>% left_join(TVDo1, by=c("Well","Stage"))
  t4 <- t3 %>% left_join(TVDSSo1, by=c("Well","Stage"))
  e1 <- t4$`Easting ft (Abs)`
  e2 <- t4$`Easting (ft)1.y`
  n1 <- t4$`Northing ft (Abs)`
  n2 <- t4$`Northing (ft)1.y`
  d1 <- t4$`Depth ft (TVD)`
  d2 <- t4$`TVD (ft)1.y`
  t4 <- t4 %>% mutate("Distance From Array 1" = sqrt(((e1-e2)^2)+((n1-n2)^2)+
                                          ((d1-d2)^2)))
  colnames(t4)[colnames(t4) == "Easting (ft)1.y"] <- 'Easting (ft)Obs1'
  colnames(t4)[colnames(t4) == "Northing (ft)1.y"] <- 'Northing (ft)Obs1'
  colnames(t4)[colnames(t4) == "TVD (ft)1.y"] <- 'TVD (ft)Obs1'
  colnames(t4)[colnames(t4) == "TVDSS (ft)1"] <- 'TVDSS (ft)Obs1'
  array1 <- t4
  array1 <- data.frame(array1$Well,array1$Stage, array1$`Event File`, array1$`Distance From Perfs`, array1$`Distance From Heel`, array1$`Distance From Array 1`)
  names(array1)[names(array1) == 'array1.Well'] <- 'Well'
  names(array1)[names(array1) == 'array1.Stage'] <- 'Stage'
  #array1 <- array1[order(array1$array1..Event.File.),]
  fpoint <- 1
  lpoint <- which.max(is.na(array1$array1..Event.File.))
  if (lpoint == 1){
    lpoint = length(array1$array1..Event.File.)+1
  }
  array1 <- array1[(1:(lpoint-1)),]
  write.csv(array1, file = "array1.csv")
}
#Array 2
if("Obs2" %in% wellnames){
  t5 <- perf_heel %>% left_join(EASTo2, by=c("Well","Stage"))
  t6 <- t5 %>% left_join(NORTHo2, by=c("Well","Stage"))
  t7 <- t6 %>% left_join(TVDo2, by=c("Well","Stage"))
  t8 <- t7 %>% left_join(TVDSSo2, by=c("Well","Stage"))
  e1 <- t8$`Easting ft (Abs)`
  e2 <- t8$`Easting (ft)1`
  n1 <- t8$`Northing ft (Abs)`
  n2 <- t8$`Northing (ft)1`
  d1 <- t8$`Depth ft (TVD)`
  d2 <- t8$`TVD (ft)1`
  t8 <- t8 %>% mutate("Distance From Array 2" = sqrt(((e1-e2)^2)+((n1-n2)^2)+
                                                       ((d1-d2)^2)))
  colnames(t8)[colnames(t8) == "Easting (ft)2.y"] <- 'Easting (ft)Obs2'
  colnames(t8)[colnames(t8) == "Northing (ft)2.y"] <- 'Northing (ft)Obs2'
  colnames(t8)[colnames(t8) == "TVD (ft)2.y"] <- 'TVD (ft)Obs2'
  colnames(t8)[colnames(t8) == "TVDSS (ft)2"] <- 'TVDSS (ft)Obs2'
  array2 <- t8
  array2 <- data.frame(array2$Well,array2$Stage, array2$`Event File`, array2$`Distance From Perfs`, array2$`Distance From Heel`, array2$`Distance From Array 2`)
  names(array2)[names(array2) == 'array2.Well'] <- 'Well'
  names(array2)[names(array2) == 'array2.Stage'] <- 'Stage'
  #array2 <- array2[order(array2$array2..Event.File.),]
  fpoint <- 1
  lpoint <- which.max(is.na(array2$array2..Event.File.))
  if (lpoint == 1){
    lpoint = length(array2$array2..Event.File.)+1
  }
  array2 <- array2[(1:(lpoint-1)),]

  write.csv(array2, file = "array2.csv")
}
#Array 3
if("Obs3" %in% wellnames){
  t7 <- perf_heel %>% right_join(EASTo3, by=c("Well","Stage"))
  t8 <- t7 %>% right_join(NORTHo3, by=c("Well","Stage"))
  t9 <- t8 %>% right_join(TVDo3, by=c("Well","Stage"))
  t10 <- t9 %>% right_join(TVDSSo1, by=c("Well","Stage"))
  e1 <- t10$`Easting ft (Abs)`
  e2 <- t10$`Easting (ft)1`
  n1 <- t10$`Northing ft (Abs)`
  n2 <- t10$`Northing (ft)1`
  d1 <- t10$`Depth ft (TVD)`
  d2 <- t10$`TVD (ft)1`
  t10 <- t10 %>% mutate("Distance From Array 3" = sqrt(((e1-e2)^2)+((n1-n2)^2)+
                                                       ((d1-d2)^2)))
  array3 <- t10
  array3 <- data.frame(array3$Well,array3$Stage, array3$`Event File`, array3$`Distance From Perfs`, array3$`Distance From Heel`, array3$`Distance From Array 3`)
  names(array3)[names(array3) == 'array3.Well'] <- 'Well'
  names(array3)[names(array3) == 'array3.Stage'] <- 'Stage'
  #array3 <- array3[-1,]
  write.csv(array3, file = "array3.csv")
}
#Array 4
if("Obs4" %in% wellnames){
  t10 <- perf_heel %>% right_join(EASTo4, by=c("Well","Stage"))
  t11 <- t10 %>% right_join(NORTHo4, by=c("Well","Stage"))
  t12 <- t11 %>% right_join(TVDo4, by=c("Well","Stage"))
  t13 <- t12 %>% right_join(TVDSSo1, by=c("Well","Stage"))
  e1 <- t13$`Easting ft (Abs)`
  e2 <- t13$`Easting (ft)1`
  n1 <- t13$`Northing ft (Abs)`
  n2 <- t13$`Northing (ft)1`
  d1 <- t13$`Depth ft (TVD)`
  d2 <- t13$`TVD (ft)1`
  t13 <- t12 %>% mutate("Distance From Array 4" = sqrt(((e1-e2)^2)+((n1-n2)^2)+
                                                       ((d1-d2)^2)))
  
  array4 <- t13
  array4 <- data.frame(array4$Well,array4$Stage, array4$`Event File`, array4$`Distance From Perfs`, array4$`Distance From Heel`, array4$`Distance From Array 4`)
  names(array4)[names(array4) == 'array4.Well'] <- 'Well'
  names(array4)[names(array4) == 'array4.Stage'] <- 'Stage'
  #array4 <- array4[-1,]
  write.csv(array4, file = "array4.csv")
  #Final_out<-merge(array1,array2, all=TRUE)
}

print("assigning side of lateral to events")





# Which Side of well ------------------------------------------------------


if("Frac1" %in% wellnames){
  Frac1TW <- Frac1[1 ,9]
  Frac1min<-data.frame(Frac1$Well,Frac1$Stage,Frac1$`Easting (ft)`,Frac1$`Northing (ft)`,Frac1$`TVD (ft)`,Frac1$`MD (ft)`)
  Frac1min<-Frac1min %>%slice(which.min(Frac1..MD..ft..))
  Frac1min<-data.frame("Well" = Frac1min$Frac1.Well,Stage = Frac1min$Frac1.Stage,Frac1min$Frac1..Easting..ft..,
                       Frac1min$Frac1..Northing..ft..,Frac1min$Frac1..TVD..ft..,Frac1min$Frac1..MD..ft..)
  names(Frac1min)[names(Frac1min) == 'Frac1min.Frac1..Easting..ft..'] <- 'x'
  names(Frac1min)[names(Frac1min) == 'Frac1min.Frac1..Northing..ft..'] <- 'y'
  
  Frac1max<-data.frame(Frac1$Well,Frac1$Stage,Frac1$`Easting (ft)`,Frac1$`Northing (ft)`,Frac1$`TVD (ft)`,Frac1$`MD (ft)`)
  Frac1max<-Frac1max %>%slice(which.max(Frac1..MD..ft..))
  Frac1max<-data.frame("Well" = Frac1max$Frac1.Well,Stage = Frac1max$Frac1.Stage,Frac1max$Frac1..Easting..ft..,
                       Frac1max$Frac1..Northing..ft..,Frac1max$Frac1..TVD..ft..,Frac1max$Frac1..MD..ft..)
  names(Frac1max)[names(Frac1max) == 'Frac1max.Frac1..Easting..ft..'] <- 'x'
  names(Frac1max)[names(Frac1max) == 'Frac1max.Frac1..Northing..ft..'] <- 'y'
  
  Frac1_line <- data.frame(x=c(Frac1min$x,Frac1max$x), 
                           y=c(Frac1min$y,Frac1max$y))
  
  
  msd.l <- MSM_data %>% mutate(well_side = ((Frac1max$x - Frac1min$x)*(MSM_data$`Northing ft (Abs)` - Frac1min$y)) - ((MSM_data$`Easting ft (Abs)` - Frac1min$x)*(Frac1max$y - Frac1min$y)))
  msd.l$Side <- ifelse(msd.l$well_side>0,1,-1)
  msd.l1<- msd.l %>% filter(Well==Frac1TW)

}

if("Frac2" %in% wellnames){
  Frac2TW <- Frac2[1 ,9]
  Frac2min<-data.frame(Frac2$Well,Frac2$Stage,Frac2$`Easting (ft)`,Frac2$`Northing (ft)`,Frac2$`TVD (ft)`,Frac2$`MD (ft)`)
  Frac2min<-Frac2min %>%slice(which.min(Frac2..MD..ft..))
  Frac2min<-data.frame("Well" = Frac2min$Frac2.Well,Stage = Frac2min$Frac2.Stage,Frac2min$Frac2..Easting..ft..,
                       Frac2min$Frac2..Northing..ft..,Frac2min$Frac2..TVD..ft..,Frac2min$Frac2..MD..ft..)
  names(Frac2min)[names(Frac2min) == 'Frac2min.Frac2..Easting..ft..'] <- 'x'
  names(Frac2min)[names(Frac2min) == 'Frac2min.Frac2..Northing..ft..'] <- 'y'
  
  Frac2max<-data.frame(Frac2$Well,Frac2$Stage,Frac2$`Easting (ft)`,Frac2$`Northing (ft)`,Frac2$`TVD (ft)`,Frac2$`MD (ft)`)
  Frac2max<-Frac2max %>%slice(which.max(Frac2..MD..ft..))
  Frac2max<-data.frame("Well" = Frac2max$Frac2.Well,Stage = Frac2max$Frac2.Stage,Frac2max$Frac2..Easting..ft..,
                       Frac2max$Frac2..Northing..ft..,Frac2max$Frac2..TVD..ft..,Frac2max$Frac2..MD..ft..)
  names(Frac2max)[names(Frac2max) == 'Frac2max.Frac2..Easting..ft..'] <- 'x'
  names(Frac2max)[names(Frac2max) == 'Frac2max.Frac2..Northing..ft..'] <- 'y'
  
  Frac2_line <- data.frame(x=c(Frac2min$x,Frac2max$x), 
                           y=c(Frac2min$y,Frac2max$y))
  
  
  msd.l <- MSM_data %>% mutate(well_side = ((Frac2max$x - Frac2min$x)*(MSM_data$`Northing ft (Abs)` - Frac2min$y)) - ((MSM_data$`Easting ft (Abs)` - Frac2min$x)*(Frac2max$y - Frac2min$y)))
  msd.l$Side <- ifelse(msd.l$well_side>0,1,-1)
  msd.l2<- msd.l %>% filter(Well==Frac2TW)
  
}

if("Frac3" %in% wellnames){
  Frac3TW <- Frac3[1 ,9]
  Frac3min<-data.frame(Frac3$Well,Frac3$Stage,Frac3$`Easting (ft)`,Frac3$`Northing (ft)`,Frac3$`TVD (ft)`,Frac3$`MD (ft)`)
  Frac3min<-Frac3min %>%slice(which.min(Frac3..MD..ft..))
  Frac3min<-data.frame("Well" = Frac3min$Frac3.Well,Stage = Frac3min$Frac3.Stage,Frac3min$Frac3..Easting..ft..,
                       Frac3min$Frac3..Northing..ft..,Frac3min$Frac3..TVD..ft..,Frac3min$Frac3..MD..ft..)
  names(Frac3min)[names(Frac3min) == 'Frac3min.Frac3..Easting..ft..'] <- 'x'
  names(Frac3min)[names(Frac3min) == 'Frac3min.Frac3..Northing..ft..'] <- 'y'
  
  Frac3max<-data.frame(Frac3$Well,Frac3$Stage,Frac3$`Easting (ft)`,Frac3$`Northing (ft)`,Frac3$`TVD (ft)`,Frac3$`MD (ft)`)
  Frac3max<-Frac3max %>%slice(which.max(Frac3..MD..ft..))
  Frac3max<-data.frame("Well" = Frac3max$Frac3.Well,Stage = Frac3max$Frac3.Stage,Frac3max$Frac3..Easting..ft..,
                       Frac3max$Frac3..Northing..ft..,Frac3max$Frac3..TVD..ft..,Frac3max$Frac3..MD..ft..)
  names(Frac3max)[names(Frac3max) == 'Frac3max.Frac3..Easting..ft..'] <- 'x'
  names(Frac3max)[names(Frac3max) == 'Frac3max.Frac3..Northing..ft..'] <- 'y'
  
  Frac3_line <- data.frame(x=c(Frac3min$x,Frac3max$x), 
                           y=c(Frac3min$y,Frac3max$y))
  
  
  msd.l <- MSM_data %>% mutate(well_side = ((Frac3max$x - Frac3min$x)*(MSM_data$`Northing ft (Abs)` - Frac3min$y)) - ((MSM_data$`Easting ft (Abs)` - Frac3min$x)*(Frac3max$y - Frac3min$y)))
  msd.l$Side <- ifelse(msd.l$well_side>0,1,-1)
  msd.l3<- msd.l %>% filter(Well==Frac3TW)
  
}

if("Frac4" %in% wellnames){
  Frac4TW <- Frac4[1 ,9]
  Frac4min<-data.frame(Frac4$Well,Frac4$Stage,Frac4$`Easting (ft)`,Frac4$`Northing (ft)`,Frac4$`TVD (ft)`,Frac4$`MD (ft)`)
  Frac4min<-Frac4min %>%slice(which.min(Frac4..MD..ft..))
  Frac4min<-data.frame("Well" = Frac4min$Frac4.Well,Stage = Frac4min$Frac4.Stage,Frac4min$Frac4..Easting..ft..,
                       Frac4min$Frac4..Northing..ft..,Frac4min$Frac4..TVD..ft..,Frac4min$Frac4..MD..ft..)
  names(Frac4min)[names(Frac4min) == 'Frac4min.Frac4..Easting..ft..'] <- 'x'
  names(Frac4min)[names(Frac4min) == 'Frac4min.Frac4..Northing..ft..'] <- 'y'
  
  Frac4max<-data.frame(Frac4$Well,Frac4$Stage,Frac4$`Easting (ft)`,Frac4$`Northing (ft)`,Frac4$`TVD (ft)`,Frac4$`MD (ft)`)
  Frac4max<-Frac4max %>%slice(which.max(Frac4..MD..ft..))
  Frac4max<-data.frame("Well" = Frac4max$Frac4.Well,Stage = Frac4max$Frac4.Stage,Frac4max$Frac4..Easting..ft..,
                       Frac4max$Frac4..Northing..ft..,Frac4max$Frac4..TVD..ft..,Frac4max$Frac4..MD..ft..)
  names(Frac4max)[names(Frac4max) == 'Frac4max.Frac4..Easting..ft..'] <- 'x'
  names(Frac4max)[names(Frac4max) == 'Frac4max.Frac4..Northing..ft..'] <- 'y'
  
  Frac4_line <- data.frame(x=c(Frac4min$x,Frac4max$x), 
                           y=c(Frac4min$y,Frac4max$y))
  
  
  msd.l <- MSM_data %>% mutate(well_side = ((Frac4max$x - Frac4min$x)*(MSM_data$`Northing ft (Abs)` - Frac4min$y)) - ((MSM_data$`Easting ft (Abs)` - Frac4min$x)*(Frac4max$y - Frac4min$y)))
  msd.l$Side <- ifelse(msd.l$well_side>0,1,-1)
  msd.l4<- msd.l %>% filter(Well==Frac4TW)
  
}

if("Frac5" %in% wellnames){
  Frac5TW <- Frac5[1 ,9]
  Frac5min<-data.frame(Frac5$Well,Frac5$Stage.x,Frac5$`Easting (ft)`,Frac5$`Northing (ft)`,Frac5$`TVD (ft)`,Frac5$`MD (ft)`)
  Frac5min<-Frac5min %>%slice(which.min(Frac5..MD..ft..))
  Frac5min<-data.frame("Well" = Frac5min$Frac5.Well,Stage = Frac5min$Frac5.Stage.x,Frac5min$Frac5..Easting..ft..,
                       Frac5min$Frac5..Northing..ft..,Frac5min$Frac5..TVD..ft..,Frac5min$Frac5..MD..ft..)
  names(Frac5min)[names(Frac5min) == 'Frac5min.Frac5..Easting..ft..'] <- 'x'
  names(Frac5min)[names(Frac5min) == 'Frac5min.Frac5..Northing..ft..'] <- 'y'
  
  Frac5max<-data.frame(Frac5$Well,Frac5$Stage.x,Frac5$`Easting (ft)`,Frac5$`Northing (ft)`,Frac5$`TVD (ft)`,Frac5$`MD (ft)`)
  Frac5max<-Frac5max %>%slice(which.max(Frac5..MD..ft..))
  Frac5max<-data.frame("Well" = Frac5max$Frac5.Well,Stage = Frac5max$Frac5.Stage.x,Frac5max$Frac5..Easting..ft..,
                       Frac5max$Frac5..Northing..ft..,Frac5max$Frac5..TVD..ft..,Frac5max$Frac5..MD..ft..)
  names(Frac5max)[names(Frac5max) == 'Frac5max.Frac5..Easting..ft..'] <- 'x'
  names(Frac5max)[names(Frac5max) == 'Frac5max.Frac5..Northing..ft..'] <- 'y'
  
  Frac5_line <- data.frame(x=c(Frac5min$x,Frac5max$x), 
                           y=c(Frac5min$y,Frac5max$y))
  
  
  msd.l <- MSM_data %>% mutate(well_side = ((Frac5max$x - Frac5min$x)*(MSM_data$`Northing ft (Abs)` - Frac5min$y)) - ((MSM_data$`Easting ft (Abs)` - Frac5min$x)*(Frac5max$y - Frac5min$y)))
  msd.l$Side <- ifelse(msd.l$well_side>0,1,-1)
  msd.l5<- msd.l %>% filter(Well==Frac5TW)
  
}

if("Frac6" %in% wellnames){
  Frac6TW <- Frac6[1 ,9]
  Frac6min<-data.frame(Frac6$Well,Frac6$Stage.x,Frac6$`Easting (ft)`,Frac6$`Northing (ft)`,Frac6$`TVD (ft)`,Frac6$`MD (ft)`)
  Frac6min<-Frac6min %>%slice(which.min(Frac6..MD..ft..))
  Frac6min<-data.frame("Well" = Frac6min$Frac6.Well,Stage = Frac6min$Frac6.Stage.x,Frac6min$Frac6..Easting..ft..,
                       Frac6min$Frac6..Northing..ft..,Frac6min$Frac6..TVD..ft..,Frac6min$Frac6..MD..ft..)
  names(Frac6min)[names(Frac6min) == 'Frac6min.Frac6..Easting..ft..'] <- 'x'
  names(Frac6min)[names(Frac6min) == 'Frac6min.Frac6..Northing..ft..'] <- 'y'
  
  Frac6max<-data.frame(Frac6$Well,Frac6$Stage.x,Frac6$`Easting (ft)`,Frac6$`Northing (ft)`,Frac6$`TVD (ft)`,Frac6$`MD (ft)`)
  Frac6max<-Frac6max %>%slice(which.max(Frac6..MD..ft..))
  Frac6max<-data.frame("Well" = Frac6max$Frac6.Well,Stage = Frac6max$Frac6.Stage.x,Frac6max$Frac6..Easting..ft..,
                       Frac6max$Frac6..Northing..ft..,Frac6max$Frac6..TVD..ft..,Frac6max$Frac6..MD..ft..)
  names(Frac6max)[names(Frac6max) == 'Frac6max.Frac6..Easting..ft..'] <- 'x'
  names(Frac6max)[names(Frac6max) == 'Frac6max.Frac6..Northing..ft..'] <- 'y'
  
  Frac6_line <- data.frame(x=c(Frac6min$x,Frac6max$x), 
                           y=c(Frac6min$y,Frac6max$y))
  
  
  msd.l <- MSM_data %>% mutate(well_side = ((Frac6max$x - Frac6min$x)*(MSM_data$`Northing ft (Abs)` - Frac6min$y)) - ((MSM_data$`Easting ft (Abs)` - Frac6min$x)*(Frac6max$y - Frac6min$y)))
  msd.l$Side <- ifelse(msd.l$well_side>0,1,-1)
  msd.l6<- msd.l %>% filter(Well==Frac6TW)
  
}

if("Frac7" %in% wellnames){
  Frac7TW <- Frac7[1 ,9]
  Frac7min<-data.frame(Frac7$Well,Frac7$Stage.x,Frac7$`Easting (ft)`,Frac7$`Northing (ft)`,Frac7$`TVD (ft)`,Frac7$`MD (ft)`)
  Frac7min<-Frac7min %>%slice(which.min(Frac7..MD..ft..))
  Frac7min<-data.frame("Well" = Frac7min$Frac7.Well,Stage = Frac7min$Frac7.Stage.x,Frac7min$Frac7..Easting..ft..,
                       Frac7min$Frac7..Northing..ft..,Frac7min$Frac7..TVD..ft..,Frac7min$Frac7..MD..ft..)
  names(Frac7min)[names(Frac7min) == 'Frac7min.Frac7..Easting..ft..'] <- 'x'
  names(Frac7min)[names(Frac7min) == 'Frac7min.Frac7..Northing..ft..'] <- 'y'
  
  Frac7max<-data.frame(Frac7$Well,Frac7$Stage.x,Frac7$`Easting (ft)`,Frac7$`Northing (ft)`,Frac7$`TVD (ft)`,Frac7$`MD (ft)`)
  Frac7max<-Frac7max %>%slice(which.max(Frac7..MD..ft..))
  Frac7max<-data.frame("Well" = Frac7max$Frac7.Well,Stage = Frac7max$Frac7.Stage.x,Frac7max$Frac7..Easting..ft..,
                       Frac7max$Frac7..Northing..ft..,Frac7max$Frac7..TVD..ft..,Frac7max$Frac7..MD..ft..)
  names(Frac7max)[names(Frac7max) == 'Frac7max.Frac7..Easting..ft..'] <- 'x'
  names(Frac7max)[names(Frac7max) == 'Frac7max.Frac7..Northing..ft..'] <- 'y'
  
  Frac7_line <- data.frame(x=c(Frac7min$x,Frac7max$x), 
                           y=c(Frac7min$y,Frac7max$y))
  
  
  msd.l <- MSM_data %>% mutate(well_side = ((Frac7max$x - Frac7min$x)*(MSM_data$`Northing ft (Abs)` - Frac7min$y)) - ((MSM_data$`Easting ft (Abs)` - Frac7min$x)*(Frac7max$y - Frac7min$y)))
  msd.l$Side <- ifelse(msd.l$well_side>0,1,-1)
  msd.l7<- msd.l %>% filter(Well==Frac7TW)
  
}

if("Frac8" %in% wellnames){
  Frac8TW <- Frac8[1 ,9]
  Frac8min<-data.frame(Frac8$Well,Frac8$Stage.x,Frac8$`Easting (ft)`,Frac8$`Northing (ft)`,Frac8$`TVD (ft)`,Frac8$`MD (ft)`)
  Frac8min<-Frac8min %>%slice(which.min(Frac8..MD..ft..))
  Frac8min<-data.frame("Well" = Frac8min$Frac8.Well,Stage = Frac8min$Frac8.Stage.x,Frac8min$Frac8..Easting..ft..,
                       Frac8min$Frac8..Northing..ft..,Frac8min$Frac8..TVD..ft..,Frac8min$Frac8..MD..ft..)
  names(Frac8min)[names(Frac8min) == 'Frac8min.Frac8..Easting..ft..'] <- 'x'
  names(Frac8min)[names(Frac8min) == 'Frac8min.Frac8..Northing..ft..'] <- 'y'
  
  Frac8max<-data.frame(Frac8$Well,Frac8$Stage.x,Frac8$`Easting (ft)`,Frac8$`Northing (ft)`,Frac8$`TVD (ft)`,Frac8$`MD (ft)`)
  Frac8max<-Frac8max %>%slice(which.max(Frac8..MD..ft..))
  Frac8max<-data.frame("Well" = Frac8max$Frac8.Well,Stage = Frac8max$Frac8.Stage.x,Frac8max$Frac8..Easting..ft..,
                       Frac8max$Frac8..Northing..ft..,Frac8max$Frac8..TVD..ft..,Frac8max$Frac8..MD..ft..)
  names(Frac8max)[names(Frac8max) == 'Frac8max.Frac8..Easting..ft..'] <- 'x'
  names(Frac8max)[names(Frac8max) == 'Frac8max.Frac8..Northing..ft..'] <- 'y'
  
  Frac8_line <- data.frame(x=c(Frac8min$x,Frac8max$x), 
                           y=c(Frac8min$y,Frac8max$y))
  
  
  msd.l <- MSM_data %>% mutate(well_side = ((Frac8max$x - Frac8min$x)*(MSM_data$`Northing ft (Abs)` - Frac8min$y)) - ((MSM_data$`Easting ft (Abs)` - Frac8min$x)*(Frac8max$y - Frac8min$y)))
  msd.l$Side <- ifelse(msd.l$well_side>0,1,-1)
  msd.l8<- msd.l %>% filter(Well==Frac8TW)
  
}

if("Frac9" %in% wellnames){
  Frac9TW <- Frac9[1 ,9]
  Frac9min<-data.frame(Frac9$Well,Frac9$Stage.x,Frac9$`Easting (ft)`,Frac9$`Northing (ft)`,Frac9$`TVD (ft)`,Frac9$`MD (ft)`)
  Frac9min<-Frac9min %>%slice(which.min(Frac9..MD..ft..))
  Frac9min<-data.frame("Well" = Frac9min$Frac9.Well,Stage = Frac9min$Frac9.Stage.x,Frac9min$Frac9..Easting..ft..,
                       Frac9min$Frac9..Northing..ft..,Frac9min$Frac9..TVD..ft..,Frac9min$Frac9..MD..ft..)
  names(Frac9min)[names(Frac9min) == 'Frac9min.Frac9..Easting..ft..'] <- 'x'
  names(Frac9min)[names(Frac9min) == 'Frac9min.Frac9..Northing..ft..'] <- 'y'
  
  Frac9max<-data.frame(Frac9$Well,Frac9$Stage.x,Frac9$`Easting (ft)`,Frac9$`Northing (ft)`,Frac9$`TVD (ft)`,Frac9$`MD (ft)`)
  Frac9max<-Frac9max %>%slice(which.max(Frac9..MD..ft..))
  Frac9max<-data.frame("Well" = Frac9max$Frac9.Well,Stage = Frac9max$Frac9.Stage.x,Frac9max$Frac9..Easting..ft..,
                       Frac9max$Frac9..Northing..ft..,Frac9max$Frac9..TVD..ft..,Frac9max$Frac9..MD..ft..)
  names(Frac9max)[names(Frac9max) == 'Frac9max.Frac9..Easting..ft..'] <- 'x'
  names(Frac9max)[names(Frac9max) == 'Frac9max.Frac9..Northing..ft..'] <- 'y'
  
  Frac9_line <- data.frame(x=c(Frac9min$x,Frac9max$x), 
                           y=c(Frac9min$y,Frac9max$y))
  
  
  msd.l <- MSM_data %>% mutate(well_side = ((Frac9max$x - Frac9min$x)*(MSM_data$`Northing ft (Abs)` - Frac9min$y)) - ((MSM_data$`Easting ft (Abs)` - Frac9min$x)*(Frac9max$y - Frac9min$y)))
  msd.l$Side <- ifelse(msd.l$well_side>0,1,-1)
  msd.l9<- msd.l %>% filter(Well==Frac9TW)
  
}

if("Frac10" %in% wellnames){
  Frac10TW <- Frac10[1 ,9]
  Frac10min<-data.frame(Frac10$Well,Frac10$Stage.x,Frac10$`Easting (ft)`,Frac10$`Northing (ft)`,Frac10$`TVD (ft)`,Frac10$`MD (ft)`)
  Frac10min<-Frac10min %>%slice(which.min(Frac10..MD..ft..))
  Frac10min<-data.frame("Well" = Frac10min$Frac10.Well,Stage = Frac10min$Frac10.Stage.x,Frac10min$Frac10..Easting..ft..,
                       Frac10min$Frac10..Northing..ft..,Frac10min$Frac10..TVD..ft..,Frac10min$Frac10..MD..ft..)
  names(Frac10min)[names(Frac10min) == 'Frac10min.Frac10..Easting..ft..'] <- 'x'
  names(Frac10min)[names(Frac10min) == 'Frac10min.Frac10..Northing..ft..'] <- 'y'
  
  Frac10max<-data.frame(Frac10$Well,Frac10$Stage.x,Frac10$`Easting (ft)`,Frac10$`Northing (ft)`,Frac10$`TVD (ft)`,Frac10$`MD (ft)`)
  Frac10max<-Frac10max %>%slice(which.max(Frac10..MD..ft..))
  Frac10max<-data.frame("Well" = Frac10max$Frac10.Well,Stage = Frac10max$Frac10.Stage.x,Frac10max$Frac10..Easting..ft..,
                       Frac10max$Frac10..Northing..ft..,Frac10max$Frac10..TVD..ft..,Frac10max$Frac10..MD..ft..)
  names(Frac10max)[names(Frac10max) == 'Frac10max.Frac10..Easting..ft..'] <- 'x'
  names(Frac10max)[names(Frac10max) == 'Frac10max.Frac10..Northing..ft..'] <- 'y'
  
  Frac10_line <- data.frame(x=c(Frac10min$x,Frac10max$x), 
                           y=c(Frac10min$y,Frac10max$y))
  
  
  msd.l <- MSM_data %>% mutate(well_side = ((Frac10max$x - Frac10min$x)*(MSM_data$`Northing ft (Abs)` - Frac10min$y)) - ((MSM_data$`Easting ft (Abs)` - Frac10min$x)*(Frac10max$y - Frac10min$y)))
  msd.l$Side <- ifelse(msd.l$well_side>0,1,-1)
  msd.l10<- msd.l %>% filter(Well==Frac10TW)
  
}

if("Frac11" %in% wellnames){
  Frac11TW <- Frac11[1 ,9]
  Frac11min<-data.frame(Frac11$Well,Frac11$Stage.x,Frac11$`Easting (ft)`,Frac11$`Northing (ft)`,Frac11$`TVD (ft)`,Frac11$`MD (ft)`)
  Frac11min<-Frac11min %>%slice(which.min(Frac11..MD..ft..))
  Frac11min<-data.frame("Well" = Frac11min$Frac11.Well,Stage = Frac11min$Frac11.Stage.x,Frac11min$Frac11..Easting..ft..,
                        Frac11min$Frac11..Northing..ft..,Frac11min$Frac11..TVD..ft..,Frac11min$Frac11..MD..ft..)
  names(Frac11min)[names(Frac11min) == 'Frac11min.Frac11..Easting..ft..'] <- 'x'
  names(Frac11min)[names(Frac11min) == 'Frac11min.Frac11..Northing..ft..'] <- 'y'
  
  Frac11max<-data.frame(Frac11$Well,Frac11$Stage.x,Frac11$`Easting (ft)`,Frac11$`Northing (ft)`,Frac11$`TVD (ft)`,Frac11$`MD (ft)`)
  Frac11max<-Frac11max %>%slice(which.max(Frac11..MD..ft..))
  Frac11max<-data.frame("Well" = Frac11max$Frac11.Well,Stage = Frac11max$Frac11.Stage.x,Frac11max$Frac11..Easting..ft..,
                        Frac11max$Frac11..Northing..ft..,Frac11max$Frac11..TVD..ft..,Frac11max$Frac11..MD..ft..)
  names(Frac11max)[names(Frac11max) == 'Frac11max.Frac11..Easting..ft..'] <- 'x'
  names(Frac11max)[names(Frac11max) == 'Frac11max.Frac11..Northing..ft..'] <- 'y'
  
  Frac11_line <- data.frame(x=c(Frac11min$x,Frac11max$x), 
                            y=c(Frac11min$y,Frac11max$y))
  
  
  msd.l <- MSM_data %>% mutate(well_side = ((Frac11max$x - Frac11min$x)*(MSM_data$`Northing ft (Abs)` - Frac11min$y)) - ((MSM_data$`Easting ft (Abs)` - Frac11min$x)*(Frac11max$y - Frac11min$y)))
  msd.l$Side <- ifelse(msd.l$well_side>0,1,-1)
  msd.l11<- msd.l %>% filter(Well==Frac11TW)
  
}

if("Frac12" %in% wellnames){
  Frac12TW <- Frac12[1 ,9]
  Frac12min<-data.frame(Frac12$Well,Frac12$Stage.x,Frac12$`Easting (ft)`,Frac12$`Northing (ft)`,Frac12$`TVD (ft)`,Frac12$`MD (ft)`)
  Frac12min<-Frac12min %>%slice(which.min(Frac12..MD..ft..))
  Frac12min<-data.frame("Well" = Frac12min$Frac12.Well,Stage = Frac12min$Frac12.Stage.x,Frac12min$Frac12..Easting..ft..,
                        Frac12min$Frac12..Northing..ft..,Frac12min$Frac12..TVD..ft..,Frac12min$Frac12..MD..ft..)
  names(Frac12min)[names(Frac12min) == 'Frac12min.Frac12..Easting..ft..'] <- 'x'
  names(Frac12min)[names(Frac12min) == 'Frac12min.Frac12..Northing..ft..'] <- 'y'
  
  Frac12max<-data.frame(Frac12$Well,Frac12$Stage.x,Frac12$`Easting (ft)`,Frac12$`Northing (ft)`,Frac12$`TVD (ft)`,Frac12$`MD (ft)`)
  Frac12max<-Frac12max %>%slice(which.max(Frac12..MD..ft..))
  Frac12max<-data.frame("Well" = Frac12max$Frac12.Well,Stage = Frac12max$Frac12.Stage.x,Frac12max$Frac12..Easting..ft..,
                        Frac12max$Frac12..Northing..ft..,Frac12max$Frac12..TVD..ft..,Frac12max$Frac12..MD..ft..)
  names(Frac12max)[names(Frac12max) == 'Frac12max.Frac12..Easting..ft..'] <- 'x'
  names(Frac12max)[names(Frac12max) == 'Frac12max.Frac12..Northing..ft..'] <- 'y'
  
  Frac12_line <- data.frame(x=c(Frac12min$x,Frac12max$x), 
                            y=c(Frac12min$y,Frac12max$y))
  
  
  msd.l <- MSM_data %>% mutate(well_side = ((Frac12max$x - Frac12min$x)*(MSM_data$`Northing ft (Abs)` - Frac12min$y)) - ((MSM_data$`Easting ft (Abs)` - Frac12min$x)*(Frac12max$y - Frac12min$y)))
  msd.l$Side <- ifelse(msd.l$well_side>0,1,-1)
  msd.l12<- msd.l %>% filter(Well==Frac12TW)
  
}

if("Frac13" %in% wellnames){
  Frac13TW <- Frac13[1 ,9]
  Frac13min<-data.frame(Frac13$Well,Frac13$Stage.x,Frac13$`Easting (ft)`,Frac13$`Northing (ft)`,Frac13$`TVD (ft)`,Frac13$`MD (ft)`)
  Frac13min<-Frac13min %>%slice(which.min(Frac13..MD..ft..))
  Frac13min<-data.frame("Well" = Frac13min$Frac13.Well,Stage = Frac13min$Frac13.Stage.x,Frac13min$Frac13..Easting..ft..,
                        Frac13min$Frac13..Northing..ft..,Frac13min$Frac13..TVD..ft..,Frac13min$Frac13..MD..ft..)
  names(Frac13min)[names(Frac13min) == 'Frac13min.Frac13..Easting..ft..'] <- 'x'
  names(Frac13min)[names(Frac13min) == 'Frac13min.Frac13..Northing..ft..'] <- 'y'
  
  Frac13max<-data.frame(Frac13$Well,Frac13$Stage.x,Frac13$`Easting (ft)`,Frac13$`Northing (ft)`,Frac13$`TVD (ft)`,Frac13$`MD (ft)`)
  Frac13max<-Frac13max %>%slice(which.max(Frac13..MD..ft..))
  Frac13max<-data.frame("Well" = Frac13max$Frac13.Well,Stage = Frac13max$Frac13.Stage.x,Frac13max$Frac13..Easting..ft..,
                        Frac13max$Frac13..Northing..ft..,Frac13max$Frac13..TVD..ft..,Frac13max$Frac13..MD..ft..)
  names(Frac13max)[names(Frac13max) == 'Frac13max.Frac13..Easting..ft..'] <- 'x'
  names(Frac13max)[names(Frac13max) == 'Frac13max.Frac13..Northing..ft..'] <- 'y'
  
  Frac13_line <- data.frame(x=c(Frac13min$x,Frac13max$x), 
                            y=c(Frac13min$y,Frac13max$y))
  
  
  msd.l <- MSM_data %>% mutate(well_side = ((Frac13max$x - Frac13min$x)*(MSM_data$`Northing ft (Abs)` - Frac13min$y)) - ((MSM_data$`Easting ft (Abs)` - Frac13min$x)*(Frac13max$y - Frac13min$y)))
  msd.l$Side <- ifelse(msd.l$well_side>0,1,-1)
  msd.l13<- msd.l %>% filter(Well==Frac13TW)
  
}

if("Frac14" %in% wellnames){
  Frac14TW <- Frac14[1 ,9]
  Frac14min<-data.frame(Frac14$Well,Frac14$Stage.x,Frac14$`Easting (ft)`,Frac14$`Northing (ft)`,Frac14$`TVD (ft)`,Frac14$`MD (ft)`)
  Frac14min<-Frac14min %>%slice(which.min(Frac14..MD..ft..))
  Frac14min<-data.frame("Well" = Frac14min$Frac14.Well,Stage = Frac14min$Frac14.Stage.x,Frac14min$Frac14..Easting..ft..,
                        Frac14min$Frac14..Northing..ft..,Frac14min$Frac14..TVD..ft..,Frac14min$Frac14..MD..ft..)
  names(Frac14min)[names(Frac14min) == 'Frac14min.Frac14..Easting..ft..'] <- 'x'
  names(Frac14min)[names(Frac14min) == 'Frac14min.Frac14..Northing..ft..'] <- 'y'
  
  Frac14max<-data.frame(Frac14$Well,Frac14$Stage.x,Frac14$`Easting (ft)`,Frac14$`Northing (ft)`,Frac14$`TVD (ft)`,Frac14$`MD (ft)`)
  Frac14max<-Frac14max %>%slice(which.max(Frac14..MD..ft..))
  Frac14max<-data.frame("Well" = Frac14max$Frac14.Well,Stage = Frac14max$Frac14.Stage.x,Frac14max$Frac14..Easting..ft..,
                        Frac14max$Frac14..Northing..ft..,Frac14max$Frac14..TVD..ft..,Frac14max$Frac14..MD..ft..)
  names(Frac14max)[names(Frac14max) == 'Frac14max.Frac14..Easting..ft..'] <- 'x'
  names(Frac14max)[names(Frac14max) == 'Frac14max.Frac14..Northing..ft..'] <- 'y'
  
  Frac14_line <- data.frame(x=c(Frac14min$x,Frac14max$x), 
                            y=c(Frac14min$y,Frac14max$y))
  
  
  msd.l <- MSM_data %>% mutate(well_side = ((Frac14max$x - Frac14min$x)*(MSM_data$`Northing ft (Abs)` - Frac14min$y)) - ((MSM_data$`Easting ft (Abs)` - Frac14min$x)*(Frac14max$y - Frac14min$y)))
  msd.l$Side <- ifelse(msd.l$well_side>0,1,-1)
  msd.l14<- msd.l %>% filter(Well==Frac14TW)
  
}

if("Frac15" %in% wellnames){
  Frac15TW <- Frac15[1 ,9]
  Frac15min<-data.frame(Frac15$Well,Frac15$Stage.x,Frac15$`Easting (ft)`,Frac15$`Northing (ft)`,Frac15$`TVD (ft)`,Frac15$`MD (ft)`)
  Frac15min<-Frac15min %>%slice(which.min(Frac15..MD..ft..))
  Frac15min<-data.frame("Well" = Frac15min$Frac15.Well,Stage = Frac15min$Frac15.Stage.x,Frac15min$Frac15..Easting..ft..,
                        Frac15min$Frac15..Northing..ft..,Frac15min$Frac15..TVD..ft..,Frac15min$Frac15..MD..ft..)
  names(Frac15min)[names(Frac15min) == 'Frac15min.Frac15..Easting..ft..'] <- 'x'
  names(Frac15min)[names(Frac15min) == 'Frac15min.Frac15..Northing..ft..'] <- 'y'
  
  Frac15max<-data.frame(Frac15$Well,Frac15$Stage.x,Frac15$`Easting (ft)`,Frac15$`Northing (ft)`,Frac15$`TVD (ft)`,Frac15$`MD (ft)`)
  Frac15max<-Frac15max %>%slice(which.max(Frac15..MD..ft..))
  Frac15max<-data.frame("Well" = Frac15max$Frac15.Well,Stage = Frac15max$Frac15.Stage.x,Frac15max$Frac15..Easting..ft..,
                        Frac15max$Frac15..Northing..ft..,Frac15max$Frac15..TVD..ft..,Frac15max$Frac15..MD..ft..)
  names(Frac15max)[names(Frac15max) == 'Frac15max.Frac15..Easting..ft..'] <- 'x'
  names(Frac15max)[names(Frac15max) == 'Frac15max.Frac15..Northing..ft..'] <- 'y'
  
  Frac15_line <- data.frame(x=c(Frac15min$x,Frac15max$x), 
                            y=c(Frac15min$y,Frac15max$y))
  
  
  msd.l <- MSM_data %>% mutate(well_side = ((Frac15max$x - Frac15min$x)*(MSM_data$`Northing ft (Abs)` - Frac15min$y)) - ((MSM_data$`Easting ft (Abs)` - Frac15min$x)*(Frac15max$y - Frac15min$y)))
  msd.l$Side <- ifelse(msd.l$well_side>0,1,-1)
  msd.l15<- msd.l %>% filter(Well==Frac15TW)
  
}

if("Frac16" %in% wellnames){
  Frac16TW <- Frac16[1 ,9]
  Frac16min<-data.frame(Frac16$Well,Frac16$Stage.x,Frac16$`Easting (ft)`,Frac16$`Northing (ft)`,Frac16$`TVD (ft)`,Frac16$`MD (ft)`)
  Frac16min<-Frac16min %>%slice(which.min(Frac16..MD..ft..))
  Frac16min<-data.frame("Well" = Frac16min$Frac16.Well,Stage = Frac16min$Frac16.Stage.x,Frac16min$Frac16..Easting..ft..,
                        Frac16min$Frac16..Northing..ft..,Frac16min$Frac16..TVD..ft..,Frac16min$Frac16..MD..ft..)
  names(Frac16min)[names(Frac16min) == 'Frac16min.Frac16..Easting..ft..'] <- 'x'
  names(Frac16min)[names(Frac16min) == 'Frac16min.Frac16..Northing..ft..'] <- 'y'
  
  Frac16max<-data.frame(Frac16$Well,Frac16$Stage.x,Frac16$`Easting (ft)`,Frac16$`Northing (ft)`,Frac16$`TVD (ft)`,Frac16$`MD (ft)`)
  Frac16max<-Frac16max %>%slice(which.max(Frac16..MD..ft..))
  Frac16max<-data.frame("Well" = Frac16max$Frac16.Well,Stage = Frac16max$Frac16.Stage.x,Frac16max$Frac16..Easting..ft..,
                        Frac16max$Frac16..Northing..ft..,Frac16max$Frac16..TVD..ft..,Frac16max$Frac16..MD..ft..)
  names(Frac16max)[names(Frac16max) == 'Frac16max.Frac16..Easting..ft..'] <- 'x'
  names(Frac16max)[names(Frac16max) == 'Frac16max.Frac16..Northing..ft..'] <- 'y'
  
  Frac16_line <- data.frame(x=c(Frac16min$x,Frac16max$x), 
                            y=c(Frac16min$y,Frac16max$y))
  
  
  msd.l <- MSM_data %>% mutate(well_side = ((Frac16max$x - Frac16min$x)*(MSM_data$`Northing ft (Abs)` - Frac16min$y)) - ((MSM_data$`Easting ft (Abs)` - Frac16min$x)*(Frac16max$y - Frac16min$y)))
  msd.l$Side <- ifelse(msd.l$well_side>0,1,-1)
  msd.l16<- msd.l %>% filter(Well==Frac16TW)
  
}

if("Frac17" %in% wellnames){
  Frac17TW <- Frac17[1 ,9]
  Frac17min<-data.frame(Frac17$Well,Frac17$Stage.x,Frac17$`Easting (ft)`,Frac17$`Northing (ft)`,Frac17$`TVD (ft)`,Frac17$`MD (ft)`)
  Frac17min<-Frac17min %>%slice(which.min(Frac17..MD..ft..))
  Frac17min<-data.frame("Well" = Frac17min$Frac17.Well,Stage = Frac17min$Frac17.Stage.x,Frac17min$Frac17..Easting..ft..,
                        Frac17min$Frac17..Northing..ft..,Frac17min$Frac17..TVD..ft..,Frac17min$Frac17..MD..ft..)
  names(Frac17min)[names(Frac17min) == 'Frac17min.Frac17..Easting..ft..'] <- 'x'
  names(Frac17min)[names(Frac17min) == 'Frac17min.Frac17..Northing..ft..'] <- 'y'
  
  Frac17max<-data.frame(Frac17$Well,Frac17$Stage.x,Frac17$`Easting (ft)`,Frac17$`Northing (ft)`,Frac17$`TVD (ft)`,Frac17$`MD (ft)`)
  Frac17max<-Frac17max %>%slice(which.max(Frac17..MD..ft..))
  Frac17max<-data.frame("Well" = Frac17max$Frac17.Well,Stage = Frac17max$Frac17.Stage.x,Frac17max$Frac17..Easting..ft..,
                        Frac17max$Frac17..Northing..ft..,Frac17max$Frac17..TVD..ft..,Frac17max$Frac17..MD..ft..)
  names(Frac17max)[names(Frac17max) == 'Frac17max.Frac17..Easting..ft..'] <- 'x'
  names(Frac17max)[names(Frac17max) == 'Frac17max.Frac17..Northing..ft..'] <- 'y'
  
  Frac17_line <- data.frame(x=c(Frac17min$x,Frac17max$x), 
                            y=c(Frac17min$y,Frac17max$y))
  
  
  msd.l <- MSM_data %>% mutate(well_side = ((Frac17max$x - Frac17min$x)*(MSM_data$`Northing ft (Abs)` - Frac17min$y)) - ((MSM_data$`Easting ft (Abs)` - Frac17min$x)*(Frac17max$y - Frac17min$y)))
  msd.l$Side <- ifelse(msd.l$well_side>0,1,-1)
  msd.l17<- msd.l %>% filter(Well==Frac17TW)
  
}

if("Frac18" %in% wellnames){
  Frac18TW <- Frac18[1 ,9]
  Frac18min<-data.frame(Frac18$Well,Frac18$Stage.x,Frac18$`Easting (ft)`,Frac18$`Northing (ft)`,Frac18$`TVD (ft)`,Frac18$`MD (ft)`)
  Frac18min<-Frac18min %>%slice(which.min(Frac18..MD..ft..))
  Frac18min<-data.frame("Well" = Frac18min$Frac18.Well,Stage = Frac18min$Frac18.Stage.x,Frac18min$Frac18..Easting..ft..,
                        Frac18min$Frac18..Northing..ft..,Frac18min$Frac18..TVD..ft..,Frac18min$Frac18..MD..ft..)
  names(Frac18min)[names(Frac18min) == 'Frac18min.Frac18..Easting..ft..'] <- 'x'
  names(Frac18min)[names(Frac18min) == 'Frac18min.Frac18..Northing..ft..'] <- 'y'
  
  Frac18max<-data.frame(Frac18$Well,Frac18$Stage.x,Frac18$`Easting (ft)`,Frac18$`Northing (ft)`,Frac18$`TVD (ft)`,Frac18$`MD (ft)`)
  Frac18max<-Frac18max %>%slice(which.max(Frac18..MD..ft..))
  Frac18max<-data.frame("Well" = Frac18max$Frac18.Well,Stage = Frac18max$Frac18.Stage.x,Frac18max$Frac18..Easting..ft..,
                        Frac18max$Frac18..Northing..ft..,Frac18max$Frac18..TVD..ft..,Frac18max$Frac18..MD..ft..)
  names(Frac18max)[names(Frac18max) == 'Frac18max.Frac18..Easting..ft..'] <- 'x'
  names(Frac18max)[names(Frac18max) == 'Frac18max.Frac18..Northing..ft..'] <- 'y'
  
  Frac18_line <- data.frame(x=c(Frac18min$x,Frac18max$x), 
                            y=c(Frac18min$y,Frac18max$y))
  
  
  msd.l <- MSM_data %>% mutate(well_side = ((Frac18max$x - Frac18min$x)*(MSM_data$`Northing ft (Abs)` - Frac18min$y)) - ((MSM_data$`Easting ft (Abs)` - Frac18min$x)*(Frac18max$y - Frac18min$y)))
  msd.l$Side <- ifelse(msd.l$well_side>0,1,-1)
  msd.l18<- msd.l %>% filter(Well==Frac18TW)
  
}

if("Frac19" %in% wellnames){
  Frac19TW <- Frac19[1 ,9]
  Frac19min<-data.frame(Frac19$Well,Frac19$Stage.x,Frac19$`Easting (ft)`,Frac19$`Northing (ft)`,Frac19$`TVD (ft)`,Frac19$`MD (ft)`)
  Frac19min<-Frac19min %>%slice(which.min(Frac19..MD..ft..))
  Frac19min<-data.frame("Well" = Frac19min$Frac19.Well,Stage = Frac19min$Frac19.Stage.x,Frac19min$Frac19..Easting..ft..,
                        Frac19min$Frac19..Northing..ft..,Frac19min$Frac19..TVD..ft..,Frac19min$Frac19..MD..ft..)
  names(Frac19min)[names(Frac19min) == 'Frac19min.Frac19..Easting..ft..'] <- 'x'
  names(Frac19min)[names(Frac19min) == 'Frac19min.Frac19..Northing..ft..'] <- 'y'
  
  Frac19max<-data.frame(Frac19$Well,Frac19$Stage.x,Frac19$`Easting (ft)`,Frac19$`Northing (ft)`,Frac19$`TVD (ft)`,Frac19$`MD (ft)`)
  Frac19max<-Frac19max %>%slice(which.max(Frac19..MD..ft..))
  Frac19max<-data.frame("Well" = Frac19max$Frac19.Well,Stage = Frac19max$Frac19.Stage.x,Frac19max$Frac19..Easting..ft..,
                        Frac19max$Frac19..Northing..ft..,Frac19max$Frac19..TVD..ft..,Frac19max$Frac19..MD..ft..)
  names(Frac19max)[names(Frac19max) == 'Frac19max.Frac19..Easting..ft..'] <- 'x'
  names(Frac19max)[names(Frac19max) == 'Frac19max.Frac19..Northing..ft..'] <- 'y'
  
  Frac19_line <- data.frame(x=c(Frac19min$x,Frac19max$x), 
                            y=c(Frac19min$y,Frac19max$y))
  
  
  msd.l <- MSM_data %>% mutate(well_side = ((Frac19max$x - Frac19min$x)*(MSM_data$`Northing ft (Abs)` - Frac19min$y)) - ((MSM_data$`Easting ft (Abs)` - Frac19min$x)*(Frac19max$y - Frac19min$y)))
  msd.l$Side <- ifelse(msd.l$well_side>0,1,-1)
  msd.l19<- msd.l %>% filter(Well==Frac19TW)
  
}

if("Frac20" %in% wellnames){
  Frac20TW <- Frac20[1 ,9]
  Frac20min<-data.frame(Frac20$Well,Frac20$Stage.x,Frac20$`Easting (ft)`,Frac20$`Northing (ft)`,Frac20$`TVD (ft)`,Frac20$`MD (ft)`)
  Frac20min<-Frac20min %>%slice(which.min(Frac20..MD..ft..))
  Frac20min<-data.frame("Well" = Frac20min$Frac20.Well,Stage = Frac20min$Frac20.Stage.x,Frac20min$Frac20..Easting..ft..,
                        Frac20min$Frac20..Northing..ft..,Frac20min$Frac20..TVD..ft..,Frac20min$Frac20..MD..ft..)
  names(Frac20min)[names(Frac20min) == 'Frac20min.Frac20..Easting..ft..'] <- 'x'
  names(Frac20min)[names(Frac20min) == 'Frac20min.Frac20..Northing..ft..'] <- 'y'
  
  Frac20max<-data.frame(Frac20$Well,Frac20$Stage.x,Frac20$`Easting (ft)`,Frac20$`Northing (ft)`,Frac20$`TVD (ft)`,Frac20$`MD (ft)`)
  Frac20max<-Frac20max %>%slice(which.max(Frac20..MD..ft..))
  Frac20max<-data.frame("Well" = Frac20max$Frac20.Well,Stage = Frac20max$Frac20.Stage.x,Frac20max$Frac20..Easting..ft..,
                        Frac20max$Frac20..Northing..ft..,Frac20max$Frac20..TVD..ft..,Frac20max$Frac20..MD..ft..)
  names(Frac20max)[names(Frac20max) == 'Frac20max.Frac20..Easting..ft..'] <- 'x'
  names(Frac20max)[names(Frac20max) == 'Frac20max.Frac20..Northing..ft..'] <- 'y'
  
  Frac20_line <- data.frame(x=c(Frac20min$x,Frac20max$x), 
                            y=c(Frac20min$y,Frac20max$y))
  
  
  msd.l <- MSM_data %>% mutate(well_side = ((Frac20max$x - Frac20min$x)*(MSM_data$`Northing ft (Abs)` - Frac20min$y)) - ((MSM_data$`Easting ft (Abs)` - Frac20min$x)*(Frac20max$y - Frac20min$y)))
  msd.l$Side <- ifelse(msd.l$well_side>0,1,-1)
  msd.l20<- msd.l %>% filter(Well==Frac20TW)
  
}

if("Frac21" %in% wellnames){
  Frac21TW <- Frac21[1 ,9]
  Frac21min<-data.frame(Frac21$Well,Frac21$Stage.x,Frac21$`Easting (ft)`,Frac21$`Northing (ft)`,Frac21$`TVD (ft)`,Frac21$`MD (ft)`)
  Frac21min<-Frac21min %>%slice(which.min(Frac21..MD..ft..))
  Frac21min<-data.frame("Well" = Frac21min$Frac21.Well,Stage = Frac21min$Frac21.Stage.x,Frac21min$Frac21..Easting..ft..,
                        Frac21min$Frac21..Northing..ft..,Frac21min$Frac21..TVD..ft..,Frac21min$Frac21..MD..ft..)
  names(Frac21min)[names(Frac21min) == 'Frac21min.Frac21..Easting..ft..'] <- 'x'
  names(Frac21min)[names(Frac21min) == 'Frac21min.Frac21..Northing..ft..'] <- 'y'
  
  Frac21max<-data.frame(Frac21$Well,Frac21$Stage.x,Frac21$`Easting (ft)`,Frac21$`Northing (ft)`,Frac21$`TVD (ft)`,Frac21$`MD (ft)`)
  Frac21max<-Frac21max %>%slice(which.max(Frac21..MD..ft..))
  Frac21max<-data.frame("Well" = Frac21max$Frac21.Well,Stage = Frac21max$Frac21.Stage.x,Frac21max$Frac21..Easting..ft..,
                        Frac21max$Frac21..Northing..ft..,Frac21max$Frac21..TVD..ft..,Frac21max$Frac21..MD..ft..)
  names(Frac21max)[names(Frac21max) == 'Frac21max.Frac21..Easting..ft..'] <- 'x'
  names(Frac21max)[names(Frac21max) == 'Frac21max.Frac21..Northing..ft..'] <- 'y'
  
  Frac21_line <- data.frame(x=c(Frac21min$x,Frac21max$x), 
                            y=c(Frac21min$y,Frac21max$y))
  
  
  msd.l <- MSM_data %>% mutate(well_side = ((Frac21max$x - Frac21min$x)*(MSM_data$`Northing ft (Abs)` - Frac21min$y)) - ((MSM_data$`Easting ft (Abs)` - Frac21min$x)*(Frac21max$y - Frac21min$y)))
  msd.l$Side <- ifelse(msd.l$well_side>0,1,-1)
  msd.l21<- msd.l %>% filter(Well==Frac21TW)
  
}

if("Frac22" %in% wellnames){
  Frac22TW <- Frac22[1 ,9]
  Frac22min<-data.frame(Frac22$Well,Frac22$Stage.x,Frac22$`Easting (ft)`,Frac22$`Northing (ft)`,Frac22$`TVD (ft)`,Frac22$`MD (ft)`)
  Frac22min<-Frac22min %>%slice(which.min(Frac22..MD..ft..))
  Frac22min<-data.frame("Well" = Frac22min$Frac22.Well,Stage = Frac22min$Frac22.Stage.x,Frac22min$Frac22..Easting..ft..,
                        Frac22min$Frac22..Northing..ft..,Frac22min$Frac22..TVD..ft..,Frac22min$Frac22..MD..ft..)
  names(Frac22min)[names(Frac22min) == 'Frac22min.Frac22..Easting..ft..'] <- 'x'
  names(Frac22min)[names(Frac22min) == 'Frac22min.Frac22..Northing..ft..'] <- 'y'
  
  Frac22max<-data.frame(Frac22$Well,Frac22$Stage.x,Frac22$`Easting (ft)`,Frac22$`Northing (ft)`,Frac22$`TVD (ft)`,Frac22$`MD (ft)`)
  Frac22max<-Frac22max %>%slice(which.max(Frac22..MD..ft..))
  Frac22max<-data.frame("Well" = Frac22max$Frac22.Well,Stage = Frac22max$Frac22.Stage.x,Frac22max$Frac22..Easting..ft..,
                        Frac22max$Frac22..Northing..ft..,Frac22max$Frac22..TVD..ft..,Frac22max$Frac22..MD..ft..)
  names(Frac22max)[names(Frac22max) == 'Frac22max.Frac22..Easting..ft..'] <- 'x'
  names(Frac22max)[names(Frac22max) == 'Frac22max.Frac22..Northing..ft..'] <- 'y'
  
  Frac22_line <- data.frame(x=c(Frac22min$x,Frac22max$x), 
                            y=c(Frac22min$y,Frac22max$y))
  
  
  msd.l <- MSM_data %>% mutate(well_side = ((Frac22max$x - Frac22min$x)*(MSM_data$`Northing ft (Abs)` - Frac22min$y)) - ((MSM_data$`Easting ft (Abs)` - Frac22min$x)*(Frac22max$y - Frac22min$y)))
  msd.l$Side <- ifelse(msd.l$well_side>0,1,-1)
  msd.l22<- msd.l %>% filter(Well==Frac22TW)
  
}

if("Frac23" %in% wellnames){
  Frac23TW <- Frac23[1 ,9]
  Frac23min<-data.frame(Frac23$Well,Frac23$Stage.x,Frac23$`Easting (ft)`,Frac23$`Northing (ft)`,Frac23$`TVD (ft)`,Frac23$`MD (ft)`)
  Frac23min<-Frac23min %>%slice(which.min(Frac23..MD..ft..))
  Frac23min<-data.frame("Well" = Frac23min$Frac23.Well,Stage = Frac23min$Frac23.Stage.x,Frac23min$Frac23..Easting..ft..,
                        Frac23min$Frac23..Northing..ft..,Frac23min$Frac23..TVD..ft..,Frac23min$Frac23..MD..ft..)
  names(Frac23min)[names(Frac23min) == 'Frac23min.Frac23..Easting..ft..'] <- 'x'
  names(Frac23min)[names(Frac23min) == 'Frac23min.Frac23..Northing..ft..'] <- 'y'
  
  Frac23max<-data.frame(Frac23$Well,Frac23$Stage.x,Frac23$`Easting (ft)`,Frac23$`Northing (ft)`,Frac23$`TVD (ft)`,Frac23$`MD (ft)`)
  Frac23max<-Frac23max %>%slice(which.max(Frac23..MD..ft..))
  Frac23max<-data.frame("Well" = Frac23max$Frac23.Well,Stage = Frac23max$Frac23.Stage.x,Frac23max$Frac23..Easting..ft..,
                        Frac23max$Frac23..Northing..ft..,Frac23max$Frac23..TVD..ft..,Frac23max$Frac23..MD..ft..)
  names(Frac23max)[names(Frac23max) == 'Frac23max.Frac23..Easting..ft..'] <- 'x'
  names(Frac23max)[names(Frac23max) == 'Frac23max.Frac23..Northing..ft..'] <- 'y'
  
  Frac23_line <- data.frame(x=c(Frac23min$x,Frac23max$x), 
                            y=c(Frac23min$y,Frac23max$y))
  
  
  msd.l <- MSM_data %>% mutate(well_side = ((Frac23max$x - Frac23min$x)*(MSM_data$`Northing ft (Abs)` - Frac23min$y)) - ((MSM_data$`Easting ft (Abs)` - Frac23min$x)*(Frac23max$y - Frac23min$y)))
  msd.l$Side <- ifelse(msd.l$well_side>0,1,-1)
  msd.l23<- msd.l %>% filter(Well==Frac23TW)
  
}
# merging files


if("Frac1" %in% wellnames){
  wellside <- msd.l1
}

if("Frac2" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2)) 
}

if("Frac3" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3)) 
}

if("Frac4" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4)) 
}

if("Frac5" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5)) 
}

if("Frac6" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6)) 
}

if("Frac7" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7)) 
}

if("Frac8" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8)) 
}

if("Frac9" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9)) 
}

if("Frac10" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9, msd.l10)) 
}

if("Frac11" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9, msd.l10, msd.l11)) 
}

if("Frac12" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9, msd.l10, msd.l11, msd.l12)) 
}

if("Frac13" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9, msd.l10, msd.l11, msd.l12, msd.l13)) 
}

if("Frac14" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9, msd.l10, msd.l11, msd.l12, msd.l13, msd.l14)) 
}

if("Frac15" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9, msd.l10, msd.l11, msd.l12, msd.l13, msd.l14, msd.l15)) 
}

if("Frac16" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9, msd.l10, msd.l11, msd.l12, msd.l13, msd.l14, msd.l15, msd.l16)) 
}

if("Frac17" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9, msd.l10, msd.l11, msd.l12, msd.l13, msd.l14, msd.l15, msd.l16, msd.l17)) 
}

if("Frac18" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9, msd.l10, msd.l11, msd.l12, msd.l13, msd.l14, msd.l15, msd.l16, msd.l17, msd.l18)) 
}

if("Frac19" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9, msd.l10, msd.l11, msd.l12, msd.l13, msd.l14, msd.l15, msd.l16, msd.l17, msd.l18, msd.l19)) 
}

if("Frac20" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9, msd.l10, msd.l11, msd.l12, msd.l13, msd.l14, msd.l15, msd.l16, msd.l17, msd.l18, msd.l19, msd.l20)) 
}

if("Frac21" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9, msd.l10, msd.l11, msd.l12, msd.l13, msd.l14, msd.l15, msd.l16, msd.l17, msd.l18, msd.l19, msd.l20, msd.l21)) 
}

if("Frac22" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9, msd.l10, msd.l11, msd.l12, msd.l13, msd.l14, msd.l15, msd.l16, msd.l17, msd.l18, msd.l19, msd.l20, msd.l21, msd.l22)) 
}

if("Frac23" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9, msd.l10, msd.l11, msd.l12, msd.l13, msd.l14, msd.l15, msd.l16, msd.l17, msd.l18, msd.l19, msd.l20, msd.l21, msd.l22, msd.l23)) 
}



# All_Array Creation -----------------------------------------------------


if("Obs1" %in% wellnames){
  all_array <- t4
  #write.xlsx(all_array, 'check.xlsx')
}

if("Obs2" %in% wellnames){
  all_array <- full_join(all_array, t8)
}

if("Obs3" %in% wellnames){
  all_array <- full_join(all_array, t10)
}

if("Obs4" %in% wellnames){
  all_array <- full_join(all_array, t13)
}


all_array <- all_array %>% arrange(all_array$`Event File`)
rows <- nrow(all_array)
events<-nrow(MSM_data)
print(rows)
print(events)
all_array <- all_array[-(events+1:rows),]

# write.xlsx(wellside, 'wellside.xlsx')
# write.xlsx(all_array, 'all_array.xlsx')

#wellside <-import('wellside.xlsx')
#all_array <- import('all_array.xlsx')

wellside <- wellside %>% arrange(wellside$`Event File`)
wellside$WS <- wellside$Side
#temp <- ifelse(wellside$WS == 1, wellside$`Distance From Perfs`,-1*(wellside$`Distance From Perfs`))

all_array$WS <- wellside$Side
all_array$WellSide <- ifelse(all_array$WS == 1, all_array$`Distance From Perfs`,-1*(all_array$`Distance From Perfs`))

# merging files


if("Frac1" %in% wellnames){
  wellside <- msd.l1
}

if("Frac2" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2)) 
}

if("Frac3" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3)) 
}

if("Frac4" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4)) 
}

if("Frac5" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5)) 
}

if("Frac6" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6)) 
}

if("Frac7" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7)) 
}

if("Frac8" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8)) 
}

if("Frac9" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9)) 
}

if("Frac10" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9, msd.l10)) 
}

if("Frac11" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9, msd.l10, msd.l11)) 
}

if("Frac12" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9, msd.l10, msd.l11, msd.l12)) 
}

if("Frac13" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9, msd.l10, msd.l11, msd.l12, msd.l13)) 
}

if("Frac14" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9, msd.l10, msd.l11, msd.l12, msd.l13, msd.l14)) 
}

if("Frac15" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9, msd.l10, msd.l11, msd.l12, msd.l13, msd.l14, msd.l15)) 
}

if("Frac16" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9, msd.l10, msd.l11, msd.l12, msd.l13, msd.l14, msd.l15, msd.l16)) 
}

if("Frac17" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9, msd.l10, msd.l11, msd.l12, msd.l13, msd.l14, msd.l15, msd.l16, msd.l17)) 
}

if("Frac18" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9, msd.l10, msd.l11, msd.l12, msd.l13, msd.l14, msd.l15, msd.l16, msd.l17, msd.l18)) 
}

if("Frac19" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9, msd.l10, msd.l11, msd.l12, msd.l13, msd.l14, msd.l15, msd.l16, msd.l17, msd.l18, msd.l19)) 
}

if("Frac20" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9, msd.l10, msd.l11, msd.l12, msd.l13, msd.l14, msd.l15, msd.l16, msd.l17, msd.l18, msd.l19, msd.l20)) 
}

if("Frac21" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9, msd.l10, msd.l11, msd.l12, msd.l13, msd.l14, msd.l15, msd.l16, msd.l17, msd.l18, msd.l19, msd.l20, msd.l21)) 
}

if("Frac22" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9, msd.l10, msd.l11, msd.l12, msd.l13, msd.l14, msd.l15, msd.l16, msd.l17, msd.l18, msd.l19, msd.l20, msd.l21, msd.l22)) 
}
      
if("Frac23" %in% wellnames){
  wellside <- Reduce(function(...) merge(..., all=TRUE), list(msd.l1, msd.l2, msd.l3, msd.l4, msd.l5, msd.l6, msd.l7, msd.l8, msd.l9, msd.l10, msd.l11, msd.l12, msd.l13, msd.l14, msd.l15, msd.l16, msd.l17, msd.l18, msd.l19, msd.l20, msd.l21, msd.l22, msd.l23)) 
}


print("Get Obs Loc") 


# Get Obs Loc -------------------------------------------------------------


# EAST
if("Obs1" %in% wellnames){
  colnames(EASTo1)[3] <- "EAST"
}

if("Obs2" %in% wellnames){
  colnames(EASTo2)[3] <- "EAST"
}

if("Obs3" %in% wellnames){
  colnames(EASTo3)[3] <- "EAST"
}

if("Obs1" %in% wellnames){
  oeast <- EASTo1
}

if("Obs2" %in% wellnames){
  oeast <- Reduce(function(...) merge(..., all=TRUE), list(EASTo1,EASTo2))
}

if("Obs3" %in% wellnames){
  oeast <- Reduce(function(...) merge(..., all=TRUE), list(EASTo1,EASTo2,EASTo3))
}

# NORTH
if("Obs1" %in% wellnames){
  colnames(NORTHo1)[3] <- "NORTH"
}

if("Obs2" %in% wellnames){
  colnames(NORTHo2)[3] <- "NORTH"
}

if("Obs3" %in% wellnames){
  colnames(NORTHo3)[3] <- "NORTH"
}

if("Obs1" %in% wellnames){
  onorth <- NORTHo1
}
if("Obs2" %in% wellnames){
  onorth <- Reduce(function(...) merge(..., all=TRUE), list(NORTHo1,NORTHo2))
}

if("Obs3" %in% wellnames){
  onorth <- Reduce(function(...) merge(..., all=TRUE), list(NORTHo1,NORTHo2,NORTHo3))
}

# TVD
if("Obs1" %in% wellnames){
  colnames(TVDo1)[3] <- "TVD"
}

if("Obs2" %in% wellnames){
  colnames(TVDo2)[3] <- "TVD"
}

if("Obs3" %in% wellnames){
  colnames(TVDo3)[3] <- "TVD"
}


if("Obs1" %in% wellnames){
  otvd <- TVDo1
}
if("Obs2" %in% wellnames){
  otvd <- Reduce(function(...) merge(..., all=TRUE), list(TVDo1,TVDo2))
}

if("Obs3" %in% wellnames){
  otvd <- Reduce(function(...) merge(..., all=TRUE), list(TVDo1,TVDo2,TVDo3))
}

# TVDSS
if("Obs1" %in% wellnames){
  colnames(TVDSSo1)[3] <- "TVDSS"
}

if("Obs2" %in% wellnames){
  colnames(TVDSSo2)[3] <- "TVDSS"
}

if("Obs3" %in% wellnames){
  colnames(TVDSSo3)[3] <- "TVDSS"
}


if("Obs1" %in% wellnames){
  otvdss <- TVDSSo1
}
if("Obs2" %in% wellnames){
  otvdss <- Reduce(function(...) merge(..., all=TRUE), list(TVDSSo1,TVDSSo2))
}

if("Obs3" %in% wellnames){
  otvdss <- Reduce(function(...) merge(..., all=TRUE), list(TVDSSo1,TVDSSo2,TVDSSo3))
}




Obs_loc <- oeast %>% right_join(onorth, by=c("Well", "Stage"))
Obs_loc <- Obs_loc %>% right_join(otvd, by=c("Well", "Stage"))
Obs_loc <- Obs_loc %>% right_join(otvdss, by=c("Well", "Stage"))

ocolnames <- c("Well", "Stage", "oEast","oNorth","oTVD","oTVDSS")
colnames(Obs_loc) <- ocolnames




# Get Perf Loc ------------------------------------------------------------
print("Get Perf Loc")

# EAST
if("Frac1" %in% wellnames){
  colnames(EAST1)[3] <- "EAST"
}

if("Frac2" %in% wellnames){
  colnames(EAST2)[3] <- "EAST"
}

if("Frac3" %in% wellnames){
  colnames(EAST3)[3] <- "EAST"
}

if("Frac4" %in% wellnames){
  colnames(EAST4)[3] <- "EAST"
}

if("Frac1" %in% wellnames){
  peast <- EAST1
}

if("Frac2" %in% wellnames){
  peast <- Reduce(function(...) merge(..., all=TRUE), list(EAST1,EAST2))
}

if("Frac3" %in% wellnames){
  peast <- Reduce(function(...) merge(..., all=TRUE), list(EAST1,EAST2,EAST3))
}

if("Frac4" %in% wellnames){
  peast <- Reduce(function(...) merge(..., all=TRUE), list(EAST1,EAST2,EAST3,EAST4))
}

# NORTH
if("Frac1" %in% wellnames){
  colnames(NORTH1)[3] <- "NORTH"
}

if("Frac2" %in% wellnames){
  colnames(NORTH2)[3] <- "NORTH"
}

if("Frac3" %in% wellnames){
  colnames(NORTH3)[3] <- "NORTH"
}

if("Frac4" %in% wellnames){
  colnames(NORTH4)[3] <- "NORTH"
}


if("Frac1" %in% wellnames){
  pnorth <- NORTH1
}
if("Frac2" %in% wellnames){
  pnorth <- Reduce(function(...) merge(..., all=TRUE), list(NORTH1,NORTH2))
}

if("Frac3" %in% wellnames){
  pnorth <- Reduce(function(...) merge(..., all=TRUE), list(NORTH1,NORTH2,NORTH3))
}

if("Frac4" %in% wellnames){
  pnorth <- Reduce(function(...) merge(..., all=TRUE), list(NORTH1,NORTH2,NORTH3,NORTH4))
}

# TVD
if("Frac1" %in% wellnames){
  colnames(TVD1)[3] <- "TVD"
}

if("Frac2" %in% wellnames){
  colnames(TVD2)[3] <- "TVD"
}

if("Frac3" %in% wellnames){
  colnames(TVD3)[3] <- "TVD"
}

if("Frac4" %in% wellnames){
  colnames(TVD4)[3] <- "TVD"
}


if("Frac1" %in% wellnames){
  ptvd <- TVD1
}
if("Frac2" %in% wellnames){
  ptvd <- Reduce(function(...) merge(..., all=TRUE), list(TVD1,TVD2))
  }
  
if("Frac3" %in% wellnames){
  ptvd <- Reduce(function(...) merge(..., all=TRUE), list(TVD1,TVD2,TVD3))
}

if("Frac4" %in% wellnames){
  ptvd <- Reduce(function(...) merge(..., all=TRUE), list(TVD1,TVD2,TVD3,TVD4))
}

# TVDSS
if("Frac1" %in% wellnames){
  TVDSS1 <- TVD1
  TVDSS1$TVDSS <- (TVD1$TVD -Frac1_KB)*-1
}

if("Frac2" %in% wellnames){
  TVDSS2 <- TVD2
  TVDSS2$TVDSS <- (TVD2$TVD -Frac2_KB)*-1
}

if("Frac3" %in% wellnames){
  TVDSS3 <- TVD3
  TVDSS3$TVDSS <- (TVD3$TVD -Frac3_KB)*-1
}

if("Frac4" %in% wellnames){
  TVDSS4 <- TVD4
  TVDSS4$TVDSS <- (TVD4$TVD -Frac4_KB)*-1
}

if("Frac1" %in% wellnames){
  ptvdss <- TVDSS1
}
if("Frac2" %in% wellnames){
  ptvdss <- Reduce(function(...) merge(..., all=TRUE), list(TVDSS1,TVDSS2))
}

if("Frac3" %in% wellnames){
  ptvdss <- Reduce(function(...) merge(..., all=TRUE), list(TVDSS1,TVDSS2,TVDSS3))
}

if("Frac4" %in% wellnames){
  ptvdss <- Reduce(function(...) merge(..., all=TRUE), list(TVDSS1,TVDSS2,TVDSS3,TVDSS4))
}

# MD
if("Frac1" %in% wellnames){
  colnames(MD1)[3] <- "MD"
}

if("Frac2" %in% wellnames){
  colnames(MD2)[3] <- "MD"
}

if("Frac3" %in% wellnames){
  colnames(MD3)[3] <- "MD"
}

if("Frac4" %in% wellnames){
  colnames(MD4)[3] <- "MD"
}

if("Frac1" %in% wellnames){
  pmd <- MD1
}
if("Frac2" %in% wellnames){
  pmd <- Reduce(function(...) merge(..., all=TRUE), list(MD1,MD2))
}

if("Frac3" %in% wellnames){
  pmd <- Reduce(function(...) merge(..., all=TRUE), list(MD1,MD2,MD3))
}

if("Frac4" %in% wellnames){
  pmd <- Reduce(function(...) merge(..., all=TRUE), list(MD1,MD2,MD3,MD4))
}

Perf_loc <- peast %>% right_join(pnorth, by=c("Well", "Stage"))
Perf_loc <- Perf_loc %>% right_join(ptvd, by=c("Well", "Stage"))
Perf_loc <- Perf_loc %>% right_join(pmd, by=c("Well", "Stage"))
Perf_loc <- Perf_loc %>% right_join(ptvdss, by=c("Well", "Stage"))
Perf_loc <- Perf_loc[,c(1,2,3,4,5,8,6)]

pcolnames <- c("Well", "Stage", "pEast","pNorth","pTVD","pTVDSS","pMD")
colnames(Perf_loc) <- pcolnames

all_array <- all_array %>% left_join(Obs_loc, by=c("Well", "Stage"))
all_array <- all_array %>% left_join(Perf_loc, by=c("Well", "Stage"))

# Side Edge View ----------------------------------------------------------------
print("Creating Side-Edge Projections")

all_array <- all_array %>% arrange(all_array$`Event File`)
rows <- nrow(all_array)
events<-nrow(MSM_data)

#taking this out for debug
#all_array <- all_array[-(events+1:rows),] 

# RNC <- unique(all_array$Well)
# RNC <- length(RNC)
# RNE <- paste("Frac",RNC,"a.Frac",RNC,"..MD..ft..", sep="")


all_array <- all_array %>% arrange(all_array$`Event File`)

cmcol <- ncol(all_array)+1

all_array <- all_array %>%
  group_by(Well,Stage) %>%
  mutate(cumsum(abs(Magnitude)))
colnames(all_array)[cmcol] <- "Cum Mag"

all_array <- all_array %>% mutate(AB = (pTVD - `Depth ft (TVD)`))

all_array <- all_array %>% mutate(SideView = ((-1*`Easting ft (Abs)`) * sin(side * pi / 180))+ (`Northing ft (Abs)` * cos(side * pi / 180)))
all_array <- all_array %>% mutate(EdgeView = ((-1*`Easting ft (Abs)`) * sin(edge * pi / 180))+ (`Northing ft (Abs)` * cos(edge * pi / 180)))

all_array <- all_array %>% mutate(pSideView = ((-1*pEast) * sin(side * pi / 180))+ (pNorth * cos(side * pi / 180)))
all_array <- all_array %>% mutate(pEdgeView = ((-1*pEast) * sin(edge * pi / 180))+ (pNorth * cos(edge* pi / 180)))

all_array <- all_array %>% mutate(oSideView = ((-1*oEast) * sin(side * pi / 180))+ (oNorth * cos(side * pi / 180)))
all_array <- all_array %>% mutate(oEdgeView = ((-1*oEast) * sin(edge * pi / 180))+ (oNorth * cos(edge * pi / 180)))

# all_array[["Pressure (psi)"]][is.na(all_array[["Pressure (psi)"]])] <- 0
# all_array[["Flow (bpm)"]][is.na(all_array[["Flow (bpm)"]])] <- 0
# all_array[["Conc (ppg)"]][is.na(all_array[["Conc (ppg)"]])] <- 0

all_array$Moment <- (all_array$Magnitude * 3/2)+9.1 
all_array$Moment <- 10^all_array$Moment



all_array <- all_array %>%
  group_by(Well,Stage) %>%
  mutate("Cum Moment" = cumsum(abs(Moment)))


# Get lat long ------------------------------------------------------------
print("Creating Lat - Long")
 
d <<- data.frame(lon=all_array$`Easting ft (Abs)`, lat=all_array$`Northing ft (Abs)`)

d[["lon"]][is.na(d[["lon"]])] <- 0
d[["lat"]][is.na(d[["lat"]])] <- 0
tail(d)

coordinates(d) <- c("lon", "lat")
proj4string(d) <- CRS(paste("+init=epsg:",epsg,sep="")) # WGS state plane 32039-Texas Central, 32020-North Dakota North
cord.latlong <<- spTransform(d, CRS("+init=epsg:4267"))
cords <- data.frame(cord.latlong)

all_array['lat'] = cords['lat']
all_array['lon'] = cords['lon']

write.csv(all_array, file = "Values.csv")
#write.csv(all_array, file = "tmp.csv")
all_array <- read_csv("Values.csv")
                      
date_time <<- paste(all_array$"Acquisition Date (Local)",all_array$"Acquisition Time (Local)")
all_array <- add_column(all_array, date_time)

all_array <- all_array %>% mutate("Distance To Mid Perf" = sqrt(((all_array$`Easting ft (Abs)`-all_array$pEast)^2)+((all_array$`Northing ft (Abs)`-all_array$pNorth)^2)))

# Wellbores ---------------------------------------------------------------
print("Preparing Wellbores")

trace_colnames <- c("MD (ft)","Depth ft (TVD)",
                 "Depth ft (TVDSS)","Easting (ft)","Northing (ft)","Well", "lat", "lon")
if("Trace1" %in% wellnames){
  Trace1<-data.frame(wellsheets$Trace1)
  Trace1 <- subset(Trace1, select=-c(1,4,5))
  colnames(Trace1) <- trace_colnames
}

if("Trace2" %in% wellnames){
  Trace2<-data.frame(wellsheets$Trace2)
  Trace2 <- subset(Trace2, select=-c(1,4,5))
  colnames(Trace2) <- trace_colnames
}

if("Trace3" %in% wellnames){
  Trace3<-data.frame(wellsheets$Trace3)
  Trace3 <- subset(Trace3, select=-c(1,4,5))
  colnames(Trace3) <- trace_colnames
}

if("Trace4" %in% wellnames){
  Trace4<-data.frame(wellsheets$Trace4)
  Trace4 <- subset(Trace4, select=-c(1,4,5))
  colnames(Trace4) <- trace_colnames
}

if("Trace5" %in% wellnames){
  Trace5<-data.frame(wellsheets$Trace5)
  Trace5 <- subset(Trace5, select=-c(1,4,5))
  colnames(Trace5) <- trace_colnames
}

if("Trace6" %in% wellnames){
  Trace6<-data.frame(wellsheets$Trace6)
  Trace6 <- subset(Trace6, select=-c(1,4,5))
  colnames(Trace6) <- trace_colnames
}

if("Trace7" %in% wellnames){
  Trace7<-data.frame(wellsheets$Trace7)
  Trace7 <- subset(Trace7, select=-c(1,4,5))
  colnames(Trace7) <- trace_colnames
}

if("Trace8" %in% wellnames){
  Trace8<-data.frame(wellsheets$Trace4)
  Trace8 <- subset(Trace8, select=-c(1,4,5))
  colnames(Trace8) <- trace_colnames
}

if("ObTrace1" %in% wellnames){
  ObTrace1<-data.frame(wellsheets$ObTrace1)
  ObTrace1 <- subset(ObTrace1, select=-c(1,4,5))
  colnames(ObTrace1) <- trace_colnames
}

if("ObTrace2" %in% wellnames){
  ObTrace2<-data.frame(wellsheets$ObTrace2)
  ObTrace2 <- subset(ObTrace2, select=-c(1,4,5))
  colnames(ObTrace2) <- trace_colnames
}

if("RefTrace1" %in% wellnames){
  RTrace1<-data.frame(wellsheets$RefTrace1)
 RTrace1 <- subset(RTrace1, select=-c(1,4,5))
  colnames(RTrace1) <- trace_colnames
}

if("RefTrace2" %in% wellnames){
  RTrace2<-data.frame(wellsheets$RefTrace2)
  RTrace2 <- subset(RTrace2, select=-c(1,4,5))
  colnames(RTrace2) <- trace_colnames
}

if("RefTrace3" %in% wellnames){
  RTrace3<-data.frame(wellsheets$RefTrace3)
  RTrace3 <- subset(RTrace3, select=-c(1,4,5))
  colnames(RTrace3) <- trace_colnames
}

if("RefTrace4" %in% wellnames){
  RTrace4<-data.frame(wellsheets$RefTrace4)
  RTrace4 <- subset(RTrace4, select=-c(1,4,5))
  colnames(RTrace4) <- trace_colnames
}

if("RefTrace5" %in% wellnames){
  RTrace5<-data.frame(wellsheets$RefTrace5)
  RTrace5 <- subset(RTrace5, select=-c(1,4,5))
  colnames(RTrace5) <- trace_colnames
}

if("RefTrace6" %in% wellnames){
  RTrace6<-data.frame(wellsheets$RefTrace6)
  RTrace6 <- subset(RTrace6, select=-c(1,4,5))
  colnames(RTrace6) <- trace_colnames
}

if("RefTrace7" %in% wellnames){
  RTrace7<-data.frame(wellsheets$RefTrace7)
  RTrace7 <- subset(RTrace7, select=-c(1,4,5))
  colnames(RTrace7) <- trace_colnames
}

if("RefTrace8" %in% wellnames){
  RTrace8<-data.frame(wellsheets$RefTrace8)
  RTrace8 <- subset(RTrace8, select=-c(1,4,5))
  colnames(RTrace8) <- trace_colnames
}

if("RefTrace9" %in% wellnames){
  RTrace9<-data.frame(wellsheets$RefTrace9)
  RTrace9 <- subset(RTrace9, select=-c(1,4,5))
  colnames(RTrace9) <- trace_colnames
}

if("RefTrace10" %in% wellnames){
  RTrace10<-data.frame(wellsheets$RefTrace10)
  RTrace10 <- subset(RTrace10, select=-c(1,4,5))
  colnames(RTrace10) <- trace_colnames
}

if("RefTrace11" %in% wellnames){
  RTrace11<-data.frame(wellsheets$RefTrace11)
  RTrace11 <- subset(RTrace11, select=-c(1,4,5))
  colnames(RTrace11) <- trace_colnames
}

if("RefTrace12" %in% wellnames){
  RTrace12<-data.frame(wellsheets$RefTrace12)
  RTrace12 <- subset(RTrace12, select=-c(1,4,5))
  colnames(RTrace12) <- trace_colnames
}



if("Frac1" %in% wellnames){
  fplot1 <- Frac1
  fplot1 <- fplot1[,c(3,4,5,6,9)]
  fplotnames <- c("Depth ft (TVD)", "Depth ft (TVDSS)", "Easting ft (Abs)","Northing ft (Abs)","Well")
  colnames(fplot1) <- fplotnames
}
  
if("Frac2" %in% wellnames){
  fplot2 <- Frac2
  fplot2 <- fplot2[,c(3,4,5,6,9)]
  fplotnames <- c("Depth ft (TVD)", "Depth ft (TVDSS)", "Easting ft (Abs)","Northing ft (Abs)","Well")
  colnames(fplot2) <- fplotnames
}
  
if("Frac3" %in% wellnames){
  fplot3 <- Frac3
  fplot3 <- fplot3[,c(3,4,5,6,9)]
  fplotnames <- c("Depth ft (TVD)", "Depth ft (TVDSS)", "Easting ft (Abs)","Northing ft (Abs)","Well")
  colnames(fplot3) <- fplotnames
}

if("Frac4" %in% wellnames){
  fplot4 <- Frac4
  fplot4 <- fplot4[,c(3,4,5,6,9)]
  fplotnames <- c("Depth ft (TVD)", "Depth ft (TVDSS)", "Easting ft (Abs)","Northing ft (Abs)","Well")
  colnames(fplot4) <- fplotnames
}

if("Frac5" %in% wellnames){
  fplot5 <- Frac5
  fplot5 <- fplot5[,c(3,4,5,6,9)]
  fplotnames <- c("Depth ft (TVD)", "Depth ft (TVDSS)", "Easting ft (Abs)","Northing ft (Abs)","Well")
  colnames(fplot5) <- fplotnames
}

if("Frac6" %in% wellnames){
  fplot6 <- Frac6
  fplot6 <- fplot6[,c(3,4,5,6,9)]
  fplotnames <- c("Depth ft (TVD)", "Depth ft (TVDSS)", "Easting ft (Abs)","Northing ft (Abs)","Well")
  colnames(fplot6) <- fplotnames
}

if("Frac7" %in% wellnames){
  fplot7 <- Frac7
  fplot7 <- fplot7[,c(3,4,5,6,9)]
  fplotnames <- c("Depth ft (TVD)", "Depth ft (TVDSS)", "Easting ft (Abs)","Northing ft (Abs)","Well")
  colnames(fplot7) <- fplotnames
}

if("Frac8" %in% wellnames){
  fplot8 <- Frac8
  fplot8 <- fplot8[,c(3,4,5,6,9)]
  fplotnames <- c("Depth ft (TVD)", "Depth ft (TVDSS)", "Easting ft (Abs)","Northing ft (Abs)","Well")
  colnames(fplot8) <- fplotnames
}

if("Frac1" %in% wellnames){
  all_array <- all_array %>% full_join(fplot1, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS).y"] <- 'Depth ft (TVDSS).f1'
  colnames(all_array)[colnames(all_array) == "Easting ft (Abs).y"] <- 'Easting ft (Abs).f1'
  colnames(all_array)[colnames(all_array) == "Northing ft (Abs).y"] <- 'Northing ft (Abs).f1'
}

if("Frac2" %in% wellnames){
  all_array <- all_array %>% full_join(fplot2, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).f2'
  colnames(all_array)[colnames(all_array) == "Easting ft (Abs)"] <- 'Easting ft (Abs).f2'
  colnames(all_array)[colnames(all_array) == "Northing ft (Abs)"] <- 'Northing ft (Abs).f2'
}

if("Frac3" %in% wellnames){
  all_array <- all_array %>% full_join(fplot3, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).f3'
  colnames(all_array)[colnames(all_array) == "Easting ft (Abs)"] <- 'Easting ft (Abs).f3'
  colnames(all_array)[colnames(all_array) == "Northing ft (Abs)"] <- 'Northing ft (Abs).f3'
}

if("Frac4" %in% wellnames){
  all_array <- all_array %>% full_join(fplot4, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).f4'
  colnames(all_array)[colnames(all_array) == "Easting ft (Abs)"] <- 'Easting ft (Abs).f4'
  colnames(all_array)[colnames(all_array) == "Northing ft (Abs)"] <- 'Northing ft (Abs).f4'
}

if("Frac5" %in% wellnames){
  all_array <- all_array %>% full_join(fplot5, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).f5'
  colnames(all_array)[colnames(all_array) == "Easting ft (Abs)"] <- 'Easting ft (Abs).f5'
  colnames(all_array)[colnames(all_array) == "Northing ft (Abs)"] <- 'Northing ft (Abs).f5'
}

if("Frac6" %in% wellnames){
  all_array <- all_array %>% full_join(fplot6, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).f6'
  colnames(all_array)[colnames(all_array) == "Easting ft (Abs)"] <- 'Easting ft (Abs).f6'
  colnames(all_array)[colnames(all_array) == "Northing ft (Abs)"] <- 'Northing ft (Abs).f6'
}

if("Frac7" %in% wellnames){
  all_array <- all_array %>% full_join(fplot7, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).f7'
  colnames(all_array)[colnames(all_array) == "Easting ft (Abs)"] <- 'Easting ft (Abs).f7'
  colnames(all_array)[colnames(all_array) == "Northing ft (Abs)"] <- 'Northing ft (Abs).f7'
}

if("Frac8" %in% wellnames){
  all_array <- all_array %>% full_join(fplot8, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).f8'
  colnames(all_array)[colnames(all_array) == "Easting ft (Abs)"] <- 'Easting ft (Abs).f8'
  colnames(all_array)[colnames(all_array) == "Northing ft (Abs)"] <- 'Northing ft (Abs).f8'
}

if("Frac1" %in% wellnames){
  all_array <- all_array %>% full_join(Trace1, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "MD (ft)"] <- 'MD (ft).t1'
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).t1'
  colnames(all_array)[colnames(all_array) == "Easting (ft)"] <- 'Easting (ft).t1'
  colnames(all_array)[colnames(all_array) == "Northing (ft)"] <- 'Northing (ft).t1'
  colnames(all_array)[colnames(all_array) == "lat.y"] <- 'lat.t1'
  colnames(all_array)[colnames(all_array) == "lon.y"] <- 'long.t1'
  
}

if("Frac2" %in% wellnames){
  all_array <- all_array %>% full_join(Trace2, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "MD (ft)"] <- 'MD (ft).t2'
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).t2'
  colnames(all_array)[colnames(all_array) == "Easting (ft)"] <- 'Easting (ft).t2'
  colnames(all_array)[colnames(all_array) == "Northing (ft)"] <- 'Northing (ft).t2'
  colnames(all_array)[colnames(all_array) == "lat"] <- 'lat.t2'
  colnames(all_array)[colnames(all_array) == "lon"] <- 'long.t2'
}

if("Frac3" %in% wellnames){
  all_array <- all_array %>% full_join(Trace3, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "MD (ft)"] <- 'MD (ft).t3'
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).t3'
  colnames(all_array)[colnames(all_array) == "Easting (ft)"] <- 'Easting (ft).t3'
  colnames(all_array)[colnames(all_array) == "Northing (ft)"] <- 'Northing (ft).t3'
  colnames(all_array)[colnames(all_array) == "lat"] <- 'lat.t3'
  colnames(all_array)[colnames(all_array) == "lon"] <- 'long.t3'
}

if("Frac4" %in% wellnames){
  all_array <- all_array %>% full_join(Trace4, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "MD (ft)"] <- 'MD (ft).t4'
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).t4'
  colnames(all_array)[colnames(all_array) == "Easting (ft)"] <- 'Easting (ft).t4'
  colnames(all_array)[colnames(all_array) == "Northing (ft)"] <- 'Northing (ft).t4'
  colnames(all_array)[colnames(all_array) == "lat"] <- 'lat.t4'
  colnames(all_array)[colnames(all_array) == "lon"] <- 'long.t4'
}

if("Frac5" %in% wellnames){
  all_array <- all_array %>% full_join(Trace5, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "MD (ft)"] <- 'MD (ft).t5'
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).t5'
  colnames(all_array)[colnames(all_array) == "Easting (ft)"] <- 'Easting (ft).t5'
  colnames(all_array)[colnames(all_array) == "Northing (ft)"] <- 'Northing (ft).t5'
  colnames(all_array)[colnames(all_array) == "lat"] <- 'lat.t5'
  colnames(all_array)[colnames(all_array) == "lon"] <- 'long.t5'
}

if("Frac6" %in% wellnames){
  all_array <- all_array %>% full_join(Trace6, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "MD (ft)"] <- 'MD (ft).t6'
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).t6'
  colnames(all_array)[colnames(all_array) == "Easting (ft)"] <- 'Easting (ft).t6'
  colnames(all_array)[colnames(all_array) == "Northing (ft)"] <- 'Northing (ft).t6'
  colnames(all_array)[colnames(all_array) == "lat"] <- 'lat.t6'
  colnames(all_array)[colnames(all_array) == "lon"] <- 'long.t6'
}

if("Frac7" %in% wellnames){
  all_array <- all_array %>% full_join(Trace7, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "MD (ft)"] <- 'MD (ft).t7'
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).t7'
  colnames(all_array)[colnames(all_array) == "Easting (ft)"] <- 'Easting (ft).t7'
  colnames(all_array)[colnames(all_array) == "Northing (ft)"] <- 'Northing (ft).t7'
  colnames(all_array)[colnames(all_array) == "lat"] <- 'lat.t7'
  colnames(all_array)[colnames(all_array) == "lon"] <- 'long.t7'
}

if("Frac8" %in% wellnames){
  all_array <- all_array %>% full_join(Trace8, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "MD (ft)"] <- 'MD (ft).t8'
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).t8'
  colnames(all_array)[colnames(all_array) == "Easting (ft)"] <- 'Easting (ft).t8'
  colnames(all_array)[colnames(all_array) == "Northing (ft)"] <- 'Northing (ft).t8'
  colnames(all_array)[colnames(all_array) == "lat"] <- 'lat.t8'
  colnames(all_array)[colnames(all_array) == "lon"] <- 'long.t8'
}

if("Obs1" %in% wellnames){
  all_array <- all_array %>% full_join(ObTrace1, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "MD (ft)"] <- 'MD (ft).Obt1'
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).Obt1'
  colnames(all_array)[colnames(all_array) == "Easting (ft)"] <- 'Easting (ft).Obt1'
  colnames(all_array)[colnames(all_array) == "Northing (ft)"] <- 'Northing (ft).Obt1'
  colnames(all_array)[colnames(all_array) == "lat"] <- 'lat.Obt1'
  colnames(all_array)[colnames(all_array) == "lon"] <- 'long.Obt1'
}

if("Obs2" %in% wellnames){
  all_array <- all_array %>% full_join(ObTrace2, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "MD (ft)"] <- 'MD (ft).Obt2'
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).Obt2'
  colnames(all_array)[colnames(all_array) == "Easting (ft)"] <- 'Easting (ft).Obt2'
  colnames(all_array)[colnames(all_array) == "Northing (ft)"] <- 'Northing (ft).Obt2'
  colnames(all_array)[colnames(all_array) == "lat"] <- 'lat.Obt2'
  colnames(all_array)[colnames(all_array) == "lon"] <- 'long.Obt2'
}

if("RefTrace1" %in% wellnames){
  all_array <- all_array %>% full_join(RTrace1, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "MD (ft)"] <- 'MD (ft).R1'
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).R1'
  colnames(all_array)[colnames(all_array) == "Easting (ft)"] <- 'Easting (ft).R1'
  colnames(all_array)[colnames(all_array) == "Northing (ft)"] <- 'Northing (ft).R1'
  colnames(all_array)[colnames(all_array) == "lat"] <- 'lat.R1'
  colnames(all_array)[colnames(all_array) == "lon"] <- 'long.R1'
}

if("RefTrace2" %in% wellnames){
  all_array <- all_array %>% full_join(RTrace2, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "MD (ft)"] <- 'MD (ft).R2'
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).R2'
  colnames(all_array)[colnames(all_array) == "Easting (ft)"] <- 'Easting (ft).R2'
  colnames(all_array)[colnames(all_array) == "Northing (ft)"] <- 'Northing (ft).R2'
  colnames(all_array)[colnames(all_array) == "lat"] <- 'lat.R2'
  colnames(all_array)[colnames(all_array) == "lon"] <- 'long.R2'
  
}

if("RefTrace3" %in% wellnames){
  all_array <- all_array %>% full_join(RTrace3, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "MD (ft)"] <- 'MD (ft).R3'
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).R3'
  colnames(all_array)[colnames(all_array) == "Easting (ft)"] <- 'Easting (ft).R3'
  colnames(all_array)[colnames(all_array) == "Northing (ft)"] <- 'Northing (ft).R3'
  colnames(all_array)[colnames(all_array) == "lat"] <- 'lat.R3'
  colnames(all_array)[colnames(all_array) == "lon"] <- 'long.R3'
}

if("RefTrace4" %in% wellnames){
  all_array <- all_array %>% full_join(RTrace4, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "MD (ft)"] <- 'MD (ft).R4'
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).R4'
  colnames(all_array)[colnames(all_array) == "Easting (ft)"] <- 'Easting (ft).R4'
  colnames(all_array)[colnames(all_array) == "Northing (ft)"] <- 'Northing (ft).R4'
  colnames(all_array)[colnames(all_array) == "lat"] <- 'lat.R4'
  colnames(all_array)[colnames(all_array) == "lon"] <- 'long.R4'
}

if("RefTrace5" %in% wellnames){
  all_array <- all_array %>% full_join(RTrace5, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "MD (ft)"] <- 'MD (ft).R5'
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).R5'
  colnames(all_array)[colnames(all_array) == "Easting (ft)"] <- 'Easting (ft).R5'
  colnames(all_array)[colnames(all_array) == "Northing (ft)"] <- 'Northing (ft).R5'
  colnames(all_array)[colnames(all_array) == "lat"] <- 'lat.R5'
  colnames(all_array)[colnames(all_array) == "lon"] <- 'long.R5'
}

if("RefTrace6" %in% wellnames){
  all_array <- all_array %>% full_join(RTrace6, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "MD (ft)"] <- 'MD (ft).R6'
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).R6'
  colnames(all_array)[colnames(all_array) == "Easting (ft)"] <- 'Easting (ft).R6'
  colnames(all_array)[colnames(all_array) == "Northing (ft)"] <- 'Northing (ft).R6'
  colnames(all_array)[colnames(all_array) == "lat"] <- 'lat.R6'
  colnames(all_array)[colnames(all_array) == "lon"] <- 'long.R6'
}

if("RefTrace7" %in% wellnames){
  all_array <- all_array %>% full_join(RTrace7, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "MD (ft)"] <- 'MD (ft).R7'
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).R7'
  colnames(all_array)[colnames(all_array) == "Easting (ft)"] <- 'Easting (ft).R7'
  colnames(all_array)[colnames(all_array) == "Northing (ft)"] <- 'Northing (ft).R7'
  colnames(all_array)[colnames(all_array) == "lat"] <- 'lat.R7'
  colnames(all_array)[colnames(all_array) == "lon"] <- 'long.R7'
}

if("RefTrace8" %in% wellnames){
  all_array <- all_array %>% full_join(RTrace8, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "MD (ft)"] <- 'MD (ft).R8'
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).R8'
  colnames(all_array)[colnames(all_array) == "Easting (ft)"] <- 'Easting (ft).R8'
  colnames(all_array)[colnames(all_array) == "Northing (ft)"] <- 'Northing (ft).R8'
  colnames(all_array)[colnames(all_array) == "lat"] <- 'lat.R8'
  colnames(all_array)[colnames(all_array) == "lon"] <- 'long.R8'
}

if("RefTrace9" %in% wellnames){
  all_array <- all_array %>% full_join(RTrace9, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "MD (ft)"] <- 'MD (ft).R9'
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).R9'
  colnames(all_array)[colnames(all_array) == "Easting (ft)"] <- 'Easting (ft).R9'
  colnames(all_array)[colnames(all_array) == "Northing (ft)"] <- 'Northing (ft).R9'
  colnames(all_array)[colnames(all_array) == "lat"] <- 'lat.R9'
  colnames(all_array)[colnames(all_array) == "lon"] <- 'long.R9'
}

if("RefTrace10" %in% wellnames){
  all_array <- all_array %>% full_join(RTrace10, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "MD (ft)"] <- 'MD (ft).R10'
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).R10'
  colnames(all_array)[colnames(all_array) == "Easting (ft)"] <- 'Easting (ft).R10'
  colnames(all_array)[colnames(all_array) == "Northing (ft)"] <- 'Northing (ft).R10'
  colnames(all_array)[colnames(all_array) == "lat"] <- 'lat.R10'
  colnames(all_array)[colnames(all_array) == "lon"] <- 'long.R10'
}

if("RefTrace11" %in% wellnames){
  all_array <- all_array %>% full_join(RTrace11, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "MD (ft)"] <- 'MD (ft).R11'
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).R11'
  colnames(all_array)[colnames(all_array) == "Easting (ft)"] <- 'Easting (ft).R11'
  colnames(all_array)[colnames(all_array) == "Northing (ft)"] <- 'Northing (ft).R11'
  colnames(all_array)[colnames(all_array) == "lat"] <- 'lat.R11'
  colnames(all_array)[colnames(all_array) == "lon"] <- 'long.R11'
}

if("RefTrace12" %in% wellnames){
  all_array <- all_array %>% full_join(RTrace12, by=c("Well", "Depth ft (TVD)" ))
  colnames(all_array)[colnames(all_array) == "MD (ft)"] <- 'MD (ft).R12'
  colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS)"] <- 'Depth ft (TVDSS).R12'
  colnames(all_array)[colnames(all_array) == "Easting (ft)"] <- 'Easting (ft).R12'
  colnames(all_array)[colnames(all_array) == "Northing (ft)"] <- 'Northing (ft).R12'
  colnames(all_array)[colnames(all_array) == "lat"] <- 'lat.R12'
  colnames(all_array)[colnames(all_array) == "lon"] <- 'long.R12'
}


all_array <- all_array %>% arrange(all_array$`Event File`)
#all_array <-all_array %>% replace(is.na(.), "")
all_array <- subset(all_array, select=-c(X1))




if("Frac1" %in% wellnames){
  all_array <- all_array  %>% mutate_at(all_array$'MD (ft).t1',as.numeric)
  all_array <- all_array  %>% mutate_at('Depth ft (TVDSS).t1',as.numeric)
  all_array <- all_array  %>% mutate_at('Easting (ft).t1',as.numeric)
  all_array <- all_array  %>% mutate_at('Northing (ft).t1',as.numeric)
  all_array <- all_array  %>% mutate_at('lat.t1',as.numeric)
  all_array <- all_array  %>% mutate_at('long.t1',as.numeric)
  
}


if("Frac2" %in% wellnames){
  all_array <- all_array  %>% mutate_at('MD (ft).t2',as.numeric)
  all_array <- all_array  %>% mutate_at('Depth ft (TVDSS).t2',as.numeric)
  all_array <- all_array  %>% mutate_at('Easting (ft).t2',as.numeric)
  all_array <- all_array  %>% mutate_at('Northing (ft).t2',as.numeric)
  all_array <- all_array  %>% mutate_at('lat.t2',as.numeric)
  all_array <- all_array  %>% mutate_at('long.t2',as.numeric)
}

if("Frac3" %in% wellnames){
  all_array <- all_array  %>% mutate_at('MD (ft).t3',as.numeric)
  all_array <- all_array  %>% mutate_at('Depth ft (TVDSS).t3',as.numeric)
  all_array <- all_array  %>% mutate_at('Easting (ft).t3',as.numeric)
  all_array <- all_array  %>% mutate_at('Northing (ft).t3',as.numeric)
  all_array <- all_array  %>% mutate_at('lat.t3',as.numeric)
  all_array <- all_array  %>% mutate_at('long.t3',as.numeric)
}

if("Frac4" %in% wellnames){
  all_array <- all_array  %>% mutate_at('MD (ft).t4',as.numeric)
  all_array <- all_array  %>% mutate_at('Depth ft (TVDSS).t4',as.numeric)
  all_array <- all_array  %>% mutate_at('Easting (ft).t4',as.numeric)
  all_array <- all_array  %>% mutate_at('Northing (ft).t4',as.numeric)
  all_array <- all_array  %>% mutate_at('lat.t4',as.numeric)
  all_array <- all_array  %>% mutate_at('long.t4',as.numeric)
}

if("Frac5" %in% wellnames){
  all_array <- all_array  %>% mutate_at('MD (ft).t5',as.numeric)
  all_array <- all_array  %>% mutate_at('Depth ft (TVDSS).t5',as.numeric)
  all_array <- all_array  %>% mutate_at('Easting (ft).t5',as.numeric)
  all_array <- all_array  %>% mutate_at('Northing (ft).t5',as.numeric)
  all_array <- all_array  %>% mutate_at('lat.t5',as.numeric)
  all_array <- all_array  %>% mutate_at('long.t5',as.numeric)
}

if("Frac6" %in% wellnames){
  all_array <- all_array  %>% mutate_at('MD (ft).t6',as.numeric)
  all_array <- all_array  %>% mutate_at('Depth ft (TVDSS).t6',as.numeric)
  all_array <- all_array  %>% mutate_at('Easting (ft).t6',as.numeric)
  all_array <- all_array  %>% mutate_at('Northing (ft).t6',as.numeric)
  all_array <- all_array  %>% mutate_at('lat.t6',as.numeric)
  all_array <- all_array  %>% mutate_at('long.t6',as.numeric)
}

if("Frac7" %in% wellnames){
  all_array <- all_array  %>% mutate_at('MD (ft).t7',as.numeric)
  all_array <- all_array  %>% mutate_at('Depth ft (TVDSS).t7',as.numeric)
  all_array <- all_array  %>% mutate_at('Easting (ft).t7',as.numeric)
  all_array <- all_array  %>% mutate_at('Northing (ft).t7',as.numeric)
  all_array <- all_array  %>% mutate_at('lat.t7',as.numeric)
  all_array <- all_array  %>% mutate_at('long.t7',as.numeric)
}

if("Frac8" %in% wellnames){
  all_array <- all_array  %>% mutate_at('MD (ft).t8',as.numeric)
  all_array <- all_array  %>% mutate_at('Depth ft (TVDSS).t8',as.numeric)
  all_array <- all_array  %>% mutate_at('Easting (ft).t8',as.numeric)
  all_array <- all_array  %>% mutate_at('Northing (ft).t8',as.numeric)
  all_array <- all_array  %>% mutate_at('lat.t8',as.numeric)
  all_array <- all_array  %>% mutate_at('long.t8',as.numeric)
}





if("Frac1" %in% wellnames){
  all_array <- all_array %>% mutate(SideView.t1 = ((-1*`Easting (ft).t1`) * sin(side * pi / 180))+ (`Northing (ft).t1` * cos(side * pi / 180)))
  all_array <- all_array %>% mutate(EdgeView.t1 = ((-1*`Easting (ft).t1`) * sin(edge * pi / 180))+ (`Northing (ft).t1` * cos(edge * pi / 180)))
}

if("Frac2" %in% wellnames){
  all_array <- all_array %>% mutate(SideView.t2 = ((-1*`Easting (ft).t2`) * sin(side * pi / 180))+ (`Northing (ft).t2` * cos(side * pi / 180)))
  all_array <- all_array %>% mutate(EdgeView.t2 = ((-1*`Easting (ft).t2`) * sin(edge * pi / 180))+ (`Northing (ft).t2` * cos(edge * pi / 180)))
}

if("Frac3" %in% wellnames){
  all_array <- all_array %>% mutate(SideView.t3 = ((-1*`Easting (ft).t3`) * sin(side * pi / 180))+ (`Northing (ft).t3` * cos(side * pi / 180)))
  all_array <- all_array %>% mutate(EdgeView.t3 = ((-1*`Easting (ft).t3`) * sin(edge * pi / 180))+ (`Northing (ft).t3` * cos(edge * pi / 180)))
}

if("Frac4" %in% wellnames){
  all_array <- all_array %>% mutate(SideView.t4 = ((-1*`Easting (ft).t4`) * sin(side * pi / 180))+ (`Northing (ft).t4` * cos(side * pi / 180)))
  all_array <- all_array %>% mutate(EdgeView.t4 = ((-1*`Easting (ft).t4`) * sin(edge * pi / 180))+ (`Northing (ft).t4` * cos(edge * pi / 180)))
}

if("Frac5" %in% wellnames){
  all_array <- all_array %>% mutate(SideView.t5 = ((-1*`Easting (ft).t5`) * sin(side * pi / 180))+ (`Northing (ft).t5` * cos(side * pi / 180)))
  all_array <- all_array %>% mutate(EdgeView.t5 = ((-1*`Easting (ft).t5`) * sin(edge * pi / 180))+ (`Northing (ft).t5` * cos(edge * pi / 180)))
}

if("Frac6" %in% wellnames){
  all_array <- all_array %>% mutate(SideView.t6 = ((-1*`Easting (ft).t6`) * sin(side * pi / 180))+ (`Northing (ft).t6` * cos(side * pi / 180)))
  all_array <- all_array %>% mutate(EdgeView.t6 = ((-1*`Easting (ft).t6`) * sin(edge * pi / 180))+ (`Northing (ft).t6` * cos(edge * pi / 180)))
}

if("Frac7" %in% wellnames){
  all_array <- all_array %>% mutate(SideView.t7 = ((-1*`Easting (ft).t7`) * sin(side * pi / 180))+ (`Northing (ft).t7` * cos(side * pi / 180)))
  all_array <- all_array %>% mutate(EdgeView.t7 = ((-1*`Easting (ft).t7`) * sin(edge * pi / 180))+ (`Northing (ft).t7` * cos(edge * pi / 180)))
}

if("Frac8" %in% wellnames){
  all_array <- all_array %>% mutate(SideView.t8 = ((-1*`Easting (ft).t8`) * sin(side * pi / 180))+ (`Northing (ft).t8` * cos(side * pi / 180)))
  all_array <- all_array %>% mutate(EdgeView.t8 = ((-1*`Easting (ft).t8`) * sin(edge * pi / 180))+ (`Northing (ft).t8` * cos(edge * pi / 180)))
}

if("Obs1" %in% wellnames){
  all_array <- all_array  %>% mutate_at('MD (ft).Obt1',as.numeric)
  all_array <- all_array  %>% mutate_at('Depth ft (TVDSS).Obt1',as.numeric)
  all_array <- all_array  %>% mutate_at('Easting (ft).Obt1',as.numeric)
  all_array <- all_array  %>% mutate_at('Northing (ft).Obt1',as.numeric)
  all_array <- all_array  %>% mutate_at('lat.Obt1',as.numeric)
  all_array <- all_array  %>% mutate_at('long.Obt1',as.numeric)

}

if("Obs1" %in% wellnames){
  all_array <- all_array %>% mutate(ObsSideView.t1 = ((-1*`Easting (ft).Obt1`) * sin(side * pi / 180))+ (`Northing (ft).Obt1` * cos(side * pi / 180)))
  all_array <- all_array %>% mutate(ObsEdgeView.t1 = ((-1*`Easting (ft).Obt1`) * sin(edge * pi / 180))+ (`Northing (ft).Obt1` * cos(edge * pi / 180)))
  
}

if("Obs2" %in% wellnames){
  all_array <- all_array  %>% mutate_at('MD (ft).Obt2',as.numeric)
  all_array <- all_array  %>% mutate_at('Depth ft (TVDSS).Obt2',as.numeric)
  all_array <- all_array  %>% mutate_at('Easting (ft).Obt2',as.numeric)
  all_array <- all_array  %>% mutate_at('Northing (ft).Obt2',as.numeric)
  all_array <- all_array  %>% mutate_at('lat.Obt2',as.numeric)
  all_array <- all_array  %>% mutate_at('long.Obt2',as.numeric)
  
}

if("Obs2" %in% wellnames){
  all_array <- all_array %>% mutate(ObsSideView.t2 = ((-1*`Easting (ft).Obt2`) * sin(side * pi / 180))+ (`Northing (ft).Obt2` * cos(side * pi / 180)))
  all_array <- all_array %>% mutate(ObsEdgeView.t2 = ((-1*`Easting (ft).Obt2`) * sin(edge * pi / 180))+ (`Northing (ft).Obt2` * cos(edge * pi / 180)))

}

if("RefTrace1" %in% wellnames){
  all_array <- all_array  %>% mutate_at('MD (ft).R1',as.numeric)
  all_array <- all_array  %>% mutate_at('Depth ft (TVDSS).R1',as.numeric)
  all_array <- all_array  %>% mutate_at('Easting (ft).R1',as.numeric)
  all_array <- all_array  %>% mutate_at('Northing (ft).R1',as.numeric)
  all_array <- all_array  %>% mutate_at('lat.R1',as.numeric)
  all_array <- all_array  %>% mutate_at('long.R1',as.numeric)
}

if("RefTrace2" %in% wellnames){
  all_array <- all_array  %>% mutate_at('MD (ft).R2',as.numeric)
  all_array <- all_array  %>% mutate_at('Depth ft (TVDSS).R2',as.numeric)
  all_array <- all_array  %>% mutate_at('Easting (ft).R2',as.numeric)
  all_array <- all_array  %>% mutate_at('Northing (ft).R2',as.numeric)
  all_array <- all_array  %>% mutate_at('lat.R2',as.numeric)
  all_array <- all_array  %>% mutate_at('long.R2',as.numeric)
}

if("RefTrace3" %in% wellnames){
  all_array <- all_array  %>% mutate_at('MD (ft).R3',as.numeric)
  all_array <- all_array  %>% mutate_at('Depth ft (TVDSS).R3',as.numeric)
  all_array <- all_array  %>% mutate_at('Easting (ft).R3',as.numeric)
  all_array <- all_array  %>% mutate_at('Northing (ft).R3',as.numeric)
  all_array <- all_array  %>% mutate_at('lat.R3',as.numeric)
  all_array <- all_array  %>% mutate_at('long.R3',as.numeric)
}

if("RefTrace4" %in% wellnames){
  all_array <- all_array  %>% mutate_at('MD (ft).R4',as.numeric)
  all_array <- all_array  %>% mutate_at('Depth ft (TVDSS).R4',as.numeric)
  all_array <- all_array  %>% mutate_at('Easting (ft).R4',as.numeric)
  all_array <- all_array  %>% mutate_at('Northing (ft).R4',as.numeric)
  all_array <- all_array  %>% mutate_at('lat.R4',as.numeric)
  all_array <- all_array  %>% mutate_at('long.R4',as.numeric)
}

if("RefTrace5" %in% wellnames){
  all_array <- all_array  %>% mutate_at('MD (ft).R5',as.numeric)
  all_array <- all_array  %>% mutate_at('Depth ft (TVDSS).R5',as.numeric)
  all_array <- all_array  %>% mutate_at('Easting (ft).R5',as.numeric)
  all_array <- all_array  %>% mutate_at('Northing (ft).R5',as.numeric)
  all_array <- all_array  %>% mutate_at('lat.R5',as.numeric)
  all_array <- all_array  %>% mutate_at('long.R5',as.numeric)
}

if("RefTrace6" %in% wellnames){
  all_array <- all_array  %>% mutate_at('MD (ft).R6',as.numeric)
  all_array <- all_array  %>% mutate_at('Depth ft (TVDSS).R6',as.numeric)
  all_array <- all_array  %>% mutate_at('Easting (ft).R6',as.numeric)
  all_array <- all_array  %>% mutate_at('Northing (ft).R6',as.numeric)
  all_array <- all_array  %>% mutate_at('lat.R6',as.numeric)
  all_array <- all_array  %>% mutate_at('long.R6',as.numeric)
}

if("RefTrace7" %in% wellnames){
  all_array <- all_array  %>% mutate_at('MD (ft).R7',as.numeric)
  all_array <- all_array  %>% mutate_at('Depth ft (TVDSS).R7',as.numeric)
  all_array <- all_array  %>% mutate_at('Easting (ft).R7',as.numeric)
  all_array <- all_array  %>% mutate_at('Northing (ft).R7',as.numeric)
  all_array <- all_array  %>% mutate_at('lat.R7',as.numeric)
  all_array <- all_array  %>% mutate_at('long.R7',as.numeric)
}

if("RefTrace8" %in% wellnames){
  all_array <- all_array  %>% mutate_at('MD (ft).R8',as.numeric)
  all_array <- all_array  %>% mutate_at('Depth ft (TVDSS).R8',as.numeric)
  all_array <- all_array  %>% mutate_at('Easting (ft).R8',as.numeric)
  all_array <- all_array  %>% mutate_at('Northing (ft).R8',as.numeric)
  all_array <- all_array  %>% mutate_at('lat.R8',as.numeric)
  all_array <- all_array  %>% mutate_at('long.R8',as.numeric)
}

if("RefTrace9" %in% wellnames){
  all_array <- all_array  %>% mutate_at('MD (ft).R9',as.numeric)
  all_array <- all_array  %>% mutate_at('Depth ft (TVDSS).R9',as.numeric)
  all_array <- all_array  %>% mutate_at('Easting (ft).R9',as.numeric)
  all_array <- all_array  %>% mutate_at('Northing (ft).R9',as.numeric)
  all_array <- all_array  %>% mutate_at('lat.R9',as.numeric)
  all_array <- all_array  %>% mutate_at('long.R9',as.numeric)
}

if("RefTrace10" %in% wellnames){
  all_array <- all_array  %>% mutate_at('MD (ft).R10',as.numeric)
  all_array <- all_array  %>% mutate_at('Depth ft (TVDSS).R10',as.numeric)
  all_array <- all_array  %>% mutate_at('Easting (ft).R10',as.numeric)
  all_array <- all_array  %>% mutate_at('Northing (ft).R10',as.numeric)
  all_array <- all_array  %>% mutate_at('lat.R10',as.numeric)
  all_array <- all_array  %>% mutate_at('long.R10',as.numeric)
}

if("RefTrace11" %in% wellnames){
  all_array <- all_array  %>% mutate_at('MD (ft).R11',as.numeric)
  all_array <- all_array  %>% mutate_at('Depth ft (TVDSS).R11',as.numeric)
  all_array <- all_array  %>% mutate_at('Easting (ft).R11',as.numeric)
  all_array <- all_array  %>% mutate_at('Northing (ft).R11',as.numeric)
  all_array <- all_array  %>% mutate_at('lat.R11',as.numeric)
  all_array <- all_array  %>% mutate_at('long.R11',as.numeric)
}

if("RefTrace12" %in% wellnames){
  all_array <- all_array  %>% mutate_at('MD (ft).R12',as.numeric)
  all_array <- all_array  %>% mutate_at('Depth ft (TVDSS).R12',as.numeric)
  all_array <- all_array  %>% mutate_at('Easting (ft).R12',as.numeric)
  all_array <- all_array  %>% mutate_at('Northing (ft).R12',as.numeric)
  all_array <- all_array  %>% mutate_at('lat.R12',as.numeric)
  all_array <- all_array  %>% mutate_at('long.R12',as.numeric)
}



if("RefTrace1" %in% wellnames){
  all_array <- all_array %>% mutate(RefSideView.t1 = ((-1*`Easting (ft).R1`) * sin(side * pi / 180))+ (`Northing (ft).R1` * cos(side * pi / 180)))
  all_array <- all_array %>% mutate(RefEdgeView.t1 = ((-1*`Easting (ft).R1`) * sin(edge * pi / 180))+ (`Northing (ft).R1` * cos(edge * pi / 180)))
}

if("RefTrace2" %in% wellnames){
  all_array <- all_array %>% mutate(RefSideView.t2 = ((-1*`Easting (ft).R2`) * sin(side * pi / 180))+ (`Northing (ft).R2` * cos(side * pi / 180)))
  all_array <- all_array %>% mutate(RefEdgeView.t2 = ((-1*`Easting (ft).R2`) * sin(edge * pi / 180))+ (`Northing (ft).R2` * cos(edge * pi / 180)))
}

if("RefTrace3" %in% wellnames){
  all_array <- all_array %>% mutate(RefSideView.t3 = ((-1*`Easting (ft).R3`) * sin(side * pi / 180))+ (`Northing (ft).R3` * cos(side * pi / 180)))
  all_array <- all_array %>% mutate(RefEdgeView.t3 = ((-1*`Easting (ft).R3`) * sin(edge * pi / 180))+ (`Northing (ft).R3` * cos(edge * pi / 180)))
}

if("RefTrace4" %in% wellnames){
  all_array <- all_array %>% mutate(RefSideView.t4 = ((-1*`Easting (ft).R4`) * sin(side * pi / 180))+ (`Northing (ft).R4` * cos(side * pi / 180)))
  all_array <- all_array %>% mutate(RefEdgeView.t4 = ((-1*`Easting (ft).R4`) * sin(edge * pi / 180))+ (`Northing (ft).R4` * cos(edge * pi / 180)))
}

if("RefTrace5" %in% wellnames){
  all_array <- all_array %>% mutate(RefSideView.t5 = ((-1*`Easting (ft).R5`) * sin(side * pi / 180))+ (`Northing (ft).R5` * cos(side * pi / 180)))
  all_array <- all_array %>% mutate(RefEdgeView.t5 = ((-1*`Easting (ft).R5`) * sin(edge * pi / 180))+ (`Northing (ft).R5` * cos(edge * pi / 180)))
}

if("RefTrace6" %in% wellnames){
  all_array <- all_array %>% mutate(RefSideView.t6 = ((-1*`Easting (ft).R6`) * sin(side * pi / 180))+ (`Northing (ft).R6` * cos(side * pi / 180)))
  all_array <- all_array %>% mutate(RefEdgeView.t6 = ((-1*`Easting (ft).R6`) * sin(edge * pi / 180))+ (`Northing (ft).R6` * cos(edge * pi / 180)))
}

if("RefTrace7" %in% wellnames){
  all_array <- all_array %>% mutate(RefSideView.t7 = ((-1*`Easting (ft).R7`) * sin(side * pi / 180))+ (`Northing (ft).R7` * cos(side * pi / 180)))
  all_array <- all_array %>% mutate(RefEdgeView.t7 = ((-1*`Easting (ft).R7`) * sin(edge * pi / 180))+ (`Northing (ft).R7` * cos(edge * pi / 180)))
}

if("RefTrace8" %in% wellnames){
  all_array <- all_array %>% mutate(RefSideView.t8 = ((-1*`Easting (ft).R8`) * sin(side * pi / 180))+ (`Northing (ft).R8` * cos(side * pi / 180)))
  all_array <- all_array %>% mutate(RefEdgeView.t8 = ((-1*`Easting (ft).R8`) * sin(edge * pi / 180))+ (`Northing (ft).R8` * cos(edge * pi / 180)))
}

if("RefTrace9" %in% wellnames){
  all_array <- all_array %>% mutate(RefSideView.t9 = ((-1*`Easting (ft).R9`) * sin(side * pi / 180))+ (`Northing (ft).R9` * cos(side * pi / 180)))
  all_array <- all_array %>% mutate(RefEdgeView.t9 = ((-1*`Easting (ft).R9`) * sin(edge * pi / 180))+ (`Northing (ft).R9` * cos(edge * pi / 180)))
}

if("RefTrace10" %in% wellnames){
  all_array <- all_array %>% mutate(RefSideView.t10 = ((-1*`Easting (ft).R10`) * sin(side * pi / 180))+ (`Northing (ft).R10` * cos(side * pi / 180)))
  all_array <- all_array %>% mutate(RefEdgeView.t10 = ((-1*`Easting (ft).R10`) * sin(edge * pi / 180))+ (`Northing (ft).R10` * cos(edge * pi / 180)))
}

if("RefTrace11" %in% wellnames){
  all_array <- all_array %>% mutate(RefSideView.t11 = ((-1*`Easting (ft).R11`) * sin(side * pi / 180))+ (`Northing (ft).R11` * cos(side * pi / 180)))
  all_array <- all_array %>% mutate(RefEdgeView.t11 = ((-1*`Easting (ft).R11`) * sin(edge * pi / 180))+ (`Northing (ft).R11` * cos(edge * pi / 180)))
}

if("RefTrace12" %in% wellnames){
  all_array <- all_array %>% mutate(RefSideView.t12 = ((-1*`Easting (ft).R12`) * sin(side * pi / 180))+ (`Northing (ft).R12` * cos(side * pi / 180)))
  all_array <- all_array %>% mutate(RefEdgeView.t12 = ((-1*`Easting (ft).R12`) * sin(edge * pi / 180))+ (`Northing (ft).R12` * cos(edge * pi / 180)))
}

colnames(all_array)[colnames(all_array) == "Easting ft (Abs).x"] <- 'Easting ft (Abs)'
colnames(all_array)[colnames(all_array) == "Northing ft (Abs).x"] <- 'Northing ft (Abs)'
colnames(all_array)[colnames(all_array) == "Depth ft (TVDSS).x"] <- 'Depth ft (TVDSS)'
colnames(all_array)[colnames(all_array) == "lat.x"] <- 'lat'
colnames(all_array)[colnames(all_array) == "lon.x"] <- 'lon'

print("Writing Outputs")


write.csv(all_array, file = "Values.csv")
write.xlsx(all_array, 'PowerBi.xlsx')

print("PowerBi file created")
events2<-nrow(all_array)
print(paste(events2, "Events Run"))

all_array2 <- all_array[-(events+1:rows),]

write.csv(all_array2, file = "JustData.csv")

# END OF RUN --------------------------------------------------------------
# all_array <- read_csv("Values.csv")
#all_array <- all_array %>%  mutate_all(~replace_na(., " "))
# all_array <- all_array %>%
#   mutate_if(is.logical, as.numeric)
 }
# run
spot_load()

