packages = c("dplyr","readxl","rio", "openxlsx", "readr", "tidyr","tcltk","tibbletime","tibble","purrr","rgdal","ggplot2" )

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})
WD <- tk_choose.dir(getwd(), "Choose a suitable folder")
setwd(WD)

Values <- import("E:\\LaredoPet\\R\\Post-3SA\\Elapsed_load.csv")
Values <- read_csv("Elapsed_load.csv", col_types = cols(Stage = col_number(), 
                                                    date_time = col_datetime(format = "%m/%d/%Y %H:%M:%S")))

Values <- read_csv("All_Treat_Elapsed-Same.csv", col_types = cols(Stage = col_number(), 
                                                  date_time = col_datetime(format = "%m/%d/%Y %H:%M:%S")))

################Start#####################################################
# Run This
################################################################################
i <- 1
################################################################################
#Values <- read_csv("Values-use.csv", col_types = cols(Stage = col_number(), 
#                                                        date_time = col_datetime(format = "%m/%d/%Y %H:%M:%S")))

Values <- import("Values-use.xlsx")
#data <- Values[,c(2:3,6:9,14,19,20,65,63,69:73,86)]
data <- Values[c("Well", "Stage","Depth ft (TVD)","Depth ft (TVDSS)","Easting ft (Abs)", "Northing ft (Abs)","Event File", "Solution","WS","pEast","pNorth","pTVD","pTVDSS","date_time")]
head(data)
# Set projection
side <- 0
edge <- 284

well_list <- unique(data$`Well`)
# wellnm <- well_list[1:3]
wellnm <- well_list[3]
print(wellnm)

i <- 1
# Run This
dataset <- data %>% filter(data$`Well` == wellnm)
stg_list <-unique(dataset$Stage)
stg_list <- stg_list[!is.na(stg_list)]
stg <- stg_list[i]
dataset <- dataset %>% filter(dataset$Stage == stg)

#dataset <- data
################################Projections###################################
dataset <- dataset %>% mutate(FracAzim = ((-1*`Easting ft (Abs)`) * sin(edge * pi / 180))+ (`Northing ft (Abs)` * cos(edge * pi / 180)))
dataset <- dataset %>% mutate(PerfAzim = ((-1*pEast) * sin(edge * pi / 180))+ (pNorth * cos(edge * pi / 180)))

dataset$FracAzim[1]
dataset$PerfAzim[1]
dataset$`Depth ft (TVDSS)`[1]
dataset$pTVDSS[1]


############ Distance Along Azimuth #################
azim <- ggplot(dataset, aes(x=FracAzim, y= `Depth ft (TVDSS)`)) + geom_point() + geom_point(x=dataset$PerfAzim,y=dataset$pTVDSS, color = "red", size=3)
dataset <- dataset %>% mutate(hl_scale = (FracAzim - PerfAzim[1]))

##########Height################
dataset <- dataset %>% mutate(height_scale = (`Depth ft (TVDSS)` - pTVDSS[1]))
# plot(dataset$hl_scale,dataset$height_scale)
above <- dataset %>% filter(dataset$height_scale > 0)
# plot(above$hl_scale,above$height_scale)
below <- dataset %>% filter(dataset$height_scale < 0)
# plot(below$hl_scale,below$height_scale)

dataset <- dataset %>% mutate("above_p90" = sort(above$height_scale)[0.90*length(above$height_scale)])
dataset <- dataset %>% mutate("below_p90" = sort(below$height_scale)[0.10*length(below$height_scale)])

west <- dataset %>% filter(dataset$hl_scale < 0)
# plot(west$hl_scale,west$`Depth ft (TVDSS)`)
east <- dataset %>% filter(dataset$hl_scale > 0)
# plot(east$hl_scale,east$`Depth ft (TVDSS)`)


dataset <- dataset %>% mutate("west_HL_p10" = sort(west$hl_scale)[0.90*length(west$hl_scale)])
dataset <- dataset %>% mutate("west_HL_p90" = sort(west$hl_scale)[0.10*length(west$hl_scale)])

dataset <- dataset %>% mutate("east_HL_p90" = sort(east$hl_scale)[0.90*length(east$hl_scale)])
dataset <- dataset %>% mutate("east_HL_p10" = sort(east$hl_scale)[0.10*length(east$hl_scale)])
miny <- min(dataset$`Depth ft (TVDSS)`)
miny <- floor(miny)
perfcenter <- data.frame(x=0, y=0)

plot <- ggplot(dataset, aes(x=hl_scale, y= height_scale)) + geom_point() + geom_point(x=0,y=0, color = "red", size=3)+ geom_point() + 
  geom_point(data = perfcenter, aes(x=0,y=0), color = "red", size=3)+
  geom_point(data = west, aes(x=hl_scale,y=height_scale), color = "gray48", size=1)+
  geom_point(data = east, aes(x=hl_scale,y=height_scale), color = "gray48", size=1)+
  theme_bw()+labs(x="Half-Length (ft)",y="Height Growth From Perforations (ft)")+ggtitle(paste0("Half-Length Plot - ",wellnm,": Stage: ",stg," (Gun Barrel View)") )+ 
  geom_vline(xintercept = dataset$west_HL_p90, linetype="dotted",color = "red", size=1.2)+geom_vline(xintercept = dataset$east_HL_p90, linetype="dotted",color = "red", size=1.2)+
  ylim(-2000, 2500)+ xlim(-2000,2000)

plot2 <- plot +
  geom_text(aes(x=west_HL_p90, label=sprintf("West - P90:  %0.2f", round(west_HL_p90, digits = 2)), y=-1000), colour="black", angle=90, vjust = 1.2, text=element_text(size=8))+
  geom_text(aes(x=0, label=sprintf("Perfs"), y=0), colour="black", angle=0, vjust = 1.5, text=element_text(size=20))+ geom_vline(xintercept = dataset$east_HL_p90, linetype="dotted",color = "red", size=1.2)+ 
  geom_text(aes(x=east_HL_p90, label=sprintf("East - P90:  %0.2f", round(east_HL_p90, digits = 2)), y=-1000), colour="black", angle=90, vjust = 1.2, text=element_text(size=8))

plot3 <- plot2 + geom_hline(yintercept = dataset$above_p90, linetype="dotted",color = "red", size=1.2)+ 
  geom_text(aes(x=800, label=sprintf("Above - P90:  %0.2f", round(dataset$above_p90, digits = 2)), y=dataset$above_p90), colour="black", angle=0, vjust = -1, text=element_text(size=8))+ 
  geom_hline(yintercept = dataset$below_p90, linetype="dotted",color = "red", size=1.2)+ 
  geom_text(aes(x=800, label=sprintf("Below - P90:  %0.2f", round(dataset$below_p90, digits = 2)), y=dataset$below_p90), colour="black", angle=0, vjust = -1, text=element_text(size=8))

 ggsave(filename= paste(wellnm,"- Stage ",stg,"-Half-Length-Height.png"), width=11, height=6.5, dpi=300)

 plot3
#write.csv(both, file = paste(wellnm,"- Stage ",stg,"Half-Length-Stats.csv"))

#########Width################
side <- 180
dataset <- dataset %>% mutate(PerfWidthAzim = ((-1*pEast) * sin(side * pi / 180))+ (pNorth * cos(side * pi / 180)))
dataset <- dataset %>% mutate(WidthAz = ((-1*`Easting ft (Abs)`) * sin(side * pi / 180))+ (`Northing ft (Abs)` * cos(side * pi / 180)))
dataset <- dataset %>% mutate(width_scale = (WidthAz - PerfWidthAzim[1]))
# plot(dataset$width_scale,dataset$height_scale)

north <- dataset %>% filter(dataset$width_scale < 0)
# plot(north$width_scale,north$height_scale)
south <- dataset %>% filter(dataset$width_scale > 0)
# plot(south$width_scale,south$height_scale)

dataset <- dataset %>% mutate("north_p90" = sort(north$width_scale)[0.10*length(north$width_scale)])
dataset <- dataset %>% mutate("south_p90" = sort(south$width_scale)[0.90*length(south$width_scale)])

plot4 <- ggplot(dataset, aes(x=width_scale, y= height_scale)) + geom_point() + geom_point(x=0,y=0, color = "red", size=3)+ geom_point() + 
  geom_point(data = perfcenter, aes(x=0,y=0), color = "red", size=3)+
  geom_point(data = north, aes(x=width_scale,y=height_scale), color = "gray48", size=1)+
  geom_point(data = south, aes(x=width_scale,y=height_scale), color = "gray48", size=1)+
  theme_bw()+labs(x="Width (ft)",y="Height Growth From Perforations (ft)")+ggtitle(paste0("Width Plot - ",wellnm,": Stage: ",stg," (Side View)") )+ 
  geom_vline(xintercept = dataset$north_p90, linetype="dotted",color = "red", size=1.2)+geom_vline(xintercept = dataset$south_p90, linetype="dotted",color = "red", size=1.2)+
  ylim(-2000, 2500)+ xlim(-1000,1500)

plot5 <- plot4 +
  geom_text(aes(x=dataset$north_p90, label=sprintf("North - P90:  %0.2f", round(dataset$north_p90, digits = 2)), y=-1500), colour="black", angle=90, vjust = 1.2, text=element_text(size=8))+
  geom_text(aes(x=0, label=sprintf("Perfs"), y=0), colour="black", angle=0, vjust = 1.5, text=element_text(size=20))+ geom_vline(xintercept = dataset$south_p90, linetype="dotted",color = "red", size=1.2)+ 
  geom_text(aes(x=dataset$south_p90, label=sprintf("South - P90:  %0.2f", round(dataset$south_p90, digits = 2)), y=-1500), colour="black", angle=90, vjust = 1.2, text=element_text(size=8))

plot6 <- plot5 + geom_hline(yintercept = dataset$above_p90, linetype="dotted",color = "red", size=1.2)+ 
  geom_text(aes(x=800, label=sprintf("Above - P90:  %0.2f", round(dataset$above_p90, digits = 2)), y=dataset$above_p90), colour="black", angle=0, vjust = -1, text=element_text(size=8))+ 
  geom_hline(yintercept = dataset$below_p90, linetype="dotted",color = "red", size=1.2)+ 
  geom_text(aes(x=800, label=sprintf("Below - P90:  %0.2f", round(dataset$below_p90, digits = 2)), y=dataset$below_p90), colour="black", angle=0, vjust = -1, text=element_text(size=8))
plot6
 ggsave(filename= paste(wellnm,"- Stage ",stg,"-Width-Height.png"), width=11, height=6.5, dpi=300)
# library("cowplot")
# library("patchwork")
# plot3/plot6
#plot_grid(plot3,plot6)
#ggsave(filename= paste(wellnm,"- Stage ",stg,"-Stats.png"), width=11, height=6.5, dpi=300)
#out = bind_rows(east,west,north,south,dist_out)
#glimpse(out)
write.csv(dataset, file = paste(wellnm,"- Stage ",stg,"-P90_Stats.csv"))
print(paste(wellnm,"- Stage ",stg))
i <- i +1

