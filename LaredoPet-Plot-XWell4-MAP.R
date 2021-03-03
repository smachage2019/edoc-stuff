packages = c("dplyr","readxl","rio","openxlsx", "readr", "plotly","tcltk","htmlwidgets","patchwork","ggplot2"  )

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

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

Values <- import("combi.xlsx")

source.list <- unique(Values$Type)

Geo <- Values %>% filter(Values$Type == "Geophone")
Das <- Values %>% filter(Values$Type == "DAS")

newcolnames <- c("Deviation Survey","MD (ft)","TVD (ft)","E/W Relative Local Tangent Plane (ft)",
                 "N/S Relative Local Tangent Plane (ft)","TVDSS (ft)","Easting (ft)",
                 "Northing (ft)","Well", "Latitude", "Longitude")

wellsheets <- read_excel_allsheets("R-Wells.xlsx")
wellnames <- names(wellsheets)
# write.xlsx(Frac1, 'frac1.xlsx')
if("Trace1" %in% wellnames){
  Trace1<-data.frame(wellsheets$Trace1)
  colnames(Trace1) <- newcolnames
  Trace1 <- Trace1[-c(4,5)]
}
write.csv(Trace1, file = "Trace1.csv")
if("Trace2" %in% wellnames){
  Trace2<-data.frame(wellsheets$Trace2)
  colnames(Trace2) <- newcolnames
  Trace2 <- Trace2[-c(4,5)]
}
write.csv(Trace2, file = "Trace2.csv")
if("Trace3" %in% wellnames){
  Trace3<-data.frame(wellsheets$Trace3)
  colnames(Trace3) <- newcolnames
  Trace3 <- Trace3[-c(4,5)]
}
write.csv(Trace3, file = "Trace3.csv")
if("Trace4" %in% wellnames){
  Trace4<-data.frame(wellsheets$Trace4)
  colnames(Trace4) <- newcolnames
  Trace4 <- Trace4[-c(4,5)]
}
write.csv(Trace4, file = "Trace4.csv")
if("Trace5" %in% wellnames){
  Trace5<-data.frame(wellsheets$Trace5)
  colnames(Trace5) <- newcolnames
  Trace5 <- Trace5[-c(4,5)]
}
write.csv(Trace5, file = "Trace5.csv")
if("RefTrace1" %in% wellnames){
  RefTrace1<-data.frame(wellsheets$RefTrace1)
  colnames(RefTrace1) <- newcolnames
  RefTrace1 <- RefTrace1[-c(4,5)]
}
write.csv(RefTrace1, file = "RefTrace1.csv")

if("RefTrace2" %in% wellnames){
  RefTrace2<-data.frame(wellsheets$RefTrace2)
  colnames(RefTrace2) <- newcolnames
  RefTrace2 <- RefTrace2[-c(4,5)]
}
write.csv(RefTrace2, file = "RefTrace2.csv")

if("RefTrace3" %in% wellnames){
  RefTrace3<-data.frame(wellsheets$RefTrace3)
  colnames(RefTrace3) <- newcolnames
  RefTrace3 <- RefTrace3[-c(4,5)]
}
write.csv(RefTrace3, file = "RefTrace3.csv")


if("ObTrace1" %in% wellnames){
  ObTrace1<-data.frame(wellsheets$ObTrace1)
  colnames(ObTrace1) <- newcolnames
  ObTrace1 <- ObTrace1[-c(4,5)]
}
write.csv(ObTrace1, file = "ObTrace1.csv")

file.list <- list.files(pattern="*.csv", recursive = FALSE)
edf.list <- sapply(file.list, read.csv, simplify=FALSE)
library(purrr)
file.list2 <- list.files(pattern='*.csv')
file.list2 <- setNames(file.list, file.list) 
library(data.table)
edf <- rbindlist(edf.list, idcol = "id", fill=TRUE)
edf <- map_df(file.list2, read.csv, .id = "id")
write.csv(edf, file = "Wells.csv")

wells <- import ("Wells.csv")
##########################################################

library(readxl)
CWC <- import("CWC_data2.xlsx")
#DASfilt <- 
Values <- import("GEOMSM_data.xlsx")
colnames(CWC)[4] <- "Stage"
colnames(CWC)[3] <- "Well"
str(CWC$Stage)
#CWC <- CWC  %>% mutate_at("Stage",as.numeric)


well_list <- unique(Values$Well)
print(well_list)

wellnm <- well_list[1]
print(wellnm)

stgdata <- Values %>% filter(Values$Well == wellnm)
#write.csv(stgdata, file = "stgdata.csv")
D <- stgdata$`Distance To Mid Perf`
# D <- stgdata$`Distance From Perfs`
M <- stgdata$Magnitude
#plot(D,M)

q <- plot_ly(x=D, y=M, type="scatter", mode="markers",text = paste("Mag:", M," <br> Dist:",D," </br>"), hoverinfo = 'text', 
             color=~stgdata$Confidence, marker=list(size=7,opacity=0.5)) %>% layout(
               title = paste(wellnm,"","Distance From Perf vs Magnitude"), scene = list(
                 xaxis = list(title = "Distance (ft)"),
                 yaxis = list(title = "Magnitude")
               )) 
# p <- plot_ly(stgdata, x=~`Distance From Array 1`, y=~Magnitude, type="scatter", mode="markers",text = paste("Mag:", stgdata$Magnitude," <br> Dist:",stgdata$`Distance From Array 1`," </br>"), hoverinfo = 'text',
#              color=~Stage, marker=list(size=5,opacity=0.3)) %>% layout(
#                title = " ", scene = list(
#                  xaxis = list(title = "Distance (ft)"),
#                  yaxis = list(title = "Magnitude")
#                ))
# p

# q <- plot_ly(stgdata, x=~`Distance To Mid Perf`, y=~Magnitude, type="scatter", mode="markers",text = paste("Mag:", stgdata$Magnitude," <br> Dist:",stgdata$`Distance To Mid Perf`," </br>"), hoverinfo = 'text', 
#              color=~stgdata$Confidence, marker=list(size=7,opacity=0.3)) %>% layout(
#                title = paste(wellnm,"","Distance From Perf vs Magnitude"), scene = list(
#                  xaxis = list(title = "Distance (ft)"),
#                  yaxis = list(title = "Magnitude")
#                )) 
q

 saveWidget(q,paste(wellnm,"", "QC.html"), selfcontained = T, libdir = "lib")
 

# MSM
stgdata <- stgdata %>% filter(stgdata$Magnitude < -0.5)
stgdata <- stgdata %>% filter(stgdata$Confidence > 2)
stgdata <- stgdata %>% filter(stgdata$`Distance From Array 1` < 3364)

stgdata <- stgdata %>% filter(stgdata$`Distance From Perfs` < 3000)

plot(stgdata$`Distance To Mid Perf`, stgdata$Magnitude)
#### Stage ####
stg_list <- unique(stgdata$Stage)
print(stg_list)

stgdata <- DASfilt %>% filter(DASfilt$Well == wellnm)
stg <- stg_list[1]
stage <- stgdata %>% filter(stgdata$Stage == stg)

fstgdata <- FRAC %>% filter(FRAC$Well == wellnm)
fstage <- fstgdata %>% filter(fstgdata$Stage == stg)

# Crosswell
cwcdata <- CWC %>% filter(CWC$Well == wellnm)

cwcdata <- cwcdata %>% filter(cwcdata$Stage == stg)

# Shark
shark <- import("All_Shark.xlsx")
shark <- shark %>% filter(shark$Confidence > 1)
shark <- shark[,c(3,4,5,9,10,11,13,14)]

colnames(stage)[11] <- "Easting ft (Abs)"
colnames(stage)[12] <- "Northing ft (Abs)"

shark <- shark %>% filter(shark$`Treatment Well` == wellnm)
shark <- shark %>% filter(shark$Stage == stg)

df <- data.frame(x = fstage$`Easting ft (Abs)`, y = fstage$`Northing ft (Abs)`, t = fstage$date_time,
                 d = densCols(fstage$`Easting ft (Abs)`,fstage$`Northing ft (Abs)`, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))

write.csv(df, file = "df.csv")

msm <- ggplot(data = stage, aes(x=`Easting ft (Abs)`,y=`Northing ft (Abs)`,text=paste("Time =",stage$date_time)))+ geom_point(color="black", size=.8)
cwc <- msm + geom_point(data = cwcdata, aes(x=`Event_Center_East(SP)-shift`, y=`Event_Center_North(SP)`), color = cwcdata$Type, shape = 15)
geo <- cwc + geom_point(data = df, aes(x= x, y= y, col = d), size =1)+
  
  scale_color_identity() +
  
  theme_bw()+labs(x="Easting (ft)",y="Northing (ft)")+
  
  theme(axis.text.x = element_text(face="bold", color="#000000", size=7, angle=90),
        
        axis.text.y = element_text(face="bold", color="#000000", size=7, angle=0))+ggtitle(paste(wellnm,"Stage",stg,"Cross-Well Fiber Hits With Microseismic"))+
  
  theme(legend.position='none')+ 
  theme(plot.title = element_text(size = 10, face = "bold"))


fig <- plot_ly( x = df$x,
  y = df$y,
  type = "contour" )

fig

#### Wells ####


w1 <- wells %>% filter(Well == "EN-RULAND-LE-156-94-3328H-1")
w1 <- data.frame(w1$Well,w1$Stage,w1$Magnitude,w1$`Easting (ft).t1`,w1$`Northing (ft).t1`,w1$`Depth ft (TVDSS).t1`,w1$`MD (ft).t1`,w1$SideView.t1,w1$date_time)
newcolnames <- c("w","c","m","x1","y1","TVD","MD","s","date_time")
colnames(w1) <- newcolnames
w1 <- w1 %>% arrange(w1$MD)

w2 <- wells %>% filter(Well == "EN-RULAND-156-94-3328H-1")
w2 <- data.frame(w2$Well,w2$Stage,w2$Magnitude,w2$`Easting (ft).t2`,w2$`Northing (ft).t2`,w2$`Depth ft (TVDSS).t2`,w2$`MD (ft).t2`,w2$SideView.t2,w2$date_time)
newcolnames <- c("w","c","m","x2","y2","TVD","MD","s","date_time")
colnames(w2) <- newcolnames
w2 <- w2 %>% arrange(w2$MD)

w3 <- wells %>% filter(Well == "EN-RULAND-156-94-3328H-6")
w3 <- data.frame(w3$Well,w3$Stage,w3$Magnitude,w3$`Easting (ft).t3`,w3$`Northing (ft).t3`,w3$`Depth ft (TVDSS).t3`,w3$`MD (ft).t3`,w3$SideView.t3,w3$date_time)
newcolnames <- c("w","c","m","x3","y3","TVD","MD","s","date_time")
colnames(w3) <- newcolnames
w3 <- w3 %>% arrange(w3$MD)

w4 <- wells %>% filter(Well == "EN-RULAND-156-94-3328H-5")
w4 <- data.frame(w4$Well,w4$Stage,w4$Magnitude,w4$`Easting (ft).t4`,w4$`Northing (ft).t4`,w4$`Depth ft (TVDSS).t4`,w4$`MD (ft).t4`,w4$SideView.t4,w4$date_time)
newcolnames <- c("w","c","m","x4","y4","TVD","MD","s","date_time")
colnames(w4) <- newcolnames
w4 <- w4 %>% arrange(w4$MD)

w5 <- wells %>% filter(Well == "EN-RULAND-156-94-3328H-3")
w5 <- data.frame(w5$Well,w5$Stage,w5$Magnitude,w5$`Easting (ft).Obt1`,w5$`Northing (ft).Obt1`,w5$`Depth ft (TVDSS).Obt1`,w5$`MD (ft).Obt1`,w5$ObsSideView.t1,w5$date_time)
newcolnames <- c("w","c","m","x5","y5","TVD","MD","s","date_time")
colnames(w5) <- newcolnames
w5 <- w5 %>% arrange(w5$MD)

w6 <- wells %>% filter(Well == "EN-RULAND-156-94-3328H-4")
w6 <- data.frame(w6$Well,w6$Stage,w6$Magnitude,w6$`Easting (ft).R1`,w6$`Northing (ft).R1`,w6$`Depth ft (TVDSS).R1`,w6$`MD (ft).R1`,w6$RefSideView.t1,w6$date_time)
newcolnames <- c("w","c","m","x6","y6","TVD","MD","s","date_time")
colnames(w6) <- newcolnames
w6 <- w6 %>% arrange(w6$MD)

w7 <- wells %>% filter(Well == "EN-RULAND-156-94-3328H-2")
w7 <- data.frame(w7$Well,w7$Stage,w7$Magnitude,w7$`Easting (ft).R2`,w7$`Northing (ft).R2`,w7$`Depth ft (TVDSS).R2`,w7$`MD (ft).R2`,w7$RefSideView.t2,w7$date_time)
newcolnames <- c("w","c","m","x7","y7","TVD","MD","s","date_time")
colnames(w7) <- newcolnames
w7 <- w7 %>% arrange(w7$MD)


Plot2 <- geo + geom_path(data = w1, aes(x=x1, y=y1), color="black", size =.5, alpha = .8)

Plot3 <- Plot2 + geom_path(data = w2, aes(x=x2, y=y2), color="black", size =.5, alpha = .8)                            

Plot4 <- Plot3 + geom_path(data = w3, aes(x=x3, y=y3), color="black", size =.5, alpha = .8)             

Plot5 <- Plot4 + geom_path(data = w4, aes(x=x4, y=y4), color="#357818", size =.5, alpha = .8)

Plot6 <- Plot5 + geom_path(data = w5, aes(x=x5, y=y5), color="black", size =.5, alpha = .8)

Plot7 <- Plot6 + geom_path(data = w6, aes(x=x6, y=y6), color="#357818", size =.5, alpha = .8)

Plot8 <- Plot7 + geom_path(data = w7, aes(x=x7, y=y7), color="black", size =.5, alpha = .8)



Plot9 <- Plot8 + geom_point(data = stage, aes(x=pEast, y=pNorth), color="black", shape=17,size =2, alpha = .8)
Plot10 <- Plot9 + coord_fixed()
p <- ggplotly(Plot10,tooltip = c("text"))
saveWidget(p, paste(wellnm,"Stage",stg,"CWC.html"), selfcontained = T, libdir = "lib")

#Plot11 <- Plot10 + ylim(c(miny,maxy))+ xlim(c(minx,maxx))
#Plot11 <- Plot10 

ggsave(paste(wellnm,"","MSM CWC Map.png"), width = 15, height = 8)

ggsave(paste(wellnm,"","MSM CWC Map-filtered.png"), width = 15, height = 8)
############# Create and save widget ###############
#### Curves ####
treat <- import("All_Treat_Elapsed.csv")

treat <- treat %>% filter(treat$Well == wellnm)

treatstg <- treat %>% filter(treat$Stage == stg)

write.xlsx(treatstg, 'treatstg.xlsx')
treatstg <- import('treatstg.xlsx')

A <- treatstg$Surf.Press..Csg...psi.
B <- treatstg$Slurry.Flow.Rate..bpm.
C <- treatstg$Proppant.Conc..ppg.
D <- treatstg$date_time

write.csv(treatstg, file = "treatstg.csv")
treatstg <- read_csv("treatstg.csv", col_types = cols(date_time = col_datetime(format = "%m/%d/%Y %H:%M:%S")))

msm <- data.frame(stage$date_time, stage$Well,stage$Stage,stage$H5dist..ft.,stage$H4dist..ft.)
newcolnames <- c("date_time","Well","Stage","H5dist (ft)","H4dist (ft)")
colnames(msm) <- newcolnames

write.csv(msm, file = "msm.csv")
msm <- read_csv("msm.csv", col_types = cols(date_time = col_datetime(format = "%m/%d/%Y %H:%M")))

cwcdata <- cwcdata[,c(4,5,6,7,24,25,26)]

dataset = bind_rows(treatstg,msm,cwcdata)
glimpse(dataset)

write.csv(dataset, file = "dataset.csv")
dataset <- read_csv("dataset.csv", col_types = cols(date_time = col_datetime(format = "%m/%d/%Y %H:%M")))

G <- dataset %>% filter(dataset$Fiber == 5)
E <- dataset %>% filter(dataset$`H5dist (ft)` > 0)
f <- dataset %>% filter(dataset$`H4dist (ft)` > 0)
H <- dataset %>% filter(dataset$Fiber == 4)


A <- dataset$Pressure
B <- dataset$Rate
C <- dataset$Conc
D <- dataset$date_time
#E <- dataset$`Distance To Mid Perf`
#G <- dataset$`Distance To Mid Perf`



library(plotly)
library(htmlwidgets)
# A <- treatstg$Pressure
# B <- treatstg$Rate
# C <- treatstg$Conc
# D <- treatstg$date_time
# E <- msm$`Distance To Mid Perf`
# G <- cwcdata$Type

library(plotly)
trace1 <- list(
  line = list(color = "rgb(244, 39, 8)"), 
  meta = list(columnNames = list(
    x = "D", 
    y = "A"
  )), 
  mode = "lines", 
  name = "Pressure", 
  type = "scatter", 
  xsrc = "smachage2020:19:bb036c", 
  x = D,
  ysrc = "smachage2020:19:8a3b1c", 
  y = A,
  showlegend = FALSE, 
  stackgroup = "A"
)
trace2 <- list(
  line = list(color = "rgb(8, 62, 245)"), 
  meta = list(columnNames = list(
    x = "D", 
    y = "B"
  )), 
  mode = "lines", 
  name = "Rate", 
  type = "scatter", 
  xsrc = "smachage2020:19:bb036c", 
  x = D,
  ysrc = "smachage2020:19:089ced", 
  y = B,
  yaxis = "y2", 
  showlegend = FALSE, 
  stackgroup = "B"
)
trace3 <- list(
  line = list(color = "rgb(53, 196, 20)"), 
  meta = list(columnNames = list(
    x = "D", 
    y = "C"
  )), 
  mode = "lines", 
  name = "Prop Conc", 
  type = "scatter", 
  xsrc = "smachage2020:19:bb036c", 
  x = D,
  ysrc = "smachage2020:19:fb15e6", 
  y = C,
  yaxis = "y3", 
  showlegend = FALSE, 
  stackgroup = "C"
)
# trace4 <- list(
#   meta = list(columnNames = list(
#     x = "D", 
#     y = "E"
#   )), 
#   mode = "markers", 
#   name = "Dist From Fiber", 
#   type = "scatter", 
#   xsrc = "smachage2020:19:bb036c", 
#   x = D,
#   ysrc = "smachage2020:19:3e6732", 
#   y = E,
#   yaxis = "y4", 
#   marker = list(color = "rgb(240, 171, 40)",
#                 size = 8), 
#   opacity = 0.55, 
#   showlegend = FALSE, 
#   stackgroup = NULL
# )
data <- list(trace1, trace2, trace3, trace4)
layout <- list(
  title = list(text = paste(wellnm,"Stage",stg)), 
  xaxis = list(
    type = "date", 
    range = c(D[1]-200, max(D)+200), 
    title = list(
      font = list(size = 14), 
      text = "Stage Time"
    ), 
    domain = c(0, 1), 
    nticks = 10, 
    tickson = "labels", 
    showline = FALSE, 
    tickfont = list(
      size = 10, 
      color = "rgb(0, 0, 0)"
    ), 
    tickmode = "auto", 
    autorange = TRUE
  ), 
  yaxis = list(
    type = "linear", 
    range = c(1, max(A)), 
    title = list(
      font = list(color = "rgb(245, 3, 7)"), 
      text = "Surface Pressure (psi)"
    ), 
    showline = TRUE, 
    tickfont = list(color = "rgb(255, 0, 34)"), 
    autorange = TRUE,
    tickprefix = "       "
    
  ), 
  legend = list(orientation = "v"), 
  yaxis2 = list(
    side = "right", 
    type = "linear", 
    range = c(0, 150), 
    title = list(
      font = list(color = "rgb(8, 103, 250)"), 
      text = "Pumping Rate (bpm)"
    ), 
    showline = TRUE, 
    tickfont = list(color = "rgb(8, 103, 250)"), 
    autorange = FALSE, 
    overlaying = "y"
  ), 
  yaxis3 = list(
    side = "right", 
    type = "linear", 
    range = c(0, 10), 
    title = list(
      font = list(color = "rgb(53, 196, 20)"), 
      text = "Prop Conc (ppg)"
    ), 
    tickfont = list(color = "rgb(53, 196, 20)"), 
    autorange = FALSE, 
    overlaying = "y", 
    tickprefix = "                       "
  ), 
  yaxis4 = list(
    side = "left", 
    type = "linear", 
    dtick = 0, 
    range = c(-100, 4000), 
    tick0 = 4, 
    title = list(text = "<br>"),
    tickfont = list(
      size = 8),  
    tickmode = "auto", 
    autorange = FALSE, 
    overlaying = "y",
    ticksuffix = "    "
  ), 
  autosize = TRUE, 
  template = list(
    data = list(
      bar = list(
        list(
          type = "bar", 
          marker = list(colorbar = list(
            ticks = "", 
            outlinewidth = 0
          ))
        )
      ), 
      table = list(
        list(
          type = "table", 
          cells = list(
            fill = list(color = "#EBF0F8"), 
            line = list(color = "white")
          ), 
          header = list(
            fill = list(color = "#C8D4E3"), 
            line = list(color = "white")
          )
        )
      ), 
      carpet = list(
        list(
          type = "carpet", 
          aaxis = list(
            gridcolor = "#C8D4E3", 
            linecolor = "#C8D4E3", 
            endlinecolor = "#2a3f5f", 
            minorgridcolor = "#C8D4E3", 
            startlinecolor = "#2a3f5f"
          ), 
          baxis = list(
            gridcolor = "#C8D4E3", 
            linecolor = "#C8D4E3", 
            endlinecolor = "#2a3f5f", 
            minorgridcolor = "#C8D4E3", 
            startlinecolor = "#2a3f5f"
          )
        )
      ), 
      mesh3d = list(
        list(
          type = "mesh3d", 
          colorbar = list(
            ticks = "", 
            outlinewidth = 0
          )
        )
      ), 
      contour = list(
        list(
          type = "contour", 
          colorbar = list(
            ticks = "", 
            outlinewidth = 0
          ), 
          autocolorscale = TRUE
        )
      ), 
      heatmap = list(
        list(
          type = "heatmap", 
          colorbar = list(
            ticks = "", 
            outlinewidth = 0
          ), 
          autocolorscale = TRUE
        )
      ), 
      scatter = list(
        list(
          type = "scatter", 
          marker = list(colorbar = list(
            ticks = "", 
            outlinewidth = 0
          ))
        )
      ), 
      surface = list(
        list(
          type = "surface", 
          colorbar = list(
            ticks = "", 
            outlinewidth = 0
          )
        )
      ), 
      heatmapgl = list(
        list(
          type = "heatmapgl", 
          colorbar = list(
            ticks = "", 
            outlinewidth = 0
          )
        )
      ), 
      histogram = list(
        list(
          type = "histogram", 
          marker = list(colorbar = list(
            ticks = "", 
            outlinewidth = 0
          ))
        )
      ), 
      parcoords = list(
        list(
          line = list(colorbar = list(
            ticks = "", 
            outlinewidth = 0
          )), 
          type = "parcoords"
        )
      ), 
      scatter3d = list(
        list(
          type = "scatter3d", 
          marker = list(colorbar = list(
            ticks = "", 
            outlinewidth = 0
          ))
        )
      ), 
      scattergl = list(
        list(
          type = "scattergl", 
          marker = list(colorbar = list(
            ticks = "", 
            outlinewidth = 0
          ))
        )
      ), 
      choropleth = list(
        list(
          type = "choropleth", 
          colorbar = list(
            ticks = "", 
            outlinewidth = 0
          )
        )
      ), 
      scattergeo = list(
        list(
          type = "scattergeo", 
          marker = list(colorbar = list(
            ticks = "", 
            outlinewidth = 0
          ))
        )
      ), 
      histogram2d = list(
        list(
          type = "histogram2d", 
          colorbar = list(
            ticks = "", 
            outlinewidth = 0
          ), 
          autocolorscale = TRUE
        )
      ), 
      scatterpolar = list(
        list(
          type = "scatterpolar", 
          marker = list(colorbar = list(
            ticks = "", 
            outlinewidth = 0
          ))
        )
      ), 
      contourcarpet = list(
        list(
          type = "contourcarpet", 
          colorbar = list(
            ticks = "", 
            outlinewidth = 0
          )
        )
      ), 
      scattercarpet = list(
        list(
          type = "scattercarpet", 
          marker = list(colorbar = list(
            ticks = "", 
            outlinewidth = 0
          ))
        )
      ), 
      scattermapbox = list(
        list(
          type = "scattermapbox", 
          marker = list(colorbar = list(
            ticks = "", 
            outlinewidth = 0
          ))
        )
      ), 
      scatterpolargl = list(
        list(
          type = "scatterpolargl", 
          marker = list(colorbar = list(
            ticks = "", 
            outlinewidth = 0
          ))
        )
      ), 
      scatterternary = list(
        list(
          type = "scatterternary", 
          marker = list(colorbar = list(
            ticks = "", 
            outlinewidth = 0
          ))
        )
      ), 
      histogram2dcontour = list(
        list(
          type = "histogram2dcontour", 
          colorbar = list(
            ticks = "", 
            outlinewidth = 0
          ), 
          autocolorscale = TRUE
        )
      )
    ), 
    layout = list(
      geo = list(
        bgcolor = "white", 
        showland = TRUE, 
        lakecolor = "white", 
        landcolor = "white", 
        showlakes = TRUE, 
        subunitcolor = "#C8D4E3"
      ), 
      font = list(color = "#2a3f5f"), 
      polar = list(
        bgcolor = "white", 
        radialaxis = list(
          ticks = "", 
          gridcolor = "#EBF0F8", 
          linecolor = "#EBF0F8"
        ), 
        angularaxis = list(
          ticks = "", 
          gridcolor = "#EBF0F8", 
          linecolor = "#EBF0F8"
        )
      ), 
      scene = list(
        xaxis = list(
          ticks = "", 
          gridcolor = "#DFE8F3", 
          gridwidth = 2, 
          linecolor = "#EBF0F8", 
          zerolinecolor = "#EBF0F8", 
          showbackground = TRUE, 
          backgroundcolor = "white"
        ), 
        yaxis = list(
          ticks = "", 
          gridcolor = "#DFE8F3", 
          gridwidth = 2, 
          linecolor = "#EBF0F8", 
          zerolinecolor = "#EBF0F8", 
          showbackground = TRUE, 
          backgroundcolor = "white"
        ), 
        zaxis = list(
          ticks = "", 
          gridcolor = "#DFE8F3", 
          gridwidth = 2, 
          linecolor = "#EBF0F8", 
          zerolinecolor = "#EBF0F8", 
          showbackground = TRUE, 
          backgroundcolor = "white"
        )
      ), 
      title = list(x = 0.05), 
      xaxis = list(
        ticks = "", 
        gridcolor = "#EBF0F8", 
        linecolor = "#EBF0F8", 
        automargin = TRUE, 
        zerolinecolor = "#EBF0F8", 
        zerolinewidth = 2
      ), 
      yaxis = list(
        ticks = "", 
        gridcolor = "#EBF0F8", 
        linecolor = "#EBF0F8", 
        automargin = TRUE, 
        zerolinecolor = "#EBF0F8", 
        zerolinewidth = 2
      ), 
      ternary = list(
        aaxis = list(
          ticks = "", 
          gridcolor = "#DFE8F3", 
          linecolor = "#A2B1C6"
        ), 
        baxis = list(
          ticks = "", 
          gridcolor = "#DFE8F3", 
          linecolor = "#A2B1C6"
        ), 
        caxis = list(
          ticks = "", 
          gridcolor = "#DFE8F3", 
          linecolor = "#A2B1C6"
        ), 
        bgcolor = "white"
      ), 
      colorway = c("#636efa", "#EF553B", "#00cc96", "#ab63fa", "#19d3f3", "#e763fa", "#fecb52", "#ffa15a", "#ff6692", "#b6e880"), 
      hovermode = "closest", 
      colorscale = list(
        diverging = list(c(0, "#8e0152"),list(0.1, "#c51b7d"),list(0.2, "#de77ae"),list(0.3, "#f1b6da"),list(0.4, "#fde0ef"),list(0.5, "#f7f7f7"),list(0.6, "#e6f5d0"),list(0.7, "#b8e186"),list(0.8, "#7fbc41"),list(0.9, "#4d9221"),list(1, "#276419")), 
        sequential = list(c(0, "#0508b8"),list(0.0893854748603352, "#1910d8"),list(0.1787709497206704, "#3c19f0"),list(0.2681564245810056, "#6b1cfb"),list(0.3575418994413408, "#981cfd"),list(0.44692737430167595, "#bf1cfd"),list(0.5363128491620112, "#dd2bfd"),list(0.6256983240223464, "#f246fe"),list(0.7150837988826816, "#fc67fd"),list(0.8044692737430168, "#fe88fc"),list(0.8938547486033519, "#fea5fd"),list(0.9832402234636871, "#febefe"),list(1, "#fec3fe")), 
        sequentialminus = list(c(0, "#0508b8"),list(0.0893854748603352, "#1910d8"),list(0.1787709497206704, "#3c19f0"),list(0.2681564245810056, "#6b1cfb"),list(0.3575418994413408, "#981cfd"),list(0.44692737430167595, "#bf1cfd"),list(0.5363128491620112, "#dd2bfd"),list(0.6256983240223464, "#f246fe"),list(0.7150837988826816, "#fc67fd"),list(0.8044692737430168, "#fe88fc"),list(0.8938547486033519, "#fea5fd"),list(0.9832402234636871, "#febefe"),list(1, "#fec3fe"))
      ), 
      plot_bgcolor = "white", 
      paper_bgcolor = "white", 
      shapedefaults = list(
        line = list(width = 0), 
        opacity = 0.4, 
        fillcolor = "#506784"
      ), 
      annotationdefaults = list(
        arrowhead = 0, 
        arrowcolor = "#506784", 
        arrowwidth = 1
      )
    ), 
    themeRef = "PLOTLY_WHITE"
  )
)
p <- plot_ly()
p <- add_trace(p, line=trace1$line, meta=trace1$meta, mode=trace1$mode, name=trace1$name, type=trace1$type, xsrc=trace1$xsrc, x=trace1$x, ysrc=trace1$ysrc, y=trace1$y, showlegend=trace1$showlegend)
p <- add_trace(p, line=trace2$line, meta=trace2$meta, mode=trace2$mode, name=trace2$name, type=trace2$type, xsrc=trace2$xsrc, x=trace2$x, ysrc=trace2$ysrc, y=trace2$y, yaxis=trace2$yaxis, showlegend=trace2$showlegend)
p <- add_trace(p, line=trace3$line, meta=trace3$meta, mode=trace3$mode, name=trace3$name, type=trace3$type, xsrc=trace3$xsrc, x=trace3$x, ysrc=trace3$ysrc, y=trace3$y, yaxis=trace3$yaxis, showlegend=trace3$showlegend)
p <- add_trace(p, x=E$date_time, y=E$`H5dist (ft)`, type = 'scatter', mode = 'markers', name="",text = paste("Distance From H5 Fiber:",E$`H5dist (ft)`,":",E$date_time), hoverinfo = 'text', yaxis=trace4$yaxis, marker=list(color = 'rgba(243, 248, 37,0.5)', size = 12, line = list(color = 'rgb(40, 40, 39)', width = 2)))
p <- add_trace(p, x=f$date_time, y=f$`H4dist (ft)`, type = 'scatter', mode = 'markers', color= "brown", name="",text = paste("Distance From H4 Fiber:",f$`H4dist (ft)`,":",f$date_time), hoverinfo = 'text', yaxis=trace4$yaxis, marker=list(color = 'rgba(250, 134, 5,0.5)',size = 12,line = list(color = 'rgb(40, 40, 39)', width = 2)))
p <- add_trace(p, x=G$date_time, y=G$Event, type = 'scatter', mode = 'markers', name="",text = paste("CWC Type:",G$Type,"Fiber:",G$Fiber,"Event:",G$Event,"-",G$date_time), hoverinfo = 'text', yaxis=trace4$yaxis, marker=list(color = 'rgba(0, 0, 0,0.9)', size = 8, line = list(color = 'rgb(145, 140, 134)', width = 2)))
p <- add_trace(p, x=H$date_time, y=H$Event, type = 'scatter', mode = 'markers', name="",text = paste("CWC Type:",H$Type,"Fiber:",H$Fiber,"Event:",H$Event,"-",H$date_time), hoverinfo = 'text', yaxis=trace4$yaxis, marker=list(color = 'rgba(74, 74, 74,0.9)', size = 8, line = list(color = 'rgb(145, 140, 134)', width = 2)))
p <- layout(p, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, showlegend = FALSE,legend=layout$legend, yaxis2=layout$yaxis2, yaxis3=layout$yaxis3, yaxis4=layout$yaxis4, autosize=layout$autosize, template=layout$template)

p

saveWidget(p, paste(wellnm,"Stage",stg,"TWM.html"), selfcontained = T, libdir = "lib")

