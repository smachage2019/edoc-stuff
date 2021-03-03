
packages = c("dplyr","readxl","rio", "openxlsx", "readr", "tidyr","tcltk","tibbletime","tibble","purrr","rgdal","ggplot2","plotly","htmlwidgets","rLiDAR","geosphere"  )

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

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


WD <- tk_choose.dir(getwd(), "Choose a suitable folder")
setwd(WD)


##############Mid Perf######
# set Conversion CRS
mp <- import("Values-use-tidy.xlsx")
mp <- mp %>% filter(Status == "Perf")

epsg <- 32039 # WGS state plane 32039 -Texas Central, 32020 -North Dakota North, 26753 - North Colorado
d <<- data.frame(lon=mp$`Easting ft (Abs)`, lat=mp$`Northing ft (Abs)`)

d[["lon"]][is.na(d[["lon"]])] <- 0
d[["lat"]][is.na(d[["lat"]])] <- 0
tail(d)

coordinates(d) <- c("lon", "lat")
proj4string(d) <- CRS(paste("+init=epsg:",epsg,sep="")) # WGS state plane 32039-Texas Central, 32020-North Dakota North
cord.latlong <<- spTransform(d, CRS("+init=epsg:4267"))
cords <- data.frame(cord.latlong)

mp['lat'] = cords['lat']
mp['lon'] = cords['lon']

write.csv(mp, file = "mp-lat-long.csv")
#write.csv(all_array, file = "tmp.csv")
all_array <- import("Values-use-tidy.xlsx")
mp <- mp %>% filter(Status == "Perf")
msm <- all_array %>% filter(Status == "MSM")


# date_time <<- paste(all_array$"Acquisition Date (Local)",all_array$"Acquisition Time (Local)")
# all_array <- add_column(all_array, date_time)

#all_array <- all_array %>% mutate("Distance To Mid Perf2" = sqrt(((msm$`Easting ft (Abs)`- mp$`Easting ft (Abs)`)^2)+((msm$`Northing ft (Abs)`- mp$`Northing ft (Abs)`)^2)))

###### Distance ######

wellsheets <- read_excel_allsheets("R-Wells.xlsx")
wellnames <- names(wellsheets)
# write.xlsx(Frac1, 'frac1.xlsx')
if("Frac1" %in% wellnames){
  Frac1<-data.frame(wellsheets$Frac1)
  
}

if("Frac2" %in% wellnames){
  Frac2<-data.frame(wellsheets$Frac2)
  
}

if("Frac3" %in% wellnames){
  Frac3<-data.frame(wellsheets$Frac3)
  
}

if("Frac4" %in% wellnames){
  Frac4<-data.frame(wellsheets$Frac4)
  
}

if("Frac5" %in% wellnames){
  Frac5<-data.frame(wellsheets$Frac5)
  
}

if("Frac6" %in% wellnames){
  Frac6<-data.frame(wellsheets$Frac6)
  
}

if("RefTrace1" %in% wellnames){
  RefTrace1<-data.frame(wellsheets$RefTrace1)
  
}

if("RefTrace2" %in% wellnames){
  RefTrace2<-data.frame(wellsheets$RefTrace2)
  
}

if("RefTrace3" %in% wellnames){
  RefTrace3<-data.frame(wellsheets$RefTrace3)
  
}

if("ObTrace1" %in% wellnames){
  ObTrace1<-data.frame(wellsheets$ObTrace1)
  
}

if("ObTrace2" %in% wellnames){
  ObTrace2<-data.frame(wellsheets$ObTrace2)
}

if("Obs1" %in% wellnames){
  Obs1<-data.frame(wellsheets$Obs1)
  
}

paste("Frac1 =",head(Frac1$Treatment.Well,1))
paste("Frac2 =",head(Frac2$Treatment.Well,1))
paste("Frac3 =",head(Frac3$Treatment.Well,1))
paste("RefTrace1 =",head(RefTrace1$Treatment.Well,1))

paste("Obs1 =",head(ObTrace1$Observation.Well ,1))
paste("Obs2 =",head(ObTrace2$Observation.Well ,1))

MSM_data <- import("Values-use-tidy.xlsx")

i = 1
j = 1
k = 1

well_list <- unique(MSM_data$Well)
well_list <- sort(well_list)
well_list <- well_list[c(4:6)]

wellnm <- well_list[i]
cut <- MSM_data %>% filter(MSM_data$Well == wellnm)
write.csv(cut, file = paste("Well-",wellnm,"-cut.csv",sep=""))
i = i+1


well_list <- sort(well_list)
wellnm <- well_list[3]

file.list <- list.files(pattern="*.csv", recursive = FALSE)
file.list <- file.list[5:7]
cut <- read_csv(file.list[3])

cut <- import("mp-lat-long.csv")
well_list <- sort(well_list)
wellnm <- well_list[3]
cut <- cut %>% filter(cut$Well == wellnm)

data <- data.frame(cut$lon,cut$lat,cut$Well,cut$Stage)
pnts <- as.matrix(data[,1:2])
plot(data$cut.lon,data$cut.lat)

xmax = max(data[,1]+.005)
xmin = min(data[,1]-0.00)
ymax = max(data[,2]+.01)
ymin = min(data[,2]-.01)
###################Well1#######################
paste("Frac1 =",head(Frac1$Treatment.Well,1))
lateral <- data.frame(Frac1$lon,Frac1$lat)
dtw <- Frac1$Treatment.Well[1]
line <- as.matrix(lateral)
d1 = dist2Line(pnts, line)
out = data.frame(data$cut.Well,data$cut.Stage,d1)
out$distFT <- out$distance * 3.28084
write.csv(out, file = paste(dtw,"-","Values.csv"))

plot(makeLine(line),type="l",xlim=c(xmin,xmax),ylim=c(ymin,ymax),
     main="Lateral Distance QC",
     xlab="Longitude",
     ylab="Latitude",
     sub="Distance From Event Back To Selected Well")
points(line)
points(pnts, col="blue", pch=20)
points(d1[,2], d1[,3], col="red", pch="x")

for (i in 1:nrow(d1)) lines(gcIntermediate(pnts[i,], d1[i,2:3],10), lwd=2)
###################Well2#######################
paste("Frac2 =",head(Frac2$Treatment.Well,1))
lateral2 <- data.frame(Frac2$lon,Frac2$lat)
dtw2 <- Frac2$Treatment.Well[1]
line2 <- as.matrix(lateral2)
d2 = dist2Line(pnts, line2)
out = data.frame(data$cut.Well,data$cut.Stage,d2)
out$distFT <- out$distance * 3.28084
write.csv(out, file = paste(dtw2,"-","Values.csv"))

plot(makeLine(line2),type="l",xlim=c(xmin,xmax),ylim=c(ymin,ymax),
     main="Lateral Distance QC",
     xlab="Longitude",
     ylab="Latitude",
     sub="Distance From Event Back To Selected Well")
points(line2)
points(pnts, col="blue", pch=20)
points(d2[,2], d2[,3], col="red", pch="x")

for (i in 1:nrow(d2)) lines(gcIntermediate(pnts[i,], d2[i,2:3],10), lwd=2)
###################Well3########################
paste("Frac3 =",head(Frac3$Treatment.Well,1))
lateral3 <- data.frame(Frac3$lon,Frac3$lat)
dtw3 <- Frac3$Treatment.Well[1]
line3 <- as.matrix(lateral3)
d3 = dist2Line(pnts, line3)
out = data.frame(data$cut.Well,data$cut.Stage,d3)
out$distFT <- out$distance * 3.28084
write.csv(out, file = paste(dtw3,"-","Values.csv"))

plot(makeLine(line3),type="l",xlim=c(xmin,xmax),ylim=c(ymin,ymax),
     main="Lateral Distance QC",
     xlab="Longitude",
     ylab="Latitude",
     sub="Distance From Event Back To Selected Well")
points(line3)
points(pnts, col="blue", pch=20)
points(d3[,2], d3[,3], col="red", pch="x")

for (i in 1:nrow(d3)) lines(gcIntermediate(pnts[i,], d3[i,2:3],10), lwd=2)
###################Well4########################
paste("Frac4 =",head(Frac4$Treatment.Well,1))
lateral4 <- data.frame(Frac4$lon,Frac4$lat)
dtw4 <- Frac4$Treatment.Well[1]
line4 <- as.matrix(lateral4)
d4 = dist2Line(pnts, line4)
out = data.frame(data$cut..Event.File.,d4)
out$distFT <- out$distance * 3.28084
write.csv(out, file = paste(dtw4,"-","Values.csv"))

plot(makeLine(line4),type="l",xlim=c(xmin,xmax),ylim=c(ymin,ymax),
     main="Lateral Distance QC",
     xlab="Longitude",
     ylab="Latitude",
     sub="Distance From Event Back To Selected Well")
points(line4)
points(pnts, col="blue", pch=20)
points(d4[,2], d4[,3], col="red", pch="x")

for (i in 1:nrow(d4)) lines(gcIntermediate(pnts[i,], d4[i,2:3],10), lwd=2)
###################Well5########################
paste("Frac5 =",head(Frac5$Treatment.Well,1))
lateral5 <- data.frame(Frac5$lon,Frac5$lat)
dtw5 <- Frac5$Treatment.Well[1]
line5 <- as.matrix(lateral5)
d5 = dist2Line(pnts, line5)
out = data.frame(data$cut..Event.File.,d5)
out$distFT <- out$distance * 3.28084
write.csv(out, file = paste(dtw5,"-","Values.csv"))

plot(makeLine(line4),type="l",xlim=c(xmin,xmax),ylim=c(ymin,ymax),
     main="Lateral Distance QC",
     xlab="Longitude",
     ylab="Latitude",
     sub="Distance From Event Back To Selected Well")
points(line5)
points(pnts, col="blue", pch=20)
points(d5[,2], d5[,3], col="red", pch="x")

for (i in 1:nrow(d5)) lines(gcIntermediate(pnts[i,], d5[i,2:3],10), lwd=2)
###################Ref1########################
paste("RefTrace1 =",head(RefTrace1$Treatment.Well,1))
lateral6 <- data.frame(RefTrace1$lon,RefTrace1$lat)
dtw6 <- RefTrace1$Treatment.Well[1]
line6 <- as.matrix(lateral6)
d6 = dist2Line(pnts, line6)
out = data.frame(data$cut.Well,data$cut.Stage,d6)
out$distFT <- out$distance * 3.28084
write.csv(out, file = paste(dtw6,"-","Values.csv"))

plot(makeLine(line6),type="l",xlim=c(xmin,xmax),ylim=c(ymin,ymax),
     main="Lateral Distance QC",
     xlab="Longitude",
     ylab="Latitude",
     sub="Distance From Event Back To Selected Well")
points(line6)
points(pnts, col="blue", pch=20)
points(d6[,2], d6[,3], col="red", pch="x")

for (i in 1:nrow(d6)) lines(gcIntermediate(pnts[i,], d6[i,2:3],10), lwd=2)
###################Ref2#########################
paste("RefTrace2 =",head(RefTrace2$Treatment.Well,1))
lateral7 <- data.frame(RefTrace2$lon,RefTrace2$lat)
dtw7 <- RefTrace2$Treatment.Well[1]
line7 <- as.matrix(lateral7)
d7 = dist2Line(pnts, line7)
out = data.frame(data$cut.Well,data$cut.Stage,d7)
out$distFT <- out$distance * 3.28084
write.csv(out, file = paste(dtw7,"-","Values.csv"))

plot(makeLine(line7),type="l",xlim=c(xmin,xmax),ylim=c(ymin,ymax),
     main="Lateral Distance QC",
     xlab="Longitude",
     ylab="Latitude",
     sub="Distance From Event Back To Selected Well")
points(line7)
points(pnts, col="blue", pch=20)
points(d7[,2], d7[,3], col="red", pch="x")

for (i in 1:nrow(d7)) lines(gcIntermediate(pnts[i,], d7[i,2:3],10), lwd=2)
###################Ref3#########################
paste("RefTrace3 =",head(RefTrace3$Treatment.Well,1))
lateral8 <- data.frame(RefTrace3$lon,RefTrace3$lat)
dtw8 <- RefTrace3$Treatment.Well[1]
line8 <- as.matrix(lateral8)
d8 = dist2Line(pnts, line8)
out = data.frame(data$cut..Event.File.,d8)
out$distFT <- out$distance * 3.28084
write.csv(out, file = paste(dtw8,"-","Values.csv"))

plot(makeLine(line8),type="l",xlim=c(xmin,xmax),ylim=c(ymin,ymax),
     main="Lateral Distance QC",
     xlab="Longitude",
     ylab="Latitude",
     sub="Distance From Event Back To Selected Well")
points(line8)
points(pnts, col="blue", pch=20)
points(d8[,2], d8[,3], col="red", pch="x")

for (i in 1:nrow(d8)) lines(gcIntermediate(pnts[i,], d8[i,2:3],10), lwd=2)
###################Obs1#########################
paste("Obs1 =",head(ObTrace1$Observation.Well ,1))
lateral6 <- data.frame(ObTrace1$lon,ObTrace1$lat)
dtw6 <- ObTrace1$Observation.Well[1]
line6 <- as.matrix(lateral6)
d6 = dist2Line(pnts, line6)
out = data.frame(data$cut.Well,data$cut.Stage,d6)
out$distFT <- out$distance * 3.28084
write.csv(out, file = paste(dtw6,"-","Values.csv"))

plot(makeLine(line6),type="l",xlim=c(xmin,xmax),ylim=c(ymin,ymax),
     main="Lateral Distance QC",
     xlab="Longitude",
     ylab="Latitude",
     sub="Distance From Event Back To Selected Well")
points(line6)
points(pnts, col="blue", pch=20)
points(d6[,2], d6[,3], col="red", pch="x")

for (i in 1:nrow(d6)) lines(gcIntermediate(pnts[i,], d6[i,2:3],10), lwd=2)
###################Obs2#########################
paste("Obs2 =",head(ObTrace2$Observation.Well ,1))
lateral7 <- data.frame(ObTrace2$lon,ObTrace2$lat)
dtw7 <- ObTrace2$Observation.Well[1]
line7 <- as.matrix(lateral7)
d7 = dist2Line(pnts, line7)
out = data.frame(data$cut.Well,data$cut.Stage,d7)
out$distFT <- out$distance * 3.28084
write.csv(out, file = paste(dtw7,"-","Values.csv"))

plot(makeLine(line7),type="l",xlim=c(xmin,xmax),ylim=c(ymin,ymax),
     main="Lateral Distance QC",
     xlab="Longitude",
     ylab="Latitude",
     sub="Distance From Event Back To Selected Well")
points(line7)
points(pnts, col="blue", pch=20)
points(d7[,2], d7[,3], col="red", pch="x")

for (i in 1:nrow(d7)) lines(gcIntermediate(pnts[i,], d7[i,2:3],10), lwd=2)


################################################




WD <- tk_choose.dir(getwd(), "Choose a suitable folder")
setwd(WD)

file.list <- list.files(pattern="*.csv", recursive = TRUE)
df.list <- sapply(file.list, read.csv, simplify=FALSE)
library(purrr)
file.list2 <- list.files(pattern='*.csv')
file.list2 <- setNames(file.list, file.list) 
library(data.table)
df <- rbindlist(df.list, idcol = "id", fill=TRUE)
df <- map_df(file.list2, read.csv, .id = "id")
write.csv(df, file = "dist.csv")


MSM_data <- import("Values-use-tidy.xlsx")
dist <- import("dist.csv")
dist <- dist[,c(4,5,9,10)]

MSM_data <- MSM_data %>% left_join(dist, by=c("Well","Stage"))

write.xlsx(MSM_data, 'MSM_data.xlsx')
########### QC Plot All #############
plot(pnts, col="gray", pch=19,xlim=c(xmin,xmax),ylim=c(ymin,ymax),
     main="Lateral Distance QC",
     xlab="Longitude",
     ylab="Latitude",
     sub="Distance From Event Back To Fiber Well")
lines(line[,1], line[,2],col = "red")
lines(line2[,1], line2[,2],col="green")
lines(line3[,1], line3[,2],col="blue")
lines(line4[,1], line4[,2],col="orange")
lines(line5[,1], line5[,2],col="black")

########### QC Plot #############

library(geosphere)
d = dist2Line(pnts, line)
xmax = max(d6[,2]+.007)
xmin = min(d6[,2]-.01)
ymax = max(d6[,3]+.03)
ymin = min(d6[,3]-.04)
plot(makeLine(line5),type="l",xlim=c(xmin,xmax),ylim=c(ymin,ymax),
     main="Lateral Distance QC",
     xlab="Longitude",
     ylab="Latitude",
     sub="Distance From Event Back To Selected Well")
points(line5)
points(pnts, col="blue", pch=20)
points(d[,2], d[,3], col="red", pch="x")

for (i in 1:nrow(d)) lines(gcIntermediate(pnts[i,], d[i,2:3],10), lwd=2)

cut <- import("Values.csv")
cut <- Values

cut <- cut[,c(3,4,14,15,86,87,90,91,92,93)]
head(cut)

cut2 <- cut

wellnm <- well_list[2]
print(wellnm)
stg <- 1


stg_list <- unique(cut2$Stage)
print(stg_list)
stg <- 1

cut2 <- cut %>% filter(cut$Well == wellnm)

cut2 <- cut2 %>% filter(cut2$Stage == stg)
cut2 <- cut2 %>% filter(cut2$Magnitude <= -.6)
cut2 <- cut2 %>% filter(cut2$Confidence >= 2.2)


dasmd <- ggplot(data = cut2, aes(x=D4309H, y=Magnitude,text = paste('Confidence: ', Confidence))) +
  geom_point(color=cut2$Confidence, fill = "white", size = 2, alpha=0.4)+
  theme_bw()+labs(x="Distance From Fiber (ft)",y="Magnitude")+
  theme(axis.text.x = element_text(face="bold", color="#000000", size=10, angle=0),
        axis.text.y = element_text(face="bold", color="#000000", size=10, angle=0))+
  ggtitle(paste(wellnm,"Stage",stg,"- DASMSM Magnitude vs Distance.html"))
dasmdp <- ggplotly(dasmd)
dasmdp

saveWidget(dasmdp, paste(wellnm,"Stage",stg,"- DASMSM Magnitude vs Distance.html"), selfcontained = T, libdir = "lib")
stg <- stg + 1

