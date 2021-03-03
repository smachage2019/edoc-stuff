packages = c("mclust","caret","mlbench","rio", "randomForest", "dplyr","e1071", "data.table", "tcltk","ggplot2","readxl","purrr","data.table","plotly","htmlwidgets","readr","stringr",
             "splitstackshape","tidyverse",
             "tibbletime","readr", 
             "zoom", "plyr", "cowplot", "grid", "gridExtra",
             "openxlsx", "gapminder","WVPlots","plotly","htmlwidgets","splancs","sp","ks","rLiDAR")

package.check <- lapply(packages, FUN = function(TVD) {
  if (!require(TVD, character.only = TRUE)) {
    install.packages(TVD, dependencies = TRUE)
    library(TVD, character.only = TRUE)
  }
})


WD <- tk_choose.dir(getwd(), "Choose a suitable folder")
setwd(WD)


##############
Values <- import("Values-use-tidy.xlsx")
source.list <- unique(Values$Type)

Geo <- Values %>% filter(Values$Type == "Geophone")
Das <- Values %>% filter(Values$Type == "DAS")
rm(Values)

well_list <- unique(Values$`Well`)
well_list <- sort(well_list)

# fpoint <- 1
# lpoint <- which.max(is.na(Values$`Event Number`))
# if( lpoint  == 1) {
#   lpoint = length(Values$`Event Number`)+1
# }
# Values <- Values[(1:(lpoint-1)),]



i <- 6
wellnm <- well_list[i]
wellnm
#Geo

#cut <- Geo %>% filter(Geo$Well == wellnm)
cut <- Das %>% filter(Das$Well == wellnm)

stg_list <- unique(cut$Stage)
stg_list <- sort(stg_list)

j <- 1
STG <- stg_list[j]
print(STG)
stg <- cut %>% filter(cut$Stage == STG)
# stg <- stg %>% filter(stg$`Depth ft (TVD)` < 10319)
# stg <- stg %>% filter(stg$`Depth ft (TVD)` > 10228)
stm <- stg[,c(2,7:9)]
stgm <- as.matrix(stm)
nrow(stg)

### 3D Volume
#######################
set.seed((1234))
# Setring the xyz coordinates and subsetting the data
xyz<-subset(stgm[,2:4])

# Finding clusters
clLAS<-kmeans(xyz, 5)

# Set the id vector
id<-as.factor(clLAS$cluster)
# Set the alpha
alpha<-0.6

# Set the plotCAS parameter
plotit=TRUE

# Set the convex hull color
#col='forestgreen'
#col="darkorchid1"
col='cornflowerblue'
#col='brown1'
#col='yellow'
# Combining xyz and id
xyzid<-cbind(xyz,id)

# Get the volume and surface area
library("rLiDAR")
library(rgl)
open3d() 
volumeList<-chullLiDAR3D(xyzid=xyzid, plotit=plotit, col=col,alpha=alpha)
summary(volumeList) # summary
vol = (sum(volumeList$crownvolume)/3.28084)
vol = round(vol, digits = 1)
title<-paste("Well-",wellnm,"Total Volume = ",vol,"cubic feet") # main title

plot3d(xyzid[,1:3], add=TRUE)   # add the 3D point cloud
axes3d(c("x+", "y-", "z-"))                 # axes
grid3d(side=c('x+', 'y-', 'z'), col="gray") # grid
title3d(xlab = "Easthing (ft)", ylab = "Northing (ft)",zlab = "TVDSS (ft)", col="red")
aspect3d(1,1,0.7) # scale
title3d(main=title,line=1)

cvol <- data.frame(Tree = volumeList[1],crown_volume = volumeList[2]/3.28084)
csurf <- data.frame(Tree = volumeList[1],crown_surface = volumeList[3]/3.28084)
# meanvol <- data.frame(Tree = 1, Total_Volume = vol)
# Tree=c(1,2,3)
# Total_Volume=c(vol,vol,vol)
# meanvol <- data.frame(Tree, Total_Volume)

vol_out <- cvol %>% right_join(csurf, by=c("Tree"))
#vol_out <- vol_out %>% right_join(meanvol, by=c("Tree"))
glimpse(vol_out)

#write.csv(volumeList, file = paste("Well-",wellnm,"-Volume.csv",sep=""))
write.csv(vol_out, file = paste("Well-",wellnm," STG-",STG,"-Total Volume-",vol,"(cu ft).csv",sep=""))
sum(volumeList$crownvolume/3.28084)


####################################################################
WD <- tk_choose.dir(getwd(), "Choose a suitable folder")
setwd(WD)

file.list <- list.files(pattern="*.csv", recursive = FALSE)
edf.list <- sapply(file.list, read.csv, simplify=FALSE)
library(purrr)
file.list2 <- list.files(pattern='*.csv')
file.list2 <- setNames(file.list, file.list) 
library(data.table)
edf <- rbindlist(edf.list, idcol = "id", fill=TRUE)
edf <- map_df(file.list2, read.csv, .id = "id")
write.csv(edf, file = "All_SRV.csv")

Values <- import("All_SRV.csv")

source.list <- unique(Values$Type)

Geo <- Values %>% filter(Values$Type == "Geophone")
Das <- Values %>% filter(Values$Type == "DAS")

Values <- Geo
Values <- Das

label <- Values[1,9]

xmax <- max(Values$Total.Volume)+500000000

p <- ggplot(data=Values, aes(x=Total.Volume),color = factor(Values$Well_Stage))+geom_density(aes(group=factor(Values$Well_Stage),color=factor(Values$Well_Stage)), size=1)+
  theme_bw()+labs(x="Volume",y="Density")+
  theme(axis.text.x = element_text(face="bold", color="#000000", size=8, angle=0),
        axis.text.y = element_text(face="bold", color="#000000", size=8, angle=0))+ggtitle(paste0(label,": SRV QC Plot") )+xlim(c(0,xmax))
ggsave(paste0(label,"-SRV QC.png"))
ggplotly(p)
saveWidget(p, "test.html", selfcontained = T, libdir = "lib")
htmltools::save_html(p, "test.html")
