WD <- tk_choose.dir(getwd(), "Choose a suitable folder")
setwd(WD)




dataset <- import("Values4.xlsx")


data <- dataset  %>% select(Well, Stage, "Easting ft (Abs)","Northing ft (Abs)",fitted,"Easting (ft)","Northing (ft)",StgClust )

well_list <- unique(data$`Well`)
well_list <- sort(well_list)
wellnm <- well_list[4]



data1H <- data %>% filter(data$`Well` == wellnm)

stg_list <-unique(data1H$Stage)
stg_list <- stg_list[!is.na(stg_list)]
stg_list <- as.numeric(stg_list)
stg_list <- sort(stg_list)
stg <- stg_list[36]
lstg <-stg_list[length(stg_list)]
data1H <- data1H %>% filter(data1H$Stage == stg)
clust_list <- unique(data1H$StgClust)
clust_list <- sort(clust_list)
seg <- clust_list[1]

data1H <- data1H %>% filter(data1H$StgClust == seg)
data1H1 <- data %>% filter(data$`StgClust` == 1)

wellnm <- well_list[1]


seg <- clust_list[1]

i <- 2



getangle <- function(i){
seg <- clust_list[i]
data1H <- data %>% filter(data$`Well` == wellnm)
data1H <- data1H %>% filter(data1H$Stage == stg)
plot(data1H$`Easting ft (Abs)`,data1H$fitted)
data1H <- data1H %>% filter(data1H$StgClust == seg)

plot(data1H$`Easting ft (Abs)`,data1H$fitted)
fit1 <- lm(data1H$fitted ~ data1H$`Easting ft (Abs)`)
co1 <- coef(fit1)
co1
abline(fit1$coefficients[1], fit1$coefficients[2], col = "red", lwd = 2)

slope = fit1$coefficients[2]
angle1 = 90 - (atan(slope) * 180 / pi)
angle2 = 360 - angle1
ang_out <- data.frame(north = angle1, compass = angle2)
write.csv(ang_out, file = paste(wellnm,"-Stage-",stg,"-Seg-",seg,".csv",sep=""))
print(paste("Ran Stage",stg,"of",lstg, "Segment",seg))
}

for(i in clust_list) {
  getangle(i)
}

file.list <- list.files(pattern="*.csv", recursive = FALSE)
edf.list <- sapply(file.list, read.csv, simplify=FALSE)
library(purrr)
file.list2 <- list.files(pattern='*.csv')
file.list2 <- setNames(file.list, file.list) 
library(data.table)
edf <- rbindlist(edf.list, idcol = "id", fill=TRUE)
edf <- map_df(file.list2, read.csv, .id = "id")
write.csv(edf, file = "angles.csv")
##################################################
angles <- import("angles.xlsx")
data <- import("Values4.xlsx")
angles <- angles  %>% select(Well, Stage, Segment, north...5)
tst <- data %>% right_join(angles, by=c("Well","Stage","Segment"))

#write.csv(tst, file = "Values5.csv")

angles <- import("Values5.csv")
angles <- angles %>% select(Well, Stage,`Distance From Perfs`, Segment, Status,SegAngle,SegAngle2)
well_list <- unique(angles$`Well`)
well_list <- sort(well_list)
wellnm <- well_list[4]
angles <- angles %>% filter(angles$`Well` == wellnm)

# angles <- angles %>% filter(angles$`Stage` == 21)

ACM_list <-unique(angles$ACM)
angles <- angles %>% filter(angles$Status == 1)
angles <- angles %>% filter(angles$Status == 2)
colors_map <- c("blue")
colors_map <- c("red")

fig <- plot_ly(
  type = 'scatterpolar',
  r = angles$`Distance From Perfs`,
  theta = angles$SegAngle,
  name = angles$Status,
  color = angles$Status,
  colors = colors_map,
  text = paste('<b>DETAILS </b><br>---------------<br>', "Well:",angles$Well,"<br>Stage:",angles$Stage,"<br>Segments:",angles$Segment,"<br>ACM:",angles$Status),
  marker=list(size = 2.5),
  mode = 'markers'
)


fig <- layout(fig,
  title = list(
    x = 0.5, 
    font = list(size = 17),
    #title='<b>Bold</b> <i>animals</i>'
    text = paste("<b>", wellnm,"-Segment Angles By Stage","ACM:",angles$Status, "</b>"), y = 0.01))%>% hide_colorbar()%>% layout(legend= list(itemsizing='constant'))
fig <- fig %>%
  layout(
    polar = list(
      domain = list(
        x = c(0,5),
        y = c(0,5)
      ),
      radialaxis = list(
        tickfont = list(
          size = 12
        )
      ),
      angularaxis = list(
        tickfont = list(
          size = 12
        ),
        rotation = 90,
        direction = 'clockwise'
      )
    ))
fig

saveWidget(fig, paste(wellnm,"Conventional","SegAngles.html"), selfcontained = T, libdir = "lib")
saveWidget(fig, paste(wellnm,"ACM","SegAngles.html"), selfcontained = T, libdir = "lib")

# fig <- plot_ly(
#   type = 'scatterpolar',
#   mode = 'lines'
# ) 
fig <- fig %>%
  add_trace(
    r = angles$`Distance From Perfs`,
    theta = angles$SegAngle,
    text = paste('<b>DETAILS </b><br>---------------<br>', "Well:",angles$Well,"<br>Stage:",angles$Stage,"<br>Segments:",angles$Segment,"<br>ACM:",angles$Status),
    fill = angles$Status,
    fillcolor = '#709Bff',
    showlegend = FALSE,
    line = list(
      color = 'black'
    )
  ) %>% hide_colorbar()%>% layout(showlegend = FALSE)
fig



library(plotly)
library(dash)
fig <- plot_ly() 
# fig <- fig %>% add_trace( ... )
# fig <- fig %>% layout( ... ) 

library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)

app <- Dash$new()
app$layout(
  htmlDiv(
    list(
      dccGraph(figure=fig) 
    )
  )
)

app$run_server(debug=TRUE, dev_tools_hot_reload=FALSE)


##############################################
angles <- import("J:/Primexx/growth/try2/ET/cluster/SVM/svm_out/reg/curves/gather/map/angles/Values5.csv")
angles <- angles %>% select(Well, Stage,`Distance From Perfs`, Segment, Status,SegAngle,SegAngle2)
well_list <- unique(angles$`Well`)
well_list <- sort(well_list)
wellnm <- well_list[4]
angles <- angles %>% filter(angles$`Well` == wellnm)

# angles <- angles %>% filter(angles$`Stage` == 21)

ACM_list <-unique(angles$ACM)
angles1 <- angles %>% filter(angles$Status == 1)
angles2 <- angles %>% filter(angles$Status == 2)
colors_map <- c("blue")
colors_map <- c("red")


fig <- plot_ly(
  type = 'scatterpolar',
  mode = 'markers'
) 
fig <- fig %>%
  add_trace(
    r = angles1$`Distance From Perfs`,
    theta = angles1$SegAngle,
    marker = list(
      size = 2.5, color='blue')
  ) 
fig <- fig %>%
  add_trace(
    r = angles2$`Distance From Perfs`,
    theta = angles2$SegAngle,
    subplot = 'polar2',
    marker = list(
      size = 2.5, color='red') 
  )
fig <- fig %>%
  layout(
    polar = list(
      domain = list(
        x = c(0,0.46),
        y = c(0,1)
      ),
      radialaxis = list(
        tickfont = list(
          size = 8
        )
      ),
      angularaxis = list(
        tickfont = list(
          size = 8
        ),
        rotation = 90,
        direction = 'clockwise'
      )
    ),
    polar2 = list(
      domain = list(
        x = c(0.54,1),
        y = c(0,1)
      ),
      radialaxis = list(
        tickfont = list(
          size = 8
        )
      ),
      angularaxis = list(
        tickfont = list(
          size = 8
        ),
        rotation = 90,
        direction = 'clockwise'
      )
    ),
    title = list(text= paste("<b>", wellnm,"-Segment Angles vs Distance From Perfs","</b>"), 
                 font = list(size = 20), y = 0.9),
    showlegend = F
  )

fig

saveWidget(fig, paste(wellnm,"SegAngles.html"), selfcontained = T, libdir = "lib")








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


# 
# plot(data1H$`Easting ft (Abs)`,data1H$fitted)
# fit1 <- lm(data1H$fitted ~ data1H$`Easting ft (Abs)`)
# co1 <- coef(fit1)
# co1
# abline(fit1$coefficients[1], fit1$coefficients[2], col = "red", lwd = 2)
# 
# plot(Frac1$Easting..ft.,Frac1$Northing..ft.)
# fit2 <- lm(Frac1$Northing..ft ~ Frac1$Easting..ft.)
# co2 <- coef(fit2)
# co2
# abline(fit2$coefficients[1], fit2$coefficients[2], col = "blue", lwd = 2)
# plot(data1H$`Easting ft (Abs)`,data1H$fitted)
# abline(fit1$coefficients[1], fit1$coefficients[2], col = "red", lwd = 2)
# abline(fit2$coefficients[1], fit2$coefficients[2], col = "blue", lwd = 2)
# m1 <- fit1$coefficients[2]
# m2 <- fit2$coefficients[2]

# 
# 
# angle = 90 + angle
# angle2 = rad2deg(angle)
# 
# angle = atan(abs((m2-m1)/(1+m1*m2)))
# library(REdaS)
# angle2 = 90 - rad2deg(angle)
# angle3 = rad2deg(angle)