packages = c("dplyr","readxl","rio","openxlsx", "readr", "plotly","tcltk","htmlwidgets","patchwork","ggplot2","plyr"  )

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

WD <- tk_choose.dir(getwd(), "Choose a suitable folder")
setwd(WD)



dataset <- read_csv("Plot-Dataset.csv", 
                 col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
                                  Time = col_time(format = "%H:%M:%S"), 
                                  date_time = col_datetime(format = "%m/%d/%Y %H:%M:%S")))


##########################################################

well_list <- unique(dataset$Well)
print(well_list)

wellnm <- well_list[1]
print(wellnm)

welldata <- dataset %>% filter(dataset$Well == wellnm)

############################################################
stg <- 1
stage <- welldata %>% filter(welldata$Stage == stg)
H5DIST <- stage$`H5 Dist`[1]
H4DIST <- stage$`H4 Dist`[1]
H3DIST <- stage$`H3 Dist`[1]


####################################################################
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
wells <- import ("Wells.csv")
status_list <- unique(wells$Status)
print(status_list)
w1 <- wells %>% filter(Status == "Trace1")

w2 <- wells %>% filter(Status == "Trace2")

w3 <- wells %>% filter(Status == "Trace3")

w4 <- wells %>% filter(Status == "RefTrace1")

w5 <- wells %>% filter(Status == "ObTrace1")

w6 <- wells %>% filter(Status == "ObTrace2")



############# Create and save widget ###############
Values <- import("Values-use-tidy2.xlsx")




well_list <- unique(Values$Well)
print(well_list)

wellnm <- well_list[3]
print(wellnm)
treat <- import("All_Treat_Elapsed.xlsx")
#treat <- import("CWC_TREATMENT2.xlsx")
# Set Stage
i <- 1
####$ Run #####
stgdata <- Values %>% filter(Values$Well == wellnm)

stg_list <- unique(stgdata$Stage)
print(stg_list)
stg_list <- stg_list[1:15]
stg <- stg_list[i]
print(stg)
stgdata <- stgdata %>% filter(stgdata$Stage == stg)
stgdata <- stgdata %>% filter(stgdata$Status == "MSM")
#stgdata <- stgdata[c(1:197),]
# write.xlsx(stgdata, 'stgdata.xlsx')
# stgdata <- import('stgdata.xlsx')

D <- stgdata$`Distance From Perfs`
maxmsm <- max(stgdata$date_time)+1000

# Fiber 1 Frac 1
cwc11 <- import("CWC_4SAFrac_From_3SS.xlsx")
if (cwc11$Well[1] != wellnm){
  rm(cwc11)
}
cwc11 <- cwc11 %>% filter(cwc11$Stage == stg)

# Fiber 2 Frac 1
cwc12 <- import("CWC_4SAFrac_From_3SA.xlsx")
if (cwc12$Well[1] != wellnm){
  rm(cwc12)
}
cwc12 <- cwc12 %>% filter(cwc12$Stage == stg)

# Fiber 2 Frac 1
cwc21 <- import("CWC_4SSFrac_From_3SS.xlsx")
if (cwc21$Well[1] != wellnm){
  rm(cwc21)
}
cwc21 <- cwc21 %>% filter(cwc21$Stage == stg)

bar <- import("bar.xlsx")
bar <- bar %>% filter(bar$Well == wellnm)

bar <- bar %>% filter(bar$Stage == stg)
bar1 <- bar %>% filter(bar$Fiber == "3SS")
bar2 <- bar %>% filter(bar$Fiber == "3SA")



#treat <- import("CWC_TREATMENT.xlsx")

treat <- treat %>% filter(treat$Well == wellnm)

treatstg <- treat %>% filter(treat$Stage == stg)

# treatstg <- treatstg %>% filter(treatstg$Surf.Press..Csg...psi. > 0)
# treatstg <- treatstg %>% filter(treatstg$Slurry.Flow.Rate..bpm. > 0)
# treatstg <- treatstg %>% filter(treatstg$Proppant.Conc..ppg. > 0)


#plot(treatstg$date_time,treatstg$Surf.Press..Csg...psi.)

A <- treatstg$Surf.Press..Csg...psi.
B <- treatstg$Slurry.Flow.Rate..bpm.
C <- treatstg$Proppant.Conc..ppg.
D <- treatstg$date_time
md<-D[!is.na(D)]
md <- min(md)-300
D[1]
md

#### Curves ####



# V <- stage$Fluid_Volume
# mv<-V[!is.na(V)]
# mv <- max(mv)

##################


library(htmlwidgets)

library(plotly)
trace1 <- list(
  line = list(color = "rgb(244, 39, 8)"),
  shape = "spline",
  connectgaps = TRUE,
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
  connectgaps = TRUE,
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
  connectgaps = TRUE,
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
trace4 <- list(
  meta = list(columnNames = list(
    x = "D",
    y = "A"
  )),
  mode = "markers",
  name = "Distance (ft)",
  type = "scatter",
  xsrc = "smachage2020:19:bb036c",
  x = D,
  ysrc = "smachage2020:19:3e6732",
  y = E,
  yaxis = "y4",
  marker = list(color = "rgb(240, 171, 40)",
                size = 8),
  opacity = 0.55,
  showlegend = FALSE,
  stackgroup = NULL
)
trace5 <- list(
  line = list(color = "rgb(130, 217, 220)"),
  meta = list(columnNames = list(
    x = "D",
    y = "V"
  )),
  mode = "lines",
  name = "Fluid Volume",
  type = "scatter",
  xsrc = "smachage2020:19:bb036c",
  x = D,
  ysrc = "smachage2020:19:3e6732",
  y = E,
  yaxis = "y5",
  marker = list(color = "rgb(130, 217, 220)",
                size = 8),
  opacity = 0.55,
  showlegend = FALSE,
  stackgroup = NULL
)
data <- list(trace1, trace2, trace3, trace4, trace5)
layout <- list(
  title = list(text = paste(wellnm,"Stage",stg)), 
  xaxis = list(
    type = "date", 
    range = c(md, maxmsm), 
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
    range = c(0, max(A)), 
    title = "", 
    showline = FALSE, 
    showticklabels = FALSE,
    tickfont = list(color = "rgb(255, 0, 34)"), 
    autorange = FALSE,
    tickprefix = "       "
    
  ), 
  legend = list(orientation = "v"), 
  yaxis2 = list(
    side = "right", 
    type = "linear", 
    range = c(0, 150), 
    title = "",
    showticklabels = FALSE,
    showline = FALSE, 
    tickfont = list(color = "rgb(8, 103, 250)"), 
    autorange = FALSE, 
    overlaying = "y"
  ), 
  yaxis3 = list(
    side = "right", 
    type = "linear", 
    range = c(0, 10), 
    title = "", 
    tickfont = list(color = "rgb(53, 196, 20)"), 
    showticklabels = FALSE,
    autorange = FALSE, 
    overlaying = "y", 
    tickprefix = "                       "
  ), 
  yaxis4 = list(
    side = "right", 
    type = "linear", 
    dtick = 0, 
    range = c(0, 4000), 
    tick0 = 4, 
    title = list(
      font = list(color = "rgb(0, 0, 0)"), 
      text = "Distance From Perfs (ft)"
    ), 
    tickfont = list(color = "rgb(0, 0, 0)"),  
    tickmode = "auto", 
    autorange = FALSE, 
    overlaying = "y",
    ticksuffix = ""
  ), 
  yaxis5 = list(
    side = "right", 
    type = "linear", 
    dtick = 0, 
    range = c(0, 100), 
    tick0 = 4, 
    title = list(
      font = list(color = "rgb(0, 0, 0)"), 
      text = ""
    ), 
    tickfont = list(color = "rgb(0, 0, 0)"),  
    tickmode = "auto", 
    autorange = FALSE, 
    overlaying = "y",
    ticksuffix = ""
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
########
p <- plot_ly()
p <- add_trace(p, line=trace1$line, meta=trace1$meta, mode=trace1$mode, connectgaps = TRUE, name=trace1$name, type=trace1$type, xsrc=trace1$xsrc, x=trace1$x, ysrc=trace1$ysrc, y=trace1$y, showlegend=trace1$showlegend)
p <- add_trace(p, line=trace2$line, meta=trace2$meta, mode=trace2$mode, connectgaps = TRUE, name=trace2$name, type=trace2$type, xsrc=trace2$xsrc, x=trace2$x, ysrc=trace2$ysrc, y=trace2$y, yaxis=trace2$yaxis, showlegend=trace2$showlegend)
p <- add_trace(p, line=trace3$line, meta=trace3$meta, mode=trace3$mode, connectgaps = TRUE, name=trace3$name, type=trace3$type, xsrc=trace3$xsrc, x=trace3$x, ysrc=trace3$ysrc, y=trace3$y, yaxis=trace3$yaxis, showlegend=trace3$showlegend)
p <- add_trace(p, x=stgdata$date_time, y=stgdata$D4SA,yaxis=trace4$yaxis, type = 'scatter', mode = 'lines', name="4SA",text = "4SA ", hoverinfo = 'text', marker=list(color = 'rgba(142, 141, 143,1)', size = .1,line = list(color = "rgba(142, 141, 143,1)", width = 3)), line=list(color = 'rgba(142, 141, 143,1)', width = 2))
p <- add_trace(p, x=stgdata$date_time, y=stgdata$D4SB,yaxis=trace4$yaxis, type = 'scatter', mode = 'lines', name="4SB",text = "4SB ", hoverinfo = 'text', marker=list(color = 'rgba(142, 141, 143,1)', size = .1, line = list(color = "rgba(142, 141, 143,1)", width = 3)), line=list(color = 'rgba(142, 141, 143,1)', width = 2))
p <- add_trace(p, x=stgdata$date_time, y=stgdata$D2SB,yaxis=trace4$yaxis, type = 'scatter', mode = 'lines', name="2SB",text = "2SB ", hoverinfo = 'text', marker=list(color = 'rgba(142, 141, 143,1)', size = .1,line = list(color = "rgba(142, 141, 143,1)", width = 3)), line=list(color = 'rgba(142, 141, 143,1)', width = 2))
p <- add_trace(p, x=stgdata$date_time, y=stgdata$D3SA,yaxis=trace4$yaxis, type = 'scatter', mode = 'lines', name="3SA",text = "3SA ", hoverinfo = 'text', marker=list(color = 'rgba(142, 141, 143,1)', size = .1,line = list(color = "rgba(142, 141, 143,1)", width = 3)), line=list(color = 'rgba(142, 141, 143,1)', width = 2))
p <- add_trace(p, x=stgdata$date_time, y=stgdata$D3SS,yaxis=trace4$yaxis, type = 'scatter', mode = 'lines', name="3SS",text = "3SS ", hoverinfo = 'text', marker=list(color = 'rgba(142, 141, 143,1)', size = .1,line = list(color = "rgba(142, 141, 143,1)", width = 3)), line=list(color = 'rgba(142, 141, 143,1)', width = 2))

p <- add_trace(p, x=stgdata$date_time, y=stgdata$`Distance From Perfs`,yaxis=trace4$yaxis, type = 'scatter', mode = 'scatter', name="DASMSM",text = paste("DASMSM Event @ Time: ",stgdata$date_time,", <br>Distance From Perfs: ",stgdata$`Distance To Mid Perf`,"ft"), hoverinfo = 'text', marker=list(color = 'rgba(244, 0, 0,0.5)', size = 7, line = list(color = "black", width = 1)))
p <- add_trace(p, x=cwc11$Event_Start_Time, y=cwc11$`Distance To Fiber`,yaxis=trace4$yaxis, type = 'scatter', mode = 'lines', name="Fiber Event",text = paste("Start Fiber Event: <br>CWC Type: ",cwc11$Event_Type_name ,"<br>Fiber: ",cwc11$Fiber, "<br>Time: ",cwc11$Event_Start_Time,sep=""), hoverinfo = 'text', yaxis=trace4$yaxis, marker=list(color = 'black', size = 9, line = list(color = 'black', width = 2)), line=list(color = 'black', width = 6))
p <- add_trace(p, x=cwc12$Event_Start_Time, y=cwc12$`Distance To Fiber`,yaxis=trace4$yaxis, type = 'scatter', mode = 'lines', name="Fiber Event",text = paste("Start Fiber Event: <br>CWC Type: ",cwc12$Event_Type_name ,"<br>Fiber: ",cwc12$Fiber, "<br>Time: ",cwc12$Event_Start_Time,sep=""), hoverinfo = 'text', yaxis=trace4$yaxis, marker=list(color = 'black', size = 9, line = list(color = 'black', width = 2)), line=list(color = 'black', width = 6))
p <- add_trace(p, x=cwc21$Event_Start_Time, y=cwc21$`Distance To Fiber`,yaxis=trace4$yaxis, type = 'scatter', mode = 'lines', name="Fiber Event",text = paste("Start Fiber Event: <br>CWC Type: ",cwc21$Event_Type_name ,"<br>Fiber: ",cwc21$Fiber, "<br>Time: ",cwc21$Event_Start_Time,sep=""), hoverinfo = 'text', yaxis=trace4$yaxis, marker=list(color = 'black', size = 9, line = list(color = 'black', width = 2)), line=list(color = 'black', width = 6))
p <- add_trace(p, x=cwc11$Event_End_Time, y=cwc11$`Distance To Fiber`,yaxis=trace4$yaxis, type = 'scatter', mode = 'lines', name="Fiber Event",text = paste("Stop Fiber Event: <br>CWC Type: ",cwc11$Event_Type_name ,"<br>Fiber: ",cwc11$Fiber, "<br>Time: ",cwc11$Event_End_Time,sep=""), hoverinfo = 'text', yaxis=trace4$yaxis, marker=list(color = 'black', size = 9, line = list(color = 'black', width = 2)), line=list(color = 'black', width = 6))
p <- add_trace(p, x=cwc12$Event_End_Time, y=cwc12$`Distance To Fiber`,yaxis=trace4$yaxis, type = 'scatter', mode = 'lines', name="Fiber Event",text = paste("Stop Fiber Event: <br>CWC Type: ",cwc12$Event_Type_name ,"<br>Fiber: ",cwc12$Fiber, "<br>Time: ",cwc12$Event_End_Time,sep=""), hoverinfo = 'text', yaxis=trace4$yaxis, marker=list(color = 'black', size = 9, line = list(color = 'black', width = 2)), line=list(color = 'black', width = 6))
p <- add_trace(p, x=cwc21$Event_End_Time, y=cwc21$`Distance To Fiber`,yaxis=trace4$yaxis, type = 'scatter', mode = 'lines', name="Fiber Event",text = paste("Stop Fiber Event: <br>CWC Type: ",cwc21$Event_Type_name ,"<br>Fiber: ",cwc21$Fiber, "<br>Time: ",cwc21$Event_End_Time,sep=""), hoverinfo = 'text', yaxis=trace4$yaxis, marker=list(color = 'black', size = 9, line = list(color = 'black', width = 2)), line=list(color = 'black', width = 6))

p <- add_trace(p, x=D, y=0, type = 'scatter', mode = 'lines', name="Perf Zero Point",text = "Active Perforations", hoverinfo = 'text', yaxis=trace4$yaxis, marker=list(color = 'rgba(240, 89, 192,1)', size = .1, line = list(color = 'rgba(240, 89, 192,1)', width = 2)), line=list(color = 'rgba(240, 89, 192,1)', width = 2))
p <- add_trace(p, x=bar1$date_time, y=bar1$`Distance To Fiber`, type = 'scatter', mode = 'lines', name="Strain Event",text = "Strain Event", hoverinfo = 'text', yaxis=trace4$yaxis, marker=list(color = 'rgba(240, 89, 192,1)', size = .1, line = list(color = 'rgba(240, 89, 192,1)', width = 2)), line=list(color = 'rgba(0, 0, 0,1)', width = 4))
p <- add_trace(p, x=bar2$date_time, y=bar2$`Distance To Fiber`, type = 'scatter', mode = 'lines', name="Strain Event",text = "Strain Event", hoverinfo = 'text', yaxis=trace4$yaxis, marker=list(color = 'rgba(240, 89, 192,1)', size = .1, line = list(color = 'rgba(240, 89, 192,1)', width = 2)), line=list(color = 'rgba(0, 0, 0,1)', width = 4))

p <- layout(p, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, showlegend = FALSE,legend=layout$legend, yaxis2=layout$yaxis2, yaxis3=layout$yaxis3, yaxis4=layout$yaxis4,yaxis5=layout$yaxis5, autosize=layout$autosize, template=layout$template)

p

saveWidget(p, paste(wellnm,"Stage",stg,"TWM.html"), selfcontained = T, libdir = "lib")




# Stop here for treatment plot
############# Map #################

# Set Stage
i <- 6
####$ Run #####
stgdata <- Values %>% filter(Values$Well == wellnm)

stg_list <- unique(stgdata$Stage)
print(stg_list)
stg_list <- stg_list[1:15]
stg <- stg_list[i]
print(stg)
stgdata <- stgdata %>% filter(stgdata$Stage == stg)

# Perfs
perfdata <- stgdata %>% filter(stgdata$Status == "Perf")
stgdata <- stgdata %>% filter(stgdata$Status == "MSM")


D <- stgdata$`Distance From Perfs`
maxmsm <- max(stgdata$date_time)+1000

# Fiber 1 Frac 1
cwc11 <- import("CWC_4SAFrac_From_3SS.xlsx")
if (cwc11$Well[1] != wellnm){
  rm(cwc11)
}
cwc11 <- cwc11 %>% filter(cwc11$Stage == stg)

# Fiber 2 Frac 1
cwc12 <- import("CWC_4SAFrac_From_3SA.xlsx")
if (cwc12$Well[1] != wellnm){
  rm(cwc12)
}
cwc12 <- cwc12 %>% filter(cwc12$Stage == stg)

# Fiber 2 Frac 1
cwc21 <- import("CWC_4SSFrac_From_3SS.xlsx")
if (cwc21$Well[1] != wellnm){
  rm(cwc21)
}
cwc21 <- cwc21 %>% filter(cwc21$Stage == stg)



####################################################################
library(plotly)
trace1 <- list(
  meta = list(columnNames = list(
    x = "x", 
    y = "y"
  )), 
  mode = "markers", 
  type = "histogram2dcontour", 
  xsrc = "smachage2020:22:87e52b", 
  x = stgdata$`Easting ft (Abs)`,
  ysrc = "smachage2020:22:458877", 
  y = stgdata$`Northing ft (Abs)`,
  colorbar = list(title = list(text = "Density")), 
  contours = list(
    end = 50, 
    size = 5, 
    start = 1
  ), 
  autocontour = TRUE, 
  autocolorscale = TRUE
)
data <- list(trace1)
layout <- list(
  title = list(text = paste(wellnm,"Stage",stg)),
  xaxis = list(
    type = "linear", 
    range = c(1445899.5, 1449499.5), 
    title = list(text = "Easting (ft)"), 
    autorange = TRUE
  ), 
  yaxis = list(
    type = "linear", 
    range = c(483250, 487250), 
    title = list(text = "Northing (ft)"), 
    autorange = TRUE
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
        sequential = list(c(0, "#FCFCFC"),list(0.0893854748603352, "#354dff"),list(0.1787709497206704, "#4a79b7"),list(0.2681564245810056, "#6da4cc"),list(0.3575418994413408, "#98cae0"),list(0.44692737430167595, "#c2e3ee"),list(0.5363128491620112, "#eaebcc"),list(0.6256983240223464, "#feda8b"),list(0.7150837988826816, "#fdb466"),list(0.8044692737430168, "#f57d4a"),list(0.8938547486033519, "#dc3c2d"),list(0.9832402234636871, "#a50027"),list(1, "#880202")), 
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
text1 = paste(wellnm,"Stage",stg)
#######
p <- plot_ly()
p <- add_trace(p, meta=trace1$meta, mode=trace1$mode, type=trace1$type, xsrc=trace1$xsrc, x=trace1$x, ysrc=trace1$ysrc, y=trace1$y, colorbar=trace1$colorbar, contours=trace1$contours, autocontour=trace1$autocontour, autocolorscale=trace1$autocolorscale)
p <- add_trace(p, x=stgdata$`Easting ft (Abs)`, y=stgdata$`Northing ft (Abs)`, type = 'scatter', mode = 'markers', name="",text = paste("DASMSM Event @",stgdata$date_time), hoverinfo = 'text', marker=list(color = 'rgba(74, 74, 74,0.9)', size = 4, line = list(color = 'rgb(145, 140, 134)', width = 1)))
p <- add_trace(p, x=w1$`Easting ft (Abs)`, y=w1$`Northing ft (Abs)`, type = 'scatter', mode = 'lines', name="",text = paste(w1$Well), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1.5))
p <- add_trace(p, x=w2$`Easting ft (Abs)`, y=w2$`Northing ft (Abs)`, type = 'scatter', mode = 'lines', name="",text = paste(w2$Well), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w3$`Easting ft (Abs)`, y=w3$`Northing ft (Abs)`, type = 'scatter', mode = 'lines', name="",text = paste(w3$Well), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w4$`Easting ft (Abs)`, y=w4$`Northing ft (Abs)`, type = 'scatter', mode = 'lines', name="",text = paste(w4$Well), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1.5))
p <- add_trace(p, x=w5$`Easting ft (Abs)`, y=w5$`Northing ft (Abs)`, type = 'scatter', mode = 'lines', name="",text = paste(w5$Well), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "orange", width = 1)),line=list(color = 'orange', width = 1))
p <- add_trace(p, x=w6$`Easting ft (Abs)`, y=w6$`Northing ft (Abs)`, type = 'scatter', mode = 'lines', name="",text = paste(w6$Well), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "orange", width = 1)),line=list(color = 'orange', width = 1))

p <- add_trace(p, x=perfdata$`Easting ft (Abs)`, y=perfdata$`Northing ft (Abs)`, type = 'scatter', mode = 'scatter', name="",text = paste("Stage:",perfdata$Stage," Mid Cluster"), hoverinfo = 'text', marker=list(color = 'white', size = 8, line = list(color = "black", width = 3)))
p <- add_trace(p, x=cwc11$`Easting ft (Abs)`, y=cwc11$`Northing ft (Abs)`, type = 'scatter', mode = 'scatter', name="",text = paste("Fiber Event: <br>CWC Type: ",cwc11$Event_Type_name ,"<br>Fiber: ",cwc11$Fiber, "<br>Time: ",cwc11$Event_Start_Time,sep=""), hoverinfo = 'text', marker=list(color = 'green', size = 10, line = list(color = 'rgb(106, 247, 67)', width = 2)))
p <- add_trace(p, x=cwc12$`Easting ft (Abs)`, y=cwc12$`Northing ft (Abs)`, type = 'scatter', mode = 'scatter', name="",text = paste("Fiber Event: <br>CWC Type: ",cwc12$Event_Type_name ,"<br>Fiber: ",cwc12$Fiber, "<br>Time: ",cwc12$Event_Start_Time,sep=""), hoverinfo = 'text', marker=list(color = 'green', size = 10, line = list(color = 'rgb(106, 247, 67)', width = 2)))
p <- add_trace(p, x=cwc21$`Easting ft (Abs)`, y=cwc21$`Northing ft (Abs)`, type = 'scatter', mode = 'scatter', name="",text = paste("Fiber Event: <br>CWC Type: ",cwc21$Event_Type_name ,"<br>Fiber: ",cwc21$Fiber, "<br>Time: ",cwc21$Event_Start_Time,sep=""), hoverinfo = 'text', marker=list(color = 'green', size = 10, line = list(color = 'rgb(106, 247, 67)', width = 2)))

p <- layout(p, xaxis=layout$xaxis, yaxis=layout$yaxis, autosize=layout$autosize, template=layout$template,title = text1, showlegend = FALSE)
p

saveWidget(p, paste(wellnm,"Stage",stg,"All-map.html"), selfcontained = T, libdir = "lib")
