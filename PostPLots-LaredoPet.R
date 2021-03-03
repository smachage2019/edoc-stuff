############### Library Declarations ###############

packages = c("dplyr","readxl","rio", "openxlsx", "readr", "tidyr","tcltk","tibbletime","tibble","purrr","rgdal","plotly","htmlwidgets" )

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})


WD <- tk_choose.dir(getwd(), "Choose a suitable folder")
setwd(WD)
####################################################

Values <- import("Values-use-tidy.xlsx")
Values <- read_csv("Values-use.csv", col_types = cols(Stage = col_number(), 
                                                    date_time = col_datetime(format = "%m/%d/%Y %H:%M:%S")))

Values3 <- read_csv("Values3.csv", col_types = cols(`Distance From Array 1` = col_double(), 
                                                    `Distance From Array 2` = col_double(), 
                                                    oEast = col_double(), oNorth = col_double(), 
                                                    oTVD = col_double(), oTVDSS = col_double(), 
                                                    oSideView = col_double(), oEdgeView = col_double(), 
                                                    date_time = col_datetime(format = "%m/%d/%Y %H:%M:%S"), 
                                                    `Depth ft (TVDSS).f1` = col_double(), 
                                                    `Easting ft (Abs).f1` = col_double(), 
                                                    `Northing ft (Abs).f1` = col_double(), 
                                                    `Depth ft (TVDSS).f2` = col_double(), 
                                                    `Easting ft (Abs).f2` = col_double(), 
                                                    `Northing ft (Abs).f2` = col_double(), 
                                                    `Depth ft (TVDSS).f3` = col_double(), 
                                                    `Easting ft (Abs).f3` = col_double()))
View(Values3)
################### 3D Plot ####################
dataset <- data.frame(x=Values$`Easting ft (Abs)`, y=Values$`Northing ft (Abs)`, z=Values$`Depth ft (TVDSS)`,md = Values$`MD (ft)` ,id = Values$Status,
                      Stage =Values$Stage, w=Values$Well,mt = Values$date_time) 



w1 <- dataset %>% filter(id == "Trace1")
w1 <- w1 %>% arrange(w1$md)
w2 <- dataset %>% filter(id == "Trace2")
w2 <- w2 %>% arrange(w2$md)
w3 <- dataset %>% filter(id == "Trace3")
w3 <- w3 %>% arrange(w3$md)
w4 <- dataset %>% filter(id == "RefTrace1")
w4 <- w4 %>% arrange(w4$md)
w5 <- dataset %>% filter(id == "ObTrace1")
w5 <- w5 %>% arrange(w5$md)
w6 <- dataset %>% filter(id == "ObTrace2")
w6 <- w6 %>% arrange(w6$md)


msm <- dataset %>% filter(id == "MSM")
perf <- dataset %>% filter(id == "Perf")

axx <- list(
  nticks = 4,
  range = c(min(dataset$x-1500),max(dataset$x+1500))
)

axy <- list(
  nticks = 4,
  range = c(min(dataset$y-1500),max(dataset$y+1500))
)

axz <- list(
  nticks = 4,
  range = c(min(dataset$z-1500),max(dataset$z+1500))
)

p <- plot_ly(msm, x=~x, y=~y, z=~z, type="scatter3d", mode="markers",text = paste("Well:", msm$w," <br> Stage:",msm$Stage," <br> Time:",msm$mt," </br>"),name="DASMSM", hoverinfo = 'text', 
             color=~Stage, marker=list(size=3,opacity=0.3)) %>% layout(
               title = "3D View", scene = list(
                 xaxis = list(title = "Easting"),
                 yaxis = list(title = "Northing"),
                 zaxis = list(title = "Depth TVD")
               )) %>%
  add_trace(x=~w1$x, y=~w1$y, z=~w1$z , type = 'scatter3d', mode = 'lines', color= "gray", name=w1$w,text = w1$w, hoverinfo = 'text', marker=list(size=.3,opacity=0.5,color= "black" ))%>%
  add_trace(x=w2$x, y=w2$y, z=w2$z , type = 'scatter3d', mode = 'lines', color= "gray", name=w2$w,text = w2$w, hoverinfo = 'text', marker=list(size=.3,opacity=0.5,color= "black" ))%>%
  add_trace(x=w3$x, y=w3$y, z=w3$z , type = 'scatter3d', mode = 'lines', color= "gray", name=w3$w,text = w3$w, hoverinfo = 'text', marker=list(size=.3,opacity=0.5,color= "black" ))%>%
  add_trace(x=w4$x, y=w4$y, z=w4$z , type = 'scatter3d', mode = 'lines', color= "gray", name=w4$w,text = w4$w, hoverinfo = 'text', marker=list(size=.3,opacity=0.5,color= "black" ))%>%
  add_trace(x=w5$x, y=w5$y, z=w5$z , type = 'scatter3d', mode = 'lines', color= "black", name=w5$w,text = w5$w, hoverinfo = 'text', marker=list(size=.3,opacity=0.5,color= "black" ))%>%
  add_trace(x=w6$x, y=w6$y, z=w6$z , type = 'scatter3d', mode = 'lines', color= "black", name=w6$w,text = w6$w, hoverinfo = 'text', marker=list(size=.3,opacity=0.5,color= "black" ))%>%
  # add_trace(x=w7$x7, y=w7$y7, z=w7$z7 , type = 'scatter3d', mode = 'lines', color= "gray", name="",text = w7$w, hoverinfo = 'text', marker=list(size=.2,opacity=0.5,color= "black" ))%>%
  # add_trace(x=w8$x8, y=w8$y8, z=w8$z8 , type = 'scatter3d', mode = 'lines', color= "black", name="",text = w8$w, hoverinfo = 'text', marker=list(size=.2,opacity=0.5,color= "black" ))%>%
  add_trace(x=perf$x, y=perf$y, z=perf$z , type = 'scatter3d', mode = 'markers', name="Mid Cluster",text = paste("Stage = ",perf$Stage, "Mid Cluster"), hoverinfo = 'text',
            color= "#CC0001", marker=list(size=4,opacity=0.5,color= "#CC0001" ))%>%
  layout(showlegend = TRUE)%>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
p
saveWidget(p, "3D Map.html", selfcontained = T, libdir = "lib")

################### Density Map ####################
dataset <- data.frame(x=Values$`Easting ft (Abs)`, y=Values$`Northing ft (Abs)`, z=Values$`Depth ft (TVDSS)`,md = Values$`MD (ft)`, id = Values$Status,
                      Stage =Values$Stage, w=Values$Well,mt = Values$date_time, m=Values$Magnitude) 



w1 <- dataset %>% filter(id == "Trace1")
w1 <- w1 %>% arrange(w1$md)
w2 <- dataset %>% filter(id == "Trace2")
w2 <- w2 %>% arrange(w2$md)
w3 <- dataset %>% filter(id == "Trace3")
w3 <- w3 %>% arrange(w3$md)
w4 <- dataset %>% filter(id == "RefTrace1")
w4 <- w4 %>% arrange(w4$md)
w5 <- dataset %>% filter(id == "ObTrace1")
w5 <- w5 %>% arrange(w5$md)
w6 <- dataset %>% filter(id == "ObTrace2")
w6 <- w6 %>% arrange(w6$md)

msm <- dataset %>% filter(id == "MSM")
perf <- dataset %>% filter(id == "Perf")

######

trace1 <- list(
  meta = list(columnNames = list(
    x = "A", 
    y = "B"
  )), 
  mode = "markers", 
  type = "histogram2dcontour", 
  xsrc = "smachage2020:26:e2fb47", 
  x = msm$x,
  ysrc = "smachage2020:26:c66a3a", 
  y = msm$y,
  zmax = 72, 
  zmin = 0, 
  zauto = FALSE, 
  contours = list(
    end = 150, 
    size = 5, 
    start = 5, 
    showlines = FALSE
  ), 
  hoverinfo = "x+y+z+name", 
  colorscale = list(c(0, "#FFFFFF"),list(0.09090909090909091, "#1C00FF"),list(0.18181818181818182, "#004CFF"),list(0.2727272727272727, "#00A4FF"),list(0.36363636363636365, "#00FFF2"),list(0.45454545454545453, "#00FF8F"),list(0.5454545454545454, "#00FF2D"),list(0.6363636363636364, "#9EFF00"),list(0.7272727272727273, "#FFF800"),list(0.8181818181818182, "#FFA100"),list(0.9090909090909091, "#FF2400"),list(1, "#3c0911")), 
  autocontour = FALSE, 
  reversescale = FALSE, 
  autocolorscale = FALSE
)
data <- list(trace1)
layout <- list(
  title = list(text = "Density Map View"), 
  xaxis = list(
    type = "linear", 
    range = c(1553249.5, 1557749.5), 
    title = list(text = "Easting (ft)"), 
    autorange = TRUE
  ), 
  yaxis = list(
    type = "linear", 
    range = c(614300, 617100), 
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
#############
p <- plot_ly()
p <- add_trace(p, meta=trace1$meta, mode=trace1$mode, type=trace1$type, xsrc=trace1$xsrc, x=trace1$x, ysrc=trace1$ysrc, y=trace1$y, zmax=trace1$zmax, zmin=trace1$zmin, zauto=trace1$zauto, contours=trace1$contours, hoverinfo=trace1$hoverinfo, colorscale=trace1$colorscale, autocontour=trace1$autocontour, reversescale=trace1$reversescale, autocolorscale=trace1$autocolorscale)
p <- add_trace(p, x=w1$x, y=w1$y, type = 'scatter', mode = 'lines', name=paste(w1$w),text = paste(w1$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w2$x, y=w2$y, type = 'scatter', mode = 'lines', name=paste(w2$w),text = paste(w2$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w3$x, y=w3$y, type = 'scatter', mode = 'lines', name=paste(w3$w),text = paste(w3$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w4$x, y=w4$y, type = 'scatter', mode = 'lines', name=paste(w4$w),text = paste(w4$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w5$x, y=w5$y, type = 'scatter', mode = 'lines', name=paste(w5$w),text = paste(w5$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'orange', width = 1))
p <- add_trace(p, x=w6$x, y=w6$y, type = 'scatter', mode = 'lines', name=paste(w6$w),text = paste(w6$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'orange', width = 1))
p <- add_trace(p, x=perf$x, y=perf$y, type = 'scatter', mode = 'scatter', name="Mid Cluster",text = paste("Stage:",perf$Stage," Mid Cluster"), hoverinfo = 'text', marker = list( line = list(color = "#444", width = 2 ), size = 8, color = "#FFFFFF",opacity = 0.8))
p <- add_trace(p, x=msm$x, y=msm$y, type = 'scatter', mode = 'scatter', name="Event Scatter",text = paste('<b>DASMSM Event </b><br>---------------<br>',msm$mt,"<br>Well:",msm$w,"<br>Stage:",msm$Stage,"<br>Magnitude:",msm$m), hoverinfo = 'text', marker = list( line = list(color = "#686666", width = 1 ), size = 1, color = "#686666",opacity = 0.5))

p <- layout(p, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, autosize=layout$autosize, template=layout$template,showlegend = TRUE)
p
saveWidget(p, "Density Map.html", selfcontained = T, libdir = "lib")

################### Side View ####################

dataset <- data.frame(x=Values$SideView, y=Values$`Depth ft (TVDSS)`, z=Values$`Depth ft (TVDSS)`,md = Values$`MD (ft)`, id = Values$Status,
                      Stage =Values$Stage, w=Values$Well,mt = Values$date_time, m=Values$Magnitude) 



w1 <- dataset %>% filter(id == "Trace1")
w1 <- w1 %>% arrange(w1$md)
w2 <- dataset %>% filter(id == "Trace2")
w2 <- w2 %>% arrange(w2$md)
w3 <- dataset %>% filter(id == "Trace3")
w3 <- w3 %>% arrange(w3$md)
w4 <- dataset %>% filter(id == "RefTrace1")
w4 <- w4 %>% arrange(w4$md)
w5 <- dataset %>% filter(id == "ObTrace1")
w5 <- w5 %>% arrange(w5$md)
w6 <- dataset %>% filter(id == "ObTrace2")
w6 <- w6 %>% arrange(w6$md)

msm <- dataset %>% filter(id == "MSM")
perf <- dataset %>% filter(id == "Perf")


######

trace1 <- list(
  meta = list(columnNames = list(
    x = "A", 
    y = "B"
  )), 
  mode = "markers", 
  type = "histogram2dcontour", 
  xsrc = "smachage2020:26:e2fb47", 
  x = msm$x,
  ysrc = "smachage2020:26:c66a3a", 
  y = msm$y,
  zmax = 72, 
  zmin = 0, 
  zauto = FALSE, 
  contours = list(
    end = 150, 
    size = 5, 
    start = 5, 
    showlines = FALSE
  ), 
  hoverinfo = "x+y+z+name", 
  colorscale = list(c(0, "#FFFFFF"),list(0.09090909090909091, "#1C00FF"),list(0.18181818181818182, "#004CFF"),list(0.2727272727272727, "#00A4FF"),list(0.36363636363636365, "#00FFF2"),list(0.45454545454545453, "#00FF8F"),list(0.5454545454545454, "#00FF2D"),list(0.6363636363636364, "#9EFF00"),list(0.7272727272727273, "#FFF800"),list(0.8181818181818182, "#FFA100"),list(0.9090909090909091, "#FF2400"),list(1, "#3c0911")), 
  autocontour = FALSE, 
  reversescale = FALSE, 
  autocolorscale = FALSE
)
data <- list(trace1)
layout <- list(
  title = list(text = "Density Side View"), 
  xaxis = list(
    type = "linear", 
    range = c(97300, 98500), 
    title = list(text = "Side View (ft)"), 
    autorange = TRUE
  ), 
  yaxis = list(
    type = "linear", 
    range = c(-6000, -2000), 
    title = list(text = "TVDSS (ft)"), 
    autorange = FALSE
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
#############
p <- plot_ly()
p <- add_trace(p, meta=trace1$meta, mode=trace1$mode, type=trace1$type, xsrc=trace1$xsrc, x=trace1$x, ysrc=trace1$ysrc, y=trace1$y, zmax=trace1$zmax, zmin=trace1$zmin, zauto=trace1$zauto, contours=trace1$contours, hoverinfo=trace1$hoverinfo, colorscale=trace1$colorscale, autocontour=trace1$autocontour, reversescale=trace1$reversescale, autocolorscale=trace1$autocolorscale)
p <- add_trace(p, x=w1$x, y=w1$y, type = 'scatter', mode = 'lines', name=w1$w,text = paste(w1$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w2$x, y=w2$y, type = 'scatter', mode = 'lines', name=w2$w,text = paste(w2$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w3$x, y=w3$y, type = 'scatter', mode = 'lines', name=w3$w,text = paste(w3$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w4$x, y=w4$y, type = 'scatter', mode = 'lines', name=w4$w,text = paste(w4$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w5$x, y=w5$y, type = 'scatter', mode = 'lines', name=w5$w,text = paste(w5$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w6$x, y=w6$y, type = 'scatter', mode = 'lines', name=w6$w,text = paste(w6$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=perf$x, y=perf$y, type = 'scatter', mode = 'scatter', name="Mid Cluster",text = paste("Stage:",perf$Stage," Mid Cluster"), hoverinfo = 'text', marker = list( line = list(color = "#444", width = 2 ), size = 8, color = "#FFFFFF",opacity = 0.8))
p <- add_trace(p, x=msm$x, y=msm$y, type = 'scatter', mode = 'scatter', name="Event Scatter",text = paste('<b>DASMSM Event </b><br>---------------<br>',msm$mt,"<br>Well:",msm$w,"<br>Stage:",msm$Stage,"<br>Magnitude:",msm$m), hoverinfo = 'text', marker = list( line = list(color = "#686666", width = 1 ), size = 1, color = "#686666",opacity = 0.5))

p <- layout(p, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, autosize=layout$autosize, template=layout$template,showlegend = TRUE)

p
saveWidget(p, "Density Side View.html", selfcontained = T, libdir = "lib")
################### Edge View ####################

dataset <- data.frame(x=Values$EdgeView, y=Values$`Depth ft (TVDSS)`, z=Values$`Depth ft (TVDSS)`,md = Values$`MD (ft)`, id = Values$Status,
                      Stage =Values$Stage, w=Values$Well,mt = Values$date_time, m=Values$Magnitude) 



w1 <- dataset %>% filter(id == "Trace1")
w1 <- w1 %>% arrange(w1$md)
w2 <- dataset %>% filter(id == "Trace2")
w2 <- w2 %>% arrange(w2$md)
w3 <- dataset %>% filter(id == "Trace3")
w3 <- w3 %>% arrange(w3$md)
w4 <- dataset %>% filter(id == "RefTrace1")
w4 <- w4 %>% arrange(w4$md)
w5 <- dataset %>% filter(id == "ObTrace1")
w5 <- w5 %>% arrange(w5$md)
w6 <- dataset %>% filter(id == "ObTrace2")
w6 <- w6 %>% arrange(w6$md)

msm <- dataset %>% filter(id == "MSM")
perf <- dataset %>% filter(id == "Perf")

######

trace1 <- list(
  meta = list(columnNames = list(
    x = "A", 
    y = "B"
  )), 
  mode = "markers", 
  type = "histogram2dcontour", 
  xsrc = "smachage2020:26:e2fb47", 
  x = msm$x,
  ysrc = "smachage2020:26:c66a3a", 
  y = msm$y,
  zmax = 72, 
  zmin = 0, 
  zauto = FALSE, 
  contours = list(
    end = 150, 
    size = 5, 
    start = 5, 
    showlines = FALSE
  ), 
  hoverinfo = "x+y+z+name", 
  colorscale = list(c(0, "#FFFFFF"),list(0.09090909090909091, "#1C00FF"),list(0.18181818181818182, "#004CFF"),list(0.2727272727272727, "#00A4FF"),list(0.36363636363636365, "#00FFF2"),list(0.45454545454545453, "#00FF8F"),list(0.5454545454545454, "#00FF2D"),list(0.6363636363636364, "#9EFF00"),list(0.7272727272727273, "#FFF800"),list(0.8181818181818182, "#FFA100"),list(0.9090909090909091, "#FF2400"),list(1, "#3c0911")), 
  autocontour = FALSE, 
  reversescale = FALSE, 
  autocolorscale = FALSE
)
data <- list(trace1)
layout <- list(
  title = list(text = "Density Edge View"), 
  xaxis = list(
    type = "linear", 
    range = c(97300, 98500), 
    title = list(text = "Edge View (ft)"), 
    autorange = TRUE
  ), 
  yaxis = list(
    type = "linear", 
    range = c(-6000, -2000), 
    title = list(text = "TVDSS (ft)"), 
    autorange = FALSE
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
#############
p <- plot_ly()
p <- add_trace(p, meta=trace1$meta, mode=trace1$mode, type=trace1$type, xsrc=trace1$xsrc, x=trace1$x, ysrc=trace1$ysrc, y=trace1$y, zmax=trace1$zmax, zmin=trace1$zmin, zauto=trace1$zauto, contours=trace1$contours, hoverinfo=trace1$hoverinfo, colorscale=trace1$colorscale, autocontour=trace1$autocontour, reversescale=trace1$reversescale, autocolorscale=trace1$autocolorscale)
p <- add_trace(p, x=w1$x, y=w1$y, type = 'scatter', mode = 'lines', name=w1$w,text = paste(w1$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w2$x, y=w2$y, type = 'scatter', mode = 'lines', name=w2$w,text = paste(w2$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w3$x, y=w3$y, type = 'scatter', mode = 'lines', name=w3$w,text = paste(w3$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w4$x, y=w4$y, type = 'scatter', mode = 'lines', name=w4$w,text = paste(w4$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w5$x, y=w5$y, type = 'scatter', mode = 'lines', name=w5$w,text = paste(w5$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w6$x, y=w6$y, type = 'scatter', mode = 'lines', name=w6$w,text = paste(w6$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=perf$x, y=perf$y, type = 'scatter', mode = 'scatter', name="Mid Cluster",text = paste("Stage:",perf$Stage," Mid Cluster"), hoverinfo = 'text', marker = list( line = list(color = "#444", width = 2 ), size = 8, color = "#FFFFFF",opacity = 0.8))
p <- add_trace(p, x=msm$x, y=msm$y, type = 'scatter', mode = 'scatter', name="Event Scatter",text = paste('<b>DASMSM Event </b><br>---------------<br>',msm$mt,"<br>Well:",msm$w,"<br>Stage:",msm$Stage,"<br>Magnitude:",msm$m), hoverinfo = 'text', marker = list( line = list(color = "#686666", width = 1 ), size = 1, color = "#686666",opacity = 0.5))

p <- layout(p, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, autosize=layout$autosize, template=layout$template,showlegend = TRUE)
p

saveWidget(p, "Density Edge View.html", selfcontained = T, libdir = "lib")

################### Mag Distance ####################
well_list <- unique(Values$Well)
well_list <- sort(well_list)
well_list <- well_list[4:6]
wellnm <- well_list[1]
cut <- Values %>% filter(Values$Well == wellnm)

dataset <- data.frame(Distance=cut$`Distance From Array 1`, Magnitude=cut$Magnitude, Stage = cut$Stage, w=cut$Well, Confidence = cut$Confidence) 

p <- plot_ly(dataset, x=~Distance, y=~Magnitude, type="scatter", mode="markers",text = paste("Well:", dataset$w," <br> Stage:",dataset$Stage," <br> Magnitude:",dataset$Magnitude," <br> Distance(ft):",dataset$Distance," <br> Confidence:",dataset$Confidence," </br>"),name="Confidence", hoverinfo = 'text', 
             color=~Confidence, marker=list(size=10,opacity=0.4)) %>% layout(title = paste(wellnm,"- Magnitude vs Distance "), scene = list(xaxis = list(title = "Distance From Tools (ft)"),
                                                                                                                      yaxis = list(title = "Magnitude")))%>%
  layout(showlegend = FALSE)
p
saveWidget(p, paste(wellnm," - Magnitude vs Distance.html"), selfcontained = T, libdir = "lib")

################### Cum Moment Map ####################
dataset <- data.frame(x=Values$`Easting ft (Abs)`, y=Values$`Northing ft (Abs)`, z=Values$`Depth ft (TVDSS)`,md = Values$`MD (ft)`, id = Values$Status,
                      Stage =Values$Stage, w=Values$Well,mt = Values$date_time, m=Values$Magnitude, cm=Values$`Cum Moment`) 



w1 <- dataset %>% filter(id == "Trace1")
w1 <- w1 %>% arrange(w1$md)
w2 <- dataset %>% filter(id == "Trace2")
w2 <- w2 %>% arrange(w2$md)
w3 <- dataset %>% filter(id == "Trace3")
w3 <- w3 %>% arrange(w3$md)
w4 <- dataset %>% filter(id == "RefTrace1")
w4 <- w4 %>% arrange(w4$md)
w5 <- dataset %>% filter(id == "ObTrace1")
w5 <- w5 %>% arrange(w5$md)
w6 <- dataset %>% filter(id == "ObTrace2")
w6 <- w6 %>% arrange(w6$md)

msm <- dataset %>% filter(id == "MSM")
perf <- dataset %>% filter(id == "Perf")


df <- data.frame(x = msm$x, y = msm$y,z = msm$cm,c=msm$c,mt = msm$mt,m=msm$m,
                 d = densCols(msm$cm, colramp = colorRampPalette(rainbow(10, end = 4/6))))

######
trace1 <- list(
  meta = list(columnNames = list(
    x = "A", 
    y = "B"
  )), 
  mode = "markers", 
  type = "histogram2dcontour", 
  xsrc = "smachage2020:26:e2fb47", 
  x = msm$x,
  ysrc = "smachage2020:26:c66a3a", 
  y = msm$y,
  zmax = 72, 
  zmin = 0, 
  zauto = FALSE, 
  contours = list(
    end = 150, 
    size = 5, 
    start = 5, 
    showlines = FALSE
  ), 
  hoverinfo = "x+y+z+name", 
  colorscale = list(c(0, "#FFFFFF"),list(0.09090909090909091, "#1C00FF"),list(0.18181818181818182, "#004CFF"),list(0.2727272727272727, "#00A4FF"),list(0.36363636363636365, "#00FFF2"),list(0.45454545454545453, "#00FF8F"),list(0.5454545454545454, "#00FF2D"),list(0.6363636363636364, "#9EFF00"),list(0.7272727272727273, "#FFF800"),list(0.8181818181818182, "#FFA100"),list(0.9090909090909091, "#FF2400"),list(1, "#3c0911")), 
  autocontour = FALSE, 
  reversescale = FALSE, 
  autocolorscale = FALSE
)
data <- list(trace1)
layout <- list(
  title = list(text = "Cumulative Moment Magnitude Map View"), 
  xaxis = list(
    type = "linear", 
    range = c(1553249.5, 1557749.5), 
    title = list(text = "Easting (ft)"), 
    autorange = TRUE
  ), 
  yaxis = list(
    type = "linear", 
    range = c(614300, 617100), 
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
#############
p <- plot_ly()
#p <- add_trace(p, meta=trace1$meta, mode=trace1$mode, type=trace1$type, xsrc=trace1$xsrc, x=trace1$x, ysrc=trace1$ysrc, y=trace1$y, zmax=trace1$zmax, zmin=trace1$zmin, zauto=trace1$zauto, contours=trace1$contours,name="Contours", hoverinfo=trace1$hoverinfo, colorscale=trace1$colorscale, autocontour=trace1$autocontour, reversescale=trace1$reversescale, autocolorscale=trace1$autocolorscale)
p <- add_trace(p, x=w1$x, y=w1$y, type = 'scatter', mode = 'lines', name=w1$w,text = paste(w1$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w2$x, y=w2$y, type = 'scatter', mode = 'lines', name=w2$w,text = paste(w2$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w3$x, y=w3$y, type = 'scatter', mode = 'lines', name=w3$w,text = paste(w3$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w4$x, y=w4$y, type = 'scatter', mode = 'lines', name=w4$w,text = paste(w4$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w5$x, y=w5$y, type = 'scatter', mode = 'lines', name=w5$w,text = paste(w5$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'orange', width = 1))
p <- add_trace(p, x=w6$x, y=w6$y, type = 'scatter', mode = 'lines', name=w6$w,text = paste(w6$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'orange', width = 1))
p <- add_trace(p, x=df$x, y=df$y, type = 'scatter', mode = 'scatter', name="DASMSM",text = paste(df$mt,"Stage:",df$c," Magnitude:",df$m), hoverinfo = 'text', marker = list( line = list(color = df$d, width = 1 ), size = 5, color = df$d,opacity = 0.5))
p <- add_trace(p, x=perf$x, y=perf$y, type = 'scatter', mode = 'scatter', name="Mid Cluster",text = paste("Stage:",perf$Stage," Mid Cluster"), hoverinfo = 'text', marker = list( line = list(color = "#444", width = 2 ), size = 8, color = "#FFFFFF",opacity = 0.8))
p <- layout(p, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, autosize=layout$autosize, template=layout$template,showlegend = TRUE)
p

################### Cum Moment Side ####################
dataset <- data.frame(x=Values$SideView, y=Values$`Depth ft (TVDSS)`, z=Values$`Depth ft (TVDSS)`,md = Values$`MD (ft)`, id = Values$Status,
                      Stage =Values$Stage, w=Values$Well,mt = Values$date_time, m=Values$Magnitude, cm=Values$`Cum Moment`) 

w1 <- dataset %>% filter(id == "Trace1")
w1 <- w1 %>% arrange(w1$md)
w2 <- dataset %>% filter(id == "Trace2")
w2 <- w2 %>% arrange(w2$md)
w3 <- dataset %>% filter(id == "Trace3")
w3 <- w3 %>% arrange(w3$md)
w4 <- dataset %>% filter(id == "RefTrace1")
w4 <- w4 %>% arrange(w4$md)
w5 <- dataset %>% filter(id == "ObTrace1")
w5 <- w5 %>% arrange(w5$md)
w6 <- dataset %>% filter(id == "ObTrace2")
w6 <- w6 %>% arrange(w6$md)

msm <- dataset %>% filter(id == "MSM")
perf <- dataset %>% filter(id == "Perf")


df <- data.frame(x = msm$x, y = msm$y,z = msm$cm,c=msm$c,mt = msm$mt,m=msm$m,
                 d = densCols(msm$cm, colramp = colorRampPalette(rainbow(10, end = 4/6))))

######

trace1 <- list(
  meta = list(columnNames = list(
    x = "A", 
    y = "B"
  )), 
  mode = "markers", 
  type = "histogram2dcontour", 
  xsrc = "smachage2020:26:e2fb47", 
  x = msm$x,
  ysrc = "smachage2020:26:c66a3a", 
  y = msm$y,
  zmax = 72, 
  zmin = 0, 
  zauto = FALSE, 
  contours = list(
    end = 150, 
    size = 5, 
    start = 5, 
    showlines = FALSE
  ), 
  hoverinfo = "x+y+z+name", 
  colorscale = list(c(0, "#FFFFFF"),list(0.09090909090909091, "#1C00FF"),list(0.18181818181818182, "#004CFF"),list(0.2727272727272727, "#00A4FF"),list(0.36363636363636365, "#00FFF2"),list(0.45454545454545453, "#00FF8F"),list(0.5454545454545454, "#00FF2D"),list(0.6363636363636364, "#9EFF00"),list(0.7272727272727273, "#FFF800"),list(0.8181818181818182, "#FFA100"),list(0.9090909090909091, "#FF2400"),list(1, "#3c0911")), 
  autocontour = FALSE, 
  reversescale = FALSE, 
  autocolorscale = FALSE
)
data <- list(trace1)
layout <- list(
  title = list(text = "Cumulative Moment Magnitude Side View"), 
  xaxis = list(
    type = "linear", 
    range = c(97300, 98500), 
    title = list(text = "Side View (ft)"), 
    autorange = TRUE
  ), 
  yaxis = list(
    type = "linear", 
    range = c(-6000, -2000), 
    title = list(text = "TVDSS (ft)"), 
    autorange = FALSE
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
#############
p <- plot_ly()
#p <- add_trace(p, meta=trace1$meta, mode=trace1$mode, type=trace1$type, xsrc=trace1$xsrc, x=trace1$x, ysrc=trace1$ysrc, y=trace1$y, zmax=trace1$zmax, zmin=trace1$zmin, zauto=trace1$zauto, contours=trace1$contours,name="Contours", hoverinfo=trace1$hoverinfo, colorscale=trace1$colorscale, autocontour=trace1$autocontour, reversescale=trace1$reversescale, autocolorscale=trace1$autocolorscale)
p <- add_trace(p, x=w1$x, y=w1$y, type = 'scatter', mode = 'lines', name=w1$w,text = paste(w1$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w2$x, y=w2$y, type = 'scatter', mode = 'lines', name=w2$w,text = paste(w2$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w3$x, y=w3$y, type = 'scatter', mode = 'lines', name=w3$w,text = paste(w3$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w4$x, y=w4$y, type = 'scatter', mode = 'lines', name=w4$w,text = paste(w4$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w5$x, y=w5$y, type = 'scatter', mode = 'lines', name=w5$w,text = paste(w5$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'orange', width = 1))
p <- add_trace(p, x=w6$x, y=w6$y, type = 'scatter', mode = 'lines', name=w6$w,text = paste(w6$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'orange', width = 1))
p <- add_trace(p, x=df$x, y=df$y, type = 'scatter', mode = 'scatter', name="DASMSM",text = paste(df$mt,"Stage:",df$c," Magnitude:",df$m), hoverinfo = 'text', marker = list( line = list(color = df$d, width = 1 ), size = 5, color = df$d,opacity = 0.5))
p <- add_trace(p, x=perf$x, y=perf$y, type = 'scatter', mode = 'scatter', name="Mid Cluster",text = paste("Stage:",perf$Stage," Mid Cluster"), hoverinfo = 'text', marker = list( line = list(color = "#444", width = 2 ), size = 8, color = "#FFFFFF",opacity = 0.8))
p <- layout(p, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, autosize=layout$autosize, template=layout$template,showlegend = TRUE)
p

################### Cum Moment Edge ####################
dataset <- data.frame(x=Values$EdgeView, y=Values$`Depth ft (TVDSS)`, z=Values$`Depth ft (TVDSS)`,md = Values$`MD (ft)`, id = Values$Status,
                      Stage =Values$Stage, w=Values$Well,mt = Values$date_time, m=Values$Magnitude, cm=Values$`Cum Moment`) 

w1 <- dataset %>% filter(id == "Trace1")
w1 <- w1 %>% arrange(w1$md)
w2 <- dataset %>% filter(id == "Trace2")
w2 <- w2 %>% arrange(w2$md)
w3 <- dataset %>% filter(id == "Trace3")
w3 <- w3 %>% arrange(w3$md)
w4 <- dataset %>% filter(id == "RefTrace1")
w4 <- w4 %>% arrange(w4$md)
w5 <- dataset %>% filter(id == "ObTrace1")
w5 <- w5 %>% arrange(w5$md)
w6 <- dataset %>% filter(id == "ObTrace2")
w6 <- w6 %>% arrange(w6$md)

msm <- dataset %>% filter(id == "MSM")
perf <- dataset %>% filter(id == "Perf")


df <- data.frame(x = msm$x, y = msm$y,z = msm$cm,c=msm$c,mt = msm$mt,m=msm$m,
                 d = densCols(msm$cm, colramp = colorRampPalette(rainbow(10, end = 4/6))))

######

trace1 <- list(
  meta = list(columnNames = list(
    x = "A", 
    y = "B"
  )), 
  mode = "markers", 
  type = "histogram2dcontour", 
  xsrc = "smachage2020:26:e2fb47", 
  x = msm$x,
  ysrc = "smachage2020:26:c66a3a", 
  y = msm$y,
  zmax = 72, 
  zmin = 0, 
  zauto = FALSE, 
  contours = list(
    end = 150, 
    size = 5, 
    start = 5, 
    showlines = FALSE
  ), 
  hoverinfo = "x+y+z+name", 
  colorscale = list(c(0, "#FFFFFF"),list(0.09090909090909091, "#1C00FF"),list(0.18181818181818182, "#004CFF"),list(0.2727272727272727, "#00A4FF"),list(0.36363636363636365, "#00FFF2"),list(0.45454545454545453, "#00FF8F"),list(0.5454545454545454, "#00FF2D"),list(0.6363636363636364, "#9EFF00"),list(0.7272727272727273, "#FFF800"),list(0.8181818181818182, "#FFA100"),list(0.9090909090909091, "#FF2400"),list(1, "#3c0911")), 
  autocontour = FALSE, 
  reversescale = FALSE, 
  autocolorscale = FALSE
)
data <- list(trace1)
layout <- list(
  title = list(text = "Cumulative Moment Magnitude Edge View"), 
  xaxis = list(
    type = "linear", 
    range = c(97300, 98500), 
    title = list(text = "Side View (ft)"), 
    autorange = TRUE
  ), 
  yaxis = list(
    type = "linear", 
    range = c(-6000, -2000), 
    title = list(text = "TVDSS (ft)"), 
    autorange = FALSE
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
#############
p <- plot_ly()
#p <- add_trace(p, meta=trace1$meta, mode=trace1$mode, type=trace1$type, xsrc=trace1$xsrc, x=trace1$x, ysrc=trace1$ysrc, y=trace1$y, zmax=trace1$zmax, zmin=trace1$zmin, zauto=trace1$zauto, contours=trace1$contours,name="Contours", hoverinfo=trace1$hoverinfo, colorscale=trace1$colorscale, autocontour=trace1$autocontour, reversescale=trace1$reversescale, autocolorscale=trace1$autocolorscale)
p <- add_trace(p, x=w1$x, y=w1$y, type = 'scatter', mode = 'lines', name=w1$w,text = paste(w1$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w2$x, y=w2$y, type = 'scatter', mode = 'lines', name=w2$w,text = paste(w2$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w3$x, y=w3$y, type = 'scatter', mode = 'lines', name=w3$w,text = paste(w3$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w4$x, y=w4$y, type = 'scatter', mode = 'lines', name=w4$w,text = paste(w4$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w5$x, y=w5$y, type = 'scatter', mode = 'lines', name=w5$w,text = paste(w5$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'orange', width = 1))
p <- add_trace(p, x=w6$x, y=w6$y, type = 'scatter', mode = 'lines', name=w6$w,text = paste(w6$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'orange', width = 1))
p <- add_trace(p, x=df$x, y=df$y, type = 'scatter', mode = 'scatter', name="DASMSM",text = paste(df$mt,"Stage:",df$c," Magnitude:",df$m), hoverinfo = 'text', marker = list( line = list(color = df$d, width = 1 ), size = 5, color = df$d,opacity = 0.5))
p <- add_trace(p, x=perf$x, y=perf$y, type = 'scatter', mode = 'scatter', name="Mid Cluster",text = paste("Stage:",perf$Stage," Mid Cluster"), hoverinfo = 'text', marker = list( line = list(color = "#444", width = 2 ), size = 8, color = "#FFFFFF",opacity = 0.8))
p <- layout(p, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, autosize=layout$autosize, template=layout$template,showlegend = TRUE)
p
####################### Contour Side #####################
dataset <- data.frame(x=Values$SideView, y=Values$`Depth ft (TVDSS)`, cm=Values$`Cum Moment`,
                      x1=Values$SideView.t1,y1=Values$`Depth ft (TVDSS).t1`,md1 = Values$`MD (ft).t1`,
                      x2=Values$SideView.t2,y2=Values$`Depth ft (TVDSS).t2`,md2 = Values$`MD (ft).t2`,
                      x3=Values$SideView.t3,y3=Values$`Depth ft (TVDSS).t3`,md3 = Values$`MD (ft).t3`,
                      x4=Values$RefSideView.t1,y4=Values$`Depth ft (TVDSS).R1`,md4 = Values$`MD (ft).R1`,
                      x5=Values$ObsSideView.t1, y5=Values$`Depth ft (TVDSS).Obt1`,md5 = Values$`MD (ft).Obt1`,
                      x6=Values$ObsSideView.t2, y6=Values$`Depth ft (TVDSS).Obt2`,md6 = Values$`MD (ft).Obt2`,
                      c=Values$Stage, w=Values$Well, perfE = Values$pSideView, perfN =Values$pTVDSS, m = Values$Magnitude, mt = Values$date_time)
write.xlsx(dataset, 'dataset.xlsx')

df <- data.frame(x = dataset$x, y = dataset$y,z = dataset$cm,c=dataset$c,mt = dataset$mt,m=dataset$m,perfE = dataset$perfE, perfN =dataset$perfN,
                 d = densCols(dataset$cm, colramp = colorRampPalette((rev(rainbow(10, end = 4/6))))))
######
w1 <- dataset[,c(4,5,6,22,23,24,25,26,27)]
w1 <- w1 %>% arrange(w1$md1)
w1 <- w1[c(1:197),]

w2 <- dataset[,c(7,8,9,22,23,24,25,26,27)]
w2 <- w2 %>% arrange(w2$md2)
w2 <- w2[c(1:207),]

w3 <- dataset[,c(10,11,12,22,23,24,25,26,27)]
w3 <- w3 %>% arrange(w3$md3)
w3 <- w3[c(1:193),]

w4 <- dataset[,c(13,14,15,22,23,24,25,26,27)]
w4 <- w4 %>% arrange(w4$md4)
w4 <- w4[c(1:197),]

w5 <- dataset[,c(16,17,18,22,23,24,25,26,27)]
w5 <- w5 %>% arrange(w5$md5)
w5 <- w5[c(1:167),]

w6 <- dataset[,c(19,20,21,22,23,24,25,26,27)]
w6 <- w6 %>% arrange(w6$md6)
w6 <- w6[c(1:176),]

######

trace1 <- list(
  meta = list(columnNames = list(
    x = "A", 
    y = "B"
  )), 
  mode = "markers", 
  type = "histogram2dcontour", 
  xsrc = "smachage2020:26:e2fb47", 
  x = dataset$x,
  ysrc = "smachage2020:26:c66a3a", 
  y = dataset$y,
  zmax = 72, 
  zmin = 0, 
  zauto = FALSE, 
  contours = list(
    end = 70, 
    size = 5, 
    start = 5, 
    showlines = FALSE
  ), 
  hoverinfo = "x+y+z+name", 
  colorscale = list(c(0, "#FFFFFF"),list(0.09090909090909091, "#1C00FF"),list(0.18181818181818182, "#004CFF"),list(0.2727272727272727, "#00A4FF"),list(0.36363636363636365, "#00FFF2"),list(0.45454545454545453, "#00FF8F"),list(0.5454545454545454, "#00FF2D"),list(0.6363636363636364, "#9EFF00"),list(0.7272727272727273, "#FFF800"),list(0.8181818181818182, "#FFA100"),list(0.9090909090909091, "#FF2400"),list(1, "#3c0911")), 
  autocontour = TRUE, 
  reversescale = FALSE, 
  autocolorscale = FALSE
)
data <- list(trace1)
layout <- list(
  title = list(text = "Density Side View"), 
  xaxis = list(
    type = "linear", 
    range = c(min(dataset$x), max(dataset$x)), 
    title = list(text = "Side View (ft)"), 
    autorange = TRUE
  ), 
  yaxis = list(
    type = "linear", 
    range = c(-8000, -4000), 
    title = list(text = "TVDSS (ft)"), 
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
#############
p <- plot_ly()
p <- add_trace(p, meta=trace1$meta, mode=trace1$mode, type=trace1$type, xsrc=trace1$xsrc, x=trace1$x, ysrc=trace1$ysrc, y=trace1$y, zmax=trace1$zmax, zmin=trace1$zmin, zauto=trace1$zauto, contours=trace1$contours, hoverinfo=trace1$hoverinfo, colorscale=trace1$colorscale, autocontour=trace1$autocontour, reversescale=trace1$reversescale, autocolorscale=trace1$autocolorscale)
p <- add_trace(p, x=w1$x1, y=w1$y1, type = 'scatter', mode = 'lines', name="",text = paste(w1$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w2$x2, y=w2$y2, type = 'scatter', mode = 'lines', name="",text = paste(w2$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w3$x3, y=w3$y3, type = 'scatter', mode = 'lines', name="",text = paste(w3$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w4$x4, y=w4$y4, type = 'scatter', mode = 'lines', name="",text = paste(w4$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'black', width = 1))
p <- add_trace(p, x=w5$x5, y=w5$y5, type = 'scatter', mode = 'lines', name="",text = paste(w5$w), hoverinfo = 'text', marker=list(color = 'white', size = .5, line = list(color = "black", width = 1)),line=list(color = 'orange', width = 1))
p <- add_trace(p, x=dataset$x, y=dataset$y, type = 'scatter', mode = 'scatter', name="",text = paste(dataset$mt,"Stage:",dataset$c," Magnitude:",dataset$m), hoverinfo = 'text', marker = list( line = list(color = dataset$d, width = 1 ), size = 3, color = dataset$d,opacity = 0.5))
p <- add_trace(p, x=dataset$perf, y=dataset$TVDSS, type = 'scatter', mode = 'scatter', name="",text = paste("Stage:",dataset$c," Mid Cluster"), hoverinfo = 'text', marker = list( line = list(color = "#444", width = 1 ), size = 6, color = "black",opacity = 0.1))


p <- layout(p, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, autosize=layout$autosize, template=layout$template)
