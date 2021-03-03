
packages = c("mclust","caret","mlbench","rio", "randomForest", "dplyr","e1071", "data.table", "tcltk","ggplot2","readxl","purrr",
             "data.table","patchwork","plotly","htmlwidgets","readr","stringr","openxlsx",'useful',"kdensity")

package.check <- lapply(packages, FUN = function(TVD) {
  if (!require(TVD, character.only = TRUE)) {
    install.packages(TVD, dependencies = TRUE)
    library(TVD, character.only = TRUE)
  }
})

WD <- tk_choose.dir(getwd(), "Choose a suitable folder")
setwd(WD)


treatetime <- function(i){
  cut <- treat
  cut <- cut %>% filter(cut$Stage == i)
  x = cut %>% 
    group_by(Well,Stage) %>% 
    arrange(`date_time`) %>% 
    mutate(time.elap=ifelse(`date_time`==0 | is.na(lag(`date_time`)) | lag(`date_time`)==0, 
                            0,
                            difftime(`date_time`, lag(`date_time`), units="min"))) %>% 
    mutate(cumtime.elap=time.elap + ifelse(is.na(lag(time.elap)), 0, lag(time.elap)))
  
  x <- x %>%
    group_by(Well,Stage) %>%
    mutate("Elapsed_Time" = cumsum(abs(time.elap)))
  
  
  print(paste("Well - ",wellnm,"Stage - ",i))
  write.csv(x, file = paste(wellnm,"-",i,"treat-Elapsed_Out.csv"))
  
}

treat.list <- list.files(pattern="*.csv", recursive = FALSE)

dataset <- read_csv(treat.list[3], col_types = cols(Stage = col_number(), 
                                                   date_time = col_datetime(format = "%m/%d/%Y %H:%M:%S")))
treat <- dataset

well_list <- unique(dataset$`Well`)
print(well_list)
wellnm <- well_list[1]

stg_list <-unique(treat$Stage)
stg_list <- stg_list[!is.na(stg_list)]
stg_list <- sort(stg_list)

for(i in stg_list) {
  treatetime(i)
}


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
write.csv(edf, file = "All_Treat_Elapsed.csv")

