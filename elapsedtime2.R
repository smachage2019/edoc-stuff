
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

dataset <- Values
dataset <- import("elapse.xlsx")
dataset <- import("merged_file.xlsx")

dataset <- read_csv("Values-use.csv", col_types = cols(Stage = col_number(), 
                                                     date_time = col_datetime(format = "%m/%d/%Y %H:%M:%S")))
dataset <- read_csv("elapsed.csv", col_types = cols(Stage = col_number(), 
                                                   date_time = col_datetime(format = "%m/%d/%Y %H:%M:%S")))



print(unique(dataset$Stage))
df <- dataset
head(df)


df <- df %>% arrange(df$`Event File`)
well_list <- unique(df$`Well`)
print(well_list)
wellnm <- well_list[3]
print(wellnm)

cut <- df %>% filter(df$`Well` == wellnm)
print(unique(cut$Stage))

stg_list <-unique(cut$Stage)
stg_list <- stg_list[!is.na(stg_list)]

stg <- stg_list[1]
#cut <- cut %>% filter(cut$Stage == stg)
i = 1

etime <- function(i){
cut <- df %>% filter(df$`Well` == wellnm)
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

#x <- x[,c(1,3,6,9)]
print(paste("Well - ",wellnm,"Stage - ",i))
write.csv(x, file = paste(wellnm,"-",i,"Elapsed_Out.csv"))

}

for(i in stg_list) {
  etime(i)
}

### Combine all files
file.list <- list.files(pattern="*.csv", recursive = FALSE)
edf.list <- sapply(file.list, read.csv, simplify=FALSE)
library(purrr)
file.list2 <- list.files(pattern='*.csv')
file.list2 <- setNames(file.list, file.list) 
library(data.table)
edf <- rbindlist(edf.list, idcol = "id", fill=TRUE)
edf <- map_df(file.list2, read.csv, .id = "id")
write.csv(edf, file = "All_Treat_Elapsed.csv")

### Read Combined file in 
elapsed <- read_csv("All_Treat_Elapsed.csv", col_types = cols(Stage = col_number(), 
                                                    date_time = col_datetime(format = "%m/%d/%Y %H:%M:%S")))

### Find Same Events and write to column
elapsed$Same <- cut(elapsed$time.elap, c(-Inf,0,0.01,Inf), c("1", "0", "0"))
write.csv(elapsed, file = "All_Treat_Elapsed-Same.csv")
