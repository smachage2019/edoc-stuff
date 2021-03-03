
packages = c("mclust","caret","mlbench","rio", "randomForest", "dplyr","e1071", "data.table", "tcltk","ggplot2","readxl","purrr",
             "data.table","patchwork","plotly","htmlwidgets","readr","stringr","openxlsx",'useful',"kdensity")

package.check <- lapply(packages, FUN = function(TVD) {
  if (!require(TVD, character.only = TRUE)) {
    install.packages(TVD, dependencies = TRUE)
    library(TVD, character.only = TRUE)
  }
})

#set working directory 

  WD <- tk_choose.dir(getwd(), "Choose a suitable folder")
  setwd(WD)


cluster_test <- import("missing.xlsx")
cluster_test <- import("PowerBi.xlsx")
cluster_test <- import("Values.csv")
cluster_test <- read_csv("Values2.csv", col_types = cols(Stage = col_number(), 
                                                        date_time = col_datetime(format = "%m/%d/%Y %H:%M:%S")))

fpoint <- 1
lpoint <- which.max(is.na(cluster_test$Stage))
if( lpoint  == 1) {
  lpoint = length(cluster_test$Stage)+1
}
cluster_test <- cluster_test[(1:(lpoint-1)),]

cluster_test <- cluster_test %>%
  arrange(`Event File`) %>%
  filter(duplicated(`Event File`) == FALSE)


sol_list <- unique(cluster_test$Solution)
solution <- sol_list[1]

cluster_test <- cluster_test %>% filter(cluster_test$Solution == solution)


well_list <- unique(cluster_test$`Well`)
# wellnm <- well_list[1:3]
wellnm <- well_list[3]
dataset <- cluster_test
dataset <- cluster_test %>% filter(cluster_test$`Well` == wellnm)
stg_list <-unique(dataset$Stage)
stg_list <- stg_list[!is.na(stg_list)]
print(stg_list)
j <- wellnm
#J = "EN-RULAND-LE-156-94-3328H-1"
#stg_list <- stg_list[2:22]
k <- 1
i <- stg_list[k]
#i <-3

newcolnames <- c("date_time","Easting (ft)","Northing (ft)", "Depth (ft)","Magnitude","mc.classification", "Event File")
###########Clustering####################################
# Well run
clust <- function(j){
  #clust <- function(i){
  dataset <- cluster_test %>% filter(cluster_test$Well == j)
  
  data <- data.frame(dataset$date_time, dataset$`Easting ft (Abs)`, dataset$`Northing ft (Abs)`, dataset$`Depth ft (TVD)`, dataset$Magnitude)
  head(data, 3)
  data[, 1:5] <- sapply(data[,1:5], as.numeric)
  fpoint <- 1
  lpoint <- which.max(is.na(data$dataset..Easting.ft..Abs..))
  if( lpoint  == 1) {
    lpoint = length(data$dataset..Easting.ft..Abs..)+1
  }
  data <- data[(1:(lpoint-1)),]
 
  df <- scale(data[, -1]) # Standardize the data
  head(data, 3)
  df <- df[!is.na(df)]
  mc <- Mclust(df)            # Model-based-clustering
  class <- mc$classification
  write.csv(mc$classification, file = "MC_cluster.csv")
  Cluster_Out <- data.frame(data, mc$classification)
  Cluster_Out <- data.frame(Cluster_Out, dataset$`Event File`)
  Cluster_Out <- arrange(Cluster_Out, dataset..Event.File.)
  Cluster_Out <- Cluster_Out[!duplicated(Cluster_Out$dataset..Event.File.),]
  colnames(Cluster_Out) <- newcolnames
  write.csv(Cluster_Out, file = paste(j,"-","Well_Cluster_Out.csv"))
}

# Stage run
clust <- function(i){
  #clust <- function(i){
  dataset <- cluster_test %>% filter(cluster_test$Well == j)
  
  dataset <- dataset %>% filter(dataset$Stage == i)
  
  data <- data.frame(dataset$date_time, dataset$`Easting ft (Abs)`, dataset$`Northing ft (Abs)`, dataset$`Depth ft (TVD)`, dataset$Magnitude)
  head(data, 3)
  data[, 1:5] <- sapply(data[,1:5], as.numeric)
  # Model-based clustering can be computed using the function Mclust() as follow:
  df <- scale(data[, -1]) # Standardize the data
  head(data, 3)
  mc <- Mclust(df)            # Model-based-clustering
  
  write.csv(mc$classification, file = "MC_cluster.csv")
  Cluster_Out <- data.frame(data, mc$classification)
  Cluster_Out <- data.frame(Cluster_Out, dataset$`Event File`)
  write.csv(Cluster_Out, file = paste(j,"-",i,"Stg_Cluster_Out.csv"))
  dataset <- cluster_test
  print(wellnm)
  k = k+1
  i <<- stg_list[k]
  print(i)
}


for(i in stg_list) {
  clust(i)
}



for(j in well_list) {
  clust(j)
}

file.list <- list.files(pattern="*.csv", recursive = FALSE)
df.list <- sapply(file.list, read.csv, simplify=FALSE)
library(purrr)
file.list2 <- list.files(pattern='*.csv')
file.list2 <- setNames(file.list, file.list) 
library(data.table)
df <- rbindlist(df.list, idcol = "id", fill=TRUE)
df <- map_df(file.list2, read.csv, .id = "id")
write.csv(df, file = "Clust.csv")

cluster_test <- read_csv("Values.csv")
cluster_test <- read_csv("Clust.csv")
#########load
##################################
WD <- tk_choose.dir(getwd(), "Choose a suitable folder")
setwd(WD)

file.list <- list.files(pattern="*.csv", recursive = FALSE)
df.list <- sapply(file.list, read.csv, simplify=FALSE)
library(purrr)
file.list2 <- list.files(pattern='*.csv')
file.list2 <- setNames(file.list, file.list) 
library(data.table)
df <- rbindlist(df.list, idcol = "id", fill=TRUE)
df <- map_df(file.list2, read.csv, .id = "id")
write.csv(df, file = "Clust.csv")
#########SVM & Regression##################################
#cluster_test <- read_csv("Values.csv")
stg_clust <- read_csv("Clust.csv")
stg_clust <- stg_clust[,c(10,11)]
colnames(stg_clust)[1] <- "StgClust"
colnames(stg_clust)[2] <- "Event File"

cluster_test <- import("missing.xlsx")
cluster_test <- import("Values3.xlsx")

df2 <- cluster_test %>% right_join(stg_clust, by=c("Event File"))

write.xlsx(df2, 'missing2.xlsx')
write.xlsx(df2, 'Values2.xlsx')

well_clust <- read_csv("Clust.csv")
well_clust <- well_clust[,c(9,10)]
colnames(well_clust)[1] <- "WellClust"
colnames(well_clust)[2] <- "Event File"


cluster_test <- import("Values2.xlsx")

df3 <- cluster_test %>% right_join(well_clust, by=c("Event File"))

write.xlsx(df3, 'Values3.xlsx')


cluster_test <- import('Values4.xlsx')


dataset <- cluster_test
well_list <- unique(dataset$Well)
stg_list <-unique(dataset$Stage)
stg_list <- sort(stg_list)
#stg_list <- stg_list[2:9]
# wellnm <- well_list[1:2]
wellnm <- well_list[3]
# initialize i
i <- 1

stg <- stg_list[i]

#stg_list <-unique(dataset$Stage)
#### RunThis ####
dataset <- cluster_test
i <- i+1
stg <- stg_list[i]
dataset <- cluster_test %>% filter(cluster_test$Well == wellnm)
dataset <- dataset %>% filter(dataset$Stage == stg)


clust_list <- unique(dataset$StgClust)
clust_list <- sort(clust_list)
#clust_list <- c(1,2,3,5,6)
#clust_list <- c(6)
#clust_list <- clust_list[1:8]

# run this
counts <- table(dataset$StgClust)
barplot(counts, main="Cluster Count", horiz=TRUE,col=c("red", "gray", "blue", "green", "yellow","cyan", "black", "magenta", "brown"))
abline(v=10, lwd=2, lty=3)
maxcnt <- max(dataset$StgClust)
text(9.9, maxcnt+.46, "Cluster   Cutoff")
print(paste("Next Stage To Run ",stg))
counts <- data.frame(counts)


# y run
#for(j in clust_list) {
  
  dataset <- cluster_test %>% filter(cluster_test$Well == wellnm)
  dataset <- dataset %>% filter(dataset$Stage == stg)
  dataset <- dataset %>% filter(dataset$StgClust == j)
  
  #stm <- dataset[,c(2:3,7:8)]
  stm <- dataset
  #stgm <- as.matrix(stm)
  p <- cbind(stm$`Easting ft (Abs)`, stm$`Northing ft (Abs)`)
  dens <- kde(p)
  dens <- kde(p, eval.points=p)
  stm$Density <- dens$estimate
  m <- mean(stm$Density)
  e <- subset(stm, subset=(Density < m/6))
  stgm <- stm %>% anti_join(e)
  #stgm <- data.frame(stgm)
  #newcolnames <- c("Well","Stage","Easting ft (Abs)","Northing ft (Abs)","Density")
  #colnames(stgm) <- newcolnames
  base <- ggplot(data=dataset, aes(x=`Easting ft (Abs)`,y=`Northing ft (Abs)`))+geom_point()
  out <- base + geom_point(data=stgm, aes(x=`Easting ft (Abs)`,y=`Northing ft (Abs)`),color = "red")
  ggsave(paste(wellnm," Stage ",stg," Cluster",j,".png"), width = 15, height = 8)
  
  X=stgm$`Easting ft (Abs)`
  Y=stgm$`Northing ft (Abs)`
  
  print("Running Cluster")
  print(paste(wellnm," Stage ",stg," Cluster",j,sep=""))
  
  #Create data frame
  train=data.frame(X,Y)
  
  #Plot the dataframe
  plot(train,pch=16)
  
  #Linear regression
  model <- lm(Y ~ X, train)
  #model <- lm(X ~ Y, train)
  
  # Plot Linear regression
  abline(model)
  
  
  # Fit a model. 
  model_svm <- svm(Y ~ X , train)
  
  # Predict on the data
  pred <- predict(model_svm, train)
  
  # Plot the predictions to see our model fit
  points(train$X, pred, col = "blue", pch=4)
  
  # eTVDtract residual to calculate rmse
  error <- model$residuals 
  lm_error <- sqrt(mean(error^2)) 
  
  # error calc = actual values (train$y) with our predictions (pred)
  error_2 <- train$Y - pred
  svm_error <- sqrt(mean(error_2^2)) 
  
  
  # perform a grid search
  svm_tune <- tune(svm, Y ~ X, data = train,
                   #ranges = list(epsilon = seq(0.68,0.84,0.005), cost = 2^(2:9))
                   #ranges = list(epsilon = seq(0,.05,0.01), cost = 2^(2:9))
                   ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9))
  )
  print(svm_tune)
  
  # Find the best model
  best_mod <- svm_tune$best.model
  best_mod_pred <- predict(best_mod, train) 
  
  error_best_mod <- train$Y - best_mod_pred 
  
  
  best_mod_RMSE <- sqrt(mean(error_best_mod^2)) # 1.290738 
  
  #plot(svm_tune)
  
  plot(train,main=paste(wellnm," ",stg," C",j,sep=""), xlab= "Easting (ft)", ylab="Northing (ft)",pch=16)
  points(train$X, pred, col = "blue", pch=4)
  points(train$X, best_mod_pred, col = "red", pch=23)
  
  
  out <- data.frame(stgm$Well,stgm$Stage,best_mod_pred,stgm$`Easting ft (Abs)`,stgm$`Northing ft (Abs)`,stgm$`Depth ft (TVDSS)`,stgm$Magnitude,stgm$`P/S Ratio` ,stgm$date_time,stgm$`Event File`,stgm$`Pressure (psi)` ,stgm$`Flow (bpm)`,stgm$`Conc (ppg)`,pred,stgm$StgClust)
  newcolnames <- c("Well","Stage","best_mod_pred","Easting ft","Northing ft","Depth ft TVD","Magnitude","P/S Ratio.", "Date-Time", "Event","Pressure","Rate","Prop","pred","Cluster")
  colnames(out) <- newcolnames
  
  
  write.csv(out, paste(wellnm,"-",stg,"-",j,"y_out.csv"))
  print("Finished Run")
}

#########run me############
run.clust()
############################
j = 6
justone()

justone <- function(){
  dataset <- cluster_test %>% filter(cluster_test$Well == wellnm)
  dataset <- dataset %>% filter(dataset$Stage == stg)
  dataset <- dataset %>% filter(dataset$StgClust == j)
  X=dataset$`Easting ft (Abs)`
  Y=dataset$`Northing ft (Abs)`
  
  print("Running Cluster")
  print(paste(wellnm," ",stg," C",j,sep=""))
  
  #Create data frame
  train=data.frame(X,Y)
  
  #Plot the dataframe
  plot(train,pch=16)
  
  #Linear regression
  model <- lm(Y ~ X, train)
  #model <- lm(X ~ Y, train)
  
  # Plot Linear regression
  abline(model)
  
  
  # Fit a model. 
  model_svm <- svm(Y ~ X , train)
  
  # Predict on the data
  pred <- predict(model_svm, train)
  
  # Plot the predictions to see our model fit
  points(train$X, pred, col = "blue", pch=4)
  
  # eTVDtract residual to calculate rmse
  error <- model$residuals 
  lm_error <- sqrt(mean(error^2)) 
  
  # error calc = actual values (train$y) with our predictions (pred)
  error_2 <- train$Y - pred
  svm_error <- sqrt(mean(error_2^2)) 
  
  
  # perform a grid search
  svm_tune <- tune(svm, Y ~ X, data = train,
                   #ranges = list(epsilon = seq(0.68,0.84,0.005), cost = 2^(2:9))
                   #ranges = list(epsilon = seq(0,.05,0.01), cost = 2^(2:9))
                   ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9))
  )
  print(svm_tune)
  
  # Find the best model
  best_mod <- svm_tune$best.model
  best_mod_pred <- predict(best_mod, train) 
  
  error_best_mod <- train$Y - best_mod_pred 
  
  
  best_mod_RMSE <- sqrt(mean(error_best_mod^2)) # 1.290738 
  
  #plot(svm_tune)
  
  plot(train,main=paste(wellnm," ",stg," C",j,sep=""), xlab= "Easting (ft)", ylab="Northing (ft)",pch=16)
  points(train$X, pred, col = "blue", pch=4)
  points(train$X, best_mod_pred, col = "red", pch=23)
  
  
  out <- data.frame(dataset$Well,dataset$Stage,best_mod_pred,dataset$`Easting ft (Abs)`,dataset$`Northing ft (Abs)`,dataset$`Depth ft (TVDSS)`,dataset$Magnitude,dataset$`P/S Ratio` ,dataset$date_time,dataset$`Event File`,dataset$`Pressure (psi)` ,dataset$`Flow (bpm)`,dataset$`Conc (ppg)`,pred,dataset$StgClust)
  newcolnames <- c("Well","Stage","best_mod_pred","Easting ft","Northing ft","Depth ft TVD","Magnitude","P/S Ratio.", "Date-Time", "Event","Pressure","Rate","Prop","pred","Cluster")
  colnames(out) <- newcolnames
  
  
  write.csv(out, paste(wellnm,"-",stg,"-",j,"y_out.csv"))
  print("Finished Run")
}


run.clust <- function(){
  

if ((counts[1,2] >= 10)){
  print("yes")
  if ("1" %in% clust_list){print("cluster found")  
    j <- clust_list[1]
    dataset <- cluster_test %>% filter(cluster_test$Well == wellnm)
    dataset <- dataset %>% filter(dataset$Stage == stg)
    dataset <- dataset %>% filter(dataset$StgClust == j)
    X=dataset$`Easting ft (Abs)`
    Y=dataset$`Northing ft (Abs)`
    
    print("Running Cluster")
    print(paste(wellnm," ",stg," C",j,sep=""))
    
    #Create data frame
    train=data.frame(X,Y)
    
    #Plot the dataframe
    plot(train,pch=16)
    
    #Linear regression
    model <- lm(Y ~ X, train)
    #model <- lm(X ~ Y, train)
    
    # Plot Linear regression
    abline(model)
    
    
    # Fit a model. 
    model_svm <- svm(Y ~ X , train)
    
    # Predict on the data
    pred <- predict(model_svm, train)
    
    # Plot the predictions to see our model fit
    points(train$X, pred, col = "blue", pch=4)
    
    # eTVDtract residual to calculate rmse
    error <- model$residuals 
    lm_error <- sqrt(mean(error^2)) 
    
    # error calc = actual values (train$y) with our predictions (pred)
    error_2 <- train$Y - pred
    svm_error <- sqrt(mean(error_2^2)) 
    
    
    # perform a grid search
    svm_tune <- tune(svm, Y ~ X, data = train,
                     #ranges = list(epsilon = seq(0.68,0.84,0.005), cost = 2^(2:9))
                     #ranges = list(epsilon = seq(0,.05,0.01), cost = 2^(2:9))
                     ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9))
    )
    print(svm_tune)
    
    # Find the best model
    best_mod <- svm_tune$best.model
    best_mod_pred <- predict(best_mod, train) 
    
    error_best_mod <- train$Y - best_mod_pred 
    
    
    best_mod_RMSE <- sqrt(mean(error_best_mod^2)) # 1.290738 
    
    plot(svm_tune)
    
    plot(train,main=paste(wellnm," ",stg," C",j,sep=""), xlab= "Easting (ft)", ylab="Northing (ft)",pch=16)
    points(train$X, pred, col = "blue", pch=4)
    points(train$X, best_mod_pred, col = "red", pch=23)
    
    
    out <- data.frame(dataset$Well,dataset$Stage,best_mod_pred,dataset$`Easting ft (Abs)`,dataset$`Northing ft (Abs)`,dataset$`Depth ft (TVDSS)`,dataset$Magnitude,dataset$`P/S Ratio` ,dataset$date_time,dataset$`Event File`,dataset$`Pressure (psi)` ,dataset$`Flow (bpm)`,dataset$`Conc (ppg)`,pred,dataset$StgClust)
    newcolnames <- c("Well","Stage","best_mod_pred","Easting ft","Northing ft","Depth ft TVD","Magnitude","P/S Ratio.", "Date-Time", "Event","Pressure","Rate","Prop","pred","Cluster")
    colnames(out) <- newcolnames
    
    
    write.csv(out, paste(wellnm,"-",stg,"-",j,"y_out.csv"))
    print("Finished Run")
  
  }
  
  } else {
    print("no")
  }
  
if ((counts[2,2] >= 10)){
    print("yes")
    if ("2" %in% clust_list){print("cluster found")  
      j <- clust_list[2]
      dataset <- cluster_test %>% filter(cluster_test$Well == wellnm)
      dataset <- dataset %>% filter(dataset$Stage == stg)
      dataset <- dataset %>% filter(dataset$StgClust == j)
      X=dataset$`Easting ft (Abs)`
      Y=dataset$`Northing ft (Abs)`
      
      print("Running Cluster")
      print(paste(wellnm," ",stg," C",j,sep=""))
      
      #Create data frame
      train=data.frame(X,Y)
      
      #Plot the dataframe
      plot(train,pch=16)
      
      #Linear regression
      model <- lm(Y ~ X, train)
      #model <- lm(X ~ Y, train)
      
      # Plot Linear regression
      abline(model)
      
      
      # Fit a model. 
      model_svm <- svm(Y ~ X , train)
      
      # Predict on the data
      pred <- predict(model_svm, train)
      
      # Plot the predictions to see our model fit
      points(train$X, pred, col = "blue", pch=4)
      
      # eTVDtract residual to calculate rmse
      error <- model$residuals 
      lm_error <- sqrt(mean(error^2)) 
      
      # error calc = actual values (train$y) with our predictions (pred)
      error_2 <- train$Y - pred
      svm_error <- sqrt(mean(error_2^2)) 
      
      
      # perform a grid search
      svm_tune <- tune(svm, Y ~ X, data = train,
                       #ranges = list(epsilon = seq(0.68,0.84,0.005), cost = 2^(2:9))
                       #ranges = list(epsilon = seq(0,.05,0.01), cost = 2^(2:9))
                       ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9))
      )
      print(svm_tune)
      
      # Find the best model
      best_mod <- svm_tune$best.model
      best_mod_pred <- predict(best_mod, train) 
      
      error_best_mod <- train$Y - best_mod_pred 
      
      
      best_mod_RMSE <- sqrt(mean(error_best_mod^2)) # 1.290738 
      
      plot(svm_tune)
      
      plot(train,main=paste(wellnm," ",stg," C",j,sep=""), xlab= "Easting (ft)", ylab="Northing (ft)",pch=16)
      points(train$X, pred, col = "blue", pch=4)
      points(train$X, best_mod_pred, col = "red", pch=23)
      
      
      out <- data.frame(dataset$Well,dataset$Stage,best_mod_pred,dataset$`Easting ft (Abs)`,dataset$`Northing ft (Abs)`,dataset$`Depth ft (TVDSS)`,dataset$Magnitude,dataset$`P/S Ratio` ,dataset$date_time,dataset$`Event File`,dataset$`Pressure (psi)` ,dataset$`Flow (bpm)`,dataset$`Conc (ppg)`,pred,dataset$StgClust)
      newcolnames <- c("Well","Stage","best_mod_pred","Easting ft","Northing ft","Depth ft TVD","Magnitude","P/S Ratio.", "Date-Time", "Event","Pressure","Rate","Prop","pred","Cluster")
      colnames(out) <- newcolnames
      
      
      write.csv(out, paste(wellnm,"-",stg,"-",j,"y_out.csv"))
      print("Finished Run")
    }
    
  } else {
    print("no")
  }

if ((counts[3,2] >= 10)){
    print("yes")
    if ("3" %in% clust_list){print("cluster found")  
      j <- clust_list[3]
      dataset <- cluster_test %>% filter(cluster_test$Well == wellnm)
      dataset <- dataset %>% filter(dataset$Stage == stg)
      dataset <- dataset %>% filter(dataset$StgClust == j)
      X=dataset$`Easting ft (Abs)`
      Y=dataset$`Northing ft (Abs)`
      
      print("Running Cluster")
      print(paste(wellnm," ",stg," C",j,sep=""))
      
      #Create data frame
      train=data.frame(X,Y)
      
      #Plot the dataframe
      plot(train,pch=16)
      
      #Linear regression
      model <- lm(Y ~ X, train)
      #model <- lm(X ~ Y, train)
      
      # Plot Linear regression
      abline(model)
      
      
      # Fit a model. 
      model_svm <- svm(Y ~ X , train)
      
      # Predict on the data
      pred <- predict(model_svm, train)
      
      # Plot the predictions to see our model fit
      points(train$X, pred, col = "blue", pch=4)
      
      # eTVDtract residual to calculate rmse
      error <- model$residuals 
      lm_error <- sqrt(mean(error^2)) 
      
      # error calc = actual values (train$y) with our predictions (pred)
      error_2 <- train$Y - pred
      svm_error <- sqrt(mean(error_2^2)) 
      
      
      # perform a grid search
      svm_tune <- tune(svm, Y ~ X, data = train,
                       #ranges = list(epsilon = seq(0.68,0.84,0.005), cost = 2^(2:9))
                       #ranges = list(epsilon = seq(0,.05,0.01), cost = 2^(2:9))
                       ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9))
      )
      print(svm_tune)
      
      # Find the best model
      best_mod <- svm_tune$best.model
      best_mod_pred <- predict(best_mod, train) 
      
      error_best_mod <- train$Y - best_mod_pred 
      
      
      best_mod_RMSE <- sqrt(mean(error_best_mod^2)) # 1.290738 
      
      plot(svm_tune)
      
      plot(train,main=paste(wellnm," ",stg," C",j,sep=""), xlab= "Easting (ft)", ylab="Northing (ft)",pch=16)
      points(train$X, pred, col = "blue", pch=4)
      points(train$X, best_mod_pred, col = "red", pch=23)
      
      
      out <- data.frame(dataset$Well,dataset$Stage,best_mod_pred,dataset$`Easting ft (Abs)`,dataset$`Northing ft (Abs)`,dataset$`Depth ft (TVDSS)`,dataset$Magnitude,dataset$`P/S Ratio` ,dataset$date_time,dataset$`Event File`,dataset$`Pressure (psi)` ,dataset$`Flow (bpm)`,dataset$`Conc (ppg)`,pred,dataset$StgClust)
      newcolnames <- c("Well","Stage","best_mod_pred","Easting ft","Northing ft","Depth ft TVD","Magnitude","P/S Ratio.", "Date-Time", "Event","Pressure","Rate","Prop","pred","Cluster")
      colnames(out) <- newcolnames
      
      
      write.csv(out, paste(wellnm,"-",stg,"-",j,"y_out.csv"))
      print("Finished Run")
    }
    
  } else {
    print("no")
  } 
  
if ((counts[4,2] >= 10)){
    print("yes")
    if ("4" %in% clust_list){print("cluster found")  
      j <- clust_list[4]
      dataset <- cluster_test %>% filter(cluster_test$Well == wellnm)
      dataset <- dataset %>% filter(dataset$Stage == stg)
      dataset <- dataset %>% filter(dataset$StgClust == j)
      X=dataset$`Easting ft (Abs)`
      Y=dataset$`Northing ft (Abs)`
      
      print("Running Cluster")
      print(paste(wellnm," ",stg," C",j,sep=""))
      
      #Create data frame
      train=data.frame(X,Y)
      
      #Plot the dataframe
      plot(train,pch=16)
      
      #Linear regression
      model <- lm(Y ~ X, train)
      #model <- lm(X ~ Y, train)
      
      # Plot Linear regression
      abline(model)
      
      
      # Fit a model. 
      model_svm <- svm(Y ~ X , train)
      
      # Predict on the data
      pred <- predict(model_svm, train)
      
      # Plot the predictions to see our model fit
      points(train$X, pred, col = "blue", pch=4)
      
      # eTVDtract residual to calculate rmse
      error <- model$residuals 
      lm_error <- sqrt(mean(error^2)) 
      
      # error calc = actual values (train$y) with our predictions (pred)
      error_2 <- train$Y - pred
      svm_error <- sqrt(mean(error_2^2)) 
      
      
      # perform a grid search
      svm_tune <- tune(svm, Y ~ X, data = train,
                       #ranges = list(epsilon = seq(0.68,0.84,0.005), cost = 2^(2:9))
                       #ranges = list(epsilon = seq(0,.05,0.01), cost = 2^(2:9))
                       ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9))
      )
      print(svm_tune)
      
      # Find the best model
      best_mod <- svm_tune$best.model
      best_mod_pred <- predict(best_mod, train) 
      
      error_best_mod <- train$Y - best_mod_pred 
      
      
      best_mod_RMSE <- sqrt(mean(error_best_mod^2)) # 1.290738 
      
      plot(svm_tune)
      
      plot(train,main=paste(wellnm," ",stg," C",j,sep=""), xlab= "Easting (ft)", ylab="Northing (ft)",pch=16)
      points(train$X, pred, col = "blue", pch=4)
      points(train$X, best_mod_pred, col = "red", pch=23)
      
      
      out <- data.frame(dataset$Well,dataset$Stage,best_mod_pred,dataset$`Easting ft (Abs)`,dataset$`Northing ft (Abs)`,dataset$`Depth ft (TVDSS)`,dataset$Magnitude,dataset$`P/S Ratio` ,dataset$date_time,dataset$`Event File`,dataset$`Pressure (psi)` ,dataset$`Flow (bpm)`,dataset$`Conc (ppg)`,pred,dataset$StgClust)
      newcolnames <- c("Well","Stage","best_mod_pred","Easting ft","Northing ft","Depth ft TVD","Magnitude","P/S Ratio.", "Date-Time", "Event","Pressure","Rate","Prop","pred","Cluster")
      colnames(out) <- newcolnames
      
      
      write.csv(out, paste(wellnm,"-",stg,"-",j,"y_out.csv"))
      print("Finished Run")
    }
    
  } else {
    print("no")
} 

if ((counts[5,2] >= 10)){
    print("yes")
    if ("5" %in% clust_list){print("cluster found")  
      j <- clust_list[5]
      dataset <- cluster_test %>% filter(cluster_test$Well == wellnm)
      dataset <- dataset %>% filter(dataset$Stage == stg)
      dataset <- dataset %>% filter(dataset$StgClust == j)
      X=dataset$`Easting ft (Abs)`
      Y=dataset$`Northing ft (Abs)`
      
      print("Running Cluster")
      print(paste(wellnm," ",stg," C",j,sep=""))
      
      #Create data frame
      train=data.frame(X,Y)
      
      #Plot the dataframe
      plot(train,pch=16)
      
      #Linear regression
      model <- lm(Y ~ X, train)
      #model <- lm(X ~ Y, train)
      
      # Plot Linear regression
      abline(model)
      
      
      # Fit a model. 
      model_svm <- svm(Y ~ X , train)
      
      # Predict on the data
      pred <- predict(model_svm, train)
      
      # Plot the predictions to see our model fit
      points(train$X, pred, col = "blue", pch=4)
      
      # eTVDtract residual to calculate rmse
      error <- model$residuals 
      lm_error <- sqrt(mean(error^2)) 
      
      # error calc = actual values (train$y) with our predictions (pred)
      error_2 <- train$Y - pred
      svm_error <- sqrt(mean(error_2^2)) 
      
      
      # perform a grid search
      svm_tune <- tune(svm, Y ~ X, data = train,
                       #ranges = list(epsilon = seq(0.68,0.84,0.005), cost = 2^(2:9))
                       #ranges = list(epsilon = seq(0,.05,0.01), cost = 2^(2:9))
                       ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9))
      )
      print(svm_tune)
      
      # Find the best model
      best_mod <- svm_tune$best.model
      best_mod_pred <- predict(best_mod, train) 
      
      error_best_mod <- train$Y - best_mod_pred 
      
      
      best_mod_RMSE <- sqrt(mean(error_best_mod^2)) # 1.290738 
      
      plot(svm_tune)
      
      plot(train,main=paste(wellnm," ",stg," C",j,sep=""), xlab= "Easting (ft)", ylab="Northing (ft)",pch=16)
      points(train$X, pred, col = "blue", pch=4)
      points(train$X, best_mod_pred, col = "red", pch=23)
      
      
      out <- data.frame(dataset$Well,dataset$Stage,best_mod_pred,dataset$`Easting ft (Abs)`,dataset$`Northing ft (Abs)`,dataset$`Depth ft (TVDSS)`,dataset$Magnitude,dataset$`P/S Ratio` ,dataset$date_time,dataset$`Event File`,dataset$`Pressure (psi)` ,dataset$`Flow (bpm)`,dataset$`Conc (ppg)`,pred,dataset$StgClust)
      newcolnames <- c("Well","Stage","best_mod_pred","Easting ft","Northing ft","Depth ft TVD","Magnitude","P/S Ratio.", "Date-Time", "Event","Pressure","Rate","Prop","pred","Cluster")
      colnames(out) <- newcolnames
      
      
      write.csv(out, paste(wellnm,"-",stg,"-",j,"y_out.csv"))
      print("Finished Run")
    }
    
  } else {
    print("no")
} 
  
if ((counts[6,2] >= 10)){
    print("yes")
    if ("6" %in% clust_list){print("cluster found")  
      j <- clust_list[6]
      dataset <- cluster_test %>% filter(cluster_test$Well == wellnm)
      dataset <- dataset %>% filter(dataset$Stage == stg)
      dataset <- dataset %>% filter(dataset$StgClust == j)
      X=dataset$`Easting ft (Abs)`
      Y=dataset$`Northing ft (Abs)`
      
      print("Running Cluster")
      print(paste(wellnm," ",stg," C",j,sep=""))
      
      #Create data frame
      train=data.frame(X,Y)
      
      #Plot the dataframe
      plot(train,pch=16)
      
      #Linear regression
      model <- lm(Y ~ X, train)
      #model <- lm(X ~ Y, train)
      
      # Plot Linear regression
      abline(model)
      
      
      # Fit a model. 
      model_svm <- svm(Y ~ X , train)
      
      # Predict on the data
      pred <- predict(model_svm, train)
      
      # Plot the predictions to see our model fit
      points(train$X, pred, col = "blue", pch=4)
      
      # eTVDtract residual to calculate rmse
      error <- model$residuals 
      lm_error <- sqrt(mean(error^2)) 
      
      # error calc = actual values (train$y) with our predictions (pred)
      error_2 <- train$Y - pred
      svm_error <- sqrt(mean(error_2^2)) 
      
      
      # perform a grid search
      svm_tune <- tune(svm, Y ~ X, data = train,
                       #ranges = list(epsilon = seq(0.68,0.84,0.005), cost = 2^(2:9))
                       #ranges = list(epsilon = seq(0,.05,0.01), cost = 2^(2:9))
                       ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9))
      )
      print(svm_tune)
      
      # Find the best model
      best_mod <- svm_tune$best.model
      best_mod_pred <- predict(best_mod, train) 
      
      error_best_mod <- train$Y - best_mod_pred 
      
      
      best_mod_RMSE <- sqrt(mean(error_best_mod^2)) # 1.290738 
      
      plot(svm_tune)
      
      plot(train,main=paste(wellnm," ",stg," C",j,sep=""), xlab= "Easting (ft)", ylab="Northing (ft)",pch=16)
      points(train$X, pred, col = "blue", pch=4)
      points(train$X, best_mod_pred, col = "red", pch=23)
      
      
      out <- data.frame(dataset$Well,dataset$Stage,best_mod_pred,dataset$`Easting ft (Abs)`,dataset$`Northing ft (Abs)`,dataset$`Depth ft (TVDSS)`,dataset$Magnitude,dataset$`P/S Ratio` ,dataset$date_time,dataset$`Event File`,dataset$`Pressure (psi)` ,dataset$`Flow (bpm)`,dataset$`Conc (ppg)`,pred,dataset$StgClust)
      newcolnames <- c("Well","Stage","best_mod_pred","Easting ft","Northing ft","Depth ft TVD","Magnitude","P/S Ratio.", "Date-Time", "Event","Pressure","Rate","Prop","pred","Cluster")
      colnames(out) <- newcolnames
      
      
      write.csv(out, paste(wellnm,"-",stg,"-",j,"y_out.csv"))
      print("Finished Run")
    }
    
  } else {
    print("no")
} 
  
if ((counts[7,2] >= 10)){
    print("yes")
    if ("7" %in% clust_list){print("cluster found")  
      j <- clust_list[7]
      dataset <- cluster_test %>% filter(cluster_test$Well == wellnm)
      dataset <- dataset %>% filter(dataset$Stage == stg)
      dataset <- dataset %>% filter(dataset$StgClust == j)
      X=dataset$`Easting ft (Abs)`
      Y=dataset$`Northing ft (Abs)`
      
      print("Running Cluster")
      print(paste(wellnm," ",stg," C",j,sep=""))
      
      #Create data frame
      train=data.frame(X,Y)
      
      #Plot the dataframe
      plot(train,pch=16)
      
      #Linear regression
      model <- lm(Y ~ X, train)
      #model <- lm(X ~ Y, train)
      
      # Plot Linear regression
      abline(model)
      
      
      # Fit a model. 
      model_svm <- svm(Y ~ X , train)
      
      # Predict on the data
      pred <- predict(model_svm, train)
      
      # Plot the predictions to see our model fit
      points(train$X, pred, col = "blue", pch=4)
      
      # eTVDtract residual to calculate rmse
      error <- model$residuals 
      lm_error <- sqrt(mean(error^2)) 
      
      # error calc = actual values (train$y) with our predictions (pred)
      error_2 <- train$Y - pred
      svm_error <- sqrt(mean(error_2^2)) 
      
      
      # perform a grid search
      svm_tune <- tune(svm, Y ~ X, data = train,
                       #ranges = list(epsilon = seq(0.68,0.84,0.005), cost = 2^(2:9))
                       #ranges = list(epsilon = seq(0,.05,0.01), cost = 2^(2:9))
                       ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9))
      )
      print(svm_tune)
      
      # Find the best model
      best_mod <- svm_tune$best.model
      best_mod_pred <- predict(best_mod, train) 
      
      error_best_mod <- train$Y - best_mod_pred 
      
      
      best_mod_RMSE <- sqrt(mean(error_best_mod^2)) # 1.290738 
      
      plot(svm_tune)
      
      plot(train,main=paste(wellnm," ",stg," C",j,sep=""), xlab= "Easting (ft)", ylab="Northing (ft)",pch=16)
      points(train$X, pred, col = "blue", pch=4)
      points(train$X, best_mod_pred, col = "red", pch=23)
      
      
      out <- data.frame(dataset$Well,dataset$Stage,best_mod_pred,dataset$`Easting ft (Abs)`,dataset$`Northing ft (Abs)`,dataset$`Depth ft (TVDSS)`,dataset$Magnitude,dataset$`P/S Ratio` ,dataset$date_time,dataset$`Event File`,dataset$`Pressure (psi)` ,dataset$`Flow (bpm)`,dataset$`Conc (ppg)`,pred,dataset$StgClust)
      newcolnames <- c("Well","Stage","best_mod_pred","Easting ft","Northing ft","Depth ft TVD","Magnitude","P/S Ratio.", "Date-Time", "Event","Pressure","Rate","Prop","pred","Cluster")
      colnames(out) <- newcolnames
      
      
      write.csv(out, paste(wellnm,"-",stg,"-",j,"y_out.csv"))
      print("Finished Run")
    }
    
  } else {
    print("no")
}  
  
if ((counts[8,2] >= 10)){
    print("yes")
    if ("8" %in% clust_list){print("cluster found")  
      j <- clust_list[8]
      dataset <- cluster_test %>% filter(cluster_test$Well == wellnm)
      dataset <- dataset %>% filter(dataset$Stage == stg)
      dataset <- dataset %>% filter(dataset$StgClust == j)
      X=dataset$`Easting ft (Abs)`
      Y=dataset$`Northing ft (Abs)`
      
      print("Running Cluster")
      print(paste(wellnm," ",stg," C",j,sep=""))
      
      #Create data frame
      train=data.frame(X,Y)
      
      #Plot the dataframe
      plot(train,pch=16)
      
      #Linear regression
      model <- lm(Y ~ X, train)
      #model <- lm(X ~ Y, train)
      
      # Plot Linear regression
      abline(model)
      
      
      # Fit a model. 
      model_svm <- svm(Y ~ X , train)
      
      # Predict on the data
      pred <- predict(model_svm, train)
      
      # Plot the predictions to see our model fit
      points(train$X, pred, col = "blue", pch=4)
      
      # eTVDtract residual to calculate rmse
      error <- model$residuals 
      lm_error <- sqrt(mean(error^2)) 
      
      # error calc = actual values (train$y) with our predictions (pred)
      error_2 <- train$Y - pred
      svm_error <- sqrt(mean(error_2^2)) 
      
      
      # perform a grid search
      svm_tune <- tune(svm, Y ~ X, data = train,
                       #ranges = list(epsilon = seq(0.68,0.84,0.005), cost = 2^(2:9))
                       #ranges = list(epsilon = seq(0,.05,0.01), cost = 2^(2:9))
                       ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9))
      )
      print(svm_tune)
      
      # Find the best model
      best_mod <- svm_tune$best.model
      best_mod_pred <- predict(best_mod, train) 
      
      error_best_mod <- train$Y - best_mod_pred 
      
      
      best_mod_RMSE <- sqrt(mean(error_best_mod^2)) # 1.290738 
      
      plot(svm_tune)
      
      plot(train,main=paste(wellnm," ",stg," C",j,sep=""), xlab= "Easting (ft)", ylab="Northing (ft)",pch=16)
      points(train$X, pred, col = "blue", pch=4)
      points(train$X, best_mod_pred, col = "red", pch=23)
      
      
      out <- data.frame(dataset$Well,dataset$Stage,best_mod_pred,dataset$`Easting ft (Abs)`,dataset$`Northing ft (Abs)`,dataset$`Depth ft (TVDSS)`,dataset$Magnitude,dataset$`P/S Ratio` ,dataset$date_time,dataset$`Event File`,dataset$`Pressure (psi)` ,dataset$`Flow (bpm)`,dataset$`Conc (ppg)`,pred,dataset$StgClust)
      newcolnames <- c("Well","Stage","best_mod_pred","Easting ft","Northing ft","Depth ft TVD","Magnitude","P/S Ratio.", "Date-Time", "Event","Pressure","Rate","Prop","pred","Cluster")
      colnames(out) <- newcolnames
      
      
      write.csv(out, paste(wellnm,"-",stg,"-",j,"y_out.csv"))
      print("Finished Run")
    }
    
  } else {
    print("no")
}  
  
if ((counts[9,2] >= 10)){
    print("yes")
    if ("9" %in% clust_list){print("cluster found")  
      j <- clust_list[9]
      dataset <- cluster_test %>% filter(cluster_test$Well == wellnm)
      dataset <- dataset %>% filter(dataset$Stage == stg)
      dataset <- dataset %>% filter(dataset$StgClust == j)
      X=dataset$`Easting ft (Abs)`
      Y=dataset$`Northing ft (Abs)`
      
      print("Running Cluster")
      print(paste(wellnm," ",stg," C",j,sep=""))
      
      #Create data frame
      train=data.frame(X,Y)
      
      #Plot the dataframe
      plot(train,pch=16)
      
      #Linear regression
      model <- lm(Y ~ X, train)
      #model <- lm(X ~ Y, train)
      
      # Plot Linear regression
      abline(model)
      
      
      # Fit a model. 
      model_svm <- svm(Y ~ X , train)
      
      # Predict on the data
      pred <- predict(model_svm, train)
      
      # Plot the predictions to see our model fit
      points(train$X, pred, col = "blue", pch=4)
      
      # eTVDtract residual to calculate rmse
      error <- model$residuals 
      lm_error <- sqrt(mean(error^2)) 
      
      # error calc = actual values (train$y) with our predictions (pred)
      error_2 <- train$Y - pred
      svm_error <- sqrt(mean(error_2^2)) 
      
      
      # perform a grid search
      svm_tune <- tune(svm, Y ~ X, data = train,
                       #ranges = list(epsilon = seq(0.68,0.84,0.005), cost = 2^(2:9))
                       #ranges = list(epsilon = seq(0,.05,0.01), cost = 2^(2:9))
                       ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9))
      )
      print(svm_tune)
      
      # Find the best model
      best_mod <- svm_tune$best.model
      best_mod_pred <- predict(best_mod, train) 
      
      error_best_mod <- train$Y - best_mod_pred 
      
      
      best_mod_RMSE <- sqrt(mean(error_best_mod^2)) # 1.290738 
      
      plot(svm_tune)
      
      plot(train,main=paste(wellnm," ",stg," C",j,sep=""), xlab= "Easting (ft)", ylab="Northing (ft)",pch=16)
      points(train$X, pred, col = "blue", pch=4)
      points(train$X, best_mod_pred, col = "red", pch=23)
      
      
      out <- data.frame(dataset$Well,dataset$Stage,best_mod_pred,dataset$`Easting ft (Abs)`,dataset$`Northing ft (Abs)`,dataset$`Depth ft (TVDSS)`,dataset$Magnitude,dataset$`P/S Ratio` ,dataset$date_time,dataset$`Event File`,dataset$`Pressure (psi)` ,dataset$`Flow (bpm)`,dataset$`Conc (ppg)`,pred,dataset$StgClust)
      newcolnames <- c("Well","Stage","best_mod_pred","Easting ft","Northing ft","Depth ft TVD","Magnitude","P/S Ratio.", "Date-Time", "Event","Pressure","Rate","Prop","pred","Cluster")
      colnames(out) <- newcolnames
      
      
      write.csv(out, paste(wellnm,"-",stg,"-",j,"y_out.csv"))
      print("Finished Run")
    }
    
  } else {
    print("no")
  }    
  counts <<- table(dataset$StgClust)
  barplot(counts, main="Cluster Count", horiz=TRUE,col=c("red", "gray", "blue", "green", "yellow","cyan", "black", "magenta", "brown"))
  abline(v=10, lwd=2, lty=3)
  maxcnt <<- max(dataset$StgClust)
  text(9.9, maxcnt+.46, "Cluster   Cutoff")
} 
  
  



# y run
for(j in clust_list) {
  
  dataset <- cluster_test %>% filter(cluster_test$Well == wellnm)
  dataset <- dataset %>% filter(dataset$Stage == stg)
  dataset <- dataset %>% filter(dataset$StgClust == j)
  
  #stm <- dataset[,c(2:3,7:8)]
  stm <- dataset
  #stgm <- as.matrix(stm)
  p <- cbind(stm$`Easting ft (Abs)`, stm$`Northing ft (Abs)`)
  dens <- kde(p)
  dens <- kde(p, eval.points=p)
  stm$Density <- dens$estimate
  m <- mean(stm$Density)
  e <- subset(stm, subset=(Density < m/6))
  stgm <- stm %>% anti_join(e)
  #stgm <- data.frame(stgm)
  #newcolnames <- c("Well","Stage","Easting ft (Abs)","Northing ft (Abs)","Density")
  #colnames(stgm) <- newcolnames
  base <- ggplot(data=dataset, aes(x=`Easting ft (Abs)`,y=`Northing ft (Abs)`))+geom_point()
  out <- base + geom_point(data=stgm, aes(x=`Easting ft (Abs)`,y=`Northing ft (Abs)`),color = "red")
  ggsave(paste(wellnm," Stage ",stg," Cluster",j,".png"), width = 15, height = 8)
  
  X=stgm$`Easting ft (Abs)`
  Y=stgm$`Northing ft (Abs)`
  
  print("Running Cluster")
  print(paste(wellnm," Stage ",stg," Cluster",j,sep=""))
  
  #Create data frame
  train=data.frame(X,Y)
  
  #Plot the dataframe
  plot(train,pch=16)
  
  #Linear regression
  model <- lm(Y ~ X, train)
  #model <- lm(X ~ Y, train)
  
  # Plot Linear regression
  abline(model)
  

  # Fit a model. 
  model_svm <- svm(Y ~ X , train)
  
  # Predict on the data
  pred <- predict(model_svm, train)
  
  # Plot the predictions to see our model fit
  points(train$X, pred, col = "blue", pch=4)
  
  # eTVDtract residual to calculate rmse
  error <- model$residuals 
  lm_error <- sqrt(mean(error^2)) 
  
  # error calc = actual values (train$y) with our predictions (pred)
  error_2 <- train$Y - pred
  svm_error <- sqrt(mean(error_2^2)) 
  
  
  # perform a grid search
  svm_tune <- tune(svm, Y ~ X, data = train,
                   #ranges = list(epsilon = seq(0.68,0.84,0.005), cost = 2^(2:9))
                   #ranges = list(epsilon = seq(0,.05,0.01), cost = 2^(2:9))
                   ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9))
  )
  print(svm_tune)
  
  # Find the best model
  best_mod <- svm_tune$best.model
  best_mod_pred <- predict(best_mod, train) 
  
  error_best_mod <- train$Y - best_mod_pred 
  
  
  best_mod_RMSE <- sqrt(mean(error_best_mod^2)) # 1.290738 
  
  #plot(svm_tune)
  
  plot(train,main=paste(wellnm," ",stg," C",j,sep=""), xlab= "Easting (ft)", ylab="Northing (ft)",pch=16)
  points(train$X, pred, col = "blue", pch=4)
  points(train$X, best_mod_pred, col = "red", pch=23)
  
  
  out <- data.frame(stgm$Well,stgm$Stage,best_mod_pred,stgm$`Easting ft (Abs)`,stgm$`Northing ft (Abs)`,stgm$`Depth ft (TVDSS)`,stgm$Magnitude,stgm$`P/S Ratio` ,stgm$date_time,stgm$`Event File`,stgm$`Pressure (psi)` ,stgm$`Flow (bpm)`,stgm$`Conc (ppg)`,pred,stgm$StgClust)
  newcolnames <- c("Well","Stage","best_mod_pred","Easting ft","Northing ft","Depth ft TVD","Magnitude","P/S Ratio.", "Date-Time", "Event","Pressure","Rate","Prop","pred","Cluster")
  colnames(out) <- newcolnames
  
  
  write.csv(out, paste(wellnm,"-",stg,"-",j,"y_out.csv"))
  print("Finished Run")
}

# x run
for(j in clust_list) {
  
  dataset <- cluster_test %>% filter(cluster_test$Well == wellnm)
  dataset <- dataset %>% filter(dataset$Stage == stg)
  dataset <- dataset %>% filter(dataset$StgClust == j)
  X=dataset$`Easting ft (Abs)`
  Y=dataset$`Northing ft (Abs)`
  
  print("Running Cluster")
  print(paste(wellnm," ",stg," C",j,sep=""))
  
  #Create data frame
  train=data.frame(X,Y)
  
  #Plot the dataframe
  plot(train$Y,train$X,pch=16)
  
  #Linear regression
  #model <- lm(Y ~ X, train)
  model <- lm(X ~ Y, train)
  
  # Plot Linear regression
  abline(model)
  
  # SVM 
  
  
  # Fit a model. 
  #model_svm <- svm(Y ~ X , train)
  model_svm <- svm(X ~ Y , train)
  # Predict on the data
  pred <- predict(model_svm, train)
  
  # Plot the predictions to see our model fit
  points(train$Y, pred, col = "blue", pch=4)
  
  # eTVDtract residual to calculate rmse
  error <- model$residuals 
  lm_error <- sqrt(mean(error^2)) 
  
  # error calc = actual values (train$y) with our predictions (pred)
  error_2 <- train$X - pred
  svm_error <- sqrt(mean(error_2^2)) 
  
  
  # perform a grid search
  svm_tune <- tune(svm, X ~ Y, data = train,
                   #ranges = list(epsilon = seq(0.68,0.84,0.005), cost = 2^(2:9))
                   #ranges = list(epsilon = seq(0,.05,0.01), cost = 2^(2:9))
                   ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9))
  )
  print(svm_tune)
  
  # Find the best model
  best_mod <- svm_tune$best.model
  best_mod_pred <- predict(best_mod, train) 
  
  error_best_mod <- train$X - best_mod_pred 
  
  
  best_mod_RMSE <- sqrt(mean(error_best_mod^2)) # 1.290738  
  
  #plot(svm_tune)
  
  plot(train$Y,train$X,main=paste(wellnm, " Stage", stg," C",j,sep=""), xlab= "Easting (ft)", ylab="Northing (ft)",pch=16)
  points(train$Y, pred, col = "blue", pch=4)
  points(train$Y, best_mod_pred, col = "red", pch=23)
  
  
  out <- data.frame(dataset$Well,dataset$Stage,best_mod_pred,dataset$`Easting ft (Abs)`,dataset$`Northing ft (Abs)`,dataset$`Depth ft (TVDSS)`,dataset$Magnitude,dataset$`P/S Ratio` ,dataset$date_time,dataset$`Event File`,dataset$`Pressure (psi)` ,dataset$`Flow (bpm)`,dataset$`Conc (ppg)`,pred)
  newcolnames <- c("Well","Stage","best_mod_pred","Easting ft","Northing ft","Depth ft TVD","Magnitude","P/S Ratio.", "Date-Time", "Event","Pressure","Rate","Prop","pred")
  colnames(out) <- newcolnames
  
  
  write.csv(out, paste(wellnm,"-",stg,"-",j,"x_out.csv"))
  print("Finished Run")
}

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
write.csv(df, file = "Clust.csv")
write.xlsx(df, 'SVM.xlsx')
write.xlsx(df, 'reg.xlsx')
################################Regression#################################
SVM_Data <- import("SVM.xlsx")

# SVM_Data$Stage <- str_sub(SVM_Data$Stage, -2)
SVM_Data$Stage <- as.numeric(SVM_Data$Stage)


well_list <- unique(SVM_Data$Well)
wellnm <- well_list[3]
SVM_Data <- SVM_Data %>% filter(SVM_Data$Well == wellnm)
#check plot
ccheck <- data.frame(SVM_Data$Well, SVM_Data$Stage, SVM_Data$Cluster)  
ccheck <-  distinct(ccheck)
ggplot(data=ccheck, aes(x=ccheck$SVM_Data.Stage, y=ccheck$SVM_Data.Cluster))+geom_point(color=ccheck$SVM_Data.Stage,size=4, shape=15)+
  theme_bw()+labs(x="Stage",y="Cluster")+
  theme(axis.text.x = element_text(face="bold", color="#000000", size=10, angle=0),
        axis.text.y = element_text(face="bold", color="#000000", size=10, angle=0))+ggtitle(paste0(wellnm,": Stage Clusters") )+
  scale_x_continuous(breaks = round(seq(min(ccheck$SVM_Data.Stage), max(ccheck$SVM_Data.Stage), by = 1),1))

i <- min(SVM_Data$Stage)
i <- stg_list[1]

#ggsave(paste0(wellnm,"Stage Clusters.jpeg"),width = 16, height = 9, units = 'in', dpi = 900)
j <- 1
clust_list <- c(1,2,3,4,5,6,7,8,9)
################run this for stage based-Y######################

stg_list <- unique(SVM_Data$Stage)
stg_list <- sort(stg_list)

REG <- SVM_Data %>% filter(SVM_Data$Stage == i)
clust_list <- unique(REG$Cluster)
clust_list <- sort(clust_list)

for(j in clust_list) {
  yreg_run(j)
}
i = i+1
#####the function######
yreg_run <- function(j){
#print(unique(REG$Cluster))
REG <- REG %>% filter(REG$Cluster == j)


X=REG$Easting.ft
Y=REG$best_mod_pred

# X=REG$best_mod_pred
# Y=REG$Northing.ft..Abs..
print("Plotting")

#Create data frame
train=data.frame(X,Y)

#Plot the dataframe
plot(train,pch=16)

#Linear regression
model <- lm(Y ~ X, train)
# model <- lm(X ~ Y, train)
#summary(model)

# Plot Linear regression
abline(model, col="red")

REG$fitted <- model$fitted.values
write.csv(REG, paste(wellnm,"-",i,"-",j,".csv"))

print(paste("Stage",i, "Cluster",j))
}

################run this for stage based-X######################

stg_list <- unique(SVM_Data$Stage)

REG <- SVM_Data %>% filter(SVM_Data$Stage == i)
clust_list <- unique(REG$Cluster)

for(j in clust_list) {
  xreg_run(j)
}
i = i+1
#####the function######
xreg_run <- function(j){
  #print(unique(REG$Cluster))
  REG <- REG %>% filter(REG$Cluster == j)
  
  # X=REG$Easting.ft..Abs..
  # Y=REG$best_mod_pred
  
  X=REG$best_mod_pred
  Y=REG$Northing.ft
  print("Plotting")
  
  #Create data frame
  train=data.frame(X,Y)
  
  #Plot the dataframe
  plot(train,pch=16)
  
  #Linear regression
  # model <- lm(Y ~ X, train)
  model <- lm(X ~ Y, train)
  #summary(model)
  
  # Plot Linear regression
  abline(model, col="red")
  
  REG$fitted <- model$fitted.values
  write.csv(REG, paste(wellnm,"-",i,"-",j,".csv"))
  
  print(paste("Stage",i, "Cluster",j))
}
###################################Add to main sheet############################
dataset <- import("Values2.xlsx")
toadd <- data.frame(df$Event, df$fitted)
newcolnames <- c("Event File","fitted")
colnames(toadd) <- newcolnames
dataset <- dataset %>% right_join(toadd, by=c("Event File"))
write.xlsx(dataset, 'Values3.xlsx')
write.xlsx(df, 'df.xlsx')
#############################Load Wells##############################################
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

if("Frac7" %in% wellnames){
  Frac7<-data.frame(wellsheets$Frac7)
}

if("Frac8" %in% wellnames){
  Frac8<-data.frame(wellsheets$Frac8)
}

if("Frac9" %in% wellnames){
  Frac9<-data.frame(wellsheets$Frac9)
}

if("Frac10" %in% wellnames){
  Frac10<-data.frame(wellsheets$Frac10)
}

if("Frac11" %in% wellnames){
  Frac11<-data.frame(wellsheets$Frac11)
}

if("Frac12" %in% wellnames){
  Frac12<-data.frame(wellsheets$Frac12)
}

if("Frac13" %in% wellnames){
  Frac13<-data.frame(wellsheets$Frac13)
}

if("Frac14" %in% wellnames){
  Frac14<-data.frame(wellsheets$Frac14)
}

if("Frac15" %in% wellnames){
  Frac15<-data.frame(wellsheets$Frac15)
}

if("Frac16" %in% wellnames){
  Frac16<-data.frame(wellsheets$Frac16)
}

if("Frac17" %in% wellnames){
  Frac17<-data.frame(wellsheets$Frac17)
}

if("Frac18" %in% wellnames){
  Frac18<-data.frame(wellsheets$Frac18)
}

if("Frac19" %in% wellnames){
  Frac19<-data.frame(wellsheets$Frac19)
}

if("Frac20" %in% wellnames){
  Frac20<-data.frame(wellsheets$Frac20)
}

if("Frac21" %in% wellnames){
  Frac21<-data.frame(wellsheets$Frac21)
}

if("Frac22" %in% wellnames){
  Frac22<-data.frame(wellsheets$Frac22)
}

if("Frac23" %in% wellnames){
  Frac23<-data.frame(wellsheets$Frac23)
}

#############################Load Perfs##############################################
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
wellsheets <- read_excel_allsheets("R-Perfs.xlsx")
#wellsheets <- read_excel_allsheets("R-Perfs -ACM.xlsx")
wellnames <- names(wellsheets)

if("Perf1" %in% wellnames){
  Perf1<-data.frame(wellsheets$Perf1)
}

if("Perf2" %in% wellnames){
  Perf2<-data.frame(wellsheets$Perf2)
}

if("Perf3" %in% wellnames){
  Perf3<-data.frame(wellsheets$Perf3)
}

if("Perf4" %in% wellnames){
  Perf4<-data.frame(wellsheets$Perf4)
}

if("Perf5" %in% wellnames){
  Perf5<-data.frame(wellsheets$Perf5)
}

if("Perf6" %in% wellnames){
  Perf6<-data.frame(wellsheets$Perf6)
}

if("Perf7" %in% wellnames){
  Perf7<-data.frame(wellsheets$Perf7)
}

if("Perf8" %in% wellnames){
  Perf8<-data.frame(wellsheets$Perf8)
}

if("Perf9" %in% wellnames){
  Perf9<-data.frame(wellsheets$Perf9)
}

if("Perf10" %in% wellnames){
  Perf10<-data.frame(wellsheets$Perf10)
}

if("Perf11" %in% wellnames){
  Perf11<-data.frame(wellsheets$Perf11)
}

if("Perf12" %in% wellnames){
  Perf12<-data.frame(wellsheets$Perf12)
}

if("Perf13" %in% wellnames){
  Perf13<-data.frame(wellsheets$Perf13)
}

if("Perf14" %in% wellnames){
  Perf14<-data.frame(wellsheets$Perf14)
}

if("Perf15" %in% wellnames){
  Perf15<-data.frame(wellsheets$Perf15)
}

if("Perf16" %in% wellnames){
  Perf16<-data.frame(wellsheets$Perf16)
}

if("Perf17" %in% wellnames){
  Perf17<-data.frame(wellsheets$Perf17)
}

if("Perf18" %in% wellnames){
  Perf18<-data.frame(wellsheets$Perf18)
}

if("Perf19" %in% wellnames){
  Perf19<-data.frame(wellsheets$Perf19)
}

if("Perf20" %in% wellnames){
  Perf20<-data.frame(wellsheets$Perf20)
}

if("Perf21" %in% wellnames){
  Perf21<-data.frame(wellsheets$Perf21)
}

if("Perf22" %in% wellnames){
  Perf22<-data.frame(wellsheets$Perf22)
}

if("Perf23" %in% wellnames){
  Perf23<-data.frame(wellsheets$Perf23)
}
##################plot########################
#clust <- import("Master.xlsx")
library(readxl)
clustm <- import("yRegClust.xlsx")
clust[clust == 0] <- NA
well_list <- unique(clustm$Well)
wellnm <- well_list[4]
wellnm <- "All Wells"

v <- 6
clust <- clustm %>% filter(clustm$Well == wellnm)
#stg_list <-unique(clust$Stage)
clust <- clust %>% filter(clust$STG == v)
#clust <- clust %>% filter(clust$STG == c(17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53))
#clust <- clust %>% filter(clust$STG >= 17)
stgs <- v
#stgs <- " ACM"



cperf <- Perf4
cperf <- Perf4 %>% filter(Stage == paste0("Stage ",v))
cperf <- Perf1 %>% filter(Perf1$STG > 17)
cperf <- Perf1 %>% filter(Stage == c("Stage 17","Stage 18","Stage 19","Stage 20","Stage 21","Stage 22","Stage 23","Stage 24","Stage 25","Stage 26","Stage 27","Stage 28","Stage 29","Stage 30","Stage 31","Stage 33","Stage 34","Stage 35","Stage 36","Stage 37","Stage 38","Stage 39","Stage 40","Stage 41","Stage 42","Stage 43","Stage 44","Stage 45","Stage 46","Stage 47","Stage 48","Stage 49","Stage 50","Stage 51","Stage 52","Stage 53"))

ACM.check <- function(){
  x = clust$Date.Time
  y = clust$Prop
  plot(x,y, main="ACM Cycle Check", type="p", col="#C70606")
}
runplt <- function() {
print(paste0("Running ",wellnm,": Stage ",v))
ACM.check()
MPLOT<-ggplot(clust, aes(x=clust$Easting.ft, y=clust$Northing.ft)) +geom_point(color = "grey30", shape = 21,size = 1, alpha=.4)+
  theme_bw()+labs(x="Easting (ft)",y="Northing (ft)")+
  theme(axis.text.x = element_text(face="bold", color="#000000", size=10, angle=90),
        axis.text.y = element_text(face="bold", color="#000000", size=10, angle=0))+ggtitle(paste0(wellnm,": All Stages") )#+ggtitle("UL Sugarloaf C 20-37-48 No 5203H 26-32")#+ggtitle("UL Sugarloaf D 20-37-48 No 5104H Stages 24-31")   

# clust$Stage <- as.factor(clust$Stage) 
# class(clust$Stage)
# 
Plot <- MPLOT + geom_point(aes(x=clust$yplotx, y=clust$yploty),color= "black", size=.6, stroke=.5)+
  geom_point(aes(x=clust$yplotx, y=clust$yploty), color = clust$STG, size=.6,alpha=.4)

# Plot <- MPLOT + geom_point(aes(x=clust$xplotx, y=clust$xploty),color= "black", size=1.5, stroke=.5)+
#   geom_point(aes(x=clust$xplotx, y=clust$xploty), color = clust$Stage, size=1,alpha=.4)

# Plot <- MPLOT + geom_point(aes(x=clust$xplotx, y=clust$yploty),color= "black", size=1.5, stroke=.5)+
#   geom_point(aes(x=clust$xplotx, y=clust$yploty), color = clust$Tcode, size=1,alpha=.4)
xmin <- 1003750
xmax <- 1008500

ymin <- 556000
ymax <- 565000



Plot2 <- Plot + geom_path(data = Frac1, aes(x=Frac1$Easting..ft., y=Frac1$Northing..ft.), color="black", size =1, alpha = .8)
Plot3 <- Plot2 + geom_path(data = Frac2, aes(x=Frac2$Easting..ft., y=Frac2$Northing..ft.), color="black", size =1, alpha = .8)
Plot4 <- Plot3 + geom_path(data = Frac3, aes(x=Frac3$Easting..ft., y=Frac3$Northing..ft.), color="black", size =1, alpha = .8)
Plot5 <- Plot4 + geom_path(data = Frac4, aes(x=Frac4$Easting..ft., y=Frac4$Northing..ft.), color="black", size =1, alpha = .8)
Plot6 <- Plot5 + geom_point(data = Perf1, aes(x=Perf1$Easting..ft., y=Perf1$Northing..ft.), color ="#616161", size=1.2, alpha = .9)
Plot7 <- Plot6 + geom_point(data = Perf2, aes(x=Perf2$Easting..ft., y=Perf2$Northing..ft.), color ="#616161", size=1.2, alpha = .9)
Plot8 <- Plot7 + geom_point(data = Perf3, aes(x=Perf3$Easting..ft., y=Perf3$Northing..ft.), color ="#616161", size=1.2, alpha = .9)
Plot9 <- Plot8 + geom_point(data = Perf4, aes(x=Perf4$Easting..ft., y=Perf4$Northing..ft.), color ="#616161", size=1.2, alpha = .9)
Plot10 <- Plot9 + geom_point(data = cperf, aes(x=Easting..ft., y=Northing..ft.), color ="#DDE713", size=1.2, alpha = .9)+
  labs(x="Easting (ft)",y="Northing (ft)")+
  ggtitle(paste0(wellnm," Stage",stgs))+xlim(c(xmin,xmax))+ylim(c(ymin,ymax))+coord_fixed()

# Plot10 <- Plot9 + geom_point(aes(x=clust$yplotx, y=clust$yploty),color= "black", size=.6, stroke=.5)+
#   geom_point(aes(x=clust$yplotx, y=clust$yploty), color = clust$STG, size=.6,alpha=.4)

w <- toWebGL(ggplotly(Plot10))
saveWidget(w, paste(wellnm,"Stage",stgs,"stick.html"), selfcontained = T, libdir = "lib")

#ggsave(paste0(wellnm," All Stages.jpeg"),width = 16, height = 9, units = 'in', dpi = 900)

x <- clust$Easting.ft
y <- clust$Northing.ft
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))
p <- ggplot(df) +
  geom_point(aes(x, y, col = d), size = 1) +
  scale_color_identity() +
  theme_bw()+labs(x="Easting (ft)",y="Northing (ft)")+
  theme(axis.text.x = element_text(face="bold", color="#000000", size=10, angle=90),
        axis.text.y = element_text(face="bold", color="#000000", size=10, angle=0))+ggtitle(paste0(wellnm,": All Stages") )



Plot2 <- p + geom_path(data = Frac1, aes(x=Frac1$Easting..ft., y=Frac1$Northing..ft.), color="black", size =1, alpha = .8)
Plot3 <- Plot2 + geom_path(data = Frac2, aes(x=Frac2$Easting..ft., y=Frac2$Northing..ft.), color="black", size =1, alpha = .8)
Plot4 <- Plot3 + geom_path(data = Frac3, aes(x=Frac3$Easting..ft., y=Frac3$Northing..ft.), color="black", size =1, alpha = .8)
Plot5 <- Plot4 + geom_path(data = Frac4, aes(x=Frac4$Easting..ft., y=Frac4$Northing..ft.), color="black", size =1, alpha = .8)
Plot6 <- Plot5 + geom_point(data = Perf1, aes(x=Perf1$Easting..ft., y=Perf1$Northing..ft.), color ="#616161", size=1.2, alpha = .9)
Plot7 <- Plot6 + geom_point(data = Perf2, aes(x=Perf2$Easting..ft., y=Perf2$Northing..ft.), color ="#616161", size=1.2, alpha = .9)
Plot8 <- Plot7 + geom_point(data = Perf3, aes(x=Perf3$Easting..ft., y=Perf3$Northing..ft.), color ="#616161", size=1.2, alpha = .9)
Plot9 <- Plot8 + geom_point(data = Perf4, aes(x=Perf4$Easting..ft., y=Perf4$Northing..ft.), color ="#616161", size=1.2, alpha = .9)
Plot10 <- Plot9 + geom_point(data = cperf, aes(x=Easting..ft., y=Northing..ft.), color ="#ECECEC", size=1.2, alpha = .9)+
  labs(x="Easting (ft)",y="Northing (ft)")+
  #ggtitle("All Wells: Density All Stages")+xlim(c(xmin,xmax))+ylim(c(ymin,ymax))#+coord_fixed()
  ggtitle(paste0(wellnm," Stage", stgs, " Density"))+xlim(c(xmin,xmax))+ylim(c(ymin,ymax))+coord_fixed()




Plot10 <- Plot10 + geom_point(aes(x=clust$yplotx, y=clust$yploty),color= "black", size=.40, stroke =.55, alpha=.4)
Plot10 <- Plot10 + theme(legend.position="none")
w <- toWebGL(ggplotly(Plot10))

saveWidget(w, paste(wellnm," Stage", stgs, " density.html"), selfcontained = T, libdir = "lib")

#plotly_json(w)



# Plot11 <- Plot10 + geom_point(aes(x=clust$xplotx, y=clust$xploty),color= "black", size=.40, stroke =.55, alpha=.4)

#ggsave("All Wells - All Wells - Density All Stages.jpeg",width = 16, height = 9, units = 'in', dpi = 900)
#(paste0(wellnm," Density All Stages.jpeg"),width = 16, height = 9, units = 'in', dpi = 900)



x <- clust$Easting.ft
y <- clust$Northing.ft
z <- clust$Pressure
df <- data.frame(x = x, y = y,z = z,
                 d = densCols(z, colramp = colorRampPalette(rev(rainbow(10, end = 2/6)))))
p <- ggplot(df) +
  geom_point(aes(x, y, col = d), size = 1) +
  scale_color_identity() +
  theme_bw()+
  theme_bw()+labs(x="Easting (ft)",y="Northing (ft)")+
  theme(axis.text.x = element_text(face="bold", color="#000000", size=10, angle=90),
        axis.text.y = element_text(face="bold", color="#000000", size=10, angle=0))+ggtitle(paste0(wellnm,": All Stages") )


Plot2 <- p + geom_path(data = Frac1, aes(x=Frac1$Easting..ft., y=Frac1$Northing..ft.), color="black", size =1, alpha = .8)
Plot3 <- Plot2 + geom_path(data = Frac2, aes(x=Frac2$Easting..ft., y=Frac2$Northing..ft.), color="black", size =1, alpha = .8)
Plot4 <- Plot3 + geom_path(data = Frac3, aes(x=Frac3$Easting..ft., y=Frac3$Northing..ft.), color="black", size =1, alpha = .8)
Plot5 <- Plot4 + geom_path(data = Frac4, aes(x=Frac4$Easting..ft., y=Frac4$Northing..ft.), color="black", size =1, alpha = .8)
Plot6 <- Plot5 + geom_point(data = Perf1, aes(x=Perf1$Easting..ft., y=Perf1$Northing..ft.), color ="#616161", size=1.2, alpha = .9)
Plot7 <- Plot6 + geom_point(data = Perf2, aes(x=Perf2$Easting..ft., y=Perf2$Northing..ft.), color ="#616161", size=1.2, alpha = .9)
Plot8 <- Plot7 + geom_point(data = Perf3, aes(x=Perf3$Easting..ft., y=Perf3$Northing..ft.), color ="#616161", size=1.2, alpha = .9)
Plot9 <- Plot8 + geom_point(data = Perf4, aes(x=Perf4$Easting..ft., y=Perf4$Northing..ft.), color ="#616161", size=1.2, alpha = .9)
Plot10 <- Plot9 + geom_point(data = cperf, aes(x=Easting..ft., y=Northing..ft.), color ="#ECECEC", size=1.2, alpha = .9)+
  labs(x="Easting (ft)",y="Northing (ft)")+
  #ggtitle("All Wells: Density All Stages")+xlim(c(xmin,xmax))+ylim(c(ymin,ymax))#+coord_fixed()
  ggtitle(paste0(wellnm," Stage", stgs, " Density"))+xlim(c(xmin,xmax))+ylim(c(ymin,ymax))+coord_fixed()




Plot10 <- Plot10 + geom_point(aes(x=clust$yplotx, y=clust$yploty), group = clust$Well, color= "black", size=.40, stroke =.55, alpha=.4)

Plot10 <- Plot10 + theme(legend.position="none")
w <- toWebGL(ggplotly(Plot10))
saveWidget(w, paste(wellnm," Stage", stgs, " press-density.html"), selfcontained = T, libdir = "lib")

v <<- v+1
}

runplt()














# Plot11 <- Plot10 + geom_point(aes(x=clust$xplotx, y=clust$xploty),color= "black", size=.40, stroke =.55, alpha=.4)

#ggsave("All Wells - All Wells - Density All Stages.jpeg",width = 16, height = 9, units = 'in', dpi = 900)
#ggsave(paste0(wellnm," Pressure - Density All Stages.jpeg"),width = 16, height = 9, units = 'in', dpi = 900)

# Plotly version
x <- clust$Easting.ft
y <- clust$Northing.ft
z <- clust$Depth.ft.TVD
c <- clust$Well
p <- ggplot(clust, aes(x, y,z)) + geom_point()

l <- list(
  font = list(
    family = "sans-serif",
    size = 12,
    color = "#000"),
  bgcolor = "#E2E2E2",
  bordercolor = "#000",
  borderwidth = 2)

axz <- list(
  nticks = 4,
  range = c(-9000,-4000)
)


p <- plot_ly(x=x, y=y, z=z, type="scatter3d", mode="markers", 
        color=~c, colors=c("#C11938","#1436E0","#49E014","#AA63DD"), marker=list(size=2,opacity=0.3)) %>% layout(
          title = "Microseismic Data ", scene = list(
            xaxis = list(title = "Easting"),
            yaxis = list(title = "Northing"),
            zaxis = list(title = "Depth TVD")
          )) %>%
  add_trace(x=Frac1$Easting..ft., y=Frac1$Northing..ft., z=Frac1$TVDSS..ft. , type = 'scatter3d', mode = 'lines',
            color= well_list[1], marker=list(size=.1,opacity=0.5 ), line = list(color="black"))%>%
  add_trace(x=Frac2$Easting..ft., y=Frac2$Northing..ft., z=Frac2$TVDSS..ft. , type = 'scatter3d', mode = 'lines',
            color= well_list[2], marker=list(size=.1,opacity=0.5 ), line = list(color="black"))%>%
  add_trace(x=Frac3$Easting..ft., y=Frac3$Northing..ft., z=Frac3$TVDSS..ft. , type = 'scatter3d', mode = 'lines',
            color= well_list[3], marker=list(size=.1,opacity=0.5 ), line = list(color="black"))%>%
  add_trace(x=Frac4$Easting..ft., y=Frac4$Northing..ft., z=Frac4$TVDSS..ft. , type = 'scatter3d', mode = 'lines',
            color= well_list[4], marker=list(size=.1,opacity=0.5 ), line = list(color="black"))%>%
  add_trace(x=Perf1$Easting..ft., y=Perf1$Northing..ft., z=Perf1$TVDSS..ft. , type = 'scatter3d', color =Perf1$Treatment.Well, marker = list(size=3,opacity=0.8))%>%
  add_trace(x=Perf2$Easting..ft., y=Perf2$Northing..ft., z=Perf2$TVDSS..ft. , type = 'scatter3d', color =Perf2$Treatment.Well, marker = list(size=3,opacity=0.8))%>%
  add_trace(x=Perf3$Easting..ft., y=Perf3$Northing..ft., z=Perf3$TVDSS..ft. , type = 'scatter3d', color =Perf3$Treatment.Well, marker = list(size=3,opacity=0.8))%>%
  add_trace(x=Perf4$Easting..ft., y=Perf4$Northing..ft., z=Perf4$TVDSS..ft. , type = 'scatter3d', color =Perf4$Treatment.Well, marker = list(size=3,opacity=0.8))%>%
  layout(legend = l)%>%
  layout(scene = list(zaxis=axz))



saveWidget(p, "3D.html", selfcontained = T, libdir = "lib")

# chart_link = api_create(p, filename="scatter3d-basic")
# chart_link

p <- plot_ly(x=x, y=y, type="scatter", mode="markers", 
             color=~c, colors=c("gray"), marker=list(size=2,opacity=0.3)) %>% layout(
               title = "Microseismic Data ", scene = list(
                 xaxis = list(title = "Easting"),
                 yaxis = list(title = "Northing"))) %>%
  add_trace(x=Frac1$Easting..ft., y=Frac1$Northing..ft., type = 'scatter', mode = 'lines',
            color= well_list[1], marker=list(size=.1,opacity=0.5 ), line = list(color="black"))%>%
  add_trace(x=Frac2$Easting..ft., y=Frac2$Northing..ft., type = 'scatter', mode = 'lines',
            color= well_list[2], marker=list(size=.1,opacity=0.5 ), line = list(color="black"))%>%
  add_trace(x=Frac3$Easting..ft., y=Frac3$Northing..ft., type = 'scatter', mode = 'lines',
            color= well_list[3], marker=list(size=.1,opacity=0.5 ), line = list(color="black"))%>%
  add_trace(x=Frac4$Easting..ft., y=Frac4$Northing..ft., type = 'scatter', mode = 'lines',
            color= well_list[4], marker=list(size=.1,opacity=0.5 ), line = list(color="black"))%>%
  add_trace(x=Perf1$Easting..ft., y=Perf1$Northing..ft., name = well_list[1], mode='markers', color ='rgba(255, 182, 193, .9)', colors=c("black"), marker = list(size=4,opacity=0.8))%>%
  add_trace(x=Perf2$Easting..ft., y=Perf2$Northing..ft., name = well_list[2],mode='markers', color ='rgba(255, 182, 193, .9)', colors=c("black"), marker = list(size=4,opacity=0.8))%>%
  add_trace(x=Perf3$Easting..ft., y=Perf3$Northing..ft., name = well_list[3],mode='markers', color ='rgba(255, 182, 193, .9)', colors=c("black"), marker = list(size=4,opacity=0.8))%>%
  add_trace(x=Perf4$Easting..ft., y=Perf4$Northing..ft., name = well_list[4],mode='markers', color ='rgba(255, 182, 193, .9)', colors=c("black"), marker = list(size=4,opacity=0.8))%>%
  layout(legend = l)%>%
  layout(scene = list(zaxis=axz))
p <- p + plot_ly(x=clust$yplotx, y=clust$yploty, type="scatter", mode="markers", 
                 color=~clust$Stage,marker=list(size=2,opacity=0.3)) 

saveWidget(p, "test.html", selfcontained = T, libdir = "lib")
