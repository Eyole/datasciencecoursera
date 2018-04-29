library(readr)
library(sqldf)

'%&%' <- function(x, y)paste0(x,y)

#################################################################################################################################################################################################

run_analysis()

##############################################################################################################

run_analysis <- function ()
{
  wd <<- getwd()
  
  setwd(wd)
  
  ReadAndCombineData()
  CalculateAverages()
  WriteDataToCSV()
  
  }

ReadAndCombineTestData <- function()
{

  wdpath <- wd %&% "/test/Inertial Signals/"
  
  filelst <<- list.files(path = wdpath, pattern = "*.txt", full.names = FALSE )
  

  for (i in 1:9)
 {
    
    switch(filelst[i], 
           "body_acc_x_test.txt" = {  
           testdata <- read.csv(wdpath %&% filelst[i], sep = " ", header = FALSE)
           sta_body_acc_x_test <- stack(testdata)
           body_acc_x_test <- na.omit(sta_body_acc_x_test)
           colnames(body_acc_x_test) <- c("body_acc_x_test", "datacola")
           }, 
           "body_acc_y_test.txt" = {
           testdata <- read.csv(wdpath %&% filelst[i], sep = " ", header = FALSE)
           sta_body_acc_y_test <- stack(testdata)
           body_acc_y_test <- na.omit(sta_body_acc_y_test)
           colnames(body_acc_y_test) <- c("body_acc_y_test", "datacolb")
           }, 
           "body_acc_z_test.txt" = {           
           testdata <- read.csv(wdpath %&% filelst[i], sep = " ", header = FALSE)
           sta_body_acc_z_test <- stack(testdata)
           body_acc_z_test <- na.omit(sta_body_acc_z_test)
           colnames(body_acc_z_test) <- c("body_acc_z_test", "datacolc")
           }, 
           "body_gyro_x_test.txt" = {           
             testdata <- read.csv(wdpath %&% filelst[i], sep = " ", header = FALSE)
             sta_body_gyro_x_test <- stack(testdata)
             body_gyro_x_test <- na.omit(sta_body_gyro_x_test)
             colnames(body_gyro_x_test) <- c("body_gyro_x_test", "datacold")
           }, 
           "body_gyro_y_test.txt" = {           
             testdata <- read.csv(wdpath %&% filelst[i], sep = " ", header = FALSE)
             sta_body_gyro_y_test<- stack(testdata)
             body_gyro_y_test <- na.omit(sta_body_gyro_y_test)
             colnames(body_gyro_y_test) <- c("body_gyro_y_test", "datacole")
           },
           "body_gyro_z_test.txt" = {           
             testdata <- read.csv(wdpath %&% filelst[i], sep = " ", header = FALSE)
             sta_body_gyro_z_test <- stack(testdata)
             body_gyro_z_test <- na.omit(sta_body_gyro_z_test)
             colnames(body_gyro_z_test) <- c("body_gyro_z_test", "datacolf")
           },
           "total_acc_x_test.txt" = {           
             testdata <- read.csv(wdpath %&% filelst[i], sep = " ", header = FALSE)
             sta_total_acc_x_test <- stack(testdata)
             total_acc_x_test <- na.omit(sta_total_acc_x_test)
             colnames(total_acc_x_test) <- c("total_acc_x_test", "datacolg")
           },
           "total_acc_y_test.txt" = {           
             testdata <- read.csv(wdpath %&% filelst[i], sep = " ", header = FALSE)
             sta_total_acc_y_test <- stack(testdata)
             total_acc_y_test <- na.omit(sta_total_acc_y_test)
             colnames(total_acc_y_test) <- c("total_acc_y_test", "datacolh")
           },
           "total_acc_z_test.txt" = {           
             testdata <- read.csv(wdpath %&% filelst[i], sep = " ", header = FALSE)
             sta_total_acc_z_test<- stack(testdata)
             total_acc_z_test <- na.omit(sta_total_acc_z_test)
             colnames(total_acc_z_test) <- c("total_acc_z_test", "datacoli")
           }
    ) 


  }

  df.all.test <<- data.frame(body_acc_x_test,body_acc_y_test,body_acc_z_test,body_gyro_x_test,body_gyro_y_test,body_gyro_z_test,total_acc_x_test,total_acc_y_test,total_acc_z_test)
  
  
}

#################################################################################################################################################################################################

ReadAndCombineTrainData <- function()
{
  
  wdpath <- wd %&% "/train/Inertial Signals/"
  
  filelst <<- list.files(path = wdpath, pattern = "*.txt", full.names = FALSE )
  
  
  for (i in 1:9)
  {
    
    switch(filelst[i], 
           "body_acc_x_train.txt" = {  
             traindata <- read.csv(wdpath %&% filelst[i], sep = " ", header = FALSE)
             sta_body_acc_x_train <- stack(traindata)
             body_acc_x_train <- na.omit(sta_body_acc_x_train)
             colnames(body_acc_x_train) <- c("body_acc_x_train", "datacola")
           }, 
           "body_acc_y_train.txt" = {
             traindata <- read.csv(wdpath %&% filelst[i], sep = " ", header = FALSE)
             sta_body_acc_y_train <- stack(traindata)
             body_acc_y_train <- na.omit(sta_body_acc_y_train)
             colnames(body_acc_y_train) <- c("body_acc_y_train", "datacolb")
           }, 
           "body_acc_z_train.txt" = {           
             traindata <- read.csv(wdpath %&% filelst[i], sep = " ", header = FALSE)
             sta_body_acc_z_train <- stack(traindata)
             body_acc_z_train <- na.omit(sta_body_acc_z_train)
             colnames(body_acc_z_train) <- c("body_acc_z_train", "datacolc")
           }, 
           "body_gyro_x_train.txt" = {           
             traindata <- read.csv(wdpath %&% filelst[i], sep = " ", header = FALSE)
             sta_body_gyro_x_train <- stack(traindata)
             body_gyro_x_train <- na.omit(sta_body_gyro_x_train)
             colnames(body_gyro_x_train) <- c("body_gyro_x_train", "datacold")
           }, 
           "body_gyro_y_train.txt" = {           
             traindata <- read.csv(wdpath %&% filelst[i], sep = " ", header = FALSE)
             sta_body_gyro_y_train<- stack(traindata)
             body_gyro_y_train <- na.omit(sta_body_gyro_y_train)
             colnames(body_gyro_y_train) <- c("body_gyro_y_train", "datacole")
           },
           "body_gyro_z_train.txt" = {           
             traindata <- read.csv(wdpath %&% filelst[i], sep = " ", header = FALSE)
             sta_body_gyro_z_train <- stack(traindata)
             body_gyro_z_train <- na.omit(sta_body_gyro_z_train)
             colnames(body_gyro_z_train) <- c("body_gyro_z_train", "datacolf")
           },
           "total_acc_x_train.txt" = {           
             traindata <- read.csv(wdpath %&% filelst[i], sep = " ", header = FALSE)
             sta_total_acc_x_train <- stack(traindata)
             total_acc_x_train <- na.omit(sta_total_acc_x_train)
             colnames(total_acc_x_train) <- c("total_acc_x_train", "datacolg")
           },
           "total_acc_y_train.txt" = {           
             traindata <- read.csv(wdpath %&% filelst[i], sep = " ", header = FALSE)
             sta_total_acc_y_train <- stack(traindata)
             total_acc_y_train <- na.omit(sta_total_acc_y_train)
             colnames(total_acc_y_train) <- c("total_acc_y_train", "datacolh")
           },
           "total_acc_z_train.txt" = {           
             traindata <- read.csv(wdpath %&% filelst[i], sep = " ", header = FALSE)
             sta_total_acc_z_train<- stack(traindata)
             total_acc_z_train <- na.omit(sta_total_acc_z_train)
             colnames(total_acc_z_train) <- c("total_acc_z_train", "datacoli")
           }
    ) 
    
    
  }
  
  df.all.train <<- data.frame(body_acc_x_train,body_acc_y_train,body_acc_z_train,body_gyro_x_train,body_gyro_y_train,body_gyro_z_train,total_acc_x_train,total_acc_y_train,total_acc_z_train)
  
  
}

#################################################################################################################################################################################################

SubjectAveragesInTest <- function()
{
  
  df.all.test.data <<- sqldf("select a.*, 'Test' from [df.all.test] a")
  colnames(df.all.test.data) <- c("body_acc_x", "Subjects_bax", 
                             "body_acc_y", "Subjects_bay",
                             "body_acc_z","Subjects_baz", 
                             "body_gyro_x","Subject_bgx",
                             "body_gyro_y","Subjects_bgy",
                             "body_gyro_z","Subjects_bgz", 
                             "total_acc_x","Subjects_tax", 
                             "total_acc_y","Subjects_tay", 
                             "total_acc_z","Subjects_taz", 
                             "Activity")
  
  df.avgs.subjects.test <<- sqldf("select distinct a.[Subjects_bax] as [Subjects], a.[Activity] as [Acivity], b.[Average Body X Acc], c.[Average Body Y Acc], d.[Average Body z Acc], e.[Average Body x Gyro], f.[Average Body y Gyro], g.[Average Body z Gyro], h.[Average Total X Acc], i.[Average Total y Acc], j.[Average Total z Acc]
                          from [df.all.test.data] a left join 
                                  (select distinct [Subjects_bax], [Activity], avg(body_acc_x) as [Average Body X Acc]  from [df.all.test.data] group by [Subjects_bax], [Activity]) b
                                  on a.[Subjects_bax] = b.[Subjects_bax] and b.[Activity] = 'Test'
                                  left join 
                                  (select distinct [Subjects_bay], [Activity], avg(body_acc_y) as [Average Body Y Acc]  from [df.all.test.data] group by [Subjects_bay], [Activity]) c
                                  on a.[Subjects_bax] = c.[Subjects_bay] and c.[Activity] = 'Test'
                                  left join
                                  (select distinct [Subjects_baz], [Activity], avg(body_acc_z) as [Average Body z Acc]  from [df.all.test.data] group by [Subjects_baz], [Activity]) d
                                  on a.[Subjects_bax] = d.[Subjects_baz]
                                  and d.[Activity] = 'Test'
                                  left join
                                  (select distinct [Subject_bgx], [Activity], avg(body_gyro_x) as [Average Body x Gyro]  from [df.all.test.data] group by [Subject_bgx], [Activity]) e
                                  on a.[Subjects_bax] = e.[Subject_bgx]
                                  and e.[Activity] = 'Test'
                                  left join
                                  (select distinct [Subjects_bgy], [Activity], avg(body_gyro_y) as [Average Body y Gyro]  from [df.all.test.data] group by [Subjects_bgy], [Activity]) f
                                  on a.[Subjects_bax] = f.[Subjects_bgy]
                                  and f.[Activity] = 'Test'
                                  left join
                                  (select distinct [Subjects_bgz], [Activity], avg(body_gyro_z) as [Average Body z Gyro]  from [df.all.test.data] group by [Subjects_bgz], [Activity]) g
                                  on a.[Subjects_bax] = g.[Subjects_bgz]
                                  and g.[Activity] = 'Test'
                                  left join
                                  (select distinct [Subjects_tax], [Activity], avg(total_acc_x) as [Average Total X Acc]  from [df.all.test.data] group by [Subjects_tax], [Activity]) h
                                  on a.[Subjects_bax] = h.[Subjects_tax]
                                  and h.[Activity] = 'Test'
                                  left join
                                  (select distinct [Subjects_tay], [Activity], avg(total_acc_y) as [Average Total y Acc]  from [df.all.test.data] group by [Subjects_tay], [Activity]) i
                                  on a.[Subjects_bax] = i.[Subjects_tay]
                                  and i.[Activity] = 'Test'
                                  left join
                                  (select distinct [Subjects_taz], [Activity], avg(total_acc_z) as [Average Total z Acc]  from [df.all.test.data] group by [Subjects_taz], [Activity]) j
                                  on a.[Subjects_bax] = j.[Subjects_taz]
                                  and j.[Activity] = 'Test'
                                  ")
  
  
}

#################################################################################################################################################################################################
SubjectAveragesInTrain <- function()
{
  
  df.all.train.data <<- sqldf("select a.*, 'Train' from [df.all.train] a")
  colnames(df.all.train.data) <- c("body_acc_x", "Subjects_bax", 
                                  "body_acc_y", "Subjects_bay",
                                  "body_acc_z","Subjects_baz", 
                                  "body_gyro_x","Subject_bgx",
                                  "body_gyro_y","Subjects_bgy",
                                  "body_gyro_z","Subjects_bgz", 
                                  "total_acc_x","Subjects_tax", 
                                  "total_acc_y","Subjects_tay", 
                                  "total_acc_z","Subjects_taz", 
                                  "Activity")
  
  df.avgs.subjects.train <<- sqldf("select distinct a.[Subjects_bax] as [Subjects], a.[Activity] as [Acivity], b.[Average Body X Acc], c.[Average Body Y Acc], d.[Average Body z Acc], e.[Average Body x Gyro], f.[Average Body y Gyro], g.[Average Body z Gyro], h.[Average Total X Acc], i.[Average Total y Acc], j.[Average Total z Acc]
                                  from [df.all.train.data] a left join 
                                  (select distinct [Subjects_bax], [Activity], avg(body_acc_x) as [Average Body X Acc]  from [df.all.Train.data] group by [Subjects_bax], [Activity]) b
                                  on a.[Subjects_bax] = b.[Subjects_bax] and b.[Activity] = 'Train'
                                  left join 
                                  (select distinct [Subjects_bay], [Activity], avg(body_acc_y) as [Average Body Y Acc]  from [df.all.Train.data] group by [Subjects_bay], [Activity]) c
                                  on a.[Subjects_bax] = c.[Subjects_bay] and c.[Activity] = 'Train'
                                  left join
                                  (select distinct [Subjects_baz], [Activity], avg(body_acc_z) as [Average Body z Acc]  from [df.all.Train.data] group by [Subjects_baz], [Activity]) d
                                  on a.[Subjects_bax] = d.[Subjects_baz]
                                  and d.[Activity] = 'Train'
                                  left join
                                  (select distinct [Subject_bgx], [Activity], avg(body_gyro_x) as [Average Body x Gyro]  from [df.all.Train.data] group by [Subject_bgx], [Activity]) e
                                  on a.[Subjects_bax] = e.[Subject_bgx]
                                  and e.[Activity] = 'Train'
                                  left join
                                  (select distinct [Subjects_bgy], [Activity], avg(body_gyro_y) as [Average Body y Gyro]  from [df.all.Train.data] group by [Subjects_bgy], [Activity]) f
                                  on a.[Subjects_bax] = f.[Subjects_bgy]
                                  and f.[Activity] = 'Train'
                                  left join
                                  (select distinct [Subjects_bgz], [Activity], avg(body_gyro_z) as [Average Body z Gyro]  from [df.all.Train.data] group by [Subjects_bgz], [Activity]) g
                                  on a.[Subjects_bax] = g.[Subjects_bgz]
                                  and g.[Activity] = 'Train'
                                  left join
                                  (select distinct [Subjects_tax], [Activity], avg(total_acc_x) as [Average Total X Acc]  from [df.all.Train.data] group by [Subjects_tax], [Activity]) h
                                  on a.[Subjects_bax] = h.[Subjects_tax]
                                  and h.[Activity] = 'Train'
                                  left join
                                  (select distinct [Subjects_tay], [Activity], avg(total_acc_y) as [Average Total y Acc]  from [df.all.Train.data] group by [Subjects_tay], [Activity]) i
                                  on a.[Subjects_bax] = i.[Subjects_tay]
                                  and i.[Activity] = 'Train'
                                  left join
                                  (select distinct [Subjects_taz], [Activity], avg(total_acc_z) as [Average Total z Acc]  from [df.all.Train.data] group by [Subjects_taz], [Activity]) j
                                  on a.[Subjects_bax] = j.[Subjects_taz]
                                  and j.[Activity] = 'Train'
                                  ")
  
  
}

CombineTrainAndTestAverages <- function()
{
  
  df.test.train.subjects.averages <<- sqldf("select a.* from [df.avgs.subjects.train] a union select b.* from [df.avgs.subjects.test] b")
  
  
}

#################################################################################################################################################################################################

ReadAndCombineData <- function()
{
  
  ReadAndCombineTestData()
  ReadAndCombineTrainData()
  
}

#################################################################################################################################################################################################

CalculateAverages <- function()
{
  
  SubjectAveragesInTest()
  SubjectAveragesInTrain()
  CombineTrainAndTestAverages()
  
  }

#################################################################################################################################################################################################


WriteDataToCSV <- function()
{
  write.csv(df.test.train.subjects.averages, wd %&% "\\df.single.tidy.dataset.csv",row.names = FALSE)
  write.table(df.test.train.subjects.averages, wd %&% "\\df.single.tidy.dataset.txt",row.names = FALSE)
  
}
#################################################################################################################################################################################################

CombineTrainTestData <- function()
{
  
  df.all.test.train <<- sqldf("select a.*, 'Test' from [df.all.test] a union select b.*, 'Train' from  [df.all.train] b")
  
  colnames(df.all.test.train) <- c("body_acc_x", "Subjects_bax", "body_acc_y", "Subjects_bay","body_acc_z","Subjects_baz", "body_gyro_x","Subject_bgx","body_gyro_y","Subjects_bgy","body_gyro_z","Subjects_bgz", "total_acc_x","Subjects_tax", "total_acc_y","Subjects_tay", "total_acc_z","Subjects_taz", "Activity")
}

#################################################################################################################################################################################################

CalcMeanAndSd <- function()
{
  ############## Mean & SD distinguished between Testing & Training Activity ##################################################
  df.avgs <<- sqldf("select distinct [Activity] as [Activity], avg(body_acc_x) as [Average Body X Acc], avg(body_acc_y) as [Average Body y Acc] , avg(body_acc_z) as [Average Body z Acc], avg(body_gyro_x) as [Average Body X Gyro], avg(body_gyro_y) as [Average Body y Gyro], avg(body_gyro_z) as [Average Body z Gyro], avg(total_acc_x) as [Average Total X Acc], avg(total_acc_y) as [Average Total y Acc], avg(total_acc_z) as [Average Total z Acc] from [df.all.test.train] group by [Activity]")
  
  view(df.avgs)
  
  df.stdv <<- sqldf("select distinct [Activity] as [Activity, stdev(body_acc_x) as [Std Dev Body X Acc], stdev(body_acc_y) as [Std Dev Body y Acc] , stdev(body_acc_z) as [Std Dev Body z Acc], stdev(body_gyro_x) as [Std Dev Body X Gyro], stdev(body_gyro_y) as [Std Dev Body y Gyro], stdev(body_gyro_z) as [Std Dev Body z Gyro], stdev(total_acc_x) as [Std Dev Total X Acc], stdev(total_acc_y) as [Std Dev Total y Acc],stdev(total_acc_z) as [Std Dev Total z Acc] from [df.all.test.train] group by [Activity]")
  
  View(df.stdv)
  
  ############ Just perform the Average & SD without distinguishing into Activities ########################################
  df.avgs <<- sqldf("select avg(body_acc_x) as [Average Body X Acc], avg(body_acc_y) as [Average Body y Acc] , avg(body_acc_z) as [Average Body z Acc], avg(body_gyro_x) as [Average Body X Gyro], avg(body_gyro_y) as [Average Body y Gyro], avg(body_gyro_z) as [Average Body z Gyro], avg(total_acc_x) as [Average Total X Acc], avg(total_acc_y) as [Average Total y Acc], avg(total_acc_z) as [Average Total z Acc] from [df.all.test.train]")
  
  view(df.avgs)
  
  df.stdv <<- sqldf("select stdev(body_acc_x) as [Std Dev Body X Acc], stdev(body_acc_y) as [Std Dev Body y Acc] , stdev(body_acc_z) as [Std Dev Body z Acc], stdev(body_gyro_x) as [Std Dev Body X Gyro], stdev(body_gyro_y) as [Std Dev Body y Gyro], stdev(body_gyro_z) as [Std Dev Body z Gyro], stdev(total_acc_x) as [Std Dev Total X Acc], stdev(total_acc_y) as [Std Dev Total y Acc],stdev(total_acc_z) as [Std Dev Total z Acc] from [df.all.test.train]")
  
  View(df.stdv)
}


  
  
  
