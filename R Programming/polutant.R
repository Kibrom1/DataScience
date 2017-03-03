polutantmean <- function(directory, polutant, id = 1:332){
  
  setwd(directory)
  result_data <- list()
  allfiles <- list.files()
  for(f in id){
  my_dataset <- read.csv(allfiles[f], encoding = "r")
  result_data <- rbind(result_data, my_dataset)
  }
  setwd('..')
  meanResult <- result_data
}

complete <- function(directory, id = 1:332){
  
  setwd(directory)
  result_data <- list()
  allfiles <- list.files()
  nobNum <- data.frame()
  finalResult <- list()
  for(i in id){
    my_datasets <- read.csv(allfiles[i], encoding = "r")
    #completeCases <- complete.cases(my_datasets)
    finalResult <- rbind(finalResult, c(id=i, nobs=nrow(na.omit(my_datasets))))
  }
  setwd('..')
  result <- finalResult
}

corr <- function(directory, thershold = 0){
  setwd(directory)
  
  allfiles <- list.files()
  sulfate <- list()
  nitrate <- list()
  for(i in 1:thershold){
    my_datasets <- read.csv(allfiles[i], encoding = "r")
    if(complete.cases(readline(my_datasets))){
      sulfate <- append(my_datasets$sulfate)
      nitrate <- append(my_datasets$nitrate)
    }
  }
  setwd('..')
  result <- corr(sulfate, nitrate)
}

