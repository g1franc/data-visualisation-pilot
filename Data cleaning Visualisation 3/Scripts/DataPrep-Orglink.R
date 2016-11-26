
library(plyr)
library(gtools)
library(data.table)
# ---------------------------------------------------------------------------------------------------------------------------------------------
# I. GLOBAL VARIABLE #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------------------------------------------------------------
# II. LOAD DATASETS #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

#set current directory
setwd("C:/Users/vandeloc/Documents/Documents/08 Cordis/Data cleaning Visualisation 1")
setwd("C:/Users/bruled/Documents/Pwc Project/2 - Project/DataVisualisation/Visualisation/1 - Cordis/3/data-visualisation-pilot/Data cleaning Visualisation 3")

### Load datasets
# H2020: https://data.europa.eu/euodp/data/dataset/cordisH2020projects

Dataset_H2020Organizations = read.csv("Input/cordis-h2020organizations.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")





# ---------------------------------------------------------------------------------------------------------------------------------------------
# III. FUNCTIONS LINK #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------


func_size <- function(vector){
  return(length(unlist(vector)))
}

func_order <- function(vector){
  return (as.character(sort(unlist(vector))))
}

func_createPair <- function(vector){
  if (length(vector) > 1) {
    return (t(combn(unlist(vector), 2)))
  }
  else
  {
    return (c())
  }
}

func_createFrame <- function(vector){
  return(data.frame(matrix(unlist(vector), ncol = 2, byrow=FALSE)))
}

# build a dataset containing all the interactions between the different countries  
func_BuildLink <- function() {
  dataset <- Dataset_H2020Organizations
  dataset <-aggregate(dataset$name, 
                      by=list(dataset$projectReference),
                      FUN=paste)
  dataset <- plyr::rename(dataset,c("Group.1"="Project",
                                    "x" = "ListOrg"))

  dataset$ListOrg <- sapply(dataset$ListOrg, func_order)
  dataset$size <- sapply(dataset$ListOrg, func_size)
  dataset<- dataset[dataset$size > 1,]  
  dataset <- subset(dataset, select=-c(size, Project))

  
  dataset$ListOrg <- sapply(dataset$ListOrg, func_createPair)

  data <- list()
  for (i in 1:nrow(dataset)) {
    data[[i]] =func_createFrame(dataset[i,])
  }
  tmp <- do.call(rbind, data)

  output <-data.frame(tmp)
  output<-aggregate(output$X1,
                    by=list(output$X1, output$X2),
                    FUN=length)
  output <-data.frame(output)
  
  output <- plyr::rename(output,c("Group.1" = "Org1",
                                  "Group.2" = "Org2",
                                  "x" = "nbLinkTmp"))
  
  tmp <-rep(frameworkContract,length(output$Group.1))
  output["frameworkContract"]<- tmp
  
  return(output)
}

func_addWeight <- function (dataset) {
  dataset <- Dataset_link
  dataset$weight <- c(rep(0, length(dataset$Org1)))
  maximum <- max(dataset$nbLink)
  
  dataset$weight <-dataset$nbLink / maximum 
  return(dataset)  
}



# ---------------------------------------------------------------------------------------------------------------------------------------------
# IV. FUNCTION ADDITIONAL INFORMATION #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------




func_ListOrg <- function() {
  dataset <- Dataset_H2020Organizations
  
  dataset <-aggregate(dataset$name,
                      by=list(dataset$name),
                      FUN=length)
  
  dataset <- plyr::rename(dataset,c("Group.1"="Organisation",
                                    "x" = "Number of project"))
  
  return (dataset)
    
  }


# ---------------------------------------------------------------------------------------------------------------------------------------------
# V. Build Dataset #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------


Sys.time()
Dataset_link <- func_BuildLink()
Dataset_link <- func_addWeight(Dataset_link)
Sys.time()

options(scipen = 10)
write.table(Dataset_link, "output/NbLinkOrg.csv", sep = ";", quote = FALSE)
options(scipen = 0)

Dataset_ListOrg <- func_ListOrg()
write.table(Dataset_ListOrg, "output/OrgInformation.csv", sep = ";", quote = FALSE)



