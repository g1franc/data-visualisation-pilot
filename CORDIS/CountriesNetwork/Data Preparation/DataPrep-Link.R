
#library(plyr)
#library(gtools)
#library(data.table)


# ---------------------------------------------------------------------------------------------------------------------------------------------
# I. LOAD DATASETS ############################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

#set current directory
setwd("C:/Users/bruled/Documents/Pwc Project/2 - Project/DataVisualisation/Visualisation/1 - Cordis/data-visualisation-pilot/CORDIS/CountriesNetwork/")

### Load datasets
# FP6: https://data.europa.eu/euodp/data/dataset/cordisfp6projects
# FP7: https://data.europa.eu/euodp/data/dataset/cordisfp7projects
# H2020: https://data.europa.eu/euodp/data/dataset/cordisH2020projects
# Population: http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=demo_pjan&lang=en
# GDP: http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=naida_10_gdp&lang=en
Dataset_FP6Organizations = read.csv("../Datasets/cordis-fp6organizations.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")

Dataset_FP7Organizations = read.csv("../Datasets/cordis-fp7organizations.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")

Dataset_H2020Organizations = read.csv("../Datasets/cordis-h2020organizations.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")

Dataset_Countries = read.csv("../Datasets/Countries.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")




# ---------------------------------------------------------------------------------------------------------------------------------------------
# II. FUNCTIONS LINK ##########################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------


func_size <- function(vector){
  return(length(unlist(vector)))
}

func_order <- function(vector){
  return (as.character(sort(unlist(vector))))
}

#take a vector (e.g. [A,B,C]) and return all the possible pair of value (e.g. [[A, B],[A, C],[B, C]]) 
func_createPair <- function(vector){
  if (length(vector) > 1) {
    return (t(combn(unlist(vector), 2)))
  }
  else
  {
    return (c())
  }
}

#transform a list [a,b,c,d,e,f] in a frame with 2 columns [[a,b,c],[d,e,f]]
func_createFrame <- function(vector){
  return(data.frame(matrix(unlist(vector), ncol = 2, byrow=FALSE)))
}

# build a dataset containing all the interactions between the different countries  
func_BuildLink <- function(frameworkContract, dataset) {
  # frameworkContract <- "FP6"
  # dataset <- Dataset_FP6Organizations
  
  #for each project, list the country that participated 
  dataset <-aggregate(dataset$country, 
                      by=list(dataset$projectReference),
                      FUN=paste)
  dataset <- plyr::rename(dataset,c("Group.1"="Project",
                                    "x" = "ListCountries"))

  # create all the link between two country per project (projA [AA,AB,BC], projB [AA,AB,BC]...) 
  dataset$ListCountries <- sapply(dataset$ListCountries, func_order)
  dataset$size <- sapply(dataset$ListCountries, func_size)
  dataset<- dataset[dataset$size > 1,]  
  dataset <- subset(dataset, select=-c(size, Project))

  
  # create all the link between two country
  dataset$ListCountries <- sapply(dataset$ListCountries, func_createPair)
  data <- list()
  for (i in 1:nrow(dataset)) {
    data[[i]] =func_createFrame(dataset[i,])
  }
  tmp <- do.call(rbind, data)
  output <-data.frame(tmp)
  
  #aggregate to have: country 1 | country 2 | nb link
  output<-aggregate(output$X1,
                    by=list(output$X1, output$X2),
                    FUN=length)
  output <-data.frame(output)
  output <- plyr::rename(output,c("Group.1" = "country1",
                                  "Group.2" = "country2",
                                  "x" = "nbLink"))
  
  output["frameworkContract"]<- frameworkContract
  
  output["differentCountry"] <- output$country1 != output$country2
  output <- output[output$differentCountry, ]
  output <- subset(output, select=-c(differentCountry))
  
  return(output)
}

func_nbProjectPerCountry<- function(dataset_link, dataset) {
  #dataset_link <- FP6_link
  #dataset <- Dataset_FP6Organizations
  
  dataset <-aggregate(dataset$country, 
                      by=list(dataset$country),
                      FUN=length)
  dataset <- plyr::rename(dataset,c("Group.1" = "country",
                                  "x" = "NbProject"))
  dataset_link <- merge(dataset_link, dataset, by.x=c("country1"), by.y=c("country"), all.x=TRUE)
  dataset_link <- merge(dataset_link, dataset, by.x=c("country2"), by.y=c("country"), all.x=TRUE)
  dataset_link <- plyr::rename(dataset_link,c("NbProject.x" = "NbProject2",
                                              "NbProject.y" = "NbProject1"))
  
  dataset_link <- dataset_link[,c(4,1,2,5,6,3)]
  return(dataset_link)
  
}

func_RemoveNonEuCountry <- function(dataset) {
  dataset <- merge(dataset, Dataset_Countries, by.x=c("country"), by.y=c("euCode"), all.x=TRUE)
  #select only EU-28 countries
  dataset <- dataset[dataset$EU28 == TRUE,]
  dataset <- dataset[!is.na(dataset$country),]
  return(dataset)
}
  
# ---------------------------------------------------------------------------------------------------------------------------------------------
# IV. Build Dataset link ######################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
Dataset_FP6Organizations <- func_RemoveNonEuCountry(Dataset_FP6Organizations)
Dataset_FP7Organizations <- func_RemoveNonEuCountry(Dataset_FP7Organizations)
Dataset_H2020Organizations <- func_RemoveNonEuCountry(Dataset_H2020Organizations)




Sys.time()
FP6_link <- func_BuildLink("FP6", Dataset_FP6Organizations)
FP6_link <- func_nbProjectPerCountry(FP6_link, Dataset_FP6Organizations)

Sys.time()
FP7_link <- func_BuildLink("FP7", Dataset_FP7Organizations)
FP7_link <- func_nbProjectPerCountry(FP7_link, Dataset_FP7Organizations)


Sys.time()
H2020_link <- func_BuildLink("H2020", Dataset_H2020Organizations)
H2020_link <- func_nbProjectPerCountry(H2020_link, Dataset_H2020Organizations)

Dataset_link <-rbind(rbind(FP6_link, FP7_link), H2020_link)



options(scipen = 10)
write.table(Dataset_link, "VisualisedData/NbLink.csv", sep = ";", quote = FALSE, row.names = FALSE)
options(scipen = 0)


