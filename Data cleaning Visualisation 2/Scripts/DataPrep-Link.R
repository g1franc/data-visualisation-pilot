
library(plyr)
library(gtools)
library(data.table)
# ---------------------------------------------------------------------------------------------------------------------------------------------
# I. GLOBAL VARIABLE #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

otherCountry <- "Others"
frameworkContract <- c("FP6", "FP7", "H2020")
# ---------------------------------------------------------------------------------------------------------------------------------------------
# II. LOAD DATASETS #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

#set current directory
setwd("C:/Users/vandeloc/Documents/Documents/08 Cordis/Data cleaning Visualisation 1")
setwd("C:/Users/bruled/Documents/Pwc Project/2 - Project/DataVisualisation/Visualisation/1 - Cordis/2")


### Load datasets
# FP6: https://data.europa.eu/euodp/data/dataset/cordisfp6projects
# FP7: https://data.europa.eu/euodp/data/dataset/cordisfp7projects
# H2020: https://data.europa.eu/euodp/data/dataset/cordisH2020projects
# Population: http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=demo_pjan&lang=en
# GDP: http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=naida_10_gdp&lang=en
Dataset_FP6Organizations = read.csv("Input/cordis-fp6organizations.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")

Dataset_FP7Organizations = read.csv("Input/cordis-fp7organizations.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")

Dataset_H2020Organizations = read.csv("Input/cordis-h2020organizations.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")

Dataset_Countries = read.csv("Input/Countries.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")
listCountries <- sort(Dataset_Countries$euCode)

Dataset_population  = read.csv("Input/demo_pjan_Data.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, comment.char="")
Dataset_GDP  = read.csv("Input/naida_10_gdp_Data.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, comment.char="")



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
func_BuildLink <- function(frameworkContract, dataset) {
  
  dataset <-aggregate(dataset$country, 
                      by=list(dataset$projectReference),
                      FUN=paste)
  dataset <- plyr::rename(dataset,c("Group.1"="Project",
                                    "x" = "ListCountries"))

  dataset$ListCountries <- sapply(dataset$ListCountries, func_order)
  dataset$size <- sapply(dataset$ListCountries, func_size)
  dataset<- dataset[dataset$size > 1,]  
  dataset <- subset(dataset, select=-c(size, Project))

  
  dataset$ListCountries <- sapply(dataset$ListCountries, func_createPair)

  ##################################################
  data <- list()
  for (i in 1:nrow(dataset)) {
    data[[i]] =func_createFrame(dataset[i,])
  }
  tmp <- do.call(rbind, data)
  ############################################################
  output <-data.frame(tmp)
  output<-aggregate(output$X1,
                    by=list(output$X1, output$X2),
                    FUN=length)
  output <-data.frame(output)
  
  output <- plyr::rename(output,c("Group.1" = "country1",
                                  "Group.2" = "country2",
                                  "x" = "nbLinkTmp"))
  
  tmp <-rep(frameworkContract,length(output$Group.1))
  output["frameworkContract"]<- tmp

  output[output$country1=="BE", ]
  return(output)
}

func_addWeight <- function (dataset) {
  dataset <- Dataset_link
  maxFP6 <- max(dataset[dataset$frameworkContract == "FP6",]$nbLink)
  maxFP7 <- max(dataset[dataset$frameworkContract == "FP7",]$nbLink)
  maxH2020 <- max(dataset[dataset$frameworkContract == "H2020",]$nbLink)
  dataset$weight <- c(rep(0, length(dataset$country1)))
  
  dataset[dataset$frameworkContract == "FP6",]$weight <-dataset[dataset$frameworkContract == "FP6",]$nbLink / maxFP6 
  dataset[dataset$frameworkContract == "FP7",]$weight <-dataset[dataset$frameworkContract == "FP7",]$nbLink / maxFP7
  dataset[dataset$frameworkContract == "H2020",]$weight <-dataset[dataset$frameworkContract == "H2020",]$nbLink / maxH2020 
  return(dataset)  
}



# ---------------------------------------------------------------------------------------------------------------------------------------------
# IV. FUNCTION ADDITIONAL INFORMATION #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------


func_nbProjectPerCountryParticipation <- function(dataset, frameworkContract)
{
  dataset <- dataset[dataset$role == "participant", ]
  dataset <-aggregate(dataset$country,
                      by=list(dataset$country),
                      FUN=length)
  dataset <- plyr::rename(dataset,c("Group.1"="Country",
                                    "x" = "Number of project participants"))
  tmp <-rep(frameworkContract,length(dataset$Country))
  dataset["frameworkContract"]<- tmp
  
  return(dataset)  
}

func_nbProjectPerCountryCoordination <- function(dataset, frameworkContract)
{
  dataset <- dataset[dataset$role == "coordinator", ]
  dataset <-aggregate(dataset$country,
                      by=list(dataset$country),
                      FUN=length)
  dataset <- plyr::rename(dataset,c("Group.1"="Country",
                                    "x" = "Number of project coordinators"))
  
  tmp <-rep(frameworkContract,length(dataset$Country))
  dataset["frameworkContract"]<- tmp
  return(dataset)  
}

func_nbOverallProject <- function(dataset, frameworkContract)
{
  dataset <-aggregate(dataset$country,
                      by=list(dataset$country),
                      FUN=length)
  dataset <- plyr::rename(dataset,c("Group.1"="Country",
                                    "x" = "Overall Number of Project"))
  tmp <-rep(frameworkContract,length(dataset$Country))
  dataset["frameworkContract"]<- tmp
  return(dataset)  
}

func_nbInstitution <- function(dataset, frameworkContract)
{
  dataset <-aggregate(dataset$name, 
                      by=list(dataset$country),
                      FUN=paste)
  dataset$x <- sapply(dataset$x, unique)
  dataset$x <- sapply(dataset$x, length)
  dataset <- plyr::rename(dataset,c("Group.1"="Country",
                                    "x" = "Number of institutions"))
  tmp <-rep(frameworkContract,length(dataset$Country))
  dataset["frameworkContract"]<- tmp
  return(dataset)  
}
  

func_projPerCountry <- function() {
  dataset_FP6Participation <- func_nbProjectPerCountryParticipation(Dataset_FP6Organizations, "FP6")
  dataset_FP7Participation <- func_nbProjectPerCountryParticipation(Dataset_FP7Organizations, "FP7")
  dataset_H2020Participation <- func_nbProjectPerCountryParticipation(Dataset_H2020Organizations, "H2020")
  dataset_Participation <- rbind(rbind(dataset_FP6Participation, dataset_FP7Participation), dataset_H2020Participation)
  
  dataset_FP6Coordination <- func_nbProjectPerCountryCoordination(Dataset_FP6Organizations, "FP6")
  dataset_FP7Coordination <- func_nbProjectPerCountryCoordination(Dataset_FP7Organizations, "FP7")
  dataset_H2020Coordination <- func_nbProjectPerCountryCoordination(Dataset_H2020Organizations, "H2020")
  dataset_Coordination <- rbind(rbind(dataset_FP6Coordination, dataset_FP7Coordination), dataset_H2020Coordination)
  
  dataset_FP6OverallProject <- func_nbOverallProject(Dataset_FP6Organizations, "FP6")
  dataset_FP7OverallProject <- func_nbOverallProject(Dataset_FP7Organizations, "FP7")
  dataset_H2020OverallProject <- func_nbOverallProject(Dataset_H2020Organizations, "H2020")
  dataset_OverallProject <- rbind(rbind(dataset_FP6OverallProject, dataset_FP7OverallProject), dataset_H2020OverallProject)
  
  dataset_FP6NbInstitution <- func_nbInstitution(Dataset_FP6Organizations, "FP6")
  dataset_FP7NbInstitution <- func_nbInstitution(Dataset_FP7Organizations, "FP7")
  dataset_H2020NbInstitution <- func_nbInstitution(Dataset_H2020Organizations, "H2020")
  dataset_NbInstitution <- rbind(rbind(dataset_FP6NbInstitution, dataset_FP7NbInstitution), dataset_H2020NbInstitution)
  
  outputDataset<-merge(dataset_Participation, dataset_Coordination, by.x=c("Country", "frameworkContract"), by.y=c("Country", "frameworkContract"), all.x=TRUE)
  outputDataset<-merge(outputDataset, dataset_OverallProject, by.x=c("Country", "frameworkContract"), by.y=c("Country", "frameworkContract"), all.x=TRUE)
  outputDataset<-merge(outputDataset, dataset_NbInstitution, by.x=c("Country", "frameworkContract"), by.y=c("Country", "frameworkContract"), all.x=TRUE)
  outputDataset<-merge(outputDataset, Dataset_Countries, by.x=c("Country"), by.y=c("euCode"), all.x=TRUE)
  outputDataset <- outputDataset[!is.na(outputDataset$name),]    
  outputDataset<-merge(outputDataset, Dataset_GDP, by.x=c("Country"), by.y=c("GEO"), all.x=TRUE)
  outputDataset<-merge(outputDataset, Dataset_population, by.x=c("Country"), by.y=c("GEO"), all.x=TRUE)
  outputDataset <- subset(outputDataset, select=-c(FP6participantCountries, FP7participantCountries, FP8participantCountries,
                                                   TIME.x, TIME.y, UNIT.x, UNIT.y, NA_ITEM, Flag.and.Footnotes.x, Flag.and.Footnotes.y,
                                                   AGE, SEX))
  outputDataset <- plyr::rename(outputDataset,c("Value.x"="GDP",
                                                "Value.y" = "Population"))
  
  outputDataset$GDP <- ifelse(outputDataset$GDP == ":", NA, outputDataset$GDP)
  outputDataset$GDP <-gsub(" ","",paste(outputDataset$GDP))
  outputDataset$Population <-gsub(" ","",paste(outputDataset$Population))
  
  outputDataset$GDP <-as.numeric(outputDataset$GDP)
  outputDataset$Population <-as.numeric(outputDataset$Population)

  outputDataset$GDPPerCapita <- as.numeric(outputDataset$GDP) / as.numeric(outputDataset$Population) * 1000000
  
  return (outputDataset)
    
  }


# ---------------------------------------------------------------------------------------------------------------------------------------------
# V. Build Dataset #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------


Sys.time()
FP6_link <- func_BuildLink("FP6", Dataset_FP6Organizations)
FP6_link$frameworkContract<- "FP6"

Sys.time()
FP7_link <- func_BuildLink("FP7", Dataset_FP7Organizations)
FP7_link$frameworkContract<- "FP7"

Sys.time()
H2020_link <- func_BuildLink("H2020", Dataset_H2020Organizations)
H2020_link$frameworkContract<- "H2020"
Sys.time()

Dataset_link <-rbind(rbind(FP6_link, FP7_link), H2020_link)

Dataset_link <- func_addWeight(Dataset_link)


options(scipen = 10)
write.table(Dataset_link, "output/NbLink.csv", sep = ";")
options(scipen = 0)


write.table(func_projPerCountry(), "output/CountryInformation.csv", sep = ";")




