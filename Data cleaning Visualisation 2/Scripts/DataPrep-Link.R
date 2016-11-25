
library(plyr)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# I. GLOBAL VARIABLE #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

otherCountry <- "Others"
frameworkContract <- c("FP6", "FP7", "H2020")
# ---------------------------------------------------------------------------------------------------------------------------------------------
# II. LOAD DATASETS #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

# #set current directory
# setwd("C:/Users/vandeloc/Documents/Documents/08 Cordis/Data cleaning Visualisation 1")
# setwd("C:/Users/bruled/Documents/Pwc Project/2 - Project/DataVisualisation/Visualisation/1 - Cordis/2")


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

listCountries <- Dataset_Countries[Dataset_Countries$Selected == TRUE,]$euCode
listCountries <-c(listCountries, otherCountry)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# III. FUNCTIONS #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

# check if a country is part of "europe", if not return otherCountry
func_ComputeCountryName <- function(countryName){
  if(!is.element(countryName, listCountries)) {
    countryName <- otherCountry
  }
  return(countryName)
}


#build a dataset that contain all combinations of framworkContract|country of origin|country of destination|Nb links
func_BuildDataset_Link <- function() {
  sizeLinkDataset <- factorial(length(listCountries)+1)/(factorial(2)*factorial(length(listCountries)+1-2)) * length(frameworkContract)
  Dataset_link <- data.frame(frameworkContract= rep("",sizeLinkDataset),
                             country1 =  rep("",sizeLinkDataset),
                             country2 =  rep("",sizeLinkDataset),
                             nbLink = rep(0,sizeLinkDataset))
  Dataset_link$frameworkContract <- as.character(Dataset_link$frameworkContract)
  Dataset_link$country1 <- as.character(Dataset_link$country1)
  Dataset_link$country2 <- as.character(Dataset_link$country2)
  Dataset_link$nbLink <- as.numeric(Dataset_link$nbLink)
  
  currentRow <- 1
  for (i in frameworkContract)
  {
    frameworkContract <- i
    for(j in 1:length(listCountries)) {
      country1 <- listCountries[j]
      for(k in j:length(listCountries)) {
        country2 <- listCountries[k]
        Dataset_link[currentRow,]$frameworkContract <- frameworkContract
        Dataset_link[currentRow,]$country1 <- country1
        Dataset_link[currentRow,]$country2 <- country2
        currentRow <- currentRow + 1
      }
    }
  }
  return(Dataset_link)
}

# build a dataset containing all the interactions between the different countries  
func_BuildLink <- function(frameworkContract, dataset, outputDataset) {
  #get the list of country per project 
  dataset <-aggregate(dataset$country, 
                   by=list(dataset$projectReference), 
                   FUN=paste)
  dataset <- plyr::rename(dataset,c("Group.1"="Project",
                                    "x" = "ListCountries"))
  #for each project 
  for(i in 1:nrow(dataset)) {
    countries <- unlist(dataset[i,]$ListCountries)
    #if there is less than one country do nothing 
    if (length(countries) > 1) {
      #for non EU country put name as othe r
      countries <- sapply(countries, func_ComputeCountryName)
      #create all combination of country 
      combineCountries <-t(combn(unlist(countries), 2))
      combineCountries <- as.data.frame(cbind(frameworkContract, combineCountries, 1))
      #get the number of link between each pair of country 
      combineCountries <- aggregate(combineCountries$V4, 
                                    by=list(combineCountries$frameworkContract,
                                            combineCountries$V2,
                                            combineCountries$V3), 
                          FUN=length)
      combineCountries <- plyr::rename(combineCountries,c("Group.1" = "frameworkContract",
                                                          "Group.2" = "country1",
                                                          "Group.3" = "country2",
                                                          "x" = "nbLinkTmp"))
      #add this project to the output dataset 
      outputDataset<-merge(outputDataset, combineCountries, 
                        by.x=c("frameworkContract","country1","country2"),
                        by.y=c("frameworkContract","country1","country2"),
                        all.x=TRUE)
      outputDataset[is.na(outputDataset)]<- 0  
      outputDataset$nbLink <-outputDataset$nbLink+ outputDataset$nbLinkTmp 
      outputDataset <- subset(outputDataset, select=-c(nbLinkTmp))
      }
  }
  return(outputDataset)
}


# ---------------------------------------------------------------------------------------------------------------------------------------------
# IV. Build Dataset #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

Dataset_link <-  func_BuildDataset_Link()

Dataset_link <- func_BuildLink("FP6", Dataset_FP6Organizations, Dataset_link)
Dataset_link <- func_BuildLink("FP7", Dataset_FP7Organizations, Dataset_link)
Dataset_link <- func_BuildLink("H2020", Dataset_H2020Organizations, Dataset_link)
write.table(Dataset_link, "Output/NbLink.txt")

write.json(Dataset_link, "Output/NbLink.txt")
