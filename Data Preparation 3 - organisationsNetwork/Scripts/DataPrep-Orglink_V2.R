# the goal of this V2 is to create the complete network at distancex 1 for a precise organisation 

# ---------------------------------------------------------------------------------------------------------------------------------------------
# I. LOAD DATASETS #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

#set current directory
setwd("C:/Users/vandeloc/Documents/Documents/08 Cordis/Data cleaning Visualisation 1")
setwd("C:/Users/bruled/Documents/Pwc Project/2 - Project/DataVisualisation/Visualisation/1 - Cordis/data-visualisation-pilot/Data cleaning Visualisation 3")

### Load datasets
# H2020: https://data.europa.eu/euodp/data/dataset/cordisH2020projects

Dataset_H2020Organizations = read.csv("../Input/cordis-h2020organizations.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")





# ---------------------------------------------------------------------------------------------------------------------------------------------
# II. FUNCTIONS LINK #################################################################################################################
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

func_BuildLinkForOneOrg <- function(dataset, orgName){
  #dataset <- Dataset_H2020Organizations
  #dataset <- head(dataset, n=5001)
  #orgName <- "JOECKEL BERNHARD"
  #orgName <- "UNIVERSITY OF PLYMOUTH"
  #orgName <- "GUIDANCE NAVIGATION HOLDINGS LTD"
  #orgName <- "CENTRE NATIONAL DE LA RECHERCHE SCIENTIFIQUE CNRS"

  listProject <- dataset[dataset$name == orgName, ]$projectReference
  listProject <- unique(listProject)
  
  listOrg  <- dataset[dataset$projectReference %in% listProject,]$name
  listOrg <- unique(listOrg)
  
  datasetTmp <- dataset[dataset$name %in% listOrg,]
  datasetTmp <- subset(datasetTmp, select=c(projectReference, name))
  datasetTmp <-aggregate(datasetTmp$name, 
                      by=list(datasetTmp$projectReference),
                      FUN=paste, simplify = FALSE)
  datasetTmp <- plyr::rename(datasetTmp,c("Group.1"="Project",
                                    "x" = "Organisations"))

  datasetTmp$Organisations <- sapply(datasetTmp$Organisations, func_order, simplify = FALSE)
  datasetTmp$size <- sapply(datasetTmp$Organisations, func_size)
  datasetTmp<- datasetTmp[datasetTmp$size > 1,]  
  datasetTmp <- subset(datasetTmp, select=-c(size))
  
  if (nrow(datasetTmp) == 0){
    output <- data.frame(Org1 = character(),
                         Org2 = character(),
                         nbLink = integer(), 
                         org = character())  
    
  }

  if (nrow(datasetTmp) != 0){
    datasetTmp$Organisations <- sapply(datasetTmp$Organisations, func_createPair, simplify = FALSE)
    data <- list()
    for (i in 1:nrow(datasetTmp)) {
      data[[i]] =func_createFrame(datasetTmp[i,]$Organisations)
    }
    
    tmp <- do.call(rbind, data)
    output <-data.frame(tmp)
    output<-aggregate(output$X1,
                      by=list(output$X1, output$X2),
                      FUN=length)
    output <-data.frame(output)
    output <- plyr::rename(output,c("Group.1" = "Org1",
                                    "Group.2" = "Org2",
                                    "x" = "nbLink"))
    output$Org <- orgName
  }
  
  return (output) 
}

# build a dataset containing all the interactions between the different countries  
func_BuildLink <- function(dataset) {
  #dataset <- Dataset_H2020Organizations
  #dataset <- head(dataset, n=20000)
  
  listOrg <- unique(dataset$name)
  data <- list()

  for (i in 1:length(listOrg)) {
    data[[i]] =func_BuildLinkForOneOrg(dataset, listOrg[i])
    print(i)
  }

  tmp <- do.call(rbind, data)
  
  output <-data.frame(tmp)
  return(output)
}


# ---------------------------------------------------------------------------------------------------------------------------------------------
# IV. FUNCTION ADDITIONAL INFORMATION #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

func_FindLink <- function(name, datasetLink){
  link <- rbind(datasetLink[datasetLink$Org1 == name,], datasetLink[datasetLink$Org2 == name,])
  link$Org1 <- as.character(link$Org1)
  link$Org2 <- as.character(link$Org2)
  return(link)
}

func_createLink2Levels <- function(name, datasetLink){
  #to remove 
  # name <- "INTEL BENELUX BV"
  # datasetLink <- Dataset_link
  #
  
  #find all org link to organisation 1 at level 2 
  linkLevel1 <- rbind(datasetLink[datasetLink$Org1 == name,], datasetLink[datasetLink$Org2 == name,])
  if (length(linkLevel1$Org1) == 0)
  {
    linkLevel1 <- data.frame(name,name,1)
    linkLevel1 <- plyr::rename(linkLevel1,c("name"="Org1",
                                      "name.1" = "Org2",
                                      "X1" = "nbLinkTmp"))
    
  }
  
  linkLevel1$Org <- name
  return(linkLevel1)
}

func_BuildLinkPerOrg <- function(datasetLink, datasetOrg){
  # #to remove 
  # datasetLink <- Dataset_link
  # datasetOrg <- Dataset_H2020Organizations
  # datasetOrg <-  head(datasetOrg, n=5000)
  # ##
  
  listOrg <- unique(datasetOrg$name)
  output <- data.frame(Org1=character(),
                       Org2=character(),
                       nbLinkTmp=character(),
                       Org=character(),
                       stringsAsFactors=FALSE) 
  for (i in 1:length(listOrg)){
    name <- listOrg[i]
    output <- rbind(output,func_createLink2Levels(name, datasetLink))
  }
  return(output)
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
Dataset_link <- func_BuildLink(Dataset_H2020Organizations)
Sys.time()
Dataset_link <- func_BuildLinkPerOrg(Dataset_link, Dataset_H2020Organizations)
Sys.time()

options(scipen = 10)
write.table(Dataset_link, "output/NbLinkOrgLevel1.csv", sep = ";", quote = FALSE)
options(scipen = 0)

Dataset_ListOrg <- func_ListOrg()
write.table(Dataset_ListOrg, "output/OrgInformation.csv", sep = ";", quote = FALSE)



