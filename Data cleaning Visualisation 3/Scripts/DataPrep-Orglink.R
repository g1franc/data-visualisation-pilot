# ---------------------------------------------------------------------------------------------------------------------------------------------
# I. LOAD DATASETS #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

#set current directory
setwd("C:/Users/vandeloc/Documents/Documents/08 Cordis/Data cleaning Visualisation 1")
setwd("C:/Users/bruled/Documents/Pwc Project/2 - Project/DataVisualisation/Visualisation/1 - Cordis/data-visualisation-pilot/Data cleaning Visualisation 3")

### Load datasets
# H2020: https://data.europa.eu/euodp/data/dataset/cordisH2020projects

Dataset_H2020Organizations = read.csv("Input/cordis-h2020organizations.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")





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

# build a dataset containing all the interactions between the different countries  
func_BuildLink <- function(dataset) {
  
  #dataset <- head(dataset, n=5000)
  
  
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
  
  # #character improve 
  # linkLevel1$Org1 <- as.character(linkLevel1$Org1)
  # linkLevel1$Org2 <- as.character(linkLevel1$Org2)
  # 
  # #find all organisation at level 1 
  # listOrgLevel2 <- unique(c(linkLevel1$Org1, linkLevel1$Org2))  
  # listOrgLevel2 <- listOrgLevel2[listOrgLevel2 != name]
  # 
  # #for each of the organisation at level 1 find org linked 
  # output <- data.frame(Org1=character(),
  #                      Org2=character(),
  #                      nbLinkTmp=character(),
  #                      stringsAsFactors=FALSE) 
  # for (i in 1:length(listOrgLevel2)){
  #   nameLvl2 <- listOrgLevel2[i]
  #   output <- rbind(output, func_FindLink(nameLvl2, datasetLink))
  # }
  # 
  # # remove name of the orginal org to avoid duplicate 
  # output <- output[output$Org1 != name,]
  # output <- output[output$Org2 != name,]
  # output <- rbind(output, linkLevel1)
  # output$Org <- name
  # return(output)
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



