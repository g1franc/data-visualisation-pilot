#install.packages("tm")
require(XML)
require(tm)
# ---------------------------------------------------------------------------------------------------------------------------------------------
# I. LOAD DATA ################################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------


setwd("C:/Users/bruled/Documents/Pwc Project/2 - Project/DataVisualisation/Visualisation/1 - Cordis/data-visualisation-pilot/TransparencyRegister/Map")

xmlfile <-  xmlTreeParse("../Datasets/full_export_new.xml", useInternalNodes = TRUE)


# ---------------------------------------------------------------------------------------------------------------------------------------------
# II. TRANSFORM DATA FROM XML TO DATA.FRAME ###################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

func_parseInterestRepresentative <- function(xmlNode){
  # xmlNode <- rootNode[["resultList"]][i][[1]]
  country <- xmlValue(xmlNode[["contactDetails"]][["country"]])
  cat <- xmlValue(xmlNode[["category"]][["mainCategory"]])
  subCat <- xmlValue(xmlNode[["category"]][["subCategory"]])
  goal <-   xmlValue(xmlNode[["goals"]])
  activity <- xmlValue(xmlNode[["activities"]][["activityEuLegislative"]])
  listInterest <-xmlSApply(xmlNode[["interests"]], function(x) xmlSApply(x, xmlValue))
  inter <- paste(listInterest, collapse=";")
  output <- data.frame(countries = character(),
                       categories = character(),
                       subCategories = character(), 
                       goals = character(),
                       activities = character(),
                       interest = character())
  output<-rbind(output, data.frame(countries = country,
                                   categories = cat, 
                                   subCategories = subCat, 
                                   goals = goal, 
                                   activities = activity,
                                   interest = inter)) 
  return(output)
}

rootNode = xmlRoot(xmlfile)
data <- list()
for (i in 1:xmlSize(rootNode[["resultList"]])){
  data[[i]] =func_parseInterestRepresentative(rootNode[["resultList"]][i][[1]])
}

tmp <- do.call(rbind, data)
dataset <- data.frame(tmp)

#clean 
remove(i)
remove(data)
remove(rootNode)
remove(xmlfile)
remove(tmp)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# III. BUILD DATA.FRAME FOR MAP ###############################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------


func_BuildDatasetForMapBasedOnFilterCategories <- function(data, filterValue){
  #data <- dataset
  #filterValue <- listCategory[1]
  data <- data[data$categories == filterValue, ]
  output <-aggregate(data$countries, 
                     by=list(data$countries),
                     FUN=length, simplify = FALSE)
  output <- plyr::rename(output,c("Group.1"="Country",
                                  "x" = as.character(filterValue)))
  return (output)
}

func_BuildDatasetForMapBasedOnFilterSubCategories <- function(data, filterValue){
  data <- data[data$subCategories == filterValue, ]
  output <-aggregate(data$countries, 
                     by=list(data$countries),
                     FUN=length, simplify = FALSE)
  output <- plyr::rename(output,c("Group.1"="Country",
                                  "x" = as.character(filterValue)))
  return (output)
}

func_BuildDatasetForMapBasedOnFilterInterest <- function(data, filterValue){
  #data <- dataset
  #filterValue <- "Communication"
  data <- data[grep(filterValue, data$interest), ]
  output <-aggregate(data$countries, 
                     by=list(data$countries),
                     FUN=length, simplify = FALSE)
  output <- plyr::rename(output,c("Group.1"="Country",
                                  "x" = as.character(filterValue)))
  return (output)
}


listCategory <- unique(dataset$categories)
listSubCategory <- unique(dataset$subCategories)
listInterst <- paste(dataset$interest, collapse = ';')
listInterst <- strsplit(listInterst, ";")[[1]]
listInterst <- unique(listInterst)
         

datasetMap <-aggregate(dataset$countries, 
                       by=list(dataset$countries),
                       FUN=length, simplify = FALSE)
datasetMap <- plyr::rename(datasetMap,c("Group.1"="Country",
                                        "x" = "all"))



data <- list()
i<-1
for (j in 1:length(listCategory)){
  data[[i]] = func_BuildDatasetForMapBasedOnFilterCategories(dataset, listCategory[j])
  i<- i+1
}
for (j in 1:length(listSubCategory)){
  data[[i]] = func_BuildDatasetForMapBasedOnFilterSubCategories(dataset, listSubCategory[j])
  i<- i+1
}
for (j in 1:length(listInterst)){
  data[[i]] = func_BuildDatasetForMapBasedOnFilterInterest(dataset, listInterst[j])
  i<- i+1
}
for (i in 1:length(data)){
  datasetMap <- merge(datasetMap, data[i], by.x=c("Country"), by.y=c("Country"), all.x=TRUE)
  
}
datasetMap <- data.frame(lapply(datasetMap, as.character), stringsAsFactors=FALSE)
write.table(datasetMap, "../Datasets/datasetMap.csv", sep = ";", quote = FALSE, row.names = FALSE)

remove(data)
remove(i)
remove(j)
remove(listCategory)
remove(listSubCategory)
remove(listInterst)



# ---------------------------------------------------------------------------------------------------------------------------------------------
# III. BUILD DATA.FRAME FOR SANKEY ############################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

func_BuildDatasetForSankeyBasedOnFilterInterest <- function(data, filterValue){
  #data <- dataset
  #filterValue <- "Communication"
  data <- data[grep(filterValue, data$interest), ]
  data$interest <- filterValue
  output <-aggregate(data$subCategories, 
                     by=list(data$subCategories, data$interest),
                     FUN=length, simplify = FALSE)
  output$Group.2 <- filterValue
  return (output)
}

datasetSankey1 <-aggregate(dataset$categories, 
                       by=list(dataset$categories, dataset$subCategories),
                       FUN=length, simplify = FALSE)

listInterst <- paste(dataset$interest, collapse = ';')
listInterst <- strsplit(listInterst, ";")[[1]]
listInterst <- unique(listInterst)
data <- list()
for (i in 1:length(listInterst)){
  data[[i]] = func_BuildDatasetForSankeyBasedOnFilterInterest(dataset, listInterst[i])
}
tmp <- do.call(rbind, data)
datasetSankey2 <- data.frame(tmp)


datasetSankey <- rbind(datasetSankey1, datasetSankey2)
datasetSankey <- plyr::rename(datasetSankey,c("Group.1"="From",
                                              "Group.2" = "To"))

datasetSankey <- data.frame(lapply(datasetSankey, as.character), stringsAsFactors=FALSE)
write.table(datasetSankey, "../Datasets/datasetSankey.csv", sep = ";", quote = FALSE, row.names = FALSE)

remove(data)
remove(listInterst)
remove(datasetSankey1)
remove(datasetSankey2)
remove(tmp)
remove(i)


# ---------------------------------------------------------------------------------------------------------------------------------------------
# III. BUILD DATA.FRAME FOR WORDCLOUD #########################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

func_CleanString <- function(data){
  docs <- Corpus(VectorSource(data) )
  docs <- tm_map(docs, removePunctuation)   
  docs <- tm_map(docs, removeNumbers)  
  docs <- tm_map(docs, tolower) 
  docs <- tm_map(docs, removeWords, c(stopwords("english"), 
                                      stopwords("danish"),
                                      stopwords("dutch"), 
                                      stopwords("finnish"), 
                                      stopwords("french"), 
                                      stopwords("german"), 
                                      stopwords("hungarian"), 
                                      stopwords("italian"),
                                      stopwords("norwegian"), 
                                      stopwords("portuguese"), 
                                      stopwords("spanish"), 
                                      stopwords("norwegian"), 
                                      stopwords("swedish"))) 
}
listGoal <- paste(dataset$goals, collapse = ' ')
listActivity <- paste(dataset$activities, collapse = ' ')
listGoal <- func_CleanString(listGoal)
listActivity <- func_CleanString(listActivity)

writeLines(as.character(listGoal), con="../Datasets/goals.txt")
writeLines(as.character(listActivity), con="../Datasets/goals.txt")
                              