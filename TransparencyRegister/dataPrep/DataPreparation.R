#RWeka does not work in JAVA_HOME environment variable is set, empty it for this R session
if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(RWeka)

install.packages("XML")
install.packages("tm")
install.packages("RJSONIO")
install.packages("plyr")
install.packages("RWeka")
require(XML)
require(tm)
require(RJSONIO)
require(plyr)
require(tools)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# I. LOAD DATA ################################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

getwd()
setwd("C:/Users/vandeloc/Documents/Documents/Cordis/GitHub/TransparencyRegister/Data Preparation")
setwd("C:/Users/bruled/Documents/Pwc Project/2 - Project/DataVisualisation/Visualisation/data-visualisation-pilot/TransparencyRegister/Data Preparation")


download.file("http://ec.europa.eu/transparencyregister/public/consultation/statistics.do?action=getLobbyistsXml&fileType=NEW", destfile = "../Datasets/full_export_new.xml")

xmlfile <-  xmlTreeParse("../Datasets/full_export_new.xml", useInternalNodes = TRUE)
countries = read.csv("../Datasets/Countries.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")


# ---------------------------------------------------------------------------------------------------------------------------------------------
# II. TRANSFORM DATA FROM XML TO DATA.FRAME ###################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

func_parseXMLNodeInterestRepresentative <- function(xmlNode){
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
  data[[i]] =func_parseXMLNodeInterestRepresentative(rootNode[["resultList"]][i][[1]])
}

tmp <- do.call(rbind, data)
dataset <- data.frame(tmp)

#remove useless variables 
remove(i)
remove(data)
remove(rootNode)
remove(xmlfile)
remove(tmp)
remove(func_parseXMLNodeInterestRepresentative)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# III. BUILD DATA.FRAME FOR MAP ###############################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

func_BuildDatasetForMapBasedOnFilterCategories <- function(data, filterValue){
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


listCategory <- unique(dataset$categories)
listSubCategory <- unique(dataset$subCategories)


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


for (i in 1:length(data)){
  datasetMap <- merge(datasetMap, data[i], by.x=c("Country"), by.y=c("Country"), all.x=TRUE)
}
datasetMap <- subset(datasetMap, select=-c(all))
datasetMap[is.na(datasetMap)] <- 0


countries$name <- toupper(countries$name)
countries <- subset (countries, select =c(name, EU28))
datasetMap <- merge(datasetMap, countries, by.x=c("Country"), by.y=c("name"), all.x=TRUE)
datasetMap <- datasetMap[datasetMap$EU28 == TRUE,]
datasetMap <- datasetMap[!is.na(datasetMap$Country),]


datasetMap <- data.frame(lapply(datasetMap, as.character), stringsAsFactors=FALSE)

countryList <- as.data.frame(datasetMap[,1])
countryList <- plyr::rename(countryList,c("datasetMap[, 1]"="Country"))
datasetMap <- subset(datasetMap, select=-c(EU28))
datasetMap$Country <- tolower(datasetMap$Country)
datasetMap$Country <- toTitleCase(datasetMap$Country)

#write info in file
writeData <- function(data) {
  for (i in 1:(nrow(datasetMap)-1)) {
    
    cat('"', file = "../Datasets/datasetMap.js", append = TRUE)
    cat(datasetMap$Country[i], file = "../Datasets/datasetMap.js", append = TRUE)
    cat('": { \n', file = "../Datasets/datasetMap.js", append = TRUE)
    
    for (j in 2:(length(datasetMap)-1)) {
      cat('"', file = "../Datasets/datasetMap.js", append = TRUE)
      cat(colnames(data)[j], file = "../Datasets/datasetMap.js", append = TRUE)
      cat('": ', file = "../Datasets/datasetMap.js", append = TRUE)
      cat(data[i,j], file = "../Datasets/datasetMap.js", append = TRUE)
      cat(", \n", file = "../Datasets/datasetMap.js", append = TRUE)
    }
    cat('"', file = "../Datasets/datasetMap.js", append = TRUE)
    cat(colnames(data)[length(datasetMap)], file = "../Datasets/datasetMap.js", append = TRUE)
    cat('": ', file = "../Datasets/datasetMap.js", append = TRUE)
    cat(data[i, length(datasetMap)], file = "../Datasets/datasetMap.js", append = TRUE)
    cat("}, \n", file = "../Datasets/datasetMap.js", append = TRUE)
    
  }
  cat('"', file = "../Datasets/datasetMap.js", append = TRUE)
  cat(datasetMap$Country[nrow(datasetMap)], file = "../Datasets/datasetMap.js", append = TRUE)
  cat('": { \n', file = "../Datasets/datasetMap.js", append = TRUE)
  
  for (j in 2:(length(datasetMap)-1)) {
    cat('"', file = "../Datasets/datasetMap.js", append = TRUE)
    cat(colnames(data)[j], file = "../Datasets/datasetMap.js", append = TRUE)
    cat('": ', file = "../Datasets/datasetMap.js", append = TRUE)
    cat(data[nrow(datasetMap), j], file = "../Datasets/datasetMap.js", append = TRUE)
    cat(", \n", file = "../Datasets/datasetMap.js", append = TRUE)
  }
  cat('"', file = "../Datasets/datasetMap.js", append = TRUE)
  cat(colnames(data)[length(datasetMap)], file = "../Datasets/datasetMap.js", append = TRUE)
  cat('": ', file = "../Datasets/datasetMap.js", append = TRUE)
  cat(data[nrow(datasetMap),length(datasetMap)], file = "../Datasets/datasetMap.js", append = TRUE)
  cat("} \n };", file = "../Datasets/datasetMap.js", append = TRUE)
  
}

cat('var categoriesData = { \n', file = "../Datasets/datasetMap.js")
writeData(datasetMap)

remove(data)
remove(i)
remove(j)
remove(listCategory)
remove(listSubCategory)
remove(countryList)
remove(datasetMap)
remove(func_BuildDatasetForMapBasedOnFilterCategories)
remove(func_BuildDatasetForMapBasedOnFilterSubCategories)
remove(writeData)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# VI. BUILD DATA.FRAME FOR BUBBLE #############################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
listInterest <- paste(dataset$interest, collapse = ';')
listInterest <- strsplit(listInterest, ";")[[1]]
listInterest <- as.data.frame(listInterest)
listInterest <- 
  aggregate(listInterest$listInterest, 
            by=list(listInterest$listInterest), 
            FUN=length)
listInterest <- plyr::rename(listInterest,c("Group.1"="name", 
                                            "x"="count"))
listInterest$name <- as.character(listInterest$name)
listInterest[listInterest$name=="Agriculture and Rural Development",]$name <- "Agriculture"
listInterest[listInterest$name=="Financial Stability, Financial Services and Capital Markets Union ",]$name <- "Finance"
listInterest[listInterest$name=="Fisheries and Aquaculture",]$name <- "Fisheries" 
listInterest[listInterest$name=="Foreign and Security Policy and Defence",]$name <- "Security Policy"
listInterest[listInterest$name=="General and Institutional Affairs",]$name <- "General Affairs"
listInterest[listInterest$name=="Justice and Fundamental Rights",]$name <- "Justice"


write.table(listInterest, "../Datasets/BubbleChartData.txt", sep = ",", quote = FALSE, row.names = FALSE)

remove(listInterest)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# V. BUILD DATA.FRAME FOR WORDCLOUD ###########################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

func_CleanString <- function(data){
  docs <- VCorpus(VectorSource(data) )
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
  docs <- tm_map(docs, stripWhitespace)
  docs <- tm_map(docs, PlainTextDocument)
}

# aggregate all goals and activities 
listActivity <- paste(dataset$activities, collapse = ' ')

# remove punctuation, number, put everything to lower, remove stop words and white space
listActivity <- func_CleanString(listActivity)

# # create document-term matrix (standard with frequency)
# dtm_activity <- DocumentTermMatrix(listActivity, control=list(wordLengths=c(3,Inf)))

# create two-gram document-term matrix (standard with frequency)
TwogramTokenizer <- function(x) NGramTokenizer(x,
                                              Weka_control(min = 2, max = 2))
dtm_activity_twogram <- DocumentTermMatrix(listActivity, control = list(tokenize = TwogramTokenizer))

# Generate list of 100 most frequent words
freq_activity_twogram <- head(sort(colSums(as.matrix(dtm_activity_twogram)), decreasing=TRUE),100)

#Multiply frequencies for low frequency files to enlarge visualisation
freq_activity_twogram_multiplied <- freq_activity_twogram * 8


# Build JSON

# JSON_activity_twogram <- toJSON(freq_activity_twogram)
JSON_activity_twogram_multiplied <- toJSON(freq_activity_twogram_multiplied)


#write info in file
cat('var data = { \n', file = "../Datasets/wordCountMapping_activity_twogram_multiplied_100.js")
cat('wordCountMapping_activity_two: \n', file = "../Datasets/wordCountMapping_activity_twogram_multiplied_100.js", append = TRUE)
cat(JSON_activity_twogram_multiplied, file = "../Datasets/wordCountMapping_activity_twogram_multiplied_100.js", append = TRUE)
cat(',
wordsList_activity_two:
[', file = "../Datasets/wordCountMapping_activity_twogram_multiplied_100.js", append = TRUE)
for (i in 1:(length(names(freq_activity_twogram_multiplied))-1)) {
  cat('"',  file = "../Datasets/wordCountMapping_activity_twogram_multiplied_100.js", append = TRUE)
  cat(names(freq_activity_twogram_multiplied)[i], file = "../Datasets/wordCountMapping_activity_twogram_multiplied_100.js", append = TRUE)
  cat('", \n', file = "../Datasets/wordCountMapping_activity_twogram_multiplied_100.js", append = TRUE)
}
cat('"',  file = "../Datasets/wordCountMapping_activity_twogram_multiplied_100.js", append = TRUE)
cat(names(freq_activity_twogram_multiplied)[length(names(freq_activity_twogram_multiplied))], file = "../Datasets/wordCountMapping_activity_twogram_multiplied_100.js", append = TRUE)
cat('"
    ]
}', file = "../Datasets/wordCountMapping_activity_twogram_multiplied_100.js", append = TRUE)
