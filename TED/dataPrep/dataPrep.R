install.packages("jsonlite",repos = "http://cran.us.r-project.org")
library(jsonlite)

### download file###
args <- commandArgs(trailingOnly=TRUE)
apiKey <- args[1]
downloadURL <- paste("http://ted.europa.eu/api/latest/notices/search?apiKey=", 
                    apiKey, 
                    "&q=AC%3D[1]&scope=1&pageNum=1&sortField=ND&reverseOrder=false&fields=ND,NUTS,DT,NC,ND,PD,PR,TD,",
                    sep="")

download.file(downloadURL, destfile = "../datasets/APIoutput.js")


### read file ###
APIoutput <- fromJSON("../datasets/APIoutput.js")


### create output ###

# create emty vectors
tmp <- vector()
names <- vector()

for (i in 1:length(APIoutput$results$ND)) {
# for every document
  
  # convert the document number to the right format
  docNumber <- APIoutput$result$ND[i]
  year <- substr(docNumber, 1, 4)
  shortDocNumber <- substr(docNumber, 5, nchar(docNumber))
  outputNumber <- paste(shortDocNumber, year, sep = "-")
  
  for (j in 1:length(APIoutput$result$NUTS[[i]])) {
  #for every NUTS region associated with this document
    
    NUTSregionName <- APIoutput$result$NUTS[[i]][j]
    
    if (any(names(tmp) == NUTSregionName)) {
    # if there is already a list of document numbers for this NUTS region, append to it
        tmp[[NUTSregionName]] <- append(tmp[[NUTSregionName]], outputNumber)
    } else {
    # if there are no document numbers for this NUTS region yet, create a new list for it and set the correct name to it
        myList <- list(outputNumber)
        tmp <- append(tmp, myList)
        names <- append(names, NUTSregionName)
        names(tmp) <- names
    }
  }
}

### write file ###
cat('var TEDdata = \n', file = "../Datasets/TEDdata.js")
cat(toJSON(tmp, pretty=TRUE), file = "../Datasets/TEDdata.js", append = TRUE)
