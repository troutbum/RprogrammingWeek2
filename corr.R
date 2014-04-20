corr <- function(directory, threshold = 0) {
        
        StnList <- complete(directory)                    # returns stations and their good obs
        StnsAbove <- subset(StnList, nobs > threshold)    # station above threholds
     
        if (nrow(StnsAbove) == 0) {                     
                empty <- numeric(0)
                return(empty)                                     # if no stations above threshold
                
        } else {
                          
                SortedStns <- StnsAbove[order(StnsAbove[,"id"]),] # sort output by ID number
                StnIDs <- SortedStns[,"id"]                       # IDs of Stations to analyze
                
                path <- paste(directory,"/", sep = "")            # full path to datafile directory
                files <- list.files(path, pattern="*.csv")        # create list of data files in directory
                
                correlations <- rep(NULL, length(StnIDs))         # pre-allocate results vector   
                i <- 1
                for(filename in files[StnIDs])                    # import each selected CSV file
                {
                        # read each CSV file
                        filedata <- read.csv(paste(path,filename,sep=""),na.strings = "NA",)  
                        good <- complete.cases(filedata)         # creates logical vector of good cases
                        completedata <- filedata[good,]          # dataframe of good cases                
                        
                        correlations[i] <- cor(completedata[,"sulfate"],completedata[,"nitrate"]) 
                        i <- i + 1  
                }   
                
                return(correlations) 
        }
        
}

## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

## Return a numeric vector of correlations