corr <- function(directory, threshold = 0) {
                   
        StnList <- complete(directory)               # returns stations and their good obs
        StnListAbove <- subset(StnList, nobs > threshold)
        
        path <- paste(directory,"/", sep = "")  # full path to datafile directory
        files <- list.files(path, pattern="*.csv")  # create list of data files
        
        # data <- data.frame(Date = NA, sulfate = NA, nitrate = NA, ID = NA) 
        # Empty dataframe to concatenate data files
        for(file in files[id])                          # import each selected CSV file
        {
                perpos <- which(strsplit(file, "")[[1]]==".")
                assign(
                        gsub(" ","",substr(file, 1, perpos-1)), 
                        filedata <- read.csv(paste(path,file,sep="")))  # read each CSV file
                
                good <- complete.cases(filedata) # creates logical vector of good cases
                good.filedata <- filedata[good,] # dataframe of good cases
                
                rows <- nrow(good.filedata)     # count good rows
                station <- filedata[1,"ID"]     # determine station ID
                
                result <- data.frame(id = station, nobs = rows)  # create a row (station ID, rows)
                
                StnObsCntList <- rbind(result,StnObsCntList)  # append data files together
                
        }   
        
        
        # return(StnListAbove)  # fix
}

## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

## Return a numeric vector of correlations