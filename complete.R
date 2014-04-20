# this function reads a directory full of files (or specified subset) and 
# reports the number of completely observed cases (no NAs) in each data file.
# returns a dataframe(filename, number_complete_cases)

complete <- function(directory, id = 1:332) {
        
        path <- paste(directory,"/", sep = "")          # full path to datafile directory
        files <- list.files(path, pattern="*.csv")      # create list of data files
       
        id_length <- length(id)                         # create empty output dataframe
        StnObsCntList <- data.frame(id = numeric(id_length), nobs = numeric(id_length))        
        i <- 1
        
        for(filename in files[id])                          # import each selected CSV file
        {
                perpos <- which(strsplit(filename, "")[[1]]==".")
                assign(
                        gsub(" ","",substr(filename, 1, perpos-1)), 
                        filedata <- read.csv(paste(path,filename,sep="")))  # read each CSV file
                
                good <- complete.cases(filedata)        # creates logical vector of good cases
                good.filedata <- filedata[good,]        # dataframe of good cases
                
                rows <- nrow(good.filedata)     # count good rows
                station <- filedata[1,"ID"]     # determine station ID
                
                StnObsCntList[i, "id"] <- station
                StnObsCntList[i, "nobs"] <- rows
                i <- i + 1            
        }
        
        return(StnObsCntList)
}
        
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
