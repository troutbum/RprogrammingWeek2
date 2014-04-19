# this function reads a directory full of files (or specified subset) and 
# reports the number of completely observed cases in each data file.
# returns a dataframe(filename, number_complete_cases)

complete <- function(directory, id = 1:332) {
        
        path <- paste(directory,"/", sep = "")          # full path to datafile directory
        files <- list.files(path, pattern="*.csv")      # create list of data files
        StnObsCntList <- data.frame(id = NA, nobs = NA) # create empty output datafrme
        
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
        StnObsCntList <- StnObsCntList[-nrow(StnObsCntList),]        # remove empty row from dataframe
        
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
