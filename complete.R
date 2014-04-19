complete <- function(directory, id = 1:332) {
        
        path <- paste(directory,"/", sep = "")  # full path to datafile directory
        files <- list.files(path, pattern="*.csv")  # create list of data files
        #data <- data.frame(Date = NA, sulfate = NA, nitrate = NA, ID = NA) # Empty dataframe to concatenate data files
        good_tally <- data.frame(id = NA, nobs = NA)
        
        for(file in files[id])                                  # import each selected data file
        {
                perpos <- which(strsplit(file, "")[[1]]==".")
                assign(
                        gsub(" ","",substr(file, 1, perpos-1)), 
                        filedata <- read.csv(paste(path,file,sep="")))
                
                good <- complete.cases(filedata) # creates logical vector of good cases
                good.filedata <- filedata[good,] # dataframe of good cases
                rows <- nrow(good.filedata)
                station <- filedata[1,"ID"]
                
                station_good_rows <- data.frame(id = station, nobs = rows)
                
                #print(rows)
                #print(station)
                
                good_tally <- rbind(station_good_rows,good_tally)                    # append data files together
                
        }
        good_tally <- good_tally[-nrow(good_tally),]      #remove empty row from dataframe
        
        return(good_tally)
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
