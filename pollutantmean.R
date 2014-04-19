pollutantmean <- function(directory, pollutant, id) {

path <- paste(directory,"/", sep = "")  # full path to datafile directory
files <- list.files(path, pattern="*.csv")  # create list of data files
data <- data.frame(Date = NA, sulfate = NA, nitrate = NA, ID = NA) # Empty dataframe to concatenate data files

for(file in files[id])                                  # import each selected data file
        {
        perpos <- which(strsplit(file, "")[[1]]==".")
        assign(
        gsub(" ","",substr(file, 1, perpos-1)), 
        filedata <- read.csv(paste(path,file,sep="")))
        data <- rbind(filedata,data)                    # append data files together
        }

data <- data[-nrow(data),]      #remove empty row from dataframe
pollutant_mean <- mean(data[, pollutant], na.rm = TRUE)  # pollutant mean from selected stations
# print(pollutant_mean)
return(pollutant_mean)
}

## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)