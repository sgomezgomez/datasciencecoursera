pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## Setting working directory to directory value
        old.dir <- getwd()
        setwd(paste(getwd(), '/', directory[1], sep = ''))
        
        ## Setting initial pollutant data file
        total_pollutant_data <- numeric()
        
        ## id vector loop
        for(i in 1:length(id)) {
                ## Setting file name
                fileid <- toString(id[i])
                if(nchar(id[i]) == 1) {
                        fileid <- paste('00', id[i], sep = '')
                } else if(nchar(id[i]) == 2) {
                        fileid <- paste('0', id[i], sep = '')
                }
                filename <- paste(fileid, '.csv', sep = '')
                
                ## Reading file
                monitor_data <- read.csv(filename)
                
                ## Calculating mean
                ## Extracting specific pollutant data
                pollutant_data <- monitor_data[pollutant[1]]
                total_pollutant_data <- append(total_pollutant_data, pollutant_data[!is.na(pollutant_data)])
                ## Removing NA values
                pollutant_mean <- mean(pollutant_data[!is.na(pollutant_data)])
                
        }
        
        ## Calculate total pollutant mean
        total_pollutant_mean <- mean(total_pollutant_data)
        
        ## Setting working directory back to old one
        setwd(old.dir)
        
        ## Returning pollutant mean
        return(total_pollutant_mean)
}