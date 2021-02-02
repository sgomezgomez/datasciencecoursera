corr <- function(directory, threshold = 0) {
        source('complete.R')
        index_complete_data <- complete('specdata', 1:332)
        index_complete_data_above_threshold <- index_complete_data[index_complete_data$nobs > threshold, ]
        
        ## Setting working directory to directory value
        old.dir <- getwd()
        setwd(paste(getwd(), '/', directory[1], sep = ''))
        
        ## Setting initial data vectors
        Date <- character()
        sulfate <- numeric()
        nitrate <- numeric()
        ID <- numeric()
        correlation_vector <- numeric()
        
        ## When no files are above threshold
        if(nrow(index_complete_data_above_threshold) == 0) {
                ## Setting working directory back to old one
                setwd(old.dir)
                return(correlation_vector)
        }
        
        ## id vector loop
        for(i in 1:nrow(index_complete_data_above_threshold)) {
                ## Setting file name
                fileid <- toString(index_complete_data_above_threshold[i,1])
                if(nchar(index_complete_data_above_threshold[i,1]) == 1) {
                        fileid <- paste('00', index_complete_data_above_threshold[i,1], sep = '')
                } else if(nchar(index_complete_data_above_threshold[i,1]) == 2) {
                        fileid <- paste('0', index_complete_data_above_threshold[i,1], sep = '')
                }
                filename <- paste(fileid, '.csv', sep = '')
                
                ## Reading file
                monitor_data <- read.csv(filename)
                ## Selecting complete cases
                complete_monitor_data <- monitor_data[complete.cases(monitor_data),]
                ## Appending monitor cases to total data above threshold per attribute
                Date <- append(Date, complete_monitor_data$Date)
                sulfate <- append(sulfate, complete_monitor_data$sulfate)
                nitrate <- append(nitrate, complete_monitor_data$nitrate)
                ID <- append(ID, complete_monitor_data$ID)
                correlation_vector[i] <- cor(complete_monitor_data$sulfate, complete_monitor_data$nitrate)
                
        }
        
        ## Set final data frame with all complete observations
        complete_data_above_threshold <- data.frame(Date, sulfate, nitrate, ID)
        correlation <- cor(complete_data_above_threshold[,2:4])
        
        ## Setting working directory back to old one
        setwd(old.dir)
        return(correlation_vector)
}