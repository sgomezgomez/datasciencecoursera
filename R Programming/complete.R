complete <- function(directory, id = 1:332) {
        ## Setting working directory to directory value
        old.dir <- getwd()
        setwd(paste(getwd(), '/', directory[1], sep = ''))
        
        ## Setting initial  data file
        complete_data <- data.frame(id = numeric(), nobs = numeric())
        
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
                ## Selecting complete cases
                complete_monitor_data <- monitor_data[complete.cases(monitor_data),]
                ## Populating return data frame
                complete_data[i,] <- c(id = id[i], nobs = nrow(complete_monitor_data))
        }
        
        ## Setting working directory back to old one
        setwd(old.dir)
        
        ## Returning output data frame
        return(complete_data)
}