rankall <- function(outcome, num = "best") {
        
        ## Read outcome data
        outcome_data <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
        
        ## Check that state and outcome are valid
        ## Validate State
        valid_states <- unique(outcome_data$State)
        valid_states <- sort(valid_states)
        ##print(valid_states)
        
        ## Validate outcome
        if(outcome == 'heart attack') {
                outcome_colnumber <- 11
        } else if(outcome == 'heart failure') {
                outcome_colnumber <- 17
        } else if(outcome == 'pneumonia') {
                outcome_colnumber <- 23
        } else {
                stop('invalid outcome')
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## Filter data by state and specific outcome columns
        ##outcome_state_specific_data <- outcome_data[(outcome_data$State == state), c(2, 7, outcome_colnumber)]
        outcome_state_specific_data <- outcome_data[, c(2, 7, outcome_colnumber)]
        ## Numeric conversion of specific outcome rate
        outcome_state_specific_data[, 3] <- as.numeric(outcome_state_specific_data[, 3])
        ## Removing NA values
        outcome_state_specific_data <- outcome_state_specific_data[!is.na(outcome_state_specific_data[, 3]), ]
        ## Order by rate, and then by hospital name
        outcome_state_specific_data <- outcome_state_specific_data[order(outcome_state_specific_data[, 3], outcome_state_specific_data$Hospital.Name), ]
        ## print(head(outcome_state_specific_data, 10))
        results <- data.frame(hospital = character(0), state = character(0))
        for(i in 1:length(valid_states)) {
                ## Filter by state
                outcome_state_specific_data_per_state <- outcome_state_specific_data[outcome_state_specific_data[, 2] == valid_states[i], ]
                ## Order by rank
                outcome_state_specific_data_per_state <- outcome_state_specific_data_per_state[order(outcome_state_specific_data_per_state[, 3], outcome_state_specific_data_per_state$Hospital.Name), ]
                ## Determine row number based on request rank
                if(num == 'best') {
                        row_to_return <- 1
                        value <- outcome_state_specific_data_per_state[row_to_return, c(1, 2)]
                } else if(num == 'worst') {
                        row_to_return <- nrow(outcome_state_specific_data_per_state)
                        value <- outcome_state_specific_data_per_state[row_to_return, c(1, 2)]
                } else if(as.numeric(num) <= nrow(outcome_state_specific_data_per_state)) {
                        row_to_return <- as.numeric(num)
                        value <- outcome_state_specific_data_per_state[row_to_return, c(1, 2)]
                } else {
                        value <- data.frame(as.character(NA), valid_states[i])
                }
                colnames(value) <- c('hospital', 'state')
                colnames(results) <- c('hospital', 'state')
                results <- rbind(results, value)
                
        }

        return(results)
}