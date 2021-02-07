rankhospital <- function(state, outcome, num = "best") {
        
        ## Read outcome data
        outcome_data <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
        
        ## Check that state and outcome are valid
        ## Validate State
        valid_states <- unique(outcome_data$State)
        if(!(state %in% valid_states)) {
                stop('invalid state')
        }
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
        outcome_state_specific_data <- outcome_data[(outcome_data$State == state), c(2, 7, outcome_colnumber)]
        ## Numeric conversion of specific outcome rate
        outcome_state_specific_data[, 3] <- as.numeric(outcome_state_specific_data[, 3])
        ## Removing NA values
        outcome_state_specific_data <- outcome_state_specific_data[!is.na(outcome_state_specific_data[, 3]), ]
        ## Order by rate, and then by hospital name
        outcome_state_specific_data <- outcome_state_specific_data[order(outcome_state_specific_data[, 3], outcome_state_specific_data$Hospital.Name), ]
        ## print(head(outcome_state_specific_data, 10))
        
        ## Determine row number based on request rank
        if(num == 'best') {
                row_to_return <- 1
        } else if(num == 'worst') {
                row_to_return <- nrow(outcome_state_specific_data)
        } else if(as.numeric(num) <= nrow(outcome_state_specific_data)) {
                row_to_return <- as.numeric(num)
        } else {
                return(NA)
        }
        
        return(outcome_state_specific_data[row_to_return, 1])
        
}
