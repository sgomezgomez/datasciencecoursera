best <- function(state, outcome) {
        
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
        ## Selecting hospitals with the minimun rate values
        min_rate_data <- outcome_state_specific_data[outcome_state_specific_data[, 3] == min(outcome_state_specific_data[, 3]), ]
        ## Ordering result -- in case there are more than one
        min_rate_data <- min_rate_data[order(min_rate_data$Hospital.Name), ]
        return(min_rate_data[1,1])
        ## Obtaining first hospital name
        ##hospital_name <- min_rate_data[, min_rate_data$Hospital.Name]
        ##print(hospital_name)
        ##return(hospital_name)
        ## rate
}

"more_than_one_min_combination <- function() {
        ## Read outcome data
        outcome_data <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
        
        ## Check that state and outcome are valid
        ## Validate State
        valid_states <- unique(outcome_data$State)
        valid_outcomes <- c('heart attack', 'heart failure', 'pneumonia')
        results <- data.frame()
        for(i in 1:length(valid_states)) {
                for(j in 1:length(valid_outcomes)) {
                        number_with_minimum <- best(valid_states[i], valid_outcomes[j])
                        results <- rbind(results, c(State = valid_states[i], Outcome = valid_outcomes[j], Result = number_with_minimum))
                }
        }
        results <- results[results[, 3] > 1, ]
        return(results)
}"