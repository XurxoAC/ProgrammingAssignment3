best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        valid_states <- unique(data$State)
        if (!(state %in% valid_states)) {
                stop('invalid state')
        }
        valid_outcomes <- c('heart attack', 'heart failure', 'pneumonia')
        if (!(outcome %in% valid_outcomes)) {
                stop('invalid outcome')
        }
        ## Return hospital name in that state with lowest 30-day death
        data_estado <- data[data$State == state, ]
        suppressWarnings(data_estado[, c(11, 17, 23)] <- as.numeric(unlist(data_estado[, c(11, 17, 23)])))

        if (outcome == 'heart attack') {
                indice <- which(data_estado[, 11] == min(data_estado[, 11], na.rm = TRUE))
                hospital_name <- data_estado[indice, 2]
        }
        if (outcome == 'heart failure') {
                indice <- which(data_estado[, 17] == min(data_estado[, 17], na.rm = TRUE))
                hospital_name <- data_estado[indice, 2]
        }       
        if (outcome == 'pneumonia') {
                indice <- which(data_estado[, 23] == min(data_estado[, 23], na.rm = TRUE))
                hospital_name <- data_estado[indice, 2]
        } 
        return(min(hospital_name))
        ## rate
}
