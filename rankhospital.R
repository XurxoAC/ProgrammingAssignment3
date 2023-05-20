rankhospital <- function(state, outcome, num = "best") {
        outcomes <- c('heart attack'=11, 'heart failure'=17, 'pneumonia'=23)
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        valid_states <- unique(data$State)
        if (!(state %in% valid_states)) {
                stop('invalid state')
        }
        if (!(outcome %in% names(outcomes))) {
                stop('invalid outcome')
        }
        ## Return hospital name in that state with the given rank
        data_recortada <- data[, c(2,7,outcomes[outcome])]
        data_estado <- data_recortada[data_recortada$State == state, ]
        suppressWarnings(data_estado[, 3] <- as.numeric(data_estado[, 3]))
        ranked_df <- data_estado[order(data_estado[, 3], data_estado[, 1]), ]
        if (num == "best") { num <- 1}
        if (num == "worst")  { num <- length(ranked_df[, 3][!is.na(ranked_df[, 3])])}
        ## 30-day death rate
        return(ranked_df[num, 'Hospital.Name'])
}
