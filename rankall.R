rankall <- function(outcome, num = "best") {
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

        ## For each state, find the hospital of the given rank
        df <- data.frame(hospital = character(), state = character())

        for (estado in valid_states) {
                df2 <- data.frame(hospital = rankhospital(estado, outcome, num), state = estado)
                df <- rbind(df, df2)
                # df <- rbind(df, c(rankhospital(estado, outcome, num), estado))

        }
        order_df <- df[order(df[, 'state']), ]
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        return(order_df)
}
