#' @title Determine tissue classes
#' 
#' @description
#' Determines tissue classes for a set of 
#' identified cutoffs
#' 
#' @param data list, containing data frames with columns key and val
#' 
#' @return list with the identified cutoffs
#' 
#' @export
#' 
#' @examples
#' data <- data.frame(
#' key=c(823,623,923,723,1023,323,1113,123), 
#' val=c(0.0027, 0.0021, 0.0020, 0.0020, 0.0013, 0.0012, 0.0004, 0.0001))
#' determineTissueClasses(data)
determineTissueClasses <- function(data) {
    selected <- NULL
    
    ##use brain values
    if (!is.null(dim(data))) {
        data <- data[order(data$key),]
    }
        
    # determine maxima which are smaller
    # than their neighbouring maxima values:
    # these are used as tissue cutoffs
    if (length(data[,1]) >= 3) {
        l <- length(data$key)
        l1 <- length(data$key)-1
        
        dt <- data$val
        s1 <- which(dt[-1]>=dt[-l])
        s2 <- which(dt[-c(1:2)]>=dt[-c(1,l)])+1
        s3 <- sort(c(s1,s2))
        selected <- data[s3[which(duplicated(s3))],]
    }
    
    return (selected)
}
