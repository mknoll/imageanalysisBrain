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
#' data <- list(data.frame(
#' key=c(823,623,923,723,1023,323,1113,123), 
#' val=c(0.0027, 0.0021, 0.0020, 0.0020, 0.0013, 0.0012, 0.0004, 0.0001)))
#' determineTissueClasses(data)
determineTissueClasses <- function(data) {
    #tissue class cutoffs
    collectedCutoffs <- list()
    
    for (i in 1:length(data)) {
        #set element to NA in case classes cannot be determined
        if (length(collectedCutoffs) <= i)  {
            collectedCutoffs[[i]] <- NA
        }
        
        ##use brain values
        vals <- data[[i]]
        if (!is.null(dim(vals))) {
            vals <- vals[order(vals$key),]
        }
        
        # determine maxima which are smaller
        # than their neighbouring maxima values:
        # these are used as tissue cutoffs
        if (length(vals[,1]) >= 3) {
            tmp <- NULL
            for (j in 2:(length(vals[,1])-1)) {
                if (vals$val[j-1] >= (vals$val[j]) && 
                        vals$val[j+1] >= (vals$val[j])) {
                    tmp <- rbind.fill(tmp, vals[j,])
                }
            }
            if (length(collectedCutoffs) <= i) {
                if (is.na(collectedCutoffs[[i]])) {
                    collectedCutoffs[[i]] <- tmp
                } else {
                    collectedCutoffs[[i]] <- cbind(collectedCutoffs[[i]], tmp)
                }
            } else {
                collectedCutoffs[[i]] <- tmp
            }
        }
    }
    
    return (collectedCutoffs)
}