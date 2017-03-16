#' @title Extract Subclass data
#' 
#' @description 
#' Splits a given dataset into subclasses.
#' 
#' @param data data.frame of volume to split
#' @param split data.frame (key, val) with values to split at (keys)
#' @param index column index with values in the volume dataset
#' 
#' @return returns a list where all list elements contain 
#' data subsets belonging to the identified tissue class
#' 
#' @export
#' 
#' @examples
#' data <- data.frame(x=rep(1:10, 5), 
#' y=c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5, 10)), 
#' z=rep(1, 50), val=rnorm(50))
#' getSubclasses(data, data.frame(key=median(data$val), val=NA))
getSubclasses <- function(data, split, index=4) {
    classes <- list()
    if (length(split[,1]) == 1) {
        ## only 1 separating value, 2 resulting classes
        classes[[1]] <- data[which(data[,index] < split[1,"key"]),]
        classes[[2]] <- data[which(data[,index] >= split[1,"key"]),]
    } else {
        
        ## Mehrere minima
        for (j in 1:length(split[,1])) {
            if (j == 1) {
                classes[[length(classes)+1]] <- data[which(data[,index] <= split[j, "key"]),]
                print(dim(classes[[length(classes)]]))
            } else {
                classes[[length(classes)+1]] <- data[which(data[,index] > split[j-1, "key"] & 
                                                           data[,index] <= split[j, "key"]),]
                print(dim(classes[[length(classes)]]))
            }
            if (j == length(split[,1])) {
                classes[[length(classes)+1]] <- data[which(data[,index] >= split[j, "key"]),]
                print(dim(classes[[length(classes)]]))
            }
        }
    }
    return(classes)
}
