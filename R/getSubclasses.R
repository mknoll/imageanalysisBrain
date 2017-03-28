#' @title Extract Subclass data
#' 
#' @description 
#' Splits a given dataset into subclasses.
#' 
#' @param data data.frame of volume to split
#' @param split data.frame (key, val) with values to split at (keys)
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
#' img <- new("brainImage", data, "test", 10)
#' getSubclasses(img, data.frame(key=median(data$val), val=NA))
getSubclasses <- function(data, split) {
    classImages <- list()
    
    ## Mehrere minima
    for (j in 1:length(split[,1])) {
        if (j == 1) {
            cpy <- setCoordToNA(data, which(data@measurements <= split[j,"key"]))
            classImages[[length(classImages) + 1 ]] <- cpy
        } else {
            cpy <- setCoordToNA(data, which(data@measurements > split[j-1,"key"] & 
                                                data@measurements <= split[j,"key"]))
            classImages[[length(classImages) + 1 ]] <- cpy
        }
        if (j == length(split[,1])) {
            cpy <- setCoordToNA(data, which(data@measurements > split[j,"key"]))
            classImages[[length(classImages) + 1 ]] <- cpy
        }
    }
    
    return(classImages)
}
