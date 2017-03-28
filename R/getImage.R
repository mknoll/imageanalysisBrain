#' @title 
#' Returns an image instance
#' 
#' @return image object
#' 
#' @import EBImage
getImage <- function(data) {
    dimX <- max(data$x,na.rm=T)
    dimY <- max(data$y,na.rm=T)
    dimZ <- max(data$z,na.rm=T)
    ar <- array(dim=c(dimX,dimY,dimZ), dimnames=c("x","y","z"))
    
    ##TODO: dopar
    for (z in 1:max(data$z, na.rm=T)) {
        for (y in 1:max(data$y, na.rm=T)) {
            line <- rep(NA, dimX)
            sel <- data[which(data$z == z & data$y == y), c(1,4)]
            line[sel$x] <- sel[,2]
            ar[,
               y,
               z] <- line
        }
    }
    
    return (new("Image", ar))
}