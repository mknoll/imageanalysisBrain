#' @title 
#' Returns an image instance
#' 
#' @return image object
#' 
#' @import EBImage
#' @export
getImage <- function(data) {
    dimX <- max(data$x,na.rm=T)
    dimY <- max(data$y,na.rm=T)
    dimZ <- max(data$z,na.rm=T)
    
    ##flat
    dt <- rep_len(NA, dimZ*dimX*dimY)
    data$pos <- (data$z-1)*dimY*dimX+dimX*(data$y-1)+data$x
    dt[data$pos] <- data[,4]
    ##unflatten
    dt <- array(dt, dim=c(dimX, dimY, dimZ))

    return (new("Image", dt))
}
