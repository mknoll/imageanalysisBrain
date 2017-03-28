#' @title 
#' Returns an image instance
#'
#' @description 
#' Creates an Image object from a (x,y,z,val) data.frame
#' 
#' @param data data.frame with the columns (x,y,z,values)
#'
#' @return image object
#' 
#' @import EBImage
#' 
#' @export
#' 
#' @examples 
#' data <- data.frame(x=1:10, y=1:10, 
#' z=c(rep(2,5), rep(3,5)), val=rnorm(100))
#' getImage(data)
getImage <- function(data) {
    if (length(data[1,]) != 4) {
        stop("Invalid data.frame dimension!
	Required: x,y,z,vals");
    }
    dimX <- max(data$x,na.rm=TRUE)
    dimY <- max(data$y,na.rm=TRUE)
    dimZ <- max(data$z,na.rm=TRUE)
    
    ##flat
    dt <- rep_len(NA, dimZ*dimX*dimY)
    data$pos <- (data$z-1)*dimY*dimX+dimX*(data$y-1)+data$x
    dt[data$pos] <- data[,4]
    ##unflatten
    dt <- array(dt, dim=c(dimX, dimY, dimZ))

    return (new("Image", dt))
}
