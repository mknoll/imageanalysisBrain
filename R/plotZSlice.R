#' @title
#' Plot z slice
#'  
#' @description
#' Plot given z slice for a dataset
#' 
#' @param data data.frame with x,y,z,val columns
#' @param z selected z slice to plot
#' @param index column with values to plot
#' @param ret return the image (T,F) as matrix
#' @param add add to existing plot
#' @param col color palette
#' @param main main
#' @param minx min X-value plotted
#' @param maxx max X-value plotted
#' @param miny min Y value plotted
#' @param maxy max Y value plotted
#' 
#' @return returns an EBImage containing the image corresponding to
#' the selected z slice if ret is set to TRUE. If no data is available
#' for the selected z-slice, NULL is returned
#' 
#' @import grDevices
#' @import graphics
#' @import EBImage
#' 
#' @export
#' 
#' @examples
#' data <- data.frame(x=rep(1:10, 5), 
#' y=c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5, 10)), 
#' z=rep(1, 50), rnorm(50))
#' plotZSlice(data, 1)
plotZSlice <- function(data, z, index=4, ret=FALSE, add=FALSE, 
                        col=grDevices::gray.colors(1024), main="", 
                        minx=NULL, maxx=NULL, miny=NULL, maxy=NULL)  {
    subD <- data[which(data$z == z),]
    ## build image matrix
    if (is.null(minx) || is.null(maxx)) {
        minx <- min(subD$x, na.rm=TRUE)
        maxx <- max(subD$x, na.rm=TRUE)
    }
    if (is.null(miny) || is.null(maxy)) {
        miny <- min(subD$y, na.rm=TRUE)
        maxy <- max(subD$y, na.rm=TRUE)
    }
    
    imgM <- matrix(ncol=((maxx-minx)+1), nrow=((maxy-miny)+1), NA)
    i<-0
    
    for (y in miny:maxy) {
        i <- i+1
        if (!any(y %in% subD$y)) {
            next
        }
        vec <- rep(NA, maxx-minx+1)
        imgM[i,]<-vec
        row <- subD[which(subD$y == y),]
        vec[row$x-minx+1] <- row[,index]
        imgM[i,] <- vec
    }
    
    if (dim(imgM)[[2]] == 0) {
        return(NULL)
    }
    
    ##plot image
    graphics::image(imgM, col=col, axes=FALSE, add=add, main=main)
    
    if (ret) {
        return(EBImage::Image(imgM))
    }
}
