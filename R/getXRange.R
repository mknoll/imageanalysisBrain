#' @title Find largest X-Dimension value
#' 
#' @description
#' Needed for scaling due to different resolutions.
#' The Scaling value is calculated to set the maximum 
#' X-dimension to 1
#' 
#' @param data data.frame of volume for which the maximal 
#' x dimension should be retrieved
#' @param steps use every n. value (n=steps)
#' 
#' @return list containing the minimal x and maximal x values
#' 
#' @import stats
#' 
#' @export
#' 
#' @examples
#' data <- data.frame(x=rep(1:10, 5), 
#' y=c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5, 10)), 
#' z=rep(1, 50), rnorm(50))
#' getXRange(data)
getXRange <- function(data, steps=500) {  
    brainSparse <- data[seq(from=1, to=length(data[,1]), by=steps),]
    
    ##relative localization
    # find largest z-slice
    #print("z...")
    ranges <- NULL
    for (z in levels(factor(brainSparse$z))) {
        subBrain <- brainSparse[brainSparse$z == z,]
        rngY <- max(subBrain$y)-min(subBrain$y)
        rngX <- max(subBrain$x)-min(subBrain$x)
        
        vec <- data.frame(z=z, rngY=rngY, rngX=rngX)
        ranges <- rbind.fill(ranges, vec)
    }
    
    max1 <- max(ranges$rngY)
    max2 <- max(ranges$rngX)
    zSel1 <- round(stats::median(as.numeric(as.character(
        ranges$z[ranges$rngY == max1]))))
    zSel2 <- round(stats::median(as.numeric(as.character(
        ranges$z[ranges$rngX == max2]))))
    zSel <- round((zSel1+zSel2)/2)
    zSlice <- data[data$z == zSel,]
    
    ##find largest y-slice
    #print("y...")
    ranges <- NULL
    for (y in levels(factor(zSlice$y))) {
        subBrain <- zSlice[zSlice$y == y, ]
        rngX <- max(subBrain$x)-min(subBrain$x)
        vec <- data.frame(y=y, rngX=rngX)
        ranges <- rbind.fill(ranges, vec)
    }
    max <- max(ranges$rngX)
    ySel <- round(stats::median(as.numeric(as.character(
        ranges$y[ranges$rngX == max]))))
    ySlice <- data[data$y == ySel,]
    xMin<-min(ySlice$x)
    xMax<-max(ySlice$x)
    
    return(list(xMin=xMin, xMax=xMax))
}
