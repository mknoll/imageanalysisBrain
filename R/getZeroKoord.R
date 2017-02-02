#' @title Find points of origins
#' 
#' @description
#' Determines the point of origin for a given volume by
#' first selecting the 
#' 
#' @param data data frame of volume for which the center or origin 
#'        should be calculated
#' @param sparse select only each n. voxel (n=sparse) to speed up calculations
#' @param draw draw the identified planes
#' @param col color of planes
#' @param drawsteps select only each n. voxel (n=drawsteps) to draw as 
#' identified plane
#' 
#' @return coordinates belonging to the point of origin (Zero Coordinate)
#' 
#' @import rgl
#' @import plyr
#' 
#' @export
#' 
#' @examples
#' data <- data.frame(x=rep(1:10, 5), 
#' y=c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5, 10)), 
#' z=rep(1, 50), rnorm(50))
#' data$COORD <- enc(data$x, data$y, data$z)
#' getZeroKoord(data, 1)
getZeroKoord <- function(data, sparse=500, draw=FALSE, col="yellow", 
                        drawsteps=15) {
    #print(paste("Sparse, by=", steps))
    # select only a subset of available voxels
    brainSparse <- data[seq(from=1, to=length(data[,1]), by=sparse),]
    
    ## for all available z values, find max X and Y dimensions
    #print("z...")
    ranges <- NULL
    for (z in levels(factor(brainSparse$z))) {
        subBrain <- brainSparse[brainSparse$z == z,]
        rngY <- max(subBrain$y)-min(subBrain$y)
        rngX <- max(subBrain$x)-min(subBrain$x)
        
        vec <- data.frame(z=z, rngY=rngY, rngX=rngX)
        ranges <- rbind.fill(ranges, vec)
    }
    
    # select the z slice as localized between the slices 
    # with the largest x and y dimensions
    max1 <- max(ranges$rngY)
    max2 <- max(ranges$rngX)
    zSel1 <- round(stats::median(as.numeric(as.character(
        ranges$z[ranges$rngY == max1]))))
    zSel2 <- round(stats::median(as.numeric(as.character(
        ranges$z[ranges$rngX == max2]))))
    zSel <- round((zSel1+zSel2)/2)
    zSlice <- data[data$z == zSel,]
    
    
    ## for all availabe y slices,  find max X dimensions
    # print("y...")
    ranges <- NULL
    for (y in levels(factor(zSlice$y))) {
        subBrain <- zSlice[zSlice$y == y, ]
        rngX <- max(subBrain$x)-min(subBrain$x)
        vec <- data.frame(y=y, rngX=rngX)
        ranges <- rbind.fill(ranges, vec)
    }
    max <- max(ranges$rngX)
    ySel <- round(median(as.numeric(as.character(
        ranges$y[ranges$rngX == max]))))
    ySlice <- data[data$y == ySel,]
    
    
    ## for all availabe x slices, find max Y dimensions
    #print("x...")
    ranges <- NULL
    for (x in levels(factor(zSlice$x))) {
        subBrain <- zSlice[zSlice$x == x, ]
        rngY <- max(subBrain$y)-min(subBrain$y)
        vec <- data.frame(x=x, rngY=rngY)
        ranges <- rbind.fill(ranges, vec)
    }
    max <- max(ranges$rngY)
    xSel <- as.numeric(as.character(
        ranges$x[round(length(ranges[,1])/2)]))
    xSlice <- data[data$x == xSel,]
    
    ##  Find Center of Origin
    #print("intersect...")
    coord <- zSlice[which(zSlice$COORD %in% 
                            intersect(ySlice$COORD, zSlice$COORD)),]
    coord <- coord[which(coord$COORD %in% 
                            intersect(coord$COORD, xSlice$COORD)),]
    
    ## draw?
    if (draw) {
        steps <- drawsteps
        zSlice <- zSlice[seq(from=1, to=length(zSlice[,1]), by=steps),]
        plot3d(zSlice$x, zSlice$y, zSlice$z, col=col, add=TRUE)
        ySlice <- ySlice[seq(from=1, to=length(ySlice[,1]), by=steps),]
        plot3d(ySlice$x, ySlice$y, ySlice$z, col=col, add=TRUE)
        xSlice <- xSlice[seq(from=1, to=length(xSlice[,1]), by=steps),]
        plot3d(xSlice$x, xSlice$y, xSlice$z, col=col, add=TRUE)
    }
    
    return(coord)
}
