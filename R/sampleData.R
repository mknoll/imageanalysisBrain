#' @title Sample data
#' 
#' @description 
#' Fast alternative for extr(), returning only minima
#' and using parallelization
#' 
#' @param data vector of values to analyze
#' @param proportion proportion of voxels to analyze 
#' (proportions*length(data)), if below 10, then the value 
#' is mutiplied by 10
#' @param iterat number of sample drawings 
#' @param test dummy, needed to pass R checks :-(
#' 
#' @return data.frame containing all identified minima
#' 
#' @import plyr
#' @import foreach
#' @import doParallel
#' @import parallel
#' 
#' @export
#' 
#' @examples
#' sampleData(1:1000, 1, 10, test=TRUE)
sampleData <- function(data, proportion=0.001, iterat=500000, test=FALSE) {
    if (test) {
        no_cores <- 1
    } else {
        no_cores <- parallel::detectCores() - 1
    }
    doParallel::registerDoParallel(no_cores)
    
    ##calculate number of voxels to draw
    size <- length(data)*proportion
    if (size < 10) { size = size*10 }
    
    ##Sample T1KM data
    ## Parameters needed for extrFast2
    # resolution: analyzing the resulting
    # density for min / max values
    delta <- 100 
    minV <- min(data, na.rm=TRUE)
    maxV <- max(data, na.rm=TRUE)  
    lengthData <- length(data)
    
    ## collect all minimal values
    allMin <- foreach (k=1:iterat) %dopar% {
        ##get small subset of voxels
        subData <- data[sample(1:lengthData, size=size)]
        
        ## get only min values
        extrD <- extrFast2(subData, minV, maxV, delta) ##set delta within func
        extrD$minima
    }
    allMins <- rbind.fill(allMin)
    
    ##get maxima of allMins
    if (length(allMins[,1]) >= 2) {
        max <- extr(allMins$key, zeichne = FALSE, delta = 10)$maxima
    } else {
        max <- allMins
    }
    
    stopImplicitCluster()
    
    return (max)
}