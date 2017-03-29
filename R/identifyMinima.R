#' @title identifies minima in a given set of values
#' 
#' @description Identifies minima by repeatedly drawing 
#' small numbers of values from a larger set. 
#' 
#' @param data vector of values to analyze
#' @param proportion proportion of values to analyze 
#' (proportions*length(data)), if below 10, then the value 
#' is mutiplied by 10 in each iteration
#' @param iterat number of sample drawings 
#' @param delta width between two points to differentiate 
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
#' identifyMinima(1:1000, 1, 10)
identifyMinima <- 
    function(data, proportion=0.001, iterat=500000, delta=1) {
    ## check for inf / na
    if (any(is.na(data)) || any(is.infinite(data))) {
        warning("Found NA/Inf! Substituted with 0!")
    }
    data[is.na(data)] <- 0
    data[is.infinite(data)] <- 0

    no_cores <- parallel::detectCores() - 1
    doParallel::registerDoParallel(no_cores)
    
    ##calculate number of voxels to draw
    size <- length(data)*proportion
    if (size < 10) { size = size*10 }
    
    ##Sample data
    ## Parameters needed for extrFast2
    # resolution: analyzing the resulting
    # density for min / max values
    minV <- min(data)
    maxV <- max(data)  
    diffV <- seq.int(from=minV, to=maxV, by=delta)
    lengthData <- length(data)
    n <- floor(size)
    
    ## collect all minimal values (Keys)
    allMin <- foreach (k=1:iterat) %dopar% {
        ##get small subset of voxels
        subData <- data[sample.int(lengthData, size, FALSE, NULL)]
        d1 <- diff(fitDensityFunGetXVals(subData, n, minV, maxV, diffV))
        tmp <- .Internal(which(d1[-length(d1)]<0 & d1[-1]>0))+1
    }
    
    allMins <- unlist(allMin)

    ##get maxima of allMins
    max <- getExtremalValues(allMins, zeichne = FALSE, delta = 10)$maxima 

    doParallel::stopImplicitCluster()

    return (determineTissueClasses(max))
}
