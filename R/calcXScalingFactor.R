#' @title
#' Calculate a scaling factor to allow inter-patient comparisons
#' 
#' @description 
#' 
#' 
#' @export
calcXScalingFactor <- function(img) {
    if (class(img) != "brainImage") {
        stop("Wrong argument! Not a brainImage instance!")
    }
    
    ## calculate x scaling factor 
    xvals <- imageanalysisBrain::getXRange(img@values)
    xfactor <- 1/(xvals$xMax - xvals$xMin)
    ## and store in brainImage slots
    img@xfactor <- xfactor
    ## keep track of the used version
    img@xfactorVersion <- xvals$version
    
    return(img)
}
