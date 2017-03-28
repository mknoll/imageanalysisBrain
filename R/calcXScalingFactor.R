#' @title
#' Calculate a scaling factor to allow inter-patient comparisons
#' 
#' @description 
#' Calculates a scaling factor which normalizes the 
#' biparietal distance to 1
#' 
#' @return list containg the factor and a version
#' 
#' @export
#' 
#' @examples
#' data <- data.frame(x=1:10, y=1:10, 
#' z=c(rep(2,5), rep(3,5)), val=rnorm(100))
#' img <- getImage(data)
#' calcXScalingFactor(img)
calcXScalingFactor <- function(img) {
    if (class(img) != "brainImage") {
        stop("Invalid parameter! Not a brainImage instance!")
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
