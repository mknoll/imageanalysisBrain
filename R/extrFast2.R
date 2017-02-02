#' @title Retrieve minimal values
#' 
#' @description 
#' Retrieve minimal values, parameters are set outside (!)
#' 
#' @param data vector af values to analyze
#' @param minV minimum value of data vector (performance)
#' @param maxV maximim value of data vector (performance) 
#' @param delta steps to analyze the resulting fitted fun 
#' 
#' @return list, containing a data.frame with the columns 
#' key and val (minima).
#' 
#' @import stats
#' 
#' @export
#' 
#' @examples
#' data <- rnorm(100)
#' minV <- min(data)
#' maxV <- max(data)
#' extrFast2(data, minV, maxV, 1)
extrFast2 <- function(data, minV, maxV, delta) {
    dens <- stats::density(data, na.rm=TRUE)
    df <- stats::approxfun(dens)
    xVal <- seq(from=minV, to=maxV, by=delta)
    d1 <- diff(df(xVal))
    
    posD1Min <- c()
    for (j in 2:length(d1)) {
        pos <- which(d1[j-1] < 0 & d1[j] > 0)
        if (length(pos) == 1) {
            posD1Min <- c(posD1Min, j)
        }
    }
    return (list(minima=data.frame(key=xVal[posD1Min],
                                    val=df(xVal[posD1Min]))))
}
