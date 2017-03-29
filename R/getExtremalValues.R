#' @title 
#' Find extremal values (min+max).
#' 
#' @description
#' Finds minima and maxima.
#' 
#' @param data vector of values to analyze
#' @param zeichne draw density and identified values
#' @param delta steps between analyzed points for the fitted density curve
#' 
#' @return list, containing two data.frames: min and max key/value pairs
#' 
#' @import stats
#' @import graphics
#' 
#' @export
#' 
#' @examples
#' getExtremalValues(c(1:10,2,3,4,1:10,10:1,2:4,1:10), 
#' zeichne = FALSE, delta = 0.5)
getExtremalValues <- function(data, zeichne=TRUE, delta=0.01) {
    minV<-min(data, na.rm=TRUE)-delta
    maxV<-max(data, na.rm=TRUE)+delta
    
    ##fit function
    df <- stats::approxfun(stats::density(data, na.rm=TRUE))
    #Sollte mit einer numerisch etwas stabileren Loesung ersetzt werden. 
    xVal <- seq(from=minV, to=maxV, by=delta)
    if (zeichne)  {
        graphics::plot(stats::density(data, na.rm=TRUE))
    }
    
    ##find 
    d1 <- diff(df(xVal))
    ## Minima
    posD1Min <- c()
    for (j in 2:length(d1)) {
        pos <- which(d1[j-1] < 0 & d1[j] > 0)
        if (length(pos) == 1) {
            posD1Min <- c(posD1Min, j)
        }
    }
    if (zeichne) {
        graphics::points(xVal[posD1Min], df(xVal[posD1Min]), col=2)
    }
    minV <- data.frame(key=xVal[posD1Min], val=df(xVal[posD1Min]))
    
    ## Maxima
    posD1Max <- c()
    for (j in 2:length(d1)) {
        pos <- which(d1[j-1] > 0 & d1[j] < 0)
        if (length(pos) == 1) {
            posD1Max <- c(posD1Max, j)
        }
    }
    if (zeichne) {
        graphics::points(xVal[posD1Max], df(xVal[posD1Max]), col=3)
    }
    #order maxima
    maxV <- data.frame(key=xVal[posD1Max], val=df(xVal[posD1Max]))
    maxV <- maxV[rev(order(maxV$val)),]
    if (zeichne) {
        #abline(v=maxV$key[1],col=6)
        graphics::abline(v=maxV$key, col=6)
        graphics::abline(v=minV$key, col=5)
    }
    
    ret <- list(minima=minV, maxima=maxV)
    return(ret)
}
