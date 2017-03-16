#' @title Retrieve all minima from a given data vector
#' 
#' @description 
#' Retrieve minimal values
#' 
#' @param data vector af values to analyze
#' @param n length of data
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
#' getMinimaKey(data, minV, maxV, 1)
getMinimaKey <- function(data, n, minV, maxV, delta) {
    #dens <- stats::density(data)

    ## ca factor 2-3 schneller als sequentielle 
    ## density / approxfun aufrufe für n~500
    df <- density.adapt(data, n, minV, maxV)
    xVal <- seq.int(from=minV, to=maxV, by=delta)
    d1 <- diff(df(xVal))
    
    ## übergänge <0 zu >0
    posD1Min <- which(d1[-length(d1)]<0 & d1[-1]>0)+1
    
    ## return 
    #ret <- df(xVal[posD1Min]) #val
    #names(ret) <- xVal[posD1Min] #key
    
    return(xVal[posD1Min]) #key
}


#' @title Helper function for function approximation
#' 
#' @description Combines density and approxfun. 
#' WARNING: All data consistency checks have been removed!
#' 
#' @param x data to approximate fun for
#' @param length of x 
#' @param minV minimum value of x
#' @param maxV maximum value of x
#' 
#' @return Function approximating the initial data
#' 
#' @examples
#' data <- rnorm(100)
#' minV <- min(data)
#' maxV <- max(data)
#' n <- length(data)
#' density.adapt(data, n, minV, maxV)
density.adapt <- function(x, N, minV, maxV) {
        weights <- rep_len(1/N, N)
        bw <- bw.nrd0(x)
        
        c1 <- 3*bw
        from <- minV - c1
        to   <- maxV + c1
        
        c1 <- c1+bw
        lo <- from - c1
        up <- to + c1

        ## This bins weighted distances
        y <- .Call(stats:::C_BinDist, x, weights, lo, up, 512) 
        
        kords <- seq.int(0, 2*(up-lo), length.out = 1024)
        kords[(514):(1024)] <- -kords[512:2]
        kords <- dnorm(kords, sd = bw)
        
        kords <- fft( fft(y)* Conj(fft(kords)), inverse=TRUE)
        kords <- pmax.int(0, Re(kords)[1L:512]/length(y))
        
        xords <- seq.int(lo, up, length.out = 512)
        x <- seq.int(from, to, length.out = 512)
        
        ##### approxfun
        y <- .Call(stats:::C_Approx, xords, kords, x, 1, NA, NA, 0)
        function(v) stats:::.approxfun(x,y,v,1,NA,NA,0)        
    }



