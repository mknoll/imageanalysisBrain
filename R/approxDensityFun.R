#' @title Helper function for density function approximation 
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
#' fitDensityFunGetX(data, n, minV, maxV)
fitDensityFunGetX <- function(x, N, minV, maxV) {
    bw <- 0.9 * .Call(stats:::C_cov, x, NULL, 5, FALSE)^0.5 * N^(-0.2)
    c1 <- 3*bw
    from <- minV - c1
    to   <- maxV + c1
    c1 <- c1+bw
    lo <- from - c1
    up <- to + c1
    y <- .Call(stats:::C_BinDist, x, rep_len(1/N, N), lo, up, 512) 
    kords <- seq.int(0, 2*(up-lo), length.out = 1024)
    kords[(514):(1024)] <- -kords[512:2]
    kords <- dnorm(kords, sd = bw)
    kords <- fft( fft(y)* Conj(fft(kords)), inverse=TRUE)
    kords <- pmax.int(0, Re(kords)[1L:512]/512)
    xords <- seq.int(lo, up, length.out = 512)
    x <- seq.int(from, to, length.out = 512)
    y <- .Call(stats:::C_Approx, xords, kords, x, 1, NA, NA, 0)
    function(v) stats:::.approxfun(x,y,v,1,NA,NA,0)        
}

fitDensityFunGetXVals <- function(x, N, minV, maxV,v ) {
    bw <- 0.9 * .Call(stats:::C_cov, x, NULL, 5, FALSE)^0.5 * N^(-0.2)
    c1 <- 3*bw
    from <- minV - c1
    to   <- maxV + c1
    c1 <- c1+bw
    lo <- from - c1
    up <- to + c1
    y <- .Call(stats:::C_BinDist, x, rep_len(1/N, N), lo, up, 512) 
    kords <- seq.int(0, 2*(up-lo), length.out = 1024)
    kords[(514):(1024)] <- -kords[512:2]
    kords <- dnorm(kords, sd = bw)
    kords <- fft( fft(y)* Conj(fft(kords)), inverse=TRUE)
    kords <- pmax.int(0, Re(kords)[1L:512]/512)
    xords <- seq.int(lo, up, length.out = 512)
    x <- seq.int(from, to, length.out = 512)
    y <- .Call(stats:::C_Approx, xords, kords, x, 1, NA, NA, 0)
    stats:::.approxfun(x,y,v,1,NA,NA,0)        
}



