#' @title Set selected coordinates to NA
#' 
#' @description
#' When creating subclass images, values not belonging
#' to the respective class can be set to NA with this function
#' 
#' @param data brainImage instance to manipulate
#' @param sel vector of positions (sequential) which
#' should be set to NA
#' 
#' @return brainImage instance with adapted values
setCoordToNA <- function(data, sel) {
    ## Manipulate image
    coords <- data@coords[sel]
    coordsXYZ <- imageanalysisBrain::decodeCOORDToXYZDf(coords)
    dimV <- dim(data@.Data)
    #(slice-1)*dimV[1]*dimV[2] + dimV[1]*(col-1) + row
    coordsXYZ$pos <- (coordsXYZ$z-1)*dimV[1]*dimV[2] + dimV[1]*(coordsXYZ$y-1) + coordsXYZ$x

    ##flatten 
    dt <- as.matrix(data@.Data, ncol=1)
    dt[coordsXYZ$pos] <- NA
    ##unflatten
    dt <- array(dt, dim=dim(data@.Data))

    #write back
    data@.Data <- dt

    ## Set measurements to NA
    data@measurements[coords] <- NA
    
    return (data)
}
