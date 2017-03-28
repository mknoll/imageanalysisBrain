setCoordToNA <- function(data, sel) {
    ## Manipulate image
    coords <- data@coords[sel]
    coordsXYZ <- imageanalysisBrain::enc2dec(coords)[,1:3]
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
