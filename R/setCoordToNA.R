setCoordToNA <- function(data, sel) {
    ## Manipulate image
    
    coords <- data@coords[sel]
    coordsXYZ <- enc2dec(coords)[,1:3]
    
    data@.Data[coordsXYZ$x,coordsXYZ$y,$coordsXYZ$z] <- NA
    
    ## Set measurements to NA
    data@measurements[coords] <- NA
    
    return (data)
}