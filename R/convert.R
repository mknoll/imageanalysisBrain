#' @title Convert ENC to DEC
#' 
#' @description
#' Converts encoded coordinates to x,y,z values 
#' 
#' @param values imaging values
#' @param enc value, containing x,y,z, values
#' 
#' @return data.frame with the decoded coordiantes
#' 
#' @export
#' 
#' @examples
#' enc<-c(51512331,51512254,51513355)
#' val<-c(342,221,123)
#' options("BITSIZE"=10)
#' enc2dec(val,enc)
enc2dec <- function(values, enc) {
    df <- data.frame(x=decX(enc), 
                    y=decY(enc), 
                    z=decZ(enc), 
                    val=values, 
                    coord=enc)
    return(df)
}

#' @title Convert DEC to ENC
#' 
#' @description 
#' Convert (x,y,z) coordinates to encoded coordinated
#' 
#' @param data data.frame for volume (x,y,z, val)
#' @param index colum with values
#' 
#' @return data.frame containing values and encoded coordinates
#' 
#' @export
#' 
#' @examples
#' data <- data.frame(
#' x=c(60,61,62),
#' y=c(40,40,40),
#' z=c(1,1,1),
#' val=c(234,112,45)
#' )
#' options("BITSIZE"=10)
#' dec2enc(data)
dec2enc <- function(data, index=4) {
    df <- data.frame(val=data[,index], 
                    COORD=enc(data$x, data$y, data$z))
    return(df)
}


