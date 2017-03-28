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
decodeCOORD <- function(enc) {
    data.frame(x=getXFromCOORD(enc), 
                    y=getYFromCOORD(enc), 
                    z=getZFromCOORD(enc))
}

