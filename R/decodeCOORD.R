#' @title Convert COORD value to (x,y,z)
#' 
#' @description
#' Converts encoded coordinates to x,y,z values (data.frame)
#' 
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
#' decodeCOORDToXYZDf(enc)
decodeCOORDToXYZDf <- function(enc) {
    data.frame(x=getXFromCOORD(enc), 
                    y=getYFromCOORD(enc), 
                    z=getZFromCOORD(enc))
}

