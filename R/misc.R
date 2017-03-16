# Bit-Size for each stored coord
options("BITSIZE"=10)

#' @title
#' Encode coordinates
#' 
#' @description
#' Enoces (x,y,z) coordinates
#' 
#' @param x x-values
#' @param y y-values
#' @param z z-values
#' 
#' @return encoded value of an (x,y,z) coord tupel
#' 
#' @export
#' 
#' @examples
#' encodeXYZToCOORD(1,2,3)
encodeXYZToCOORD <- function(x,y,z) {
    bitwShiftL(bitwShiftL(x,getOption("BITSIZE"))+y,getOption("BITSIZE"))+z
}

#' @title
#' Decode coordinates
#' 
#' @description 
#' Extracts (x,y,z) values fro msingle value
#' 
#' @param val encoded coordinates
#' 
#' @return vector containing the (x,y,z) coordinates
#' 
#' @export
#' 
#' @examples
#' decodeCOORDtoXYZ(1050627)
decodeCOORDtoXYZ <- function(val) {
    c(
        bitwShiftR(val, 2*getOption("BITSIZE")),
        bitwShiftR(val, getOption("BITSIZE"))-
            bitwShiftL(bitwShiftR(val, 2*getOption("BITSIZE")),
                        getOption("BITSIZE")),
        val-bitwShiftL((bitwShiftL(
            bitwShiftR(val, 2*getOption("BITSIZE")),
            getOption("BITSIZE"))+
                bitwShiftR(val, getOption("BITSIZE"))-
                bitwShiftL(bitwShiftR(val, 2*getOption("BITSIZE")),
                            getOption("BITSIZE"))),getOption("BITSIZE")))
}

#' @title
#' Extracts x-values 
#' 
#' @description
#' Extracts x values from encoded number
#' 
#' @param val encoded coordinate value
#' 
#' @return decoded x coordinate
#' 
#' @export
#' 
#' @examples
#' decX(1050627)
getXFromCOORD <- function(val) {
    bitwShiftR(val, 2*getOption("BITSIZE"))
}

#' @title
#' Extracts y-values 
#' 
#' @description
#' Extracts y values from encoded number
#' 
#' @param val encoded coordinate value
#' 
#' @return decoded y coordinate
#' 
#' @export
#' 
#' @examples
#' decY(1050627)
getYFromCOORD <- function(val) {
    bitwShiftR(val, getOption("BITSIZE"))-
        bitwShiftL(bitwShiftR(val, 2*getOption("BITSIZE")),
                    getOption("BITSIZE"))
}

#' @title
#' Extracts z-values 
#' 
#' @description
#' Extracts z values from encoded number
#' 
#' @param val encoded coordinate value
#' 
#' @return decoded z coordinate
#' 
#' @export
#' 
#' @examples
#' decZ(1050627)
getZFromCOORD <- function(val) {
    val-bitwShiftL((
        bitwShiftL(
            bitwShiftR(val, 2*getOption("BITSIZE")), 
            getOption("BITSIZE"))+
            bitwShiftR(val, getOption("BITSIZE"))-
            bitwShiftL(bitwShiftR(val, 2*getOption("BITSIZE")),
                        getOption("BITSIZE"))
    ),getOption("BITSIZE"))
}




getEBImage <- function() {
}


