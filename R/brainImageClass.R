
# An S4 Helper class to allow NULL values
setClassUnion("numericOrNULL", c("numeric", "NULL"))

# An S4 Helper class to allow NULL values
setClassUnion("dfOrNULL", c("data.frame", "NULL"))

#' An S4 class representing an brain image
#' 
#' @slot values Via MITK exported imaging data,
#' has the form: x,y,z,Value or Value,COORD
#' @slot filename Identifier from which the 
#' data comes from
#' @slot coordBIT: size used to encode each coordinate,
#' 10 would be enough for dimension values up to 10^2 
#' @slot coords Encoded koordinates, eases set operations
#' when manipulating volumes
#' @slot measurements actually sored measurements for this
#' object (see: values: Value)
#' @slot origin center of origin of this volume
#' @slot originVersion method to calculate the origin
#' might be prone to changes; therefore, a version
#' is stored
#' @slot xfactor factor which can be used to make 
#' distances comparable between patients; Normalizes
#' the biparietal distance to 1
#' @slot xfactorVersion see originVersion
#' @slot image EBImage::Image created from values
#' @slot selectedThresholds thresholds, which might 
#' be useful when analyzing different tissue classes
#' in MRI data
#' 
#' @name brainImage-class
#' @exportClass brainImage
brainImage <- setClass("brainImage",
                       slots=c(values="data.frame", #sequencial data storage
                               filename="character",
                               coordBIT="numeric", #number of bits/ value to store in (x,y,z) aggregated value
                               type="character", 
                               coords="numeric", 
                               measurements="numeric",
                               origin="data.frame", #center of origin
                               originVersion="numeric",
                               xfactor="numericOrNULL", #scaling of whole brain volumes -> biparietal eq 1
                               xfactorVersion="numericOrNULL",
                               image="Image", #data storage as Image 
                               selectedThresholds="dfOrNULL"
                       ),
                       contains="Image")

#' @title Constructor brainImage
#' 
#' @description Constructor for new brainImage instances
#'
#' @param .Object brainImage object
#' @param values data.frame containing x,y,z/Coord + values
#' @param filename filename of the data (values)
#' @param coordBIT bit size for each number to calculate COORDs
#'
#' @return a new brainImage instance
#'
#' @import EBImage
setMethod("initialize", "brainImage",
          function(.Object, 
                   values = data.frame, 
                   filename = character,
                   coordBIT = numeric,
                   ...) {
              cat("Instantiating new brainImage object ...  ")
              cat("\rCheck coordinates ...                  ")
              #Check if COORDS vector or x,y,z, values are given
              if (length(values[1,]) == 2 && "COORD" %in% colnames(values)) {
                  ##COORDS
                  #save current bitsize
                  bak <- options("BITSIZE")
                  options("BITSIZE"=coordBIT)
                  cat("\rExtract coordinates [x]...               ")
                  values$x <- imageanalysisBrain::getXFromCOORD(values$COORD)
                  cat("\rExtract coordinates [y]...               ")
                  values$y <- imageanalysisBrain::getYFromCOORD(values$COORD)
                  cat("\rExtract coordinates [z]...               ")
                  values$z <- imageanalysisBrain::getZFromCOORD(values$COORD)
                  values <- values[,c(3:5,1:2)]
                  #restore bitsize option
                  options(bak)
              } else if (length(values[1,]) >= 4 && "x" %in% colnames(values)
                         && "y" %in% colnames(values) 
                         && "z" %in% colnames(values)) {
                  if ("COORD" %in% colnames(values)) {
                      ##everythings already there
                  } else {
                      ##x,y,z, w/o COORD
                      ## add COORD column
                      cat("\rCreate COORD vector ...              ")
                      values$COORD <- imageanalysisBrain::encodeXYZToCOORD(values$x, values$y, values$z)
                  }
              } else {
                  print(colnames(values))
                  stop("Please provide a data.frame with the following columns:
                       x,y,z,VALS or VALS,COORD")
              }
              ## Check if column order is as expected
              if (!(colnames(values)[1] == "x" 
                    && colnames(values)[2] == "y"
                    && colnames(values)[3] == "z"
                    && colnames(values)[5] == "COORD")) {
                  stop("Something went wrong! Colnames not as expected (x,y,z,VAL,COORD)!")
                  stop(colnames(values))
              }
              
              cat("\rFurther initializations ...            ")
              .Object@measurements <- values[,4]
              .Object@values <- values
              cat("\rCreate Image instance ...              ")
              .Object@image <- imageanalysisBrain::getImage(values)
              .Object@filename <- filename
              .Object@coordBIT <- coordBIT
              .Object@type <- colnames(values)[4]
              .Object@coords <- values$COORD
              .Object@selectedThresholds <- NULL
              
              #Not calculated for every instance: default: NULL
              .Object@xfactor <- NULL
              .Object@xfactorVersion <- NULL
              
              cat("\rCalculate ZeroCoordinates...         ")
              zeroKoord <- imageanalysisBrain::getZeroKoord(values)
              .Object@origin <- zeroKoord$vals[,c("x","y","z")]
              ## Keep track of method on how the origin was calculated
              .Object@originVersion <- zeroKoord$version
              ##call Image constructor from EBImage
              .Object <- callNextMethod(.Object, .Object@image)
              cat("\rCreated new brainImage instance :)                  ")
              .Object
          })


# Is there something linke lombok for R to avoid creating getters manually?
#' @export
getCoords <- function(img) {
  checkClass(img)
  img@coords
}

#' @export
getMeasurements <- function(img) {
  checkClass(img)
  img@measurements
}

#' @export
getXFactor <- function(img) {
  checkClass(img)
  img@xfactor
}

#' @export
getOrigin <- function(img) {
  checkClass(img)
  img@origin
}

#' @export
getThresholds <- function(img) {
  checkClass(img)
  img@selectedThresholds
}

#' @export
setThresholds <- function(img, thresholds) {
  ##TODO: check consistency
  checkClass(img)
  img@selectedThresholds <- thresholds
}

#' @export
checkClass <- function(img) {
  if (class(img) != "brainImage") {
    stop("Invalid parameter! Not a brainImage instance!")
  }
}
