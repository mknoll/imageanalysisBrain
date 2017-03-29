
# An S4 Helper class to allow NULL values
setClassUnion("numericOrNULL", c("numeric", "NULL"))

# An S4 Helper class to allow NULL values
setClassUnion("dfOrNULL", c("data.frame", "NULL"))

#' An S4 class representing an brain image
#' 
#' @slot .Data see EBImage::Image@.Data
#' @slot colormode see EBImage::Image@colormode
#' @slot values Via MITK exported imaging data,
#' has the form: x,y,z,Value or Value,COORD
#' @slot filename Identifier from which the 
#' data comes from
#' @slot coordBIT size used to encode each coordinate,
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
#' @slot selectedThresholds thresholds, which might 
#' be useful when analyzing different tissue classes
#' in MRI data
#' @slot type Name of the stored datatype (colname from
#' the measurements column in the values dataframe)
#' 
#' @name brainImage-class
#' @exportClass brainImage
brainImage <- setClass("brainImage",
                        slots=c(values="data.frame", 
                                filename="character",
                                coordBIT="numeric", 
                                type="character", 
                                coords="numeric", 
                                measurements="numeric",
                                origin="data.frame", 
                                originVersion="numeric",
                                xfactor="numericOrNULL", 
                                xfactorVersion="numericOrNULL",
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
                    coordBIT = numeric
                    ) {
                    #...) {
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
                        bak <- options("BITSIZE")
                        options("BITSIZE"=coordBIT)
                        values$COORD <- 
                            imageanalysisBrain::encodeXYZToCOORD(
                                values$x, values$y, values$z)
                        options(bak)
                    }
                } else {
                    print(colnames(values))
                    stop("Please provide a data.frame with the following 
                        columns: x,y,z,VALS or VALS,COORD")
                }
                ## Check if column order is as expected
                if (!(colnames(values)[1] == "x" 
                    && colnames(values)[2] == "y"
                    && colnames(values)[3] == "z"
                    && colnames(values)[5] == "COORD")) {
                    stop("Something went wrong! Colnames 
                        not as expected (x,y,z,VAL,COORD)!")
                    stop(colnames(values))
                }
                
                cat("\rFurther initializations ...            ")
                .Object@measurements <- values[,4]
                .Object@values <- values
                cat("\rCreate Image instance ...              ")
                #.Object@image <- imageanalysisBrain::getImage(values)
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
                .Object <- callNextMethod(.Object, 
                                        imageanalysisBrain::getImage(values))
                cat("\rCreated new brainImage instance :)                  ")
                .Object
            })


# Is there something like lombok to avoid creating getters manually?
#' @title Get all encoded coordinates
#' 
#' @description Encoded coordinates (COORD vector)
#' 
#' @param img brainImage instance 
#' 
#' @return COORD vector
#' 
#' @export
#' 
#' @examples
#' data <- data.frame(x=1:10, y=1:10, 
#' z=c(rep(2,5), rep(3,5)), val=rnorm(100))
#' img <- new("brainImage", data, "test", 10)
#' getCoords(img)
getCoords <- function(img) {
    checkClass(img, "brainImage")
    img@coords
}

#' @title Get all values as vector
#' 
#' @description All measurements stored in the img object
#' are accessed
#' 
#' @param img brainImage instance
#' 
#' @return measurements vector
#' 
#' @export
#' 
#' @examples
#' data <- data.frame(x=1:10, y=1:10, 
#' z=c(rep(2,5), rep(3,5)), val=rnorm(100))
#' img <- new("brainImage", data, "test", 10)
#' getMeasurements(img)
getMeasurements <- function(img) {
    checkClass(img, "brainImage")
    img@measurements
}

#' @title Get X-Scaling factor
#' 
#' @description Factor to normalize the maximum biparietal 
#' dimension to 1. Default: NULL. Initialize with 
#' calcXScalingFactor
#' 
#' @param img brainImage instance
#' 
#' @return scaling factor
#' 
#' @export
#' 
#' @examples
#' data <- data.frame(x=1:10, y=1:10, 
#' z=c(rep(2,5), rep(3,5)), val=rnorm(100))
#' img <- new("brainImage", data, "test", 10)
#' img <- imageanalysisBrain::calcXScalingFactor(img)
#' getXFactor(img)
getXFactor <- function(img) {
    checkClass(img, "brainImage")
    img@xfactor
}

#' @title Get Center of origin
#' 
#' @description Center of origin, calculated with 
#' getZeroKoord()
#' 
#' @param img brainImage instance
#' 
#' @return coordinates (x,y,z) of the claculated origin
#' 
#' @export
#' 
#' @examples
#' data <- data.frame(x=1:10, y=1:10, 
#' z=c(rep(2,5), rep(3,5)), val=rnorm(100))
#' img <- new("brainImage", data, "test", 10)
#' getOrigin(img)
getOrigin <- function(img) {
    checkClass(img, "brainImage")
    img@origin
}

#' @title Get Number of measurements (non NA)
#' 
#' @description Returns the total amount of measurements
#' which are not NA in this image
#' 
#' @param img brainImage instance
#' 
#' @return number of non-NA measurements 
#' 
#' @export
#' 
#' @examples
#' data <- data.frame(x=1:10, y=1:10, 
#' z=c(rep(2,5), rep(3,5)), val=rnorm(100))
#' img <- new("brainImage", data, "test", 10)
#' getNumberOfMeasurements(img)
getNumberOfMeasurements <- function(img) {
    checkClass(img, "brainImage")
    return (length(!is.na(img@measurements)))
}

#' @title Get tissue class thresholds 
#' 
#' @description Returns cutoffs for different tissue 
#' classes, identified with 
#' identifyMinima(getMeasurements(brainImage), iterat=10000)
#' and set with setThresholds(). 
#' 
#' @param img brainImage instance
#' 
#' @return adjusted imageBrain object 
#' 
#' @export
#' 
#' @examples
#' options("BITSIZE"=10)
#' data <- data.frame(x=1:15, y=1:15, 
#' z=c(rep(2,15), rep(3,15)), val=rnorm(6750))
#' img <- new("brainImage", data, "test", 10)
#' img <- setThresholds(img,
#' imageanalysisBrain::identifyMinima(getMeasurements(img), iterat=1000))
#' getThresholds(img)
getThresholds <- function(img) {
    checkClass(img, "brainImage")
    img@selectedThresholds
}

#' @title Set tissue class thresholds
#' 
#' @description Sets tissue class thresholds for this 
#' object. Thresholds can be caluclated with 
#' identifyMinima().
#' 
#' @param img brainImage instance
#' @param thresholds data.frame with key/val columns,
#' calculated with identifyMinima()
#' 
#' @return adjusted imageBrain object 
#' 
#' @export
#' 
#' @examples
#' data <- data.frame(x=1:15, y=1:15, 
#' z=c(rep(2,15), rep(3,15)), val=rnorm(6750))
#' img <- new("brainImage", data, "test", 10)
#' img <- setThresholds(img,
#' imageanalysisBrain::identifyMinima(getMeasurements(img), iterat=1000))
setThresholds <- function(img, thresholds) {
    checkClass(img, "brainImage")
    #TODO: check
    img@selectedThresholds <- thresholds
    return(img)
}

#' @title Checks if object is of given class and stops 
#' if not.
#' 
#' @description An object obj is tests if its class 
#' equals expClass. If not, execution is halted.
#' 
#' @param obj Object to test
#' @param expClass character of the class 
#' 
#' @return TRUE if everything is ok. Otherwise, execution is halted.
#' 
#' @export
#' 
#' @examples
#' data <- data.frame(x=1:10, y=1:10, 
#' z=c(rep(2,5), rep(3,5)), val=rnorm(100))
#' img <- new("brainImage", data, "test", 10)
#' checkClass(img, "brainImage")
checkClass <- function(obj, expClass) {
    if (class(obj) != expClass) {
        stop(paste("Invalid parameter! Not a",expClass,"instance!"))
    }
    return (TRUE)
}
