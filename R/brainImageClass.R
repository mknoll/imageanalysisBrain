### brainImage class extends EBImage

require(EBImage)

##Helper classes to allow for NULL values
setClassUnion("numericOrNULL", c("numeric", "NULL"))
setClassUnion("dfOrNULL", c("data.frame", "NULL"))

## Main-Class
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

## Constructor brainImage
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
