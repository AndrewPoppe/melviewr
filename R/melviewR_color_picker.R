#library(grDevices)

colorPicker <- function() {
  
  w <- gwindow('Color Picker')
  mainGroup <- ggroup(horizontal = FALSE, container = w, expand=TRUE)
  topGroup <- ggroup(horizontal = TRUE, container = mainGroup, expand=TRUE)
  valueFrame <- gframe(container = topGroup)
  hueFrame <- gframe(container = topGroup, expand=TRUE)
  valuePlot <- ggraphics(height = 300, width = 300, container = valueFrame)
  huePlot <- ggraphics(height = 300, width = 10, container = hueFrame)
  selectionGroup <- ggroup(horizontal = TRUE, container = mainGroup)
  selectionLabel <- glabel("Selection:", container = selectionGroup)
  selectionPlot <- ggraphics(height = 10, width = 50, container = selectionGroup)
  buttonGroup <- ggroup(horizontal = TRUE, container = mainGroup)
  selectButton <- gbutton("OK", container = buttonGroup)
  cancelButton <- gbutton("Cancel", container = buttonGroup)
  
  
  rainbowmatrix <- matrix(seq(0, 1, length.out = 1000), nrow = 1)
  rainbowcolors <- rainbow(1000, start = 0, end = 1)
  hueColorMatrix <- NULL
  hueMatrix <- NULL
  selection <- NULL
  
  
  
  visible(huePlot) <- TRUE
  par(oma=c(0,0,0,0), mar=c(0,0,0,0))
  image(rainbowmatrix, col=rainbowcolors, axes=F)
  hueSelector <- function(h, ...) {
    x <- h$x; y <- h$y
    if(x < -1 || x > 1 || y < 0 || y > 1) return()
    index <- round(y * ncol(rainbowmatrix))
    chosenHue <- rainbowcolors[index]
    updateValuePlot(chosenHue)
  }
  addHandlerClicked(huePlot, hueSelector)
  
  updateValuePlot <- function(hue) {
    visible(valuePlot) <- TRUE
    par(oma=c(0,0,0,0), mar=c(0,0,0,0))
    hueRampPalette <- colorRampPalette(c('white',hue,'black'))
    hueColorMatrix <<- hueRampPalette(1000)
    hueMatrix <<- matrix(seq(0,1,length.out = 1000), nrow=1)
    image(hueMatrix,col=hueColorMatrix, axes=F)
  }
  
  updateValuePlot(rainbowcolors[1])
  
  valueSelector <- function(h, ...) {
    x <- h$x; y <- h$y
    if(x < -1 || x > 1 || y < 0 || y > 1) return()
    index <- round(y * ncol(hueMatrix))
    chosenValue <- hueColorMatrix[index]
    updateSelectionPlot(chosenValue)
    selection <<- chosenValue
  }
  addHandlerClicked(valuePlot, valueSelector)
  
  updateSelectionPlot <- function(value) {
    visible(selectionPlot) <- TRUE
    par(oma=c(0,0,0,0), mar=c(0,0,0,0))
    image(matrix(1), col=value, axes=FALSE)
  }
  
  finalSelection <- NULL
  
  makeSelection <- function(h,...) {
    if(!is.null(selection)) {
      finalSelection <<- selection
    } else {
      finalSelection <<- NA
    }
    dispose(w)
  }
  addHandlerClicked(selectButton, makeSelection)
  
  cancelSelection <- function(h,...) {
    finalSelection <<- NA
    dispose(w)
  }
  addHandlerClicked(cancelButton, cancelSelection)
  addHandlerUnrealize(w, cancelSelection)
  
  while(is.null(finalSelection)) {
    Sys.sleep(.5)
  }
  
  return(finalSelection)
  
}