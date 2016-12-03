

#==============================================================================#
# Functions for saving/loading graphics settings

.saveGraphicsSettings <- function(h, ...) {
    tryCatch({
        configFile <- paste(Sys.getenv('HOME'), "/.melviewR.config", sep = "")
        sink(configFile)
        for (i in 1:length(graphicsDefaults)) {
            val <- eval(parse(text = names(graphicsDefaults)[i]))
            sepChar <- ifelse(is.character(val), "\"", "")
            cat(paste(names(graphicsDefaults)[i], " <<- ", sepChar, val, sepChar, "\n", sep = ""))
        }
        sink()
        output <- list(messageTxt = paste("Config file has been saved to:", configFile), icon = "info")
        gmessage(output$messageTxt, title = "Save Graphics Settings", icon = output$icon)
    }, warning = function(war) {
        output <- list(messageTxt = paste("A warning has been raised in the attempt to save settings to:", configFile, "\n", war),
            icon = "warning")
        gmessage(output$messageTxt, title = "Save Graphics Settings", icon = output$icon)
    }, error = function(err) {
        output <- list(messageTxt = paste("An error has been raied in the attempt to save settings to:", configFile, "\n", err),
            icon = "error")
        gmessage(output$messageTxt, title = "Save Graphics Settings", icon = output$icon)
    }, finally = {
        DONE <- TRUE
    })
}

.loadGraphicsSettings <- function() {
    configFile <- paste0(Sys.getenv('HOME'), "/.melviewR.config")
    configLoaded <- FALSE
    if (file.exists(configFile)) {
        source(configFile)
        configLoaded <- TRUE
        # if any graphics options are not set in the config file, set them to their default state.
        for (i in 1:length(graphicsDefaults)) {
            if (!exists(names(graphicsDefaults[i]))) {
                assign(names(graphicsDefaults)[i], graphicsDefaults[[i]], inherits = TRUE)
            }
        }

    }
    return(configLoaded)
}

.restoreDefaultGraphicsSettings <- function(...) {
    settings$graphics <<- settings$graphicsDefaults
    if (!is.null(widgets$freqLineWidthChooser)) {
        status$suppressRedraw <<- TRUE
        svalue(widgets$ColNumInput) <<- settings$graphics$numBrainCols
        svalue(widgets$SkipInput) <<- settings$graphics$skipSlices
        svalue(widgets$ThresholdInput) <<- settings$graphics$Threshold
        svalue(widgets$BrainColSlider) <<- settings$graphics$brainColValue
        svalue(widgets$BackgroundSlider) <<- settings$graphics$brainBackgroundValue
        svalue(widgets$timeCourseLineWidthChooser) <<- settings$graphics$TimePlotLineWidth
        svalue(widgets$freqLineWidthChooser) <<- settings$graphics$FreqPlotLineWidth
        svalue(widgets$motionLineAlphaChooser) <<- settings$graphics$MotionPlotLineAlpha
        status$suppressRedraw <<- FALSE
        updatePlots(NULL)
    }
}


# TODO: Find where to put these lines:
# Attempt to load saved graphics settings.  If unable, load defaults.
#if (!loadGraphicsSettings()) {
#  restoreDefaultGraphicsSettings()
#}

# End graphics settings functions
#==============================================================================#



#==============================================================================#
# Function for creating color picker modal
colorPicker <- function() {

    w <- gwindow("Color Picker")
    mainGroup <- ggroup(horizontal = FALSE, container = w, expand = TRUE)
    topGroup <- ggroup(horizontal = TRUE, container = mainGroup, expand = TRUE)
    valueFrame <- gframe(container = topGroup)
    hueFrame <- gframe(container = topGroup, expand = TRUE)
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
    par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
    image(rainbowmatrix, col = rainbowcolors, axes = F)
    hueSelector <- function(h, ...) {
        x <- h$x
        y <- h$y
        if (x < -1 || x > 1 || y < 0 || y > 1)
            return()
        index <- round(y * ncol(rainbowmatrix))
        chosenHue <- rainbowcolors[index]
        updateValuePlot(chosenHue)
    }
    addHandlerClicked(huePlot, hueSelector)
    updateValuePlot <- function(hue) {
        visible(valuePlot) <- TRUE
        par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
        hueRampPalette <- colorRampPalette(c("white", hue, "black"))
        hueColorMatrix <<- hueRampPalette(1000)
        hueMatrix <<- matrix(seq(0, 1, length.out = 1000), nrow = 1)
        image(hueMatrix, col = hueColorMatrix, axes = F)
    }

    updateValuePlot(rainbowcolors[1])
    valueSelector <- function(h, ...) {
        x <- h$x
        y <- h$y
        if (x < -1 || x > 1 || y < 0 || y > 1)
            return()
        index <- round(y * ncol(hueMatrix))
        chosenValue <- hueColorMatrix[index]
        updateSelectionPlot(chosenValue)
        selection <<- chosenValue
    }
    addHandlerClicked(valuePlot, valueSelector)

    updateSelectionPlot <- function(value) {
        visible(selectionPlot) <- TRUE
        par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
        image(matrix(1), col = value, axes = FALSE)
    }

    finalSelection <- NULL

    makeSelection <- function(h, ...) {
        if (!is.null(selection)) {
            finalSelection <<- selection
        } else {
            finalSelection <<- NA
        }
        dispose(w)
    }
    addHandlerClicked(selectButton, makeSelection)

    cancelSelection <- function(h, ...) {
        finalSelection <<- NA
        dispose(w)
    }
    addHandlerClicked(cancelButton, cancelSelection)
    addHandlerUnrealize(w, cancelSelection)

    while (is.null(finalSelection)) {
        Sys.sleep(0.5)
    }
    return(finalSelection)
} # End colorPicker function definition
#==============================================================================#



#==============================================================================#
# Function for creating and populating the main GUI
.createGUI <- function() {

    # main window
    win <<- gwindow("Melodic Results Viewer")
    size(win) <<- c(1600, 1000)
    Sys.sleep(0.1)
    size(win) <<- c(500, 500)

    # status bar
    widgets$statusBar <<- gstatusbar("", container = win)

    # big group
    widgets$bigGroup <<- glayout(horizontal = FALSE, container = win, expand = TRUE)

    # group for the top portion (main axial viewer and component table)
    widgets$topGroup <<- glayout(horizontal = TRUE, container = widgets$bigGroup, expand = TRUE)
    widgets$bigGroup[1, 1, expand = TRUE] <<- widgets$topGroup

    # group for the bottom portion
    widgets$bottomGroup <<- ggroup(horizontal = TRUE, container = widgets$bigGroup, expand = TRUE)
    widgets$bigGroup[2, 1, expand = TRUE] <<- widgets$bottomGroup

    # group for control buttons
    widgets$buttonGroup <<- ggroup(horizontal = TRUE, container = widgets$bigGroup)
    widgets$bigGroup[3, 1] <<- widgets$buttonGroup

    # Populate Top Group
    widgets$MainPlotGroup <<- ggroup(horizontal = FALSE, container = widgets$topGroup, expand = TRUE)
    widgets$topGroup[1, 1:5, expand = TRUE] <<- widgets$MainPlotGroup
    widgets$MainPlotLabel <<- glabel("", container = widgets$MainPlotGroup)
    widgets$MainPlotFrame <<- gframe("", container = widgets$MainPlotGroup, expand = TRUE)
    widgets$MainPlot <<- ggraphics(width = 150, height = 300, container = widgets$MainPlotFrame, handler = .self$updatePlots,
        expand = TRUE)
    widgets$CompTable <<- gtable(data$COMPTABLE, container = widgets$topGroup, expand = TRUE)
    widgets$topGroup[1, 6, expand = TRUE] <<- widgets$CompTable

    # Populate Bottom Group
    widgets$PlotGroup <<- ggroup(horizontal = FALSE, container = widgets$bottomGroup, expand = TRUE)
    widgets$TimeFrame <<- gframe("Timecourse", container = widgets$PlotGroup, expand = TRUE)
    widgets$TimePlot <<- ggraphics(width = 150, height = 50, container = widgets$TimeFrame, expand = TRUE)
    widgets$FreqFrame <<- gframe("Powerspectrum of Timecourse", container = widgets$PlotGroup, expand = TRUE)
    widgets$FreqPlot <<- ggraphics(width = 150, height = 50, container = widgets$FreqFrame, expand = TRUE)
    widgets$GraphicsFrame <<- gframe("Graphics Options", container = widgets$bottomGroup)
    widgets$ClassificationFrame <<- gframe("Classification", container = widgets$bottomGroup)
    Sys.sleep(0.5)

    # Populate Graphics Frame
    widgets$GraphicsTable <<- glayout(container = widgets$GraphicsFrame)
    widgets$GraphicsTable[1, 1] <<- glabel("# Columns:", container = widgets$GraphicsTable)
    widgets$ColNumInput <<- gcombobox(1:100, selected = settings$graphicsDefaults$numBrainCols, container = widgets$GraphicsTable,
        handler = .self$updatePlots)
    widgets$GraphicsTable[1, 2] <<- widgets$ColNumInput
    widgets$GraphicsTable[2, 1] <<- glabel("Skip Slices:", container = widgets$GraphicsTable)
    widgets$SkipInput <<- gcombobox(1:100, selected = settings$graphicsDefaults$skipSlices, container = widgets$GraphicsTable,
        handler = .self$updatePlots)
    widgets$GraphicsTable[2, 2] <<- widgets$SkipInput
    widgets$GraphicsTable[3, 1] <<- glabel("Threshold: +/-", container = widgets$GraphicsTable)
    widgets$ThresholdInput <<- gedit("2.3", container = widgets$GraphicsTable, handler = .self$updatePlots)
    widgets$GraphicsTable[3, 2] <<- widgets$ThresholdInput
    widgets$GraphicsTable[4, 1] <<- glabel("Brain darkness:", container = widgets$GraphicsTable)
    widgets$BrainColSlider <<- gspinbutton(from = 0, to = 1, by = 0.2, value = settings$graphicsDefaults$brainColValue,
        container = widgets$GraphicsTable, handler = .self$updatePlots)
    widgets$GraphicsTable[4, 2] <<- widgets$BrainColSlider
    widgets$GraphicsTable[5, 1] <<- glabel("Background darkness:", container = widgets$GraphicsTable)
    widgets$BackgroundSlider <<- gspinbutton(from = 0, to = 100, by = 20, value = settings$graphicsDefaults$brainBackgroundValue,
        container = widgets$GraphicsTable, handler = .self$updatePlots)
    widgets$GraphicsTable[5, 2] <<- widgets$BackgroundSlider
    widgets$ShowMotionCheckbox <<- gcheckbox("Show Motion Plot", checked = !is.null(data$MOTIONFILE),
        container = widgets$GraphicsTable, handler = function(h, ...) {
          .self$drawTimeFigures(svalue(widgets$CompTable))
        })
    if (is.null(data$MOTIONFILE)) {
      enabled(widgets$ShowMotionCheckbox) <<- FALSE
    }
    widgets$GraphicsTable[6, 1] <<- widgets$ShowMotionCheckbox

    widgets$TimeOptionsToggle <<- gexpandgroup("Timecourse Plot Options", horizontal = FALSE, container = widgets$GraphicsTable)
    widgets$GraphicsTable[7, 1:2] <<- widgets$TimeOptionsToggle
    widgets$timeCourseLineColorButton <<- gbutton("Set Line Color", container = widgets$TimeOptionsToggle, action = "TimePlotLineColor", handler = .self$colorPickerHandler)
    widgets$timeCourseBgColorButton <<- gbutton("Set Background Color", container = widgets$TimeOptionsToggle, action = "TimePlotBackgroundColor",
        handler = .self$colorPickerHandler)
    widgets$timeCourseLabelsColorButton <<- gbutton("Set Labels Color", container = widgets$TimeOptionsToggle, action = "TimePlotLabelColor", handler = .self$colorPickerHandler)
    widgets$timeCourseLineWidthGroup <<- ggroup(horizontal = TRUE, container = widgets$TimeOptionsToggle)
    widgets$timeCourseLineWidthLabel <<- glabel("Set Line Width:", container = widgets$timeCourseLineWidthGroup)
    widgets$timeCourseLineWidthChooser <<- gspinbutton(from = 0.1, to = 3, by = 0.1, value = settings$graphics$TimePlotLineWidth, container = widgets$timeCourseLineWidthGroup,
        handler = function(h, ...) {
            settings$graphics$TimePlotLineWidth <<- svalue(widgets$timeCourseLineWidthChooser)
            .self$drawTimeFigures(svalue(widgets$CompTable))
        })

    widgets$FreqOptionsToggle <<- gexpandgroup("Powerspectrum Plot Options", horizontal = FALSE, container = widgets$GraphicsTable)
    widgets$GraphicsTable[8, 1:2] <<- widgets$FreqOptionsToggle
    widgets$freqLineColorButton <<- gbutton("Set Line Color", container = widgets$FreqOptionsToggle, action = "FreqPlotLineColor", handler = .self$colorPickerHandler)
    widgets$freqBgColorButton <<- gbutton("Set Background Color", container = widgets$FreqOptionsToggle, action = "FreqPlotBackgroundColor", handler = .self$colorPickerHandler)
    widgets$freqLabelsColorButton <<- gbutton("Set Labels Color", container = widgets$FreqOptionsToggle, action = "FreqPlotLabelColor", handler = .self$colorPickerHandler)
    widgets$freqLineWidthGroup <<- ggroup(horizontal = TRUE, container = widgets$FreqOptionsToggle)
    widgets$freqLineWidthLabel <<- glabel("Set Line Width:", container = widgets$freqLineWidthGroup)
    widgets$freqLineWidthChooser <<- gspinbutton(from = 0.1, to = 3, by = 0.1, value = settings$graphics$FreqPlotLineWidth, container = widgets$freqLineWidthGroup,
        handler = function(h, ...) {
            settings$graphics$FreqPlotLineWidth <<- svalue(widgets$freqLineWidthChooser)
            .self$drawTimeFigures(svalue(widgets$CompTable))
        })

    widgets$MotionOptionsToggle <<- gexpandgroup("Motion Plot Options", horizontal = FALSE, container = widgets$GraphicsTable)
    widgets$GraphicsTable[9, 1:2] <<- widgets$MotionOptionsToggle
    widgets$motionLineColorButton <<- gbutton("Set Line Color", container = widgets$MotionOptionsToggle, action = "MotionPlotLineColor", handler = .self$colorPickerHandler)
    widgets$motionLineAlphaGroup <<- ggroup(horizontal = T, container = widgets$MotionOptionsToggle)
    widgets$motionLineAlphaLabel <<- glabel("Line Opacity:", container = widgets$motionLineAlphaGroup)
    widgets$motionLineAlphaChooser <<- gspinbutton(from = 0, to = 99, by = 10, value = settings$graphics$MotionPlotLineAlpha, container = widgets$motionLineAlphaGroup,
        handler = function(h, ...) {
            settings$graphics$MotionPlotLineAlpha <<- svalue(widgets$motionLineAlphaChooser)
            .self$drawTimeFigures(svalue(widgets$CompTable))
        })
    if (is.null(data$MOTIONFILE))
        enabled(widgets$MotionOptionsToggle) <<- FALSE

    widgets$GraphicsTable[10, 1:2] <<- gbutton("Save Graphics Settings", container = widgets$GraphicsTable, handler = .self$saveGraphicsSettings)
    widgets$GraphicsTable[11, 1:2] <<- gbutton("Restore Default Settings", container = widgets$GraphicsTable, handler = .self$restoreDefaultGraphicsSettings)

    # Populate Classification Frame
    classificationOptions <- c("Signal", "Unknown", "Unclassified Noise", "Movement", "Cardiac", "White matter", "Non-brain", "MRI",
                               "Susceptibility-motion", "Sagittal sinus", "Respiratory")
    widgets$ClassificationRadio <<- gradio(classificationOptions, horizontal = FALSE, container = widgets$ClassificationFrame, handler = .self$updateClassLabel)


    # Populate button group
    widgets$ButtonFrame <<- gframe("", horizontal = TRUE, container = widgets$buttonGroup)
    widgets$LoadButton <<- gbutton("Load ICA directory", container = widgets$ButtonFrame, handler = .self$getICADIR)
    widgets$LoadStandardButton <<- gbutton("Load Standard File", container = widgets$ButtonFrame, handler = function(h, ...) {
      data$STANDARDFILE <<- gfile(type = "open",
                                initialfilename = paste0(Sys.getenv('FSLDIR'), '/data/standard/MNI152_T1_2mm_brain.nii.gz'))
      loadStandard(data$STANDARDFILE)
    })
    widgets$LoadMotionButton <<- gbutton("Load Motion File", container = widgets$ButtonFrame, handler = .self$loadMotionFile)
    widgets$SaveButton <<- gbutton("Save Classification File", container = widgets$ButtonFrame, handler = .self$saveClassificationFile)
    widgets$ExitButton <<- gbutton("Exit", container = widgets$ButtonFrame, handler = function(h, ...){
      status$exit <<- TRUE
      dispose(win)
    })

}  # End createGUI function definition
#==============================================================================#



#==============================================================================#
# Various widget handlers and misc functions

# Updates all three plots
.updatePlots <- function(h, ...) {
    if (status$suppressRedraw)
        return()
    compNum <- svalue(widgets$CompTable)
    svalue(widgets$MainPlotLabel) <<- compNum
    if (length(widgets$CompTable[compNum]$ClassName) > 0) {
        if (widgets$CompTable[compNum]$ClassName == "") {
            widgets$CompTable[compNum, 2] <<- "Signal"
        }
    }
    svalue(widgets$ClassificationRadio) <<- widgets$CompTable[compNum]$ClassName
    settings$graphics$Threshold <<- as.numeric(svalue(widgets$ThresholdInput))
    settings$graphics$skipSlices <<- svalue(widgets$SkipInput)
    settings$graphics$numBrainCols <<- svalue(widgets$ColNumInput)
    settings$graphics$brainColValue <<- svalue(widgets$BrainColSlider)
    settings$graphics$brainBackgroundValue <<- svalue(widgets$BackgroundSlider)
    drawTimeFigures(compNum)
    drawBrains(compNum)
}  # End updatePlots

# Handler to update `class` and `To_Remove` columns of table
.updateClassLabel <- function(h, ...) {
    compNum <- as.numeric(svalue(widgets$MainPlotLabel))
    thisClassName <- svalue(widgets$ClassificationRadio)
    widgets$CompTable[compNum]$ClassName <<- thisClassName
    widgets$CompTable[compNum]$To_Remove <<- ifelse(!thisClassName %in% c("Signal", "Unknown"), "X", "")
}  # End updateClassLabel

# Function to choose and load motion file
.loadMotionFile <- function(h, ...) {
    initialfilename <- ifelse(file.exists("../../Movement_RelativeRMS.txt"), "../../Movement_RelativeRMS.txt", ".")
    motionfile <- gfile("Select Motion File", type = "open", initialfilename = initialfilename)
    if (is.na(motionfile))
        return()
    data$MOTIONFILE <<- motionfile
    motiondata <- read.table(motionfile)
    if (ncol(motiondata) > 1) gmessage("The file you selected has multiple columns. \
Are you sure it is a summarized motion file and not a file with separate columns \
for movement in the x, y, and z directions?", title = "Warning", icon = "warning", parent = win)
    data$MOTIONDATA <<- motiondata[[1]]
    enabled(widgets$ShowMotionCheckbox) <<- TRUE
    enabled(widgets$MotionOptionsToggle) <<- TRUE
    svalue(widgets$ShowMotionCheckbox) <<- TRUE
    drawTimeFigures(svalue(widgets$CompTable))
}  # End loadMotionFile

# Function to load classification file
.loadClassificationFile <- function() {
    fname <- paste(data$ICADIR, "/.classification.csv", sep = "")
    output <- NULL
    if (file.exists(fname)) {
        output <- read.csv(fname, stringsAsFactors = FALSE)
    }
    return(output)
}  # End loadClassificationFile

# Function to save file
.saveClassificationFile <- function(...) {
    dat <- CompTable[]
    dat2 <- subset(dat, !ClassName %in% c("Signal", "Unknown", ""))
    formatted <- paste(dat2$IC, collapse = ", ")
    formatted <- paste("[", formatted, "]", sep = "")
    outfile <- ""
    outfile <- gfile(type = "save", initialfilename = paste(getwd(), "hand_labels_noise.txt", sep = "/"))
    if (outfile != "") {
        sink(outfile)
        writeLines(formatted)
        sink()
        write.csv(dat, paste(ICADIR, "/.classification.csv", sep = ""), row.names = FALSE, quote = FALSE)
    }
}  # End saveClassificationFile

# Function to load information from ICA directory Given an ica directory, populate values
.loadICADIR <- function() {
  ICADIR <- data$ICADIR

  # get number of components
  datfile <- list.files(ICADIR, pattern = '^melodic_IC.nii.*', full.names = TRUE)
  svalue(widgets$statusBar) <<- paste("Now loading", datfile); Sys.sleep(.1)
  data$MELDATA <<- RNifti::readNifti(datfile)
  svalue(widgets$statusBar) <<- paste(datfile, "loaded."); Sys.sleep(.1)
  data$NCOMPS <<- dim(data$MELDATA)[4]
  data$MELDIM <<- dim(data$MELDATA)[1:3]

  # report directory must be present to find time and frequency files
  reportDir <- paste0(ICADIR, '/report')
  if (!dir.exists(reportDir)) {
    gmessage("There is no 'report' directory in the ICA directory specified, so \
melviewr will be unable to load timecourse and powerspectrum data files.",
             title = "Warning", icon = "warning", parent = win)
  } else {
    # get time and frequency images
    tFiles <- gtools::mixedsort(list.files(reportDir,
                                           pattern = "^t.*txt",
                                           full.names = TRUE))
    fFiles <- gtools::mixedsort(list.files(reportDir,
                                           pattern = "^f.*txt",
                                           full.names = TRUE))
    if(length(tFiles) != length(fFiles) || length(tFiles) != data$NCOMPS) {
      gmessage("There is something wrong with the number of timecourse/powerspectrum files in\
 the 'report' directory of the provided ICA directory. The t*.txt and f*.txt files should be equal \
in number and should also equal the number of volumes in the 'melodic_IC' 4D Nifti file.

Until these conditions are met, timecourse and powerspectrum figures cannot be displayed.",
               title = "Warning", icon = "warning", parent = win)
    } else {
      data$TIMEDATFILES <<- tFiles
      data$FREQDATFILES <<- fFiles
    }
  }

  data$TR <<- getTR()

  initializePlot()
}  # End loadICADIR

# select an ICA directory
.getICADIR <- function(...) {
    data$ICADIR <<- gfile(type = "selectdir", initialfilename = ".")
    loadICADIR()
}  # END getICADIR

# handler for 'select color' buttons
.colorPickerHandler <- function(h, ...) {
    # note, this handler will only work with widgets that have an 'action' defined
    newColor <- colorPicker()
    if (is.na(newColor))
        return()
    settings$graphics[h$action] <<- newColor
    drawTimeFigures(svalue(widgets$CompTable))
}  # end colorPickerHandler

# Gets the TR for a given ica directory for use with timecourse and frequency plots
.getTR <- function() {
  logfile <- paste0(data$ICADIR, '/log.txt')
  if(!file.exists(logfile)) {
    gmessage("Warning: No log file was found in the ICA directory specified. A TR of 1 second will be assumed when \
creating timecourse and powerspectrum plots.")
    TR <- 1
  } else {
    logtxt <- scan(paste(data$ICADIR, "/log.txt", sep = ""), "character", quiet = TRUE)
    TRstring <- grep("--tr=", logtxt, value = TRUE)
    TR <- as.numeric(gsub("--tr=", "", TRstring))
  }
  if(length(TR) < 1) {
    return(NULL)
  } else {
    return(TR)
  }
}  # End getTR

# Load standard file data
.loadStandard <- function(...) {

  standarddat <- RNifti::readNifti(data$STANDARDFILE)
  if (!identical(dim(standarddat), data$MELDIM)) {
    gmessage("The voxel dimensions of the standard Nifti file must match those of \
the melodic_IC file in the ICA directory.", title = "Voxel Dimensions Match Error",
             icon = "error")
    return()
  }

  data$STANDARDDATA <<- standarddat

  # find first and last slices that aren't all 0s
  for (i in 1:dim(data$STANDARDDATA)[3]) {
      if (!all(data$STANDARDDATA[, , i] == 0)) {
          data$STARTSLICE <<- i
          break()
      }
  }
  for (i in dim(data$STANDARDDATA)[3]:data$STARTSLICE) {
      if (!all(data$STANDARDDATA[, , i] == 0)) {
          data$ENDSLICE <<- i
          break()
      }
  }
  updatePlots(NULL)
}  # End loadStandard

# End widget handlers definitions
#==============================================================================#



#==============================================================================#
# Functions for drawing plots

# TODO: Separate these into main plot and table functions
.initializePlot <- function() {
  data$COMPTABLE <<- data.frame(array(dim = c(data$NCOMPS, 3)), stringsAsFactors = FALSE)
  names(data$COMPTABLE) <<- c("IC", "ClassName", "To_Remove")
  if (nrow(data$COMPTABLE) > 0) {
    data$COMPTABLE$IC <<- 1:data$NCOMPS
    data$COMPTABLE$ClassName <<- ""
    data$COMPTABLE$To_Remove <<- ""
  }
  widgets$CompTable[] <<- data$COMPTABLE
  if (nrow(data$COMPTABLE) > 0) {
      drawTimeFigures(1)
      svalue(widgets$CompTable) <<- 1
      drawBrains(1)
      svalue(widgets$MainPlotLabel) <<- 1
      prevClass <- loadClassificationFile()
      if (!is.null(prevClass) && nrow(prevClass) == nrow(widgets$CompTable[]))
        widgets$CompTable[] <<- prevClass
  }
  addHandlerClicked(widgets$CompTable, handler = updatePlots)
}

# Actually draws brains to main plot
.drawBrains <- function(compNum) {
    # heat colors
    heatcols <- grDevices::heat.colors(2000)
    # cool colors
    coolcols <- grDevices::topo.colors(10000)[900:3300]
    visible(widgets$MainPlot) <<- TRUE
    bgCol <- paste0("gray", settings$graphics$brainBackgroundValue)
    braincols <- grDevices::gray.colors(n = 20000, start = 0,
                            end = settings$graphics$brainColValue, gamma = 0.6)

    # If we haven't loaded a standard yet, then we can't draw its data
    if(is.null(data$STANDARDDATA)) {
      startSlice <- 1
      endSlice <- dim(data$MELDATA)[3]
    } else {
      startSlice <- data$STARTSLICE
      endSlice <- data$ENDSLICE
    }
    thisbraindat <- data$MELDATA[, , , compNum]
    sliceIndices <- seq(startSlice, endSlice, settings$graphics$skipSlices)
    nCols <- settings$graphics$numBrainCols
    nRows <- ceiling(length(sliceIndices)/nCols)
    par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0),
        mfrow = c(nRows, nCols), bg = bgCol)
    rnge <- range(thisbraindat)
    for (i in sliceIndices) {
        if (!is.null(data$STANDARDDATA)) {
          image(data$STANDARDDATA[, , i], col = braincols, axes = F,
                useRaster = T, zlim = c(23, max(data$STANDARDDATA[, , i])))
        } else {
          plot.new()
        }
        if (rnge[2] > 0 && rnge[2] > settings$graphics$Threshold)
            image(thisbraindat[, , i], col = heatcols, axes = F,
                  useRaster = T, zlim = c(settings$graphics$Threshold, rnge[2]),
                  add = T)
        if (rnge[1] < 0 && abs(rnge[1]) > settings$graphics$Threshold)
            image(thisbraindat[, , i] * -1, col = coolcols,
                  axes = F, useRaster = T,
                  zlim = c(settings$graphics$Threshold, abs(rnge[1])), add = T)
    }
}  # End drawBrains

# Draws both time figures, calls other draw functions
.drawTimeFigures <- function(compNum) {
  if(is.null(data$TIMEDATFILES)) return()
  tdatFile <- data$TIMEDATFILES[compNum]
  fdatFile <- data$FREQDATFILES[compNum]
  tdat <- read.table(tdatFile)[[1]]
  fdat <- read.table(fdatFile)[[1]]
  nTRs <- length(tdat)
  drawTimeCourse(tdat, data$TR)
  if (svalue(widgets$ShowMotionCheckbox))
      drawMotion(tdat, data$MOTIONDAT, data$TR)
  drawFrequency(fdat, data$TR, nTRs)
}  # End drawTimeFigures

# Draws timecourse plot
.drawTimeCourse <- function(tdat, TR) {
    visible(widgets$TimePlot) <<- TRUE
    par(mar = c(3, 3, 1, 1), oma = c(0, 0, 0, 0),
        lwd = settings$graphics$TimePlotLineWidth,
        bg = settings$graphics$TimePlotBackgroundColor,
        fg = settings$graphics$TimePlotLabelColor,
        col.axis = settings$graphics$TimePlotLabelColor,
        col.lab = settings$graphics$TimePlotLabelColor)
    seconds <- TR * 1:length(tdat)
    plot(seconds, tdat, t = "l", ylab = "", xlab = "",
         col = settings$graphics$TimePlotLineColor)
    title(ylab = "Normalized Response", line = 2)
    title(xlab = paste("Time (seconds); TR =", TR, "s"), line = 2)
}  # End drawTimeCourse

# Draws motion data onto timecourse plot
.drawMotion <- function(tdat, motionDat, TR) {
    visible(widgets$TimePlot) <<- TRUE
    rnge <- max(tdat) - min(tdat)
    mdat <- motionDat/max(motionDat) * rnge/2
    mdat <- mdat + mean(range(tdat))
    seconds <- TR * 1:length(mdat)
    alphaNum <- round((settings$graphics$MotionPlotLineAlpha/100) * 256)
    alphaStr <- sprintf("%0.2x", alphaNum)
    lineColor <- paste(settings$graphics$MotionPlotLineColor, alphaStr, sep = "")
    lines(seconds, mdat, col = lineColor, lwd = settings$graphics$TimePlotLineWidth)
}  # End drawMotion

# Draws powerspectrum plot
.drawFrequency <- function(fdat, TR, nTRs) {
    visible(widgets$FreqPlot) <<- TRUE
    maximum <- 1/(TR * nTRs)/2 * nTRs
    indices <- seq(0, maximum, length.out = length(fdat))
    par(mar = c(3, 3, 1, 1), oma = c(0, 0, 0, 0),
        lwd = settings$graphics$FreqPlotLineWidth,
        bg = settings$graphics$FreqPlotBackgroundColor,
        fg = settings$graphics$FreqPlotLabelColor,
        col.axis = settings$graphics$FreqPlotLabelColor,
        col.lab = settings$graphics$FreqPlotLabelColor)
    plot(indices, fdat, t = "l", xaxp = c(0, max(indices), 7), ylab = "",
         xlab = "", col = settings$graphics$FreqPlotLineColor)
    title(ylab = "Power", line = 2)
    title(xlab = "Frequency (in Hz)", line = 2)
}  # End drawFrequency

#==============================================================================#



#==============================================================================#
# Functions for testing validity of inputs

testICADIR <- function(ICADIR) {
  if (!dir.exists(ICADIR)) stop(paste('ICA directory does not exist:', ICADIR), call. = FALSE)
  if (!file.exists(paste0(ICADIR, '/melodic_IC.nii.gz')) && !file.exists(paste0(ICADIR, '/melodic_IC.nii')))
    stop(paste('No "melodic_IC" file found in ICA directory:', ICADIR), call. = FALSE)
}

testStandardFile <- function(standard_file) {
  if (!file.exists(standard_file)) stop(paste('The standard file specified does not exist:', standard_file))
}

testMotionFile <- function(motion_file) {
  if (!file.exists(motion_file)) stop(paste('The motion file specified does not exist:', motion_file))
}

# END Functions for testing validity of inputs
#==============================================================================#



#==============================================================================#
# Function to initialize viewr object

Viewr <- setRefClass("Viewr", fields = list(
    win = "gWindow",
    widgets = "list",
    settings = "list",
    status = "list",
    data = "list"
  ), methods = list(
    createGUI = .createGUI,
    drawFrequency = .drawFrequency,
    drawMotion = .drawMotion,
    drawTimeCourse = .drawTimeCourse,
    drawTimeFigures = .drawTimeFigures,
    drawBrains = .drawBrains,
    initializePlot = .initializePlot,
    loadStandard = .loadStandard,
    getTR = .getTR,
    colorPickerHandler = .colorPickerHandler,
    getICADIR = .getICADIR,
    loadICADIR = .loadICADIR,
    saveClassificationFile = .saveClassificationFile,
    loadClassificationFile = .loadClassificationFile,
    loadMotionFile = .loadMotionFile,
    updateClassLabel = .updateClassLabel,
    updatePlots = .updatePlots,
    saveGraphicsSettings = .saveGraphicsSettings,
    loadGraphicsSettings = .loadGraphicsSettings,
    restoreDefaultGraphicsSettings = .restoreDefaultGraphicsSettings
  )
)

createViewrObject <- function() {
  viewr <- Viewr$new(
    status = list(
      suppressRedraw = FALSE,
      exit = FALSE
    ),
    settings = list(
      graphicsDefaults = list(
        skipSlices = 3,
        numBrainCols = 9,
        Threshold = 2.3,
        brainColValue = 0.5,
        brainBackgroundValue = 0,
        TimePlotLineColor = "black",
        TimePlotLineWidth = 0.5,
        TimePlotBackgroundColor = "white",
        TimePlotLabelColor = "black",
        FreqPlotLineColor = "black",
        FreqPlotLineWidth = 0.5,
        FreqPlotBackgroundColor = "white",
        FreqPlotLabelColor = "black",
        MotionPlotLineColor = "#FF0000",
        MotionPlotLineAlpha = 50
      )
    ),
    data = list(
      ICADIR = NULL,
      MOTIONFILE = NULL,
      MOTIONDATA = NULL,
      MELDATA = NULL,
      STANDARDFILE = NULL,
      STANDARDDATA = NULL,
      FSLDIR = NULL,
      MELDIM = NULL,
      NCOMPS = 0,
      TR = NULL,
      COMPTABLE = data.frame(array(dim = c(0, 3)),
                             stringsAsFactors = FALSE),
      TIMEDATFILES = NULL,
      FREQDATFILES = NULL,
      STARTSLICE = NULL,
      ENDSLICE = NULL
    )
  )
  names(viewr$data$COMPTABLE) <- c("IC", "ClassName", "To_Remove")
  return(viewr)
}  # End createViewrObject
#==============================================================================#




#==============================================================================#
# Main function the user will see.
#' melviewr
#'
#' View and Classify Components from a Melodic Analysis
#' @param melodic_dir string Path to MELODIC output directory
#' @param standard_file string Optional path to a 3-dimensional Nifti standard file
#' of the same voxel dimensions as the melodic output
#' @param motion_file string Optional path to a summary motion text file. This file
#' should have one column and as many rows as there are volumes in the functional
#' data
#' @export
#' @import gWidgets
melviewr <- function(melodic_dir, standard_file = NULL, motion_file = NULL) {

    # Keep environment tidy
    old <- options(stringsAsFactors = FALSE)
    on.exit(options(old), add = TRUE)
    options(guiToolkit = "RGtk2")

    # make viewr object
    viewr <- createViewrObject()

    # test validity of inputs
    melodic_dir <- normalizePath(melodic_dir)
    testICADIR(melodic_dir)
    if (!is.null(standard_file)) {
      testStandardFile(standard_file)
      viewr$data$STANDARDFILE <- standard_file
    }
    if (!is.null(motion_file))   testMotionFile(motion_file)

    # move to melodic dir (might not be necessary)
    oldwd <- setwd(melodic_dir)
    on.exit(setwd(oldwd), add = TRUE)

    # Begin loading data
    viewr$data$ICADIR <- melodic_dir

    viewr$createGUI()
    viewr$loadICADIR()
    if (!is.null(standard_file)) viewr$loadStandard()

    waitForExit <- function(...) {
      while (!viewr$status$exit) {
        Sys.sleep(1)
      }
    }

    addHandlerUnrealize(viewr$win , handler = function(h, ...) {
      viewr$status$exit <- TRUE
      dispose(viewr$win)
    })

    if (!interactive()) {
      cat('Not interactive\n')
      waitForExit()
    }

    invisible(viewr)
}  # End melviewr function definition
#==============================================================================#



#==============================================================================#
# Documentation for the package
#' melviewr: A viewer for MELODIC output and ICA+FIX classification.
#'
#' The melviewr package allows the user to easily view and classify
#' MELODIC output for the purposes of later running ICA+FIX. The user
#' categorizes a component as signal or noise based on its spatial
#' characteristics as well as its temporal profile. melviewr can then save
#' a text file of these classifications in the format required by ICA+FIX.
#'
#' @section melviewr functions:
#' melviewr
#'
#' @docType package
#' @name melviewr-package
NULL
