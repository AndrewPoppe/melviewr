

#==============================================================================#
# Functions for saving/loading graphics settings

saveGraphicsSettings <- function(h, ...) {
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

loadGraphicsSettings <- function() {
    configFile <- "~/.melviewR.config"
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

restoreDefaultGraphicsSettings <- function(...) {
    for (i in 1:length(graphicsDefaults)) {
        assign(names(graphicsDefaults)[i], graphicsDefaults[[i]], inherits = TRUE)
    }
    if (exists("freqLineWidthChooser")) {
        suppressRedraw <<- TRUE
        svalue(ColNumInput) <- numBrainCols
        svalue(SkipInput) <- skipSlices
        svalue(ThresholdInput) <- Threshold
        svalue(BrainColSlider) <- brainColValue
        svalue(BackgroundSlider) <- brainBackgroundValue
        svalue(timeCourseLineWidthChooser) <- TimePlotLineWidth
        svalue(freqLineWidthChooser) <- FreqPlotLineWidth
        svalue(motionLineAlphaChooser) <- MotionPlotLineAlpha
        suppressRedraw <<- FALSE
        updatePlots(NULL)
    }
}


# TODO: Find where to put these lines:
# Attempt to load saved graphics settings.  If unable, load defaults.
if (!loadGraphicsSettings()) {
    #restoreDefaultGraphicsSettings()
}

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
# Functions for creating individual widgets

# Creates the structural items for the entire gui (groups, frames, etc)
createStructuralElements <- function(viewr) {

}  # end createStructualElements

# creates the main brain plot with axial slices
createMainPlot <- function(viewr) {

}  # end createMainPlot

# creates the gtable widget
createComponentTable <- function(viewr) {

}  # End createComponentTable

# creates the TimeCourse plot
createTimecoursePlot <- function(viewr) {

}  # End createTimecoursePlot

# creates the Powerspectrum plot
createPowerspectrumPlot <- function(viewr) {

}  # End createPowerspectrumPlot

# creates classification options
createClassificationOptions <- function(viewr) {

}  # End createClassificationOptions

# creates all graphics options widgets
createGraphicsOptions <- function(viewr) {

}  # End createGraphicsOptions

# creates control buttons (buttons at the bottom of the GUI)
createControlButtons <- function(viewr) {

}  # End createControlButtons

# End individual widget creation functions
#==============================================================================#



#==============================================================================#
# Function to initialize viewr object
createViewrObject <- function() {
    viewr <- list(
        win = NULL,
        widgets = list(),
        settings = list(),
        status = list(
          suppressRedraw = FALSE,
          exit = FALSE
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
    viewr$settings$graphicsDefaults <- list(
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

    return(viewr)
}  # End createViewrObject
#==============================================================================#



#==============================================================================#
# Function for creating and populating the main GUI
createGUI <- function(viewr) {

    # main window
    viewr$win <- gwindow("Melodic Results Viewer")
    size(viewr$win) <- c(1600, 1000)
    Sys.sleep(0.1)
    size(viewr$win) <- c(500, 500)

    # big group
    viewr$widgets$bigGroup <- glayout(horizontal = FALSE, container = viewr$win, expand = TRUE)

    # group for the top portion (main axial viewer and component table)
    viewr$widgets$topGroup <- glayout(horizontal = TRUE, container = viewr$widgets$bigGroup, expand = TRUE)
    viewr$widgets$bigGroup[1, 1, expand = TRUE] <- viewr$widgets$topGroup

    # group for the bottom portion
    viewr$widgets$bottomGroup <- ggroup(horizontal = TRUE, container = viewr$widgets$bigGroup, expand = TRUE)
    viewr$widgets$bigGroup[2, 1, expand = TRUE] <- viewr$widgets$bottomGroup

    # group for control buttons
    viewr$widgets$buttonGroup <- ggroup(horizontal = TRUE, container = viewr$widgets$bigGroup)
    viewr$widgets$bigGroup[3, 1] <- viewr$widgets$buttonGroup

    # Populate Top Group
    viewr$widgets$MainPlotGroup <- ggroup(horizontal = FALSE, container = viewr$widgets$topGroup, expand = TRUE)
    viewr$widgets$topGroup[1, 1:5, expand = TRUE] <- viewr$widgets$MainPlotGroup
    viewr$widgets$MainPlotLabel <- glabel("", container = viewr$widgets$MainPlotGroup)
    viewr$widgets$MainPlotFrame <- gframe("", container = viewr$widgets$MainPlotGroup, expand = TRUE)
    viewr$widgets$MainPlot <- ggraphics(width = 300, height = 300, container = viewr$widgets$MainPlotFrame, handler = updatePlots,
        expand = TRUE)
    viewr$widgets$CompTable <- gtable(viewr$data$COMPTABLE, container = viewr$widgets$topGroup, expand = TRUE)
    viewr$widgets$topGroup[1, 6, expand = TRUE] <- viewr$widgets$CompTable

    # Populate Bottom Group
    viewr$widgets$PlotGroup <- ggroup(horizontal = FALSE, container = viewr$widgets$bottomGroup, expand = TRUE)
    viewr$widgets$TimeFrame <- gframe("Timecourse", container = viewr$widgets$PlotGroup, expand = TRUE)
    viewr$widgets$TimePlot <- ggraphics(width = 300, height = 100, container = viewr$widgets$TimeFrame, expand = TRUE)
    viewr$widgets$FreqFrame <- gframe("Powerspectrum of Timecourse", container = viewr$widgets$PlotGroup, expand = TRUE)
    viewr$widgets$FreqPlot <- ggraphics(width = 300, height = 100, container = viewr$widgets$FreqFrame, expand = TRUE)
    viewr$widgets$GraphicsFrame <- gframe("Graphics Options", container = viewr$widgets$bottomGroup)
    viewr$widgets$ClassificationFrame <- gframe("Classification", container = viewr$widgets$bottomGroup)
    Sys.sleep(0.5)

    # Populate Graphics Frame
    viewr$widgets$GraphicsTable <- glayout(container = viewr$widgets$GraphicsFrame)
    viewr$widgets$GraphicsTable[1, 1] <- glabel("# Columns:", container = viewr$widgets$GraphicsTable)
    viewr$widgets$ColNumInput <- gcombobox(1:100, selected = viewr$settings$graphicsDefaults$numBrainCols, container = viewr$widgets$GraphicsTable,
        handler = updatePlots)
    viewr$widgets$GraphicsTable[1, 2] <- viewr$widgets$ColNumInput
    viewr$widgets$GraphicsTable[2, 1] <- glabel("Skip Slices:", container = viewr$widgets$GraphicsTable)
    viewr$widgets$SkipInput <- gcombobox(1:100, selected = viewr$settings$graphicsDefaults$skipSlices, container = viewr$widgets$GraphicsTable,
        handler = updatePlots)
    viewr$widgets$GraphicsTable[2, 2] <- viewr$widgets$SkipInput
    viewr$widgets$GraphicsTable[3, 1] <- glabel("Threshold: +/-", container = viewr$widgets$GraphicsTable)
    viewr$widgets$ThresholdInput <- gedit("2.3", container = viewr$widgets$GraphicsTable, handler = updatePlots)
    viewr$widgets$GraphicsTable[3, 2] <- viewr$widgets$ThresholdInput
    viewr$widgets$GraphicsTable[4, 1] <- glabel("Brain darkness:", container = viewr$widgets$GraphicsTable)
    viewr$widgets$BrainColSlider <- gspinbutton(from = 0, to = 1, by = 0.2, value = viewr$settings$graphicsDefaults$brainColValue,
        container = viewr$widgets$GraphicsTable, handler = updatePlots)
    viewr$widgets$GraphicsTable[4, 2] <- viewr$widgets$BrainColSlider
    viewr$widgets$GraphicsTable[5, 1] <- glabel("Background darkness:", container = viewr$widgets$GraphicsTable)
    viewr$widgets$BackgroundSlider <- gspinbutton(from = 0, to = 100, by = 20, value = viewr$settings$graphicsDefaults$brainBackgroundValue,
        container = viewr$widgets$GraphicsTable, handler = updatePlots)
    viewr$widgets$GraphicsTable[5, 2] <- viewr$widgets$BackgroundSlider
    viewr$widgets$ShowMotionCheckbox <- gcheckbox("Show Motion Plot", checked = !is.null(viewr$data$MOTIONFILE),
        container = viewr$widgets$GraphicsTable, handler = function(h, ...) {
                                      drawTimeFigures(svalue(viewr$widgets$CompTable))
                                    })
    if (is.null(viewr$data$MOTIONFILE)) {
      enabled(viewr$widgets$ShowMotionCheckbox) <- FALSE
    }
    viewr$widgets$GraphicsTable[6, 1] <- viewr$widgets$ShowMotionCheckbox

    viewr$widgets$TimeOptionsToggle <- gexpandgroup("Timecourse Plot Options", horizontal = FALSE, container = viewr$widgets$GraphicsTable)
    viewr$widgets$GraphicsTable[7, 1:2] <- viewr$widgets$TimeOptionsToggle
    viewr$widgets$timeCourseLineColorButton <- gbutton("Set Line Color", container = viewr$widgets$TimeOptionsToggle, action = "TimePlotLineColor", handler = colorPickerHandler)
    viewr$widgets$timeCourseBgColorButton <- gbutton("Set Background Color", container = viewr$widgets$TimeOptionsToggle, action = "TimePlotBackgroundColor",
        handler = colorPickerHandler)
    viewr$widgets$timeCourseLabelsColorButton <- gbutton("Set Labels Color", container = viewr$widgets$TimeOptionsToggle, action = "TimePlotLabelColor", handler = colorPickerHandler)
    viewr$widgets$timeCourseLineWidthGroup <- ggroup(horizontal = TRUE, container = viewr$widgets$TimeOptionsToggle)
    viewr$widgets$timeCourseLineWidthLabel <- glabel("Set Line Width:", container = viewr$widgets$timeCourseLineWidthGroup)
    viewr$widgets$timeCourseLineWidthChooser <- gspinbutton(from = 0.1, to = 3, by = 0.1, value = viewr$settings$graphics$TimePlotLineWidth, container = viewr$widgets$timeCourseLineWidthGroup,
        handler = function(h, ...) {
            viewr$settings$graphics$TimePlotLineWidth <- svalue(viewr$widgets$timeCourseLineWidthChooser)
            drawTimeFigures(svalue(viewr$widgets$CompTable))
        })

    viewr$widgets$FreqOptionsToggle <- gexpandgroup("Powerspectrum Plot Options", horizontal = FALSE, container = viewr$widgets$GraphicsTable)
    viewr$widgets$GraphicsTable[8, 1:2] <- viewr$widgets$FreqOptionsToggle
    viewr$widgets$freqLineColorButton <- gbutton("Set Line Color", container = viewr$widgets$FreqOptionsToggle, action = "FreqPlotLineColor", handler = colorPickerHandler)
    viewr$widgets$freqBgColorButton <- gbutton("Set Background Color", container = viewr$widgets$FreqOptionsToggle, action = "FreqPlotBackgroundColor", handler = colorPickerHandler)
    viewr$widgets$freqLabelsColorButton <- gbutton("Set Labels Color", container = viewr$widgets$FreqOptionsToggle, action = "FreqPlotLabelColor", handler = colorPickerHandler)
    viewr$widgets$freqLineWidthGroup <- ggroup(horizontal = TRUE, container = viewr$widgets$FreqOptionsToggle)
    viewr$widgets$freqLineWidthLabel <- glabel("Set Line Width:", container = viewr$widgets$freqLineWidthGroup)
    viewr$widgets$freqLineWidthChooser <- gspinbutton(from = 0.1, to = 3, by = 0.1, value = viewr$settings$graphics$FreqPlotLineWidth, container = viewr$widgets$freqLineWidthGroup,
        handler = function(h, ...) {
            viewr$settings$graphics$FreqPlotLineWidth <- svalue(viewr$widgets$freqLineWidthChooser)
            drawTimeFigures(svalue(viewr$widgets$CompTable))
        })

    viewr$widgets$MotionOptionsToggle <- gexpandgroup("Motion Plot Options", horizontal = FALSE, container = viewr$widgets$GraphicsTable)
    viewr$widgets$GraphicsTable[9, 1:2] <- viewr$widgets$MotionOptionsToggle
    viewr$widgets$motionLineColorButton <- gbutton("Set Line Color", container = viewr$widgets$MotionOptionsToggle, action = "MotionPlotLineColor", handler = colorPickerHandler)
    viewr$widgets$motionLineAlphaGroup <- ggroup(horizontal = T, container = viewr$widgets$MotionOptionsToggle)
    viewr$widgets$motionLineAlphaLabel <- glabel("Line Opacity:", container = viewr$widgets$motionLineAlphaGroup)
    viewr$widgets$motionLineAlphaChooser <- gspinbutton(from = 0, to = 99, by = 10, value = viewr$settings$graphics$MotionPlotLineAlpha, container = viewr$widgets$motionLineAlphaGroup,
        handler = function(h, ...) {
            viewr$settings$graphics$MotionPlotLineAlpha <- svalue(viewr$widgets$motionLineAlphaChooser)
            drawTimeFigures(svalue(viewr$widgets$CompTable))
        })
    if (is.null(viewr$data$MOTIONFILE))
        enabled(viewr$widgets$MotionOptionsToggle) <- FALSE

    viewr$widgets$GraphicsTable[10, 1:2] <- gbutton("Save Graphics Settings", container = viewr$widgets$GraphicsTable, handler = saveGraphicsSettings)
    viewr$widgets$GraphicsTable[11, 1:2] <- gbutton("Restore Default Settings", container = viewr$widgets$GraphicsTable, handler = restoreDefaultGraphicsSettings)

    # Populate Classification Frame
    classificationOptions <- c("Signal", "Unknown", "Unclassified Noise", "Movement", "Cardiac", "White matter", "Non-brain", "MRI",
                               "Susceptibility-motion", "Sagittal sinus", "Respiratory")
    viewr$widgets$ClassificationRadio <- gradio(classificationOptions, horizontal = FALSE, container = viewr$widgets$ClassificationFrame, handler = updateClassLabel)


    # Populate button group
    viewr$widgets$ButtonFrame <- gframe("", horizontal = TRUE, container = viewr$widgets$buttonGroup)
    viewr$widgets$LoadButton <- gbutton("Load ICA directory", container = viewr$widgets$ButtonFrame, handler = getICADIR)
    viewr$widgets$LoadMotionButton <- gbutton("Load Motion File", container = viewr$widgets$ButtonFrame, handler = loadMotionFile)
    viewr$widgets$SaveButton <- gbutton("Save Classification File", container = viewr$widgets$ButtonFrame, handler = saveClassificationFile)
    viewr$widgets$ExitButton <- gbutton("Exit", container = viewr$widgets$ButtonFrame, handler = function(h, ...){
      viewr$status$exit <- TRUE
    })


    return(viewr)

}  # End createGUI function definition
#==============================================================================#



#==============================================================================#
# Various widget handlers and misc functions

# Updates all three plots
updatePlots <- function(h, ...) {
    if (suppressRedraw)
        return()
    compNum <- svalue(CompTable)
    svalue(MainPlotLabel) <- compNum
    if (length(CompTable[compNum]$ClassName) > 0) {
        if (CompTable[compNum]$ClassName == "") {
            CompTable[compNum, 2] <- "Signal"
        }
    }
    svalue(ClassificationRadio) <- CompTable[compNum]$ClassName
    Threshold <<- as.numeric(svalue(ThresholdInput))
    skipSlices <<- svalue(SkipInput)
    numBrainCols <<- svalue(ColNumInput)
    brainColValue <<- svalue(BrainColSlider)
    brainBackgroundValue <<- svalue(BackgroundSlider)
    drawTimeFigures(compNum)
    drawBrains(compNum)
}  # End updatePlots

# Handler to update `class` and `To_Remove` columns of table
updateClassLabel <- function(h, ...) {
    compNum <- as.numeric(svalue(MainPlotLabel))
    thisClassName <- svalue(ClassificationRadio)
    CompTable[compNum]$ClassName <- thisClassName
    CompTable[compNum]$To_Remove <- ifelse(!thisClassName %in% c("Signal", "Unknown"), "X", "")
}  # End updateClassLabel

# Function to choose and load motion file
loadMotionFile <- function(h, ...) {
    initialfilename <- ifelse(file.exists("../../Movement_RelativeRMS.txt"), "../../Movement_RelativeRMS.txt", ".")
    motionfile <- gfile("Select Motion File", type = "open", initialfilename = initialfilename)
    if (is.na(motionfile))
        return()
    motionFile <<- motionfile
    motionFileLoaded <<- TRUE
    motionDat <<- read.table(motionFile)[[1]]
    enabled(ShowMotionCheckbox) <- TRUE
    enabled(MotionOptionsToggle) <- TRUE
    svalue(ShowMotionCheckbox) <- TRUE
    drawTimeFigures(svalue(CompTable))
}  # End loadMotionFile

# Function to load classification file
loadClassificationFile <- function() {
    fname <- paste(ICADIR, "/.classification.csv", sep = "")
    output <- NULL
    if (file.exists(fname)) {
        output <- read.csv(fname, stringsAsFactors = FALSE)
    }
    return(output)
}  # End loadClassificationFile

# Function to save file
saveClassificationFile <- function(...) {
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
loadICADIR <- function(ICADIR) {
    # get number of components
    braindat <<- readNifti(paste(ICADIR, "/melodic_IC.nii.gz", sep = ""))
    nComps <<- dim(braindat)[4]

    # get time and frequency images
    TimeDatFiles <<- mixedsort(list.files(paste(ICADIR, "/report", sep = ""), pattern = "^t.*txt", full.names = TRUE))
    FreqDatFiles <<- mixedsort(list.files(paste(ICADIR, "/report", sep = ""), pattern = "^f.*txt", full.names = TRUE))

    initializePlot()
}  # End loadICADIR

# select an ICA directory
getICADIR <- function(...) {
    ICADIR <<- gfile(type = "selectdir", initialfilename = ".")
    loadICADIR(ICADIR)
}  # END getICADIR

# handler for 'select color' buttons
colorPickerHandler <- function(h, ...) {
    # note, this handler will only work with widgets that have an 'action' defined
    newColor <- colorPicker()
    if (is.na(newColor))
        return()
    viewr$settings$graphics[h$action] <<- newColor
    drawTimeFigures(svalue(viewr$widgets$CompTable))
}  # end colorPickerHandler

# Gets the TR for a given ica directory for use with timecourse and frequency plots
getTR <- function() {
    logtxt <- scan(paste(ICADIR, "/log.txt", sep = ""), "character", quiet = TRUE)
    TRstring <- grep("--tr=", logtxt, value = TRUE)
    TR <- as.numeric(gsub("--tr=", "", TRstring))
    return(TR)
}  # End getTR

# Load standard file data
loadStandard <- function(h, ...) {

    # find first and last slices that aren't all 0s
    for (i in 1:dim(viewr$data$STANDARDDATA)[3]) {
        if (!all(viewr$data$STANDARDDATA[, , i] == 0)) {
            viewr$data$STARTSLICE <- i
            (break)()
        }
    }
    for (i in dim(viewr$data$STANDARDDATA)[3]:viewr$data$STARTSLICE) {
        if (!all(viewr$data$STANDARDDATA[, , i] == 0)) {
            viewr$data$ENDSLICE <- i
            (break)()
        }
    }
}  # End loadStandard

# End widget handlers definitions
#==============================================================================#



#==============================================================================#
# Functions for drawing plots

# TODO: Separate these into main plot and table functions
initializePlot <- function() {
    compList <<- data.frame(array(dim = c(nComps, 3)), stringsAsFactors = FALSE)
    names(compList) <<- c("IC", "ClassName", "To_Remove")
    if (nrow(compList) > 0) {
        compList$IC <- 1:nComps
        compList$ClassName <- ""
        compList$To_Remove <- ""
    }
    CompTable[] <<- compList
    if (nrow(compList) > 0) {
        drawTimeFigures(1)
        svalue(CompTable) <- 1
        drawBrains(1)
        svalue(MainPlotLabel) <- 1
        prevClass <- loadClassificationFile()
        if (!is.null(prevClass)) {
            if (nrow(prevClass) == nrow(CompTable[]))
                CompTable[] <<- prevClass
        }
    }
    addHandlerClicked(CompTable, handler = updatePlots)
}

# Actually draws brains to main plot
drawBrains <- function(compNum) {
    # heat colors
    heatcols <- heat.colors(2000)
    # cool colors
    coolcols <- topo.colors(10000)[900:3300]
    visible(MainPlot) <- TRUE
    bgCol <- paste("gray", brainBackgroundValue, sep = "")
    braincols <- gray.colors(n = 20000, start = 0, end = brainColValue, gamma = 0.6)
    thisbraindat <- braindat[, , , compNum]
    sliceIndices <- seq(startSlice, endSlice, skipSlices)
    nCols <- numBrainCols
    nRows <- ceiling(length(sliceIndices)/nCols)
    par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), mfrow = c(nRows, nCols), bg = bgCol)
    rnge <- range(thisbraindat)
    for (i in sliceIndices) {
        image(MNIdat[, , i], col = braincols, axes = F, useRaster = T, zlim = c(23, max(MNIdat[, , i])))
        if (rnge[2] > 0 && rnge[2] > Threshold)
            image(thisbraindat[, , i], col = heatcols, axes = F, useRaster = T, zlim = c(Threshold, rnge[2]), add = T)
        if (rnge[1] < 0 && abs(rnge[1]) > Threshold)
            image(thisbraindat[, , i] * -1, col = coolcols, axes = F, useRaster = T, zlim = c(Threshold, abs(rnge[1])), add = T)
    }
}  # End drawBrains

# Draws both time figures, calls other draw functions
drawTimeFigures <- function(compNum) {
    tdatFile <- TimeDatFiles[compNum]
    fdatFile <- FreqDatFiles[compNum]
    tdat <- read.table(tdatFile)[[1]]
    fdat <- read.table(fdatFile)[[1]]
    TR <- getTR()
    nTRs <- length(tdat)
    drawTimeCourse(tdat, TR)
    if (svalue(ShowMotionCheckbox))
        drawMotion(tdat, motionDat, TR)
    drawFrequency(fdat, TR, nTRs)
}  # End drawTimeFigures

# Draws timecourse plot
drawTimeCourse <- function(tdat, TR) {
    visible(TimePlot) <- TRUE
    par(mar = c(3, 3, 1, 1), oma = c(0, 0, 0, 0), lwd = TimePlotLineWidth, bg = TimePlotBackgroundColor, fg = TimePlotLabelColor,
        col.axis = TimePlotLabelColor, col.lab = TimePlotLabelColor)
    seconds <- TR * 1:length(tdat)
    plot(seconds, tdat, t = "l", ylab = "", xlab = "", col = TimePlotLineColor)
    title(ylab = "Normalized Response", line = 2)
    title(xlab = paste("Time (seconds); TR =", TR, "s"), line = 2)
}  # End drawTimeCourse

# Draws motion data onto timecourse plot
drawMotion <- function(tdat, motionDat, TR) {
    visible(TimePlot) <- TRUE
    rnge <- max(tdat) - min(tdat)
    mdat <- motionDat/max(motionDat) * rnge/2
    mdat <- mdat + mean(range(tdat))
    seconds <- TR * 1:length(mdat)
    alphaNum <- round((MotionPlotLineAlpha/100) * 256)
    alphaStr <- sprintf("%0.2x", alphaNum)
    lineColor <- paste(MotionPlotLineColor, alphaStr, sep = "")
    lines(seconds, mdat, col = lineColor, lwd = TimePlotLineWidth)
}  # End drawMotion

# Draws powerspectrum plot
drawFrequency <- function(fdat, TR, nTRs) {
    visible(FreqPlot) <- TRUE
    maximum <- 1/(TR * nTRs)/2 * nTRs
    indices <- seq(0, maximum, length.out = length(fdat))
    par(mar = c(3, 3, 1, 1), oma = c(0, 0, 0, 0), lwd = FreqPlotLineWidth, bg = FreqPlotBackgroundColor, fg = FreqPlotLabelColor,
        col.axis = FreqPlotLabelColor, col.lab = FreqPlotLabelColor)
    plot(indices, fdat, t = "l", xaxp = c(0, max(indices), 7), ylab = "", xlab = "", col = FreqPlotLineColor)
    title(ylab = "Power", line = 2)
    title(xlab = "Frequency (in Hz)", line = 2)
}  # End drawFrequency

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
    viewr <<- createViewrObject()

    # test validity of inputs
    if (dir.exists(melodic_dir)) {
        viewr$data$ICADIR <- melodic_dir
    } else {
        stop(paste("The provided melodic_dir does not exist:", melodic_dir))
    }

    # TODO: write specific function to test standard_file
    # TODO: write specific function to test motion_file

    # TODO: find where this code should go
    #if (!is.null(ICADIR)) {
      #setwd(ICADIR)
      #loadICADIR(ICADIR)
    #} else {
      #initializePlot()
    #}

    viewr <<- createGUI(viewr)

    waitForExit <- function(...) {
      while (!viewr$status$exit) {
        Sys.sleep(1)
      }
    }

    addHandlerUnrealize(viewr$win , handler = function(h, ...) {
      viewr$status$exit <<- TRUE
    })

    if (!interactive()) {
      waitForExit()
    }

    return(viewr)
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
