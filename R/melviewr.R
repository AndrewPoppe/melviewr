

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
# Function for creating and populating the main GUI
createGUI <- function() {

    # main window
    win <<- gwindow("Melodic Results Viewer")
    size(win) <<- c(1600, 1000)
    Sys.sleep(0.1)
    size(win) <<- c(500, 500)

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
    widgets$MainPlot <<- ggraphics(width = 300, height = 300, container = widgets$MainPlotFrame, handler = updatePlots,
        expand = TRUE)
    widgets$CompTable <<- gtable(data$COMPTABLE, container = widgets$topGroup, expand = TRUE)
    widgets$topGroup[1, 6, expand = TRUE] <<- widgets$CompTable

    # Populate Bottom Group
    widgets$PlotGroup <<- ggroup(horizontal = FALSE, container = widgets$bottomGroup, expand = TRUE)
    widgets$TimeFrame <<- gframe("Timecourse", container = widgets$PlotGroup, expand = TRUE)
    widgets$TimePlot <<- ggraphics(width = 300, height = 100, container = widgets$TimeFrame, expand = TRUE)
    widgets$FreqFrame <<- gframe("Powerspectrum of Timecourse", container = widgets$PlotGroup, expand = TRUE)
    widgets$FreqPlot <<- ggraphics(width = 300, height = 100, container = widgets$FreqFrame, expand = TRUE)
    widgets$GraphicsFrame <<- gframe("Graphics Options", container = widgets$bottomGroup)
    widgets$ClassificationFrame <<- gframe("Classification", container = widgets$bottomGroup)
    Sys.sleep(0.5)

    # Populate Graphics Frame
    widgets$GraphicsTable <<- glayout(container = widgets$GraphicsFrame)
    widgets$GraphicsTable[1, 1] <<- glabel("# Columns:", container = widgets$GraphicsTable)
    widgets$ColNumInput <<- gcombobox(1:100, selected = settings$graphicsDefaults$numBrainCols, container = widgets$GraphicsTable,
        handler = updatePlots)
    widgets$GraphicsTable[1, 2] <<- widgets$ColNumInput
    widgets$GraphicsTable[2, 1] <<- glabel("Skip Slices:", container = widgets$GraphicsTable)
    widgets$SkipInput <<- gcombobox(1:100, selected = settings$graphicsDefaults$skipSlices, container = widgets$GraphicsTable,
        handler = updatePlots)
    widgets$GraphicsTable[2, 2] <<- widgets$SkipInput
    widgets$GraphicsTable[3, 1] <<- glabel("Threshold: +/-", container = widgets$GraphicsTable)
    widgets$ThresholdInput <<- gedit("2.3", container = widgets$GraphicsTable, handler = updatePlots)
    widgets$GraphicsTable[3, 2] <<- widgets$ThresholdInput
    widgets$GraphicsTable[4, 1] <<- glabel("Brain darkness:", container = widgets$GraphicsTable)
    widgets$BrainColSlider <<- gspinbutton(from = 0, to = 1, by = 0.2, value = settings$graphicsDefaults$brainColValue,
        container = widgets$GraphicsTable, handler = updatePlots)
    widgets$GraphicsTable[4, 2] <<- widgets$BrainColSlider
    widgets$GraphicsTable[5, 1] <<- glabel("Background darkness:", container = widgets$GraphicsTable)
    widgets$BackgroundSlider <<- gspinbutton(from = 0, to = 100, by = 20, value = settings$graphicsDefaults$brainBackgroundValue,
        container = widgets$GraphicsTable, handler = updatePlots)
    widgets$GraphicsTable[5, 2] <<- widgets$BackgroundSlider
    widgets$ShowMotionCheckbox <<- gcheckbox("Show Motion Plot", checked = !is.null(data$MOTIONFILE),
        container = widgets$GraphicsTable, handler = function(h, ...) {
                                      drawTimeFigures(svalue(widgets$CompTable))
                                    })
    if (is.null(data$MOTIONFILE)) {
      enabled(widgets$ShowMotionCheckbox) <<- FALSE
    }
    widgets$GraphicsTable[6, 1] <<- widgets$ShowMotionCheckbox

    widgets$TimeOptionsToggle <<- gexpandgroup("Timecourse Plot Options", horizontal = FALSE, container = widgets$GraphicsTable)
    widgets$GraphicsTable[7, 1:2] <<- widgets$TimeOptionsToggle
    widgets$timeCourseLineColorButton <<- gbutton("Set Line Color", container = widgets$TimeOptionsToggle, action = "TimePlotLineColor", handler = colorPickerHandler)
    widgets$timeCourseBgColorButton <<- gbutton("Set Background Color", container = widgets$TimeOptionsToggle, action = "TimePlotBackgroundColor",
        handler = colorPickerHandler)
    widgets$timeCourseLabelsColorButton <<- gbutton("Set Labels Color", container = widgets$TimeOptionsToggle, action = "TimePlotLabelColor", handler = colorPickerHandler)
    widgets$timeCourseLineWidthGroup <<- ggroup(horizontal = TRUE, container = widgets$TimeOptionsToggle)
    widgets$timeCourseLineWidthLabel <<- glabel("Set Line Width:", container = widgets$timeCourseLineWidthGroup)
    widgets$timeCourseLineWidthChooser <<- gspinbutton(from = 0.1, to = 3, by = 0.1, value = settings$graphics$TimePlotLineWidth, container = widgets$timeCourseLineWidthGroup,
        handler = function(h, ...) {
            settings$graphics$TimePlotLineWidth <<- svalue(widgets$timeCourseLineWidthChooser)
            drawTimeFigures(svalue(widgets$CompTable))
        })

    widgets$FreqOptionsToggle <<- gexpandgroup("Powerspectrum Plot Options", horizontal = FALSE, container = widgets$GraphicsTable)
    widgets$GraphicsTable[8, 1:2] <<- widgets$FreqOptionsToggle
    widgets$freqLineColorButton <<- gbutton("Set Line Color", container = widgets$FreqOptionsToggle, action = "FreqPlotLineColor", handler = colorPickerHandler)
    widgets$freqBgColorButton <<- gbutton("Set Background Color", container = widgets$FreqOptionsToggle, action = "FreqPlotBackgroundColor", handler = colorPickerHandler)
    widgets$freqLabelsColorButton <<- gbutton("Set Labels Color", container = widgets$FreqOptionsToggle, action = "FreqPlotLabelColor", handler = colorPickerHandler)
    widgets$freqLineWidthGroup <<- ggroup(horizontal = TRUE, container = widgets$FreqOptionsToggle)
    widgets$freqLineWidthLabel <<- glabel("Set Line Width:", container = widgets$freqLineWidthGroup)
    widgets$freqLineWidthChooser <<- gspinbutton(from = 0.1, to = 3, by = 0.1, value = settings$graphics$FreqPlotLineWidth, container = widgets$freqLineWidthGroup,
        handler = function(h, ...) {
            settings$graphics$FreqPlotLineWidth <<- svalue(widgets$freqLineWidthChooser)
            drawTimeFigures(svalue(widgets$CompTable))
        })

    widgets$MotionOptionsToggle <<- gexpandgroup("Motion Plot Options", horizontal = FALSE, container = widgets$GraphicsTable)
    widgets$GraphicsTable[9, 1:2] <<- widgets$MotionOptionsToggle
    widgets$motionLineColorButton <<- gbutton("Set Line Color", container = widgets$MotionOptionsToggle, action = "MotionPlotLineColor", handler = colorPickerHandler)
    widgets$motionLineAlphaGroup <<- ggroup(horizontal = T, container = widgets$MotionOptionsToggle)
    widgets$motionLineAlphaLabel <<- glabel("Line Opacity:", container = widgets$motionLineAlphaGroup)
    widgets$motionLineAlphaChooser <<- gspinbutton(from = 0, to = 99, by = 10, value = settings$graphics$MotionPlotLineAlpha, container = widgets$motionLineAlphaGroup,
        handler = function(h, ...) {
            settings$graphics$MotionPlotLineAlpha <<- svalue(widgets$motionLineAlphaChooser)
            drawTimeFigures(svalue(widgets$CompTable))
        })
    if (is.null(data$MOTIONFILE))
        enabled(widgets$MotionOptionsToggle) <<- FALSE

    widgets$GraphicsTable[10, 1:2] <<- gbutton("Save Graphics Settings", container = widgets$GraphicsTable, handler = saveGraphicsSettings)
    widgets$GraphicsTable[11, 1:2] <<- gbutton("Restore Default Settings", container = widgets$GraphicsTable, handler = restoreDefaultGraphicsSettings)

    # Populate Classification Frame
    classificationOptions <- c("Signal", "Unknown", "Unclassified Noise", "Movement", "Cardiac", "White matter", "Non-brain", "MRI",
                               "Susceptibility-motion", "Sagittal sinus", "Respiratory")
    widgets$ClassificationRadio <<- gradio(classificationOptions, horizontal = FALSE, container = widgets$ClassificationFrame, handler = updateClassLabel)


    # Populate button group
    widgets$ButtonFrame <<- gframe("", horizontal = TRUE, container = widgets$buttonGroup)
    widgets$LoadButton <<- gbutton("Load ICA directory", container = widgets$ButtonFrame, handler = getICADIR)
    widgets$LoadMotionButton <<- gbutton("Load Motion File", container = widgets$ButtonFrame, handler = loadMotionFile)
    widgets$SaveButton <<- gbutton("Save Classification File", container = widgets$ButtonFrame, handler = saveClassificationFile)
    widgets$ExitButton <<- gbutton("Exit", container = widgets$ButtonFrame, handler = function(h, ...){
      status$exit <<- TRUE
    })

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
# Function to initialize viewr object

Viewr <- setRefClass("Viewr", fields = list(
  win = "gWindow",
  widgets = "list",
  settings = "list",
  status = "list",
  data = "list"
), methods = list(
  createGUI = createGUI
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

    viewr$createGUI()

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
