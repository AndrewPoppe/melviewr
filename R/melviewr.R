
# These need to be moved to Description/Namespace file instead of here
pkgs <- c('gWidgetsRGtk2', 'gWidgets', 'gtools', 'RColorBrewer', 'RNifti', 'grDevices')


# This needs to happen inside main melviewr function, but also needs to reset options on close.
options("guiToolkit"="RGtk2")


source('/opt/HCP/HCPscripts/Functions/melviewR_files/melviewR_color_picker.R')

args <- commandArgs(T)
nargs <- length(args)
ICADIR <- args[1]

motionFileLoaded <- FALSE
for(i in seq(along=args)) {
	if(args[i] == '-mot') {
		motionFile <- args[i+1]
		motionFileLoaded <- TRUE
		if(!file.exists(motionFile)) stop(paste('The motion file specified does not exist:', motionFile))
		motionDat <- read.table(motionFile)[[1]]
	}
}


createGUI <- function(ICADIR=NULL) {

	# Functions for saving/loading graphics settings
	saveGraphicsSettings <- function(h,...) {
		tryCatch({
			configFile <- paste(normalizePath('~'), '/.melviewR.config', sep='')
			sink(configFile)
				for(i in 1:length(graphicsDefaults)) {
					val <- eval(parse(text=names(graphicsDefaults)[i]))
					sepChar <- ifelse(is.character(val), '"', '')
					cat(paste(names(graphicsDefaults)[i], ' <<- ', sepChar, val, sepChar, '\n', sep=''))
				}
			sink()
			output <- list(messageTxt = paste('Config file has been saved to:', configFile),
			   			   icon = "info")
			gmessage(output$messageTxt, title='Save Graphics Settings', icon=output$icon)
		}, warning = function(war) {
			output <- list(messageTxt = paste('A warning has been raised in the attempt to save settings to:',configFile,'\n',war),
						   icon = "warning")
			gmessage(output$messageTxt, title='Save Graphics Settings', icon=output$icon)
		}, error = function(err) {
			output <- list(messageTxt = paste('An error has been raied in the attempt to save settings to:',configFile,'\n',err),
						   icon = "error")
			gmessage(output$messageTxt, title='Save Graphics Settings', icon=output$icon)
		}, finally = {
			DONE <- TRUE
		})
	}

	loadGraphicsSettings <- function() {
		configFile <- '~/.melviewR.config'
		configLoaded <- FALSE
		if(file.exists(configFile)) {
			source(configFile)
			configLoaded <- TRUE
			# if any graphics options are not set in the config file, set them to their default state.
			for(i in 1:length(graphicsDefaults)) {
				if(!exists(names(graphicsDefaults[i]))) {
					assign(names(graphicsDefaults)[i], graphicsDefaults[[i]], inherits=TRUE)
				}
			}

		}
		return(configLoaded)
	}

	restoreDefaultGraphicsSettings <- function(...) {
		for(i in 1:length(graphicsDefaults)) {
			assign(names(graphicsDefaults)[i], graphicsDefaults[[i]], inherits=TRUE)
		}
		if(exists("freqLineWidthChooser")) {
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
			updatePlot(NULL)
		}
	}


	# Initalize Variables
	classificationOptions <- c('Signal', 'Unknown', 'Unclassified Noise', 'Movement', 'Cardiac',
							'White matter', 'Non-brain', 'MRI', 'Susceptibility-motion',
							'Sagittal sinus', 'Respiratory')
	compList <- data.frame(array(dim=c(0,3)), stringsAsFactors=FALSE)
	names(compList) <- c('IC', 'ClassName', 'To_Remove')
	nComps <- 0
	CompImageFiles <- ""
	TimeDatFiles <- ""
	FreqDatFiles <- ""
	MNIdat <- readNifti('/opt/fsl/fsl/data/standard/MNI152_T1_2mm_brain.nii.gz')
	startSlice <- 1
	endSlice <- dim(MNIdat)[3]
	braindat <- NULL
	suppressRedraw <- FALSE

	graphicsDefaults <- list(skipSlices = 3,
							 numBrainCols = 9,
							 Threshold = 2.3,
							 brainColValue = 0.5,
							 brainBackgroundValue = 0,
							 TimePlotLineColor = 'black',
							 TimePlotLineWidth = 0.5,
							 TimePlotBackgroundColor = 'white',
							 TimePlotLabelColor = 'black',
							 FreqPlotLineColor = 'black',
							 FreqPlotLineWidth = 0.5,
							 FreqPlotBackgroundColor = 'white',
							 FreqPlotLabelColor = 'black',
							 MotionPlotLineColor = '#FF0000',
							 MotionPlotLineAlpha = 50)

	# Attempt to load saved graphics settings.
	# If unable, load defaults.
	if(!loadGraphicsSettings()) {
		restoreDefaultGraphicsSettings()
	}


	# find first and last slices that aren't all 0s
	for(i in startSlice:endSlice) {
		if(! all(MNIdat[,,i] == 0)) {
			startSlice <- i
			break()
		}
	}
	for(i in endSlice:startSlice) {
		if(! all(MNIdat[,,i] == 0)) {
			endSlice <- i
			break()
		}
	}

	# heat colors
	heatcols <- heat.colors(2000)
	# cool colors
	coolcols <- topo.colors(10000)[900:3300]

	initializePlot <- function() {
		compList <<- data.frame(array(dim=c(nComps, 3)), stringsAsFactors=FALSE)
		names(compList) <<- c('IC', 'ClassName', 'To_Remove')
		if(nrow(compList)>0) {
			compList$IC <- 1:nComps
			compList$ClassName <- ""
			compList$To_Remove <- ""
		}
		CompTable[] <<- compList
		if(nrow(compList) > 0) {
			drawTimeFigures(1)
			svalue(CompTable) <- 1
			drawBrains(1)
			svalue(MainPlotLabel) <- 1
			prevClass <- loadClassificationFile()
			if(!is.null(prevClass)) {
				if(nrow(prevClass) == nrow(CompTable[])) CompTable[] <<- prevClass
			}
		}
		addHandlerClicked(CompTable, handler=updatePlot)
	}

	updatePlot <- function(h, ...) {
		if(suppressRedraw) return()
		compNum <- svalue(CompTable)
		svalue(MainPlotLabel) <- compNum
		if(length(CompTable[compNum]$ClassName) > 0) {
			if(CompTable[compNum]$ClassName == "") {
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
	}

	updateClassLabel <- function(h, ...) {
		compNum <- as.numeric(svalue(MainPlotLabel))
		thisClassName <- svalue(ClassificationRadio)
		CompTable[compNum]$ClassName <- thisClassName
		CompTable[compNum]$To_Remove <- ifelse(!thisClassName %in% c('Signal', 'Unknown'), 'X', '')
	}

	drawBrains <- function(compNum) {
		visible(MainPlot) <- TRUE
		bgCol <- paste('gray',brainBackgroundValue, sep='')
		braincols <- gray.colors(n=20000, start=0, end=brainColValue, gamma=.6)
		thisbraindat <- braindat[,,,compNum]
		sliceIndices <- seq(startSlice, endSlice, skipSlices)
		nCols <- numBrainCols
		nRows <- ceiling(length(sliceIndices) / nCols)
		par(mar=c(0,0,0,0), oma=c(0,0,0,0), mfrow=c(nRows,nCols), bg=bgCol)
		rnge <- range(thisbraindat)
		for(i in sliceIndices) {
			image(MNIdat[,,i], col=braincols, axes=F, useRaster=T, zlim=c(23,max(MNIdat[,,i])))
			if(rnge[2] > 0 && rnge[2] > Threshold) image(thisbraindat[,,i], col=heatcols, axes=F, useRaster=T, zlim=c(Threshold, rnge[2]), add=T)
  			if(rnge[1] < 0 && abs(rnge[1]) > Threshold) image(thisbraindat[,,i]*-1, col=coolcols, axes=F, useRaster=T, zlim=c(Threshold, abs(rnge[1])), add=T)
		}
	}

	# These functions have to do with drawing the two frequency/power spectrum plots
	drawTimeFigures <- function(compNum) {
		tdatFile <- TimeDatFiles[compNum]
		fdatFile <- FreqDatFiles[compNum]
		tdat <- read.table(tdatFile)[[1]]
		fdat <- read.table(fdatFile)[[1]]
		TR <- getTR()
		nTRs <- length(tdat)
		drawTimeCourse(tdat, TR)
		if(svalue(ShowMotionCheckbox)) drawMotion(tdat, motionDat, TR)
		drawFrequency(fdat, TR, nTRs)
	}

	drawTimeCourse <- function(tdat,TR) {
		visible(TimePlot) <- TRUE
		par(mar=c(3,3,1,1), oma=c(0,0,0,0), lwd=TimePlotLineWidth, bg=TimePlotBackgroundColor, fg=TimePlotLabelColor, col.axis=TimePlotLabelColor, col.lab=TimePlotLabelColor)
		seconds <- TR * 1:length(tdat)
		plot(seconds, tdat, t='l', ylab='', xlab='', col=TimePlotLineColor)
		title(ylab='Normalized Response', line=2)
		title(xlab=paste('Time (seconds); TR =', TR, 's'), line=2)
	}

	drawMotion <- function(tdat, motionDat, TR) {
		visible(TimePlot) <- TRUE
		rnge <- max(tdat) - min(tdat)
		mdat <- motionDat/max(motionDat) * rnge/2
		mdat <- mdat + mean(range(tdat))
		seconds <- TR * 1:length(mdat)
		alphaNum <- round((MotionPlotLineAlpha/100) * 256)
		alphaStr <- sprintf('%0.2x', alphaNum)
		lineColor <- paste(MotionPlotLineColor,alphaStr,sep='')
		lines(seconds, mdat, col=lineColor, lwd=TimePlotLineWidth)
	}

	drawFrequency <- function(fdat, TR, nTRs) {
		visible(FreqPlot) <- TRUE
		maximum <- 1/(TR*nTRs)/2 * nTRs
		indices <- seq(0,maximum, length.out=length(fdat))
		par(mar=c(3,3,1,1), oma=c(0,0,0,0), lwd=FreqPlotLineWidth, bg=FreqPlotBackgroundColor, fg=FreqPlotLabelColor, col.axis=FreqPlotLabelColor, col.lab=FreqPlotLabelColor)
		plot(indices, fdat, t='l', xaxp=c(0,max(indices), 7), ylab='', xlab='', col=FreqPlotLineColor)
		title(ylab='Power', line=2)
		title(xlab='Frequency (in Hz)', line=2)
	}

	colorPickerHandler <- function(h, ...) {
		# note, this handler will only work with widgets that have an "action" defined
		newColor <- colorPicker()
		if(is.na(newColor)) return()
		assign(h$action, newColor, inherits = TRUE)
		drawTimeFigures(svalue(CompTable))
	}

	getTR <- function() {
		logtxt <- scan(paste(ICADIR,'/log.txt', sep=''), 'character', quiet=TRUE)
		TRstring <- grep('--tr=', logtxt, value=TRUE)
		TR <- as.numeric(gsub('--tr=', '', TRstring))
		return(TR)
	}





	######################
	### SET UP THE GUI ###
	######################

	# main window
	window <- gwindow("Melodic Results Viewer")
	size(window) <- c(1600, 1000); Sys.sleep(.1); size(window) <- c(500, 500)

	# big group
	bigGroup <- glayout(horizontal=FALSE, container=window, expand=TRUE)

	# group for the top portion (main axial viewer and component table)
	topGroup <- glayout(horizontal=TRUE, container=bigGroup, expand=TRUE)
	bigGroup[1,1, expand=TRUE] <- topGroup

	# group for the bottom portion
	bottomGroup <- ggroup(horizontal=TRUE, container=bigGroup,expand=TRUE)
	bigGroup[2,1, expand=TRUE] <- bottomGroup

	# group for control buttons
	buttonGroup <- ggroup(horizontal=TRUE, container=bigGroup)
	bigGroup[3,1] <- buttonGroup

	# Populate Top Group
	MainPlotGroup <- ggroup(horizontal=FALSE, container=topGroup, expand=TRUE)
	topGroup[1,1:5, expand=TRUE] <- MainPlotGroup
	MainPlotLabel <- glabel('', container=MainPlotGroup)
	MainPlotFrame <- gframe('', container=MainPlotGroup, expand=TRUE)
	MainPlot <- ggraphics(width=300, height=300, container=MainPlotFrame, handler=updatePlot, expand=TRUE)
	CompTable <- gtable(compList, container=topGroup, expand=TRUE)
	topGroup[1,6, expand=TRUE] <- CompTable


	# Populate Bottom Group
	PlotGroup <- ggroup(horizontal=FALSE, container=bottomGroup, expand=TRUE)
	TimeFrame <- gframe('Timecourse',container=PlotGroup, expand=TRUE)
	TimePlot  <- ggraphics(width=300, height=100, container=TimeFrame, expand=TRUE)
	FreqFrame <- gframe('Powerspectrum of Timecourse', container=PlotGroup, expand=TRUE)
	FreqPlot  <- ggraphics(width=300, height=100, container=FreqFrame, expand=TRUE)
	GraphicsFrame <- gframe('Graphics Options', container=bottomGroup)
	ClassificationFrame <- gframe('Classification', container=bottomGroup)
	Sys.sleep(.5)

	# Populate Graphics Frame
	GraphicsTable <- glayout(container=GraphicsFrame)
	GraphicsTable[1,1] <- glabel("# Columns:", container=GraphicsTable)
	ColNumInput <- gcombobox(1:100, selected=numBrainCols, container=GraphicsTable, handler=updatePlot)
	GraphicsTable[1,2] <-ColNumInput
	GraphicsTable[2,1] <- glabel("Skip Slices:", container=GraphicsTable)
	SkipInput <- gcombobox(1:100, selected=skipSlices, container=GraphicsTable, handler=updatePlot)
	GraphicsTable[2,2] <- SkipInput
	GraphicsTable[3,1] <- glabel("Threshold: +/-", container=GraphicsTable)
	ThresholdInput <- gedit('2.3', container=GraphicsTable, handler=updatePlot)
	GraphicsTable[3,2] <- ThresholdInput
	GraphicsTable[4,1] <- glabel("Brain darkness:", container=GraphicsTable)
	BrainColSlider <- gspinbutton(from=0, to=1, by=.20, value=brainColValue, container=GraphicsTable, handler=updatePlot)
	GraphicsTable[4,2] <- BrainColSlider
	GraphicsTable[5,1] <- glabel("Background darkness:", container=GraphicsTable)
	BackgroundSlider <- gspinbutton(from=0, to=100, by=20, value=brainBackgroundValue, container=GraphicsTable, handler=updatePlot)
	GraphicsTable[5,2] <- BackgroundSlider
	ShowMotionCheckbox <- gcheckbox("Show Motion Plot", checked = motionFileLoaded, container=GraphicsTable, handler=function(h,...){
		drawTimeFigures(svalue(CompTable))
	})
	if(!motionFileLoaded) enabled(ShowMotionCheckbox) <- FALSE
	GraphicsTable[6,1] <- ShowMotionCheckbox

	TimeOptionsToggle <- gexpandgroup('Timecourse Plot Options', horizontal=FALSE, container=GraphicsTable)
	GraphicsTable[7,1:2] <- TimeOptionsToggle
	timeCourseLineColorButton <- gbutton('Set Line Color', container=TimeOptionsToggle, action="TimePlotLineColor", handler=colorPickerHandler)
	timeCourseBgColorButton <- gbutton('Set Background Color', container=TimeOptionsToggle, action="TimePlotBackgroundColor", handler=colorPickerHandler)
	timeCourseLabelsColorButton <- gbutton('Set Labels Color', container=TimeOptionsToggle, action="TimePlotLabelColor", handler=colorPickerHandler)
	timeCourseLineWidthGroup <- ggroup(horizontal=TRUE, container=TimeOptionsToggle)
	timeCourseLineWidthLabel <- glabel('Set Line Width:', container=timeCourseLineWidthGroup)
	timeCourseLineWidthChooser <- gspinbutton(from=0.1, to=3, by=0.1, value=TimePlotLineWidth, container= timeCourseLineWidthGroup, handler=function(h,...){
		TimePlotLineWidth <<- svalue(timeCourseLineWidthChooser)
		drawTimeFigures(svalue(CompTable))
	})

	FreqOptionsToggle <- gexpandgroup('Powerspectrum Plot Options', horizontal=FALSE, container=GraphicsTable)
	GraphicsTable[8,1:2] <- FreqOptionsToggle
	freqLineColorButton <- gbutton('Set Line Color', container=FreqOptionsToggle, action="FreqPlotLineColor", handler=colorPickerHandler)
	freqBgColorButton <- gbutton('Set Background Color', container=FreqOptionsToggle, action="FreqPlotBackgroundColor", handler=colorPickerHandler)
	freqLabelsColorButton <- gbutton('Set Labels Color', container=FreqOptionsToggle, action="FreqPlotLabelColor", handler=colorPickerHandler)
	freqLineWidthGroup <- ggroup(horizontal=TRUE, container=FreqOptionsToggle)
	freqLineWidthLabel <- glabel('Set Line Width:', container=freqLineWidthGroup)
	freqLineWidthChooser <- gspinbutton(from=0.1, to=3, by=0.1, value=FreqPlotLineWidth, container=freqLineWidthGroup, handler=function(h,...){
		FreqPlotLineWidth <<- svalue(freqLineWidthChooser)
		drawTimeFigures(svalue(CompTable))
	})

	MotionOptionsToggle <- gexpandgroup('Motion Plot Options', horizontal=FALSE, container=GraphicsTable)
	GraphicsTable[9,1:2] <- MotionOptionsToggle
	motionLineColorButton <- gbutton('Set Line Color', container=MotionOptionsToggle, action="MotionPlotLineColor", handler=colorPickerHandler)
	motionLineAlphaGroup <- ggroup(horizontal=T, container=MotionOptionsToggle)
	motionLineAlphaLabel <- glabel('Line Opacity:', container=motionLineAlphaGroup)
	motionLineAlphaChooser <- gspinbutton(from=0, to=99, by=10, value=MotionPlotLineAlpha, container=motionLineAlphaGroup, handler=function(h,...){
		MotionPlotLineAlpha <<- svalue(motionLineAlphaChooser)
		drawTimeFigures(svalue(CompTable))
	})
	if(!motionFileLoaded) enabled(MotionOptionsToggle) <- FALSE

	GraphicsTable[10,1:2] <- gbutton('Save Graphics Settings', container=GraphicsTable, handler=saveGraphicsSettings)
	GraphicsTable[11,1:2] <- gbutton('Restore Default Settings', container=GraphicsTable, handler=restoreDefaultGraphicsSettings)



	# Populate Classification Frame
	ClassificationRadio <- gradio(classificationOptions, horizontal=FALSE, container=ClassificationFrame, handler=updateClassLabel)


	# select an ICA directory
	getICADIR <- function(...) {
		ICADIR <<- gfile(type='selectdir', initialfilename='.')
		loadICADIR(ICADIR)
	}

	exitGUI <- function(...) {
		shouldIExit <<- TRUE
	}


	# Given an ica directory, populate values
	loadICADIR <- function(ICADIR) {
		# get number of components
		braindat <<- readNifti(paste(ICADIR, '/melodic_IC.nii.gz', sep=''))
		nComps <<- dim(braindat)[4]

		# get time and frequency images
		TimeDatFiles <<- mixedsort(list.files(paste(ICADIR, '/report',sep=''), pattern='^t.*txt', full.names=TRUE))
		FreqDatFiles <<- mixedsort(list.files(paste(ICADIR, '/report',sep=''), pattern='^f.*txt', full.names=TRUE))

		initializePlot()
	}

	# Function to save file
	saveTextFile <- function(...) {
		dat <- CompTable[]
		dat2 <- subset(dat, !ClassName %in% c("Signal", "Unknown", ""))
		formatted <- paste(dat2$IC, collapse=', ')
		formatted <- paste('[',formatted,']', sep='')
		outfile <- ''
		outfile <- gfile(type='save', initialfilename=paste(getwd(),'hand_labels_noise.txt', sep='/'))
		if(outfile != '') {
			sink(outfile)
			writeLines(formatted)
			sink()
			write.csv(dat, paste(ICADIR, '/.classification.csv',sep=''), row.names=FALSE, quote=FALSE)
		}
	}

	# Function to load classification file
	loadClassificationFile <- function() {
		fname <- paste(ICADIR, '/.classification.csv', sep='')
		output <- NULL
		if(file.exists(fname)) {
			output <- read.csv(fname, stringsAsFactors=FALSE)
		}
		return(output)
	}

	# Function to choose and load motion file
	loadMotionFile <- function(h,...) {
		initialfilename <- ifelse(file.exists('../../Movement_RelativeRMS.txt'), '../../Movement_RelativeRMS.txt', '.')
		motionfile <- gfile('Select Motion File', type='open', initialfilename=initialfilename)
		if(is.na(motionfile)) return()
		motionFile <<- motionfile
		motionFileLoaded <<- TRUE
		motionDat <<- read.table(motionFile)[[1]]
		enabled(ShowMotionCheckbox) <- TRUE
		enabled(MotionOptionsToggle) <- TRUE
		svalue(ShowMotionCheckbox) <- TRUE
		drawTimeFigures(svalue(CompTable))
	}



	# Populate button group
	ButtonFrame <- gframe('', horizontal=TRUE, container=buttonGroup)
	LoadButton <- gbutton('Load ICA directory', container=ButtonFrame, handler=getICADIR)
	LoadMotionButton <- gbutton('Load Motion File', container=ButtonFrame, handler=loadMotionFile)
	SaveButton <- gbutton('Save Classification File', container=ButtonFrame, handler=saveTextFile)
	ExitButton <- gbutton('Exit', container=ButtonFrame, handler=exitGUI)

	if(!is.null(ICADIR)) {
		setwd(ICADIR)
		loadICADIR(ICADIR)
	} else {
		initializePlot()
	}

	waitForExit <- function(...) {
		while(!shouldIExit) {
			Sys.sleep(1)
		}
	}

	addHandlerUnrealize(window, handler = function(h,...) { shouldIExit <<- TRUE })

	shouldIExit <- FALSE
	waitForExit()

}


if(is.na(ICADIR)) {
	createGUI()
} else {
	createGUI(ICADIR)
}



