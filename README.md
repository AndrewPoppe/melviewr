# melviewr

[![Rdoc](http://www.rdocumentation.org/badges/version/melviewr)](http://www.rdocumentation.org/packages/melviewr)

==========================

Overview
--------

This [R](https://cran.r-project.org/) package provides a graphical user interface (GUI) whose purpose is to view and classify the results of a [MELODIC](https://fsl.fmrib.ox.ac.uk/fsl/fslwiki/MELODIC) analysis in order to train the [ICA+FIX](https://fsl.fmrib.ox.ac.uk/fsl/fslwiki/FIX) classifier. 

This package is an attempt to reimplement the [Melview](http://fsl.fmrib.ox.ac.uk/fsl/fslwiki/Melview) project in R, as installation of that viewer is sometimes difficult.

Installation
------------

Stable releases of the `melviewr` package can be installed from the [CRAN](https://cran.r-project.org/web/packages/) repository. First, make sure that [GTK](https://www.gtk.org/) is installed on your system. Next, open `R` either in a terminal or through [RStudio](https://www.rstudio.com/) and type the following:
```r
install.packages('melviewr', dependencies = TRUE)
```
To view examples of how to use the GUI, do:
```r
library(melviewr)
?melviewr
```

You can install the most up-to-date version of the package by doing the following in R:
```r
install.packages('devtools', dependencies = TRUE)
devtools::install_github('AndrewPoppe/melviewr')
```

You can also specify a particular release version when installing from github by appending a `@` and the release tag name, i.e.:
```r
devtools::install_github('AndrewPoppe/melviewr@v0.0.1')
```
Release tag names can be found by clicking on the ![Releases](http://i.imgur.com/u8YA5Iq.png) link on this page and then looking on the left hand side to find the tag name of the release you're interested in: ![TAG](http://i.imgur.com/Mh7pZI9.png).


Other Usage
-----------

In addition to running the command from within R interactively, you can also write a shell script to open the GUI directly from the terminal (if you are using Linux or Mac). Something like the following should work, assuming `melviewr` has already been installed. Just save the following code to a file called "melviewr" and use `chmod` to make it executable. Then, put it somewhere in your `PATH`.

```r
#! /usr/bin/env Rscript

args <- commandArgs(TRUE)

if (length(args) == 0) {
	writeLines("
melviewr: A MELODIC Viewer

Usage: melviewr <melodic output directory> -mot <motion file> -std <standard>

    -mot    Optional. The path to a single column motion file, such as a 
            Relative RMS or a column of FD values.
    -std    Optional. The path to a standard Nifti file on which to display the 
            MELODIC results. The voxel dimensions of this file must match those 
            of the melodic_IC.nii.gz file within the melodic output directory.

For more information, visit https://github.com/AndrewPoppe/melviewr
")
	q()
}

library(melviewr)

motion_file <- NULL
standard_file <- NULL

if (length(args) > 2) {
	for (i in 2:(length(args)-1)) {
		if (args[i] == "-std") standard_file <- args[i + 1]
		if (args[i] == "-mot") motion_file <- args[i + 1]
	}
}
		
melviewr(args[1], standard_file, motion_file)


```


Getting Help
------------

Please email Andrew Poppe at `Poppe076 at gmail.com`
