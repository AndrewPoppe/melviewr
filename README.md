# melviewr
==========================

Overview
--------

This [R](https://cran.r-project.org/) package provides a graphical user interface (GUI) whose purpose is to view and classify the results of a [MELODIC](https://fsl.fmrib.ox.ac.uk/fsl/fslwiki/MELODIC) analysis in order to train the [ICA+FIX](https://fsl.fmrib.ox.ac.uk/fsl/fslwiki/FIX) classifier. 

This package is an attempt to reimplement the [Melview](http://fsl.fmrib.ox.ac.uk/fsl/fslwiki/Melview) project in R, as installation of that viewer is sometimes difficult.

Installation
------------

As of now, this package has not been accepted into [CRAN](https://cran.r-project.org/web/packages/), so the best way to install it is first make sure that [GTK](https://www.gtk.org/) is installed on your system. Next, open `R` either in a terminal or through [RStudio](https://www.rstudio.com/) and type the following:
```r
install.packages('devtools', dependencies = TRUE)
install_github('AndrewPoppe/melviewr')
```
To view examples of how to use the GUI, do:
```r
library(melviewr)
?melviewr
```

Once the package is accepted to CRAN, it can be installed with the following:
```r
install.packages('melviewr', dependencies = TRUE)
```

Other Usage
-----------

In addition to running the command from within R interactively, you can also write a shell script to open the GUI directly from the terminal. Something like the following should work, assuming `melviewr` has already been installed. Just save the following code to a file called "melviewr" and use `chmod` to make it executable. Then, put it somewhere in your `PATH`.

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
