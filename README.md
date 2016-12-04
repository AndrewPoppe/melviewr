# melviewr
==========================

Overview
--------

This [R]() package provides a graphical user interface (GUI) whose purpose is to view and classify the results of a [MELODIC](https://fsl.fmrib.ox.ac.uk/fsl/fslwiki/MELODIC) analysis in order to train the [ICA+FIX](https://fsl.fmrib.ox.ac.uk/fsl/fslwiki/FIX) classifier. 

This package is an attempt to reimplement the [Melview](http://fsl.fmrib.ox.ac.uk/fsl/fslwiki/Melview) project in R, as installation of that viewer is sometimes difficult.

Installation
------------

As of now, this package has not been accepted into [CRAN](), so the best way to install it is first make sure that [GTK](https://www.gtk.org/) is installed on your system. Next, open `R` either in a terminal or through [RStudio]() and type the following:
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

Getting Help
------------

Please email Andrew Poppe at Poppe076 at gmail.com
