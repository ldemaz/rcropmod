# rcropmod

R package for crop model simulations, primarily providing wrapper functions for running and analyzing the outputs of [DSSAT CSM](http://dssat.net), including the production of gridded results. The long-term goal is to extend this package so that it also interfaces with other models (hence the package name). 

The package includes a number of utilities, including pedotransfer functions for soil hydraulic properties (Saxton and Rawls, 2006). For gridded simulations, the model relies on [dtraster](https://github.com/ldemaz/dtraster), which provides conversions between `raster` and `data.table` structures. 

This library is primarily intended for use on Mac and Linux systems. The most recent version has been tested on Mac only, but earlier versions of these functions have been used on Linux clusters, and the code has been used at least once in a Windows environment. 

## Installation
The model requires a fully working install of DSSAT CSM to function. The source code for the model, as well as instructions for compiling its source code, can be requested from the [DSSAT Foundation](http://dssat.net).

```r
library(devtools)
install_github("ldemaz/rcropmod", build_vignettes = TRUE)
```
