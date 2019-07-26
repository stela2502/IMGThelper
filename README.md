# IMGThelper

IMGThelper is a naive R6 R package that interacts with a IMGT results folder.
The package is split into two logical parts, one population based that tries to enlighten the user about the features
of the given VDJ analysis (focusing on mouse heavy chain segments) and a more general usage single cell part.

The population based approach can (hopefully) be used to reproduce the figures in Stijn's paper.

The single cell approach should later on help to integrate IMGT data into a single cell analysis using my BioData::SingleCells R class.

# Install

The package is under heavy development and I strongly discurrage a usage of the package.

To install the package I recommend the devtools R package:

```{R}
  #install.packages(c('devtools'))
  library(devtools)
  install_github( 'stela2502/IMGThelper', ref='master')
```
