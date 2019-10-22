# QRFcapacity
Estimating carrying capacity using paired fish/habitat data and quantile random forest models.

## Getting Started
This repository contains the data and scripts to compile the fish and habitat data, and several vignettes to document specific analyses. In addtion, it functions like an R package, containing several useful functions and packaged data sets. Therefore, it can be forked or cloned, and then either built as a pacakge by the user, or installed as a package.

To install `QRFcapacity` you can use Hadley Wickham's `devtools` package. To install and load the `devtools` package use:

```
install.packages("devtools")
library(devtools)
```

NOTE: To use `devtools`, you may also have to download and install Rtools (although it may already be installed). The latest version on Rtools can be found at
https://cran.r-project.org/bin/windows/Rtools/

Once `devtools` is successfully installed, use the following to install QRFcapacity:

```
devtools::install_github("KevinSee/QRFcapacity")
```

## Contributions

Currently `QRFcapacity` is a private repository, with limited collaborators. The most straightforward method of collaboration may be for each collaborator to fork this repository. Then if they make any changes or add anything to it, they can instigate a pull request to have those changes merged into the master branch of this repository, where others can then pull from.
