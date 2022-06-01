# Master thesis from Martin Böckling
## Introduction

## Dependencies
The coding related to the thesis is implemented in Python and R. For the different programming languages the dependencies are outlined in the following sections.

### R
The R coding is based on the R version 4.1.2.
The dependencies of the project can be found in the [renv.lock](wildfirearea/renv.lock) file. The dependencies can directly be installed in R by using the [renv](https://cran.r-project.org/web/packages/renv/index.html) package. To install all necessary packages simply use the command renv::restore(lockfile='*lockfile path*') within your R environment.

For using the geospatial packages like sf within R GDAL, GEOS and Proj.4 needs to be installed. Instructions for downloading can be found under the following [link](https://r-spatial.github.io/sf/#installing)

### Python
The python coding is based on python version 3.9.
The dependencies can be found in the [requirements.txt](wildfirearea/requirements.txt) file. To install the packages used in this project please run pip install -r requirements.txt in your virtual environement.
## Setup for github repo

## Data
The data can be found under the following OneDrive [Folder](https://1drv.ms/u/s!AijsqF7qjxxBhcdw369GMZSGwCQB0Q?e=bnFiac). The structure is the same as in the Github Data Folder
