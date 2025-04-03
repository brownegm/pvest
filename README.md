# Hands-free pressure-volume curve parameter estimation (AutoPV)

This project aims to automate the determination of pressure volume curve parameters using intrinsic relationships among parameters to draw lines and predict saturated water content, osmotic potential at full turgor and leaf water potential at turgor loss. The automated prediction of these parameters specifically reduce the inter-investigator disparity that may occur and allows for the rapid determination of all pressure volume curve parameters for large data sets.

Pressure volume curves are created by repeated measurements of leaf fresh mass and leaf water 
potential ($\Psi_L$; i.e., sum of leaf solute ($\Psi_s$) and pressure ($\Psi_p$) potentials) 
across dehydration states (ideally, at 0.2-0.3 MPa intervals).

## Installation
To download the package use `install_github()` from devtools:

```{r}
# download package
library(pak)
pak("brownegm/pvest")  

```

## Usage

The package contains functions for estimating individual parameters as well as all parameters. There are vignettes for usage (see `browseVignettes("pvest")`). 

```{r}

library(pvest) # load package 

estParams()# this function estimates all pressure volume curve parameters for you in one step. 

```

## TBD 
- Check coverage
- FIGURE OUT HOW/WHERE TO IMPLEMENT THE N_PTS ASPECT
