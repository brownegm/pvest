
# pvest <img src="man/figures/logo_lightgreen.png" align="right" height="138" />

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

estPV()  # estimates all pressure volume curve parameters in one step

```

## Model Logic

**pvest** automates PV curve parameter estimation using standard major axis (SMA) regression, removing operator subjectivity in locating the turgor loss point (TLP).

### Key Assumptions

1. Before turgor loss, $\Psi_L$ and water content exhibit a **negative and linear** relationship.
2. The osmotic potential at full turgor ($\pi_o$) equals the negative of the pressure potential at full turgor ($-\Psi_{p,o}$).
3. By default, the last 4 combined measurements are assumed to fall below turgor loss; all others are above.

### Estimation Workflow

1. **Saturated water content (SWC)** — y-intercept of the SMA line fit to points *above* TLP in $\Psi_L$ vs. fresh mass space.

2. **Osmotic potential at full turgor ($\pi_o$)** — y-intercept of the SMA line fit to points *below* TLP in $-1/\Psi_L$ vs. relative water deficit (RWD) space. Osmotic potential at each hydration state is then:
$$\Psi_s = \frac{\pi_o}{RWC_{sym}}$$

3. **Turgor loss point** — SMA of pressure potential ($\Psi_p = \Psi_L - \Psi_s$) vs. symplastic RWD for points *above* TLP; the x-intercept gives $RWD_{tlp}$, and $RWC_{tlp} = 100 - RWD_{tlp}$.

4. **Water potential at TLP ($\pi_{tlp}$)** — solved by substituting $RWD_{tlp}$ into the SMA parameters from step 2.

5. **Derived parameters** — modulus of elasticity, capacitance, and apoplastic fraction follow from the above estimates.

The TLP threshold can also be optimized automatically by minimizing RMSE or AICc, or maximizing $R^2$ across SMA fits on both sides (see `?optim_thres()`).
