# Diagnosing `pio` and `pi_tlp` Estimate Discrepancies

## Background

`pio` (osmotic potential at full turgor) and `pi_tlp` (osmotic potential at turgor loss) both
deviate from the 1:1 line more than other parameters. Understanding why requires looking at how
each is estimated in the package vs. the handmade approach.

### Package estimation pipeline

| Parameter | Method | Key inputs |
|-----------|--------|------------|
| `pio` | SMA x-intercept on last `n_row` (default 5) points | RWD ~ -1/ψ |
| `pi_tlp` | `pi_sat / srwc_tlp` from nonlinear pressure-potential model | `pio`, nonlinear fit of ψ_p vs symplastic RWC |

Both sit at the **start** of the estimation chain, so errors propagate downstream.

---

## Likely Causes

### 1. SMA vs. OLS for `pio`

The package uses **standard major axis (SMA)** regression to fit the RWD ~ -1/ψ relationship
below turgor loss (`osmotic_estimates.R`, line ~406). If the handmade estimates used **OLS**, they
will give systematically different intercepts. The difference grows when the x-variable (RWD) has
high variance, because:

- OLS minimises only y-residuals → biased slope when x has measurement error
- SMA minimises both axes → different intercept, especially at the tails

### 2. Different number of below-TLP points (`n_row`)

The package uses the last `n_row = 5` observations for the SMA fit. If the handmade approach used
a different cutoff (or hand-selected points), the intercept shifts. This is particularly impactful
for curves where the below-TLP region has few or noisy points.

### 3. Fundamentally different algorithm for `pi_tlp`

The package estimates:
```
pi_tlp = pi_sat / srwc_tlp
```
where `srwc_tlp` is a parameter extracted from a **nonlinear pressure-potential model**
(`calc_nonlin_psip`). The classic handmade approach finds TLP as the **intersection of the linear
(above-TLP) and osmotic (below-TLP) lines**, then back-calculates ψ_tlp from the osmotic SMA.
These are fundamentally different methods, so scatter around the 1:1 line is expected. Any error
in `pio` also propagates directly into `pi_tlp`.

---

## Diagnostic Workflow

### Step 1 — Identify the worst-performing leaves

```r
load(here("exec/pkgTest", "combined_pv_estimates.rda"))
com <- combined_manual_auto_estimates

com |>
  mutate(
    pio_err   = pio - pi_o,
    pitlp_err = pi_tlp - psi_tlp
  ) |>
  arrange(desc(abs(pio_err))) |>
  select(spcind, pio, pi_o, pio_err, pi_tlp, psi_tlp, pitlp_err)
```

Look for patterns: same species? same leaf? Does a large `pio_err` always accompany a large
`pitlp_err`? (If so, error in `pio` is the upstream driver.)

---

### Step 2 — Check for systematic vs. random bias

```r
# slope ≈ 1 and intercept ≈ 0 means random noise from different algorithms.
# Significant deviation indicates a method-level mismatch.
summary(lm(pi_o ~ pio, data = com))
summary(lm(psi_tlp ~ pi_tlp, data = com))
```

If the intercept is non-zero or slope ≠ 1, one method is systematically offset from the other.

---

### Step 3 — Plot residuals against covariates

Check whether the error is driven by a measurable leaf trait:

```r
library(ggplot2)
library(patchwork)

com <- com |>
  mutate(
    pio_err   = pio - pi_o,
    pitlp_err = pi_tlp - psi_tlp
  )

p1 <- ggplot(com, aes(x = af_est,      y = pio_err)) + geom_point() + geom_smooth() +
        geom_hline(yintercept = 0, lty = 2) + labs(x = "Apoplastic fraction", y = "pio error")

p2 <- ggplot(com, aes(x = swc_est,     y = pio_err)) + geom_point() + geom_smooth() +
        geom_hline(yintercept = 0, lty = 2) + labs(x = "Sat. water content",  y = "pio error")

p3 <- ggplot(com, aes(x = rwc_tlp_est, y = pio_err)) + geom_point() + geom_smooth() +
        geom_hline(yintercept = 0, lty = 2) + labs(x = "RWC at TLP",          y = "pio error")

p1 / p2 / p3
```

If apoplastic fraction predicts the error, the SMA intercept is sensitive to how many below-TLP
points were included (or to the apoplastic correction itself).

---

### Step 4 — Check `n_row` sensitivity on outlier leaves

For the worst-performing leaves, rerun `estOsmotic` across a range of `n_row` values and see how
much `pio` moves:

```r
library(pvest)
library(purrr)

# Replace with the actual species/leaf from Step 1
leaf_data <- test |> filter(species == "alma", leaf == 1)

rwc_obj <- estRWC(leaf_data, fresh.weight, water.potential, dry.weight)

pio_sensitivity <- map_dfr(3:8, \(n) {
  osm <- estOsmotic(rwc_obj, n_row = n)
  tibble(n_row = n, pio = osm$pio, pi_tlp = osm$pi_tlp)
})

pio_sensitivity
```

A stable estimate across `n_row` values means the linear region is clean. A large swing means one
or more tail points are influential outliers or are not truly below TLP.

---

### Step 5 — Visual inspection of Hofler diagrams for outliers

Open `hofler_diagrams.pdf` and look at the leaves flagged in Step 1. Key things to check:

- Do the below-TLP points fall on a **clean straight line** in the -1/ψ vs. RWD panel?
  If not, the SMA intercept is unreliable regardless of method.
- Is there a **clear bend** at the TLP in the ψ vs. RWC panel?
  If the bend is gradual or ambiguous, TLP location (and thus `pi_tlp`) will differ between methods.
- Are there **outlier points** just above or just below the estimated TLP that might be
  misclassified?

---

## Summary Table

| Parameter | Most likely cause | Diagnostic |
|-----------|-------------------|------------|
| `pio` | SMA vs. OLS; or different `n_row` cutoff | Steps 1–4 |
| `pi_tlp` | Different algorithm (nonlinear model vs. line intersection); `pio` error propagation | Steps 1–2, 5 |

### Recommended order

1. **Step 2** first — if slope/intercept show systematic bias, the methods are mismatched at a
   fundamental level and the other steps tell you which leaves are most affected.
2. **Step 1** to rank leaves by error magnitude.
3. **Step 4** on the top offenders to test `n_row` sensitivity.
4. **Step 5** to visually confirm the data quality for those leaves.