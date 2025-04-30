# fastecon

`fastecon` is a high-performance R package for fast, scalable econometric analysis.  
It is designed to simplify and accelerate common tasks in empirical research, including rolling window regressions, clustered standard errors, and table formatting for publication.

## Features

- Fast rolling OLS with matrix caching
- Optional parallel computation
- Clean, minimal API
- Output formatted for LaTeX tables 
- Modular design for integration into larger workflows

## Installation

```r
# Install from GitHub 
devtools::install_github("MengqiLi0000/fastecon")
```
## Functions
1. rollreg_fast()
Purpose:
Run fast rolling window OLS for time-series or panel data. Uses cached matrix updates.
Usage:
```r
result <- rollreg_fast(data, y = "y", x = c("x1", "x2"), time = "date", window = 60, step = 1)
```
2. etable_latex()
Purpose:
Create clean LaTeX regression tables from model output
Usage:
```r
etable_latex(list(lm(y ~ x1, data = df), lm(y ~ x1 + x2, data = df)))
```
3. clustboot_fast()
Purpose:
Run clustered bootstrap for linear regression. Supports one-way clustering and parallelism.
Usage:
```r
clustboot_fast(data, formula = y ~ x1 + x2, cluster = "firm", B = 200, seed = 123)
```
4. feols_fast()
Purpose:
Estimate fixed effects regression (1-way or 2-way) using within-transformation.
Usage:
```r
feols_fast(data, y = "y", x = c("x1", "x2"), fe = c("firm", "year"))
```
5. forecast_fast()
Purpose:
Make 1-step-ahead predictions using a model or coefficient vector.
Usage:
```r
forecast_fast(model, newdata = df[1, ], xvars = c("x1", "x2"), ci = TRUE)
```
