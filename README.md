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
# Install from local clone
devtools::install("path/to/fastecon")

# Or from GitHub 
devtools::install_github("MengqiLi0000/fastecon")
