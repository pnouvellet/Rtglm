# Rtglm

Similar to EpiEstim [1], Rtglm [2] is a tool to estimate the time-varying instantaneous reproduction number during epidemics.
The key difference between EpiEstim and Rtglm are:
- Rtglm relies on a generalisation of the renewal equation, which allows inference within the Generalized Linear Model framework.
- Rtglm can largely reproduce EpiEstim results, i.e. in the simple application of EpiEstim (e.g. estimating Rt when daily incidence is known and information on the Generation time is know), with or without overalpping time-windows.
- Rtglm can estimate smooth version of temporal and spatial trends in Rt, relying on GAM (i.e. Genralized Additive Models). Currently, Rtglm internally call the 'gam' function of the mgcv R-package.

To install the latest version, use:
```r
devtools::install_github("pnouvellet/Rtglm",build = TRUE)
```

### Vignettes
Please see https://mrc-ide.github.io/EpiEstim/ for vignettes with worked examples, 
FAQs and details about how EpiEstim can be used alongside some other R packages 
in an outbreak analysis workflow.

### References
[1] Anne Cori, Neil M. Ferguson, Christophe Fraser, Simon Cauchemez, [A New Framework and Software to Estimate Time-Varying Reproduction Numbers During Epidemics](https://doi.org/10.1093/aje/kwt133), American Journal of Epidemiology, Volume 178, Issue 9, 1 November 2013, Pages 1505–1512. 

[2] Pierre Nouvellet, [Rtglm: Unifying estimation of the time-varying reproduction number, Rt , under the Generalised Linear and Additive Models](https://doi.org/10.1101/2025.06.24.25330176), MedRxiv, 2025. 