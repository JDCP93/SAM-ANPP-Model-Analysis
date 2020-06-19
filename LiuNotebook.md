Replicating Liu et al
================
Jon Page
16/06/2020

This notebook is to document my replication of Liu et al’s model from
their 2019 paper.

I have downloaded the model code from github
(<https://github.com/yliu11/NEE_Bayes_model>, accessed 15/06/20).

I need to coerce data into the required input formats and produce a
wrapper script.

Let’s activate\!

## Working on Walnut Gulch

I will first attempt to replicate the modelling at one site, Walnut
Gulch (FluxNet SiteID US\_Wkg).

I need to access the full fluxnet data for this, as the data I have from
previous modelling of this site does not include SWC. Daily FluxNet data
will be used.

``` r
if (file.exists(paste0(Site,"_DailyData.Rdata"))){
  print(paste0("Gosh! Processed daily FluxNet data already exists for ",Site))
} else {
  source("FluxNetProcess.R")
  FluxNetProcess(Site)
}
```

    ## Warning! There is a run of 7 consecutive days with poor data!

TO DO:

  - Description for NDVIProcess
  - Add NDVIProcess to FluxNetProcess (with check for file)
