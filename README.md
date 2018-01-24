# Reconstructing the last known movements of one of Nature's giants

Author(s): [Clive N. Trueman](mailto:trueman@noc.soton.ac.uk), Andrew L. Jackson and [Natalie Cooper](mailto:natalie.cooper.@nhm.ac.uk)  

This repository contains all the code and data used in the [link to paper will appear here](). This paper came from a BES small project grant (5771/6815) and focuses on *the* blue whale now adorning Hintze Hall at the Natural History Museum, London.

To cite the paper: 
> XXX

To cite this repo: 
> XXX

## Data
All data are available from the [NHM Data Portal](https://doi.org/10.5519/0093278).
However, to aid reproducibility we have also included data required to replicate our analyses/figures in the `data/` folder. 

If you use the data please cite as follows: 
> Clive N Trueman; Andrew L Jackson; Katharyn S Chadwick; Ellen J Coombs et al. (2018). Dataset: Baleen stable isotope data. Natural History Museum Data Portal (data.nhm.ac.uk). [https://doi.org/10.5519/0093278](https://doi.org/10.5519/0093278).

* `raw-whale-isotope-data.csv` contains d13C and d15N stable isotope values taken from *the* blue whale (NHMUK_) at the Natural History Museum, London. Plus some other rorquals to be covered in a later paper. These data are used throughout including Figure 1, Figure 3, Figures S1 & S2.
* `TroLev4_d13C.grd` and `TroLev4_d13C.gri`	are rasters of trophic level four d13C values required for extracting models in script 01.
* `bottom10percent.csv` and `top10percent.csv` are the top 10% and bottom 10% of movement simulations, required for Figure 2 and Figure S7.
* `top10smooth.csv` are the loess smoothed values for the top 10% of movement simulations, required for Figure 3.
* `mid.bottom10percent.csv` and `mid.top10percent.csv` are the top 10% and bottom 10% of movement simulations but for behavioural phase two only, required for Figure 4.
* `Atl_Annual_d13C.grd`, `Atl_Annual_d13C.gri`, `Atl_Annual_d15N.grd` and `Atl_Annual_d15N`.gri are rasters of Atlantic Ocean stable isotope values needed for Figure S3.
* `Atl.Res.TL.csv`, `Canaries.Res.TL.csv`, `CV.TL.csv`, `Ireland.Res.TL.csv`, `Norway.Res.TL.csv` and `Mauritania.Res.TL.csv` are model simulation outputs for different residency models, required for Figure S4.
* `all.r2.csv` is the r2 values from the models compared to the real data required for Figure S5.
* `max.lat.csv` and `sd.lat.csv` are the extracted maximum latitudes and their standard deviations from the top 10% and of models, required for Figure S6.

Note that due to size limitations, the input data for the movement models (bathymetry data and outputs from NEMO MEDUSA models providing d13C, plankton biomass, and sea surface temperature values), and the model simulation outputs themselves are only available from the [NHM Data Portal](https://doi.org/10.5519/0093278).

To use the NEMO MEDUSA data please cite: 
> A. Yool, E. E. Popova, and T. R. Anderson. 2013MEDUSA-2.0: an intermediate complexity biogeochemical model of the marine carbon cycle for climate change and ocean acidification studies. Geosci. Model Dev., 6, 1767–1811. www.geosci-model-dev.net/6/1767/2013/ doi:10.5194/gmd-6-1767-2013. Also see [https://www.nemo-ocean.eu/](https://www.nemo-ocean.eu/).

-------
## Analyses
The analysis is divided as follows.

1. Extraction of d13C and d15N isotopes from baleen. Raw samples are available on request from NHM. The output values for d13C and d15N can be found on the [NHM Data Portal](https://doi.org/10.5519/0093278).
1. Fitting movement simulation models.
1. Downstream analyses and figures. 

## Movement simulation models
The code CT used to run the models is provided in the `movement-model/` folder. However, this has only been minimally cleaned and tidied for public consumption, so 1) please don't judge(!) and 2) this may not be reproducible (sorry!). If you're interested in using the model on your own data feel free to drop Clive an [email](mailto:trueman@noc.soton.ac.uk) if you're confused.

## Downstream analyses and figures
All code used to run downstream analyses and make figures is included in the `code/` folder. Before starting remember to either set your working directory to the **blue-whale-bes** folder on your computer, or open an RStudio project from that folder.

* **00-fix-simulations-file.R** removes simulations with fewer than 3019 days.
* **01-extract-models.R** extracts the top 10% and bottom 10% of movement models, and the maximum and standard deviation of the latitudes of these models. It writes these to the `data/` folder for use in building figures.
* **Figure-1-code.R** does what it says on the tin - creates Figure 1.
* **Figure-2-code.R** is code for Figure 2.
* **Figure-3-code.R** is code for Figure 3.
* **Figure-S1-S2-code.R** is code for Figures S1 and S2.
* **Figure-S3-code.R** is code for Figure S3.
* **Figure-S4-code.R** is code for Figure S4.
* **Figure-S5-code.R** is code for Figure S5.
* **Figure-S6-code.R** is code for Figure S6.
* **Figure-S7-code.R** is code for Figure S7.

Most of this code was written by Clive Trueman, with some tidying/modifications by Andrew Jackson and Natalie Cooper. Unfortunately/interestingly (depending on your opinion!) we all code in different ways (`tidyverse` vs `base` being the biggest difference). So some of the code may be a bit hard to follow, depending on your preferred approach, and it is not as consistent as we'd like. But it works goddammit! And we are all too busy to convince the others that one way is better...

## Session Info
For reproducibility purposes, here is the output of `devtools::session_info()` used to perform the analyses in the publication.

    Session info ------------------------------------------------------------------
    setting  value                       
    version  R version 3.4.3 (2017-11-30)
    system   x86_64, darwin15.6.0        
    ui       RStudio (1.1.414)           
    language (EN)                        
    collate  en_GB.UTF-8                 
    tz       Europe/London               
    date     2018-01-23                  

    Packages ----------------------------------------------------------------------
    package        * version date       source        
    abind            1.4-5   2016-07-21 cran (@1.4-5) 
    base           * 3.4.3   2017-12-07 local         
    colorspace       1.3-2   2016-12-14 CRAN (R 3.4.0)
    compiler         3.4.3   2017-12-07 local         
    curl             2.8.1   2017-07-21 CRAN (R 3.4.1)
    datasets       * 3.4.3   2017-12-07 local         
    deldir           0.1-14  2017-04-22 CRAN (R 3.4.0)
    devtools         1.13.4  2017-11-09 CRAN (R 3.4.2)
    digest           0.6.12  2017-01-27 CRAN (R 3.4.0)
    ggplot2          2.2.1   2016-12-30 CRAN (R 3.4.0)
    goftest          1.1-1   2017-04-03 CRAN (R 3.4.0)
    graphics       * 3.4.3   2017-12-07 local         
    grDevices      * 3.4.3   2017-12-07 local         
    grid           * 3.4.3   2017-12-07 local         
    gridExtra      * 2.3     2017-09-09 CRAN (R 3.4.1)
    gtable         * 0.2.0   2016-02-26 CRAN (R 3.4.0)
    lattice          0.20-35 2017-03-25 CRAN (R 3.4.3)
    lazyeval         0.2.1   2017-10-29 CRAN (R 3.4.2)
    leaps          * 3.0     2017-01-10 CRAN (R 3.4.0)
	 locfit         * 1.5-9.1 2013-04-20 CRAN (R 3.4.0)
	 mapdata        * 2.2-6   2016-01-14 CRAN (R 3.4.0)
	 maps           * 3.2.0   2017-06-08 cran (@3.2.0) 
	 Matrix           1.2-12  2017-11-20 CRAN (R 3.4.3)
	 memoise          1.1.0   2017-04-21 CRAN (R 3.4.0)
	 methods        * 3.4.3   2017-12-07 local         
	 mgcv           * 1.8-22  2017-09-24 CRAN (R 3.4.3)
	 munsell          0.4.3   2016-02-13 CRAN (R 3.4.0)
	 nlme           * 3.1-131 2017-02-06 CRAN (R 3.4.3)
	 plyr             1.8.4   2016-06-08 CRAN (R 3.4.0)
	 polyclip         1.6-1   2017-03-22 CRAN (R 3.4.0)
	 quadprog         1.5-5   2013-04-17 cran (@1.5-5) 
	 quantmod         0.4-12  2017-12-10 CRAN (R 3.4.3)
	 RColorBrewer   * 1.1-2   2014-12-07 CRAN (R 3.4.0)
	 Rcpp             0.12.15 2018-01-20 CRAN (R 3.4.3)
	 rlang            0.1.6   2017-12-21 CRAN (R 3.4.3)
	 rpart          * 4.1-11  2017-03-13 CRAN (R 3.4.3)
	 scales           0.5.0   2017-08-24 CRAN (R 3.4.1)
	 spatstat       * 1.54-0  2017-11-21 CRAN (R 3.4.3)
	 spatstat.data  * 1.2-0   2017-11-20 CRAN (R 3.4.3)
	 spatstat.utils   1.8-0   2017-11-20 CRAN (R 3.4.3)
	 stats          * 3.4.3   2017-12-07 local         
	 tensor           1.5     2012-05-05 CRAN (R 3.4.0)
	 tibble           1.3.4   2017-08-22 CRAN (R 3.4.1)
	 tools            3.4.3   2017-12-07 local         
	 TSA            * 1.01    2012-11-13 CRAN (R 3.4.0)
	 tseries        * 0.10-42 2017-06-22 CRAN (R 3.4.1)
	 TTR              0.23-2  2017-07-11 CRAN (R 3.4.1)
	 utils          * 3.4.3   2017-12-07 local         
	 viridis        * 0.4.1   2018-01-08 CRAN (R 3.4.3)
	 viridisLite    * 0.2.0   2017-03-24 CRAN (R 3.4.0)
	 withr            2.0.0   2017-07-28 CRAN (R 3.4.1)
	 xts              0.10-1  2017-12-20 CRAN (R 3.4.3)
	 yaml             2.1.14  2016-11-12 cran (@2.1.14)
	 zoo              1.8-1   2018-01-08 CRAN (R 3.4.3)

## Checkpoint for reproducibility
To rerun all the code with packages as they existed on CRAN at time of our analyses we recommend using the `checkpoint` package, and running this code prior to the analysis:

```{r}
checkpoint("2018-01-24")
```
