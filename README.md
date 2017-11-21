# Reconstructing the last known movements of one of Nature's giants

Author(s): Clive N. Trueman, Andrew L. Jackson and [Natalie Cooper](mailto:natalie.cooper.@nhm.ac.uk)  

This repository contains all the code and data used in the [link to paper will appear here](). This paper came from a BES small project grant (5771/6815) and focuses on *the* blue whale now adorning Hintze Hall at the Natural History Museum, London.

To cite the paper: 
> XXX

To cite this repo: 
> XXX

## Data
These analyses are based on d13C and d15N stable isotope values taken from *the* blue whale (NHMUK_) at the Natural History Museum, London. 
All isotope data are available from the [NHM Data Portal](DOI here) and can be found in the `data/` folder. 
We then created 1000 movement model simulations. These data are available from the [NHM Data Portal](DOI here) but the file is too large for GitHub. The top 10% and bottom 10% of models are however available (`data/top10.csv` and `data/bottom10.csv`), along with their extracted maximum latitudes.

If you use the data please cite as follows: 
> XXX.

-------
## Analyses
The analysis is divided as follows.

1. Extraction of d13C and d15N isotopes from baleen. Raw samples are available on request from NHM. The output values for d13C and d15N can be found on the NHM Data Portal (link).
1. Fitting movement models.
1. Downstream analyses and figures. 

The movement models were written by Clive Trueman and are currently not available in this repo. EXPLAIN

## Downstream analyses and figures
All code used to run downstream analyses and make figures is included in the `code/` folder. Before starting remember to either set your working directory to the **blue-whale-bes** folder on your computer, or open an RStudio project from that folder.

* **Extract-models.R** extracts the top 10% and bottom 10% of movement models, and the maximum and standard deviation of the latitudes of these models. It writes these to the `data/` folder for use in building figures.
* **Figure-1-code.R	** does what is says on the tin - creates Figure 1.
* **Figure-2-code.R** is code for Figure 2.
* **Figure-3-code.R** is code for Figure 3.
* **Figure-S1-S2-code.R** is code for Figures S1 and S2.
* **Figure-S3-code.R** is code for Figure S3.
* **Figure-S4-code.R** is code for Figure S4.
* **Figure-S5-code.R** is code for Figure S4.
* **Figure-S6-code.R** is code for Figure S4.

Most of this code was written by Clive Trueman, with some tidying/modifications by Andrew Jackson and Natalie Cooper. Unfortunately/interestingly (depending on your opinion!) we all code in different ways (tidyverse vs base being the biggest difference). So some of the code may be a bit hard to follow, depending on your preferred approach, and it is not as consistent as we'd like. But it works goddammit! And we are all too busy to convince the others that one way is better...

## Session Info
For reproducibility purposes, here is the output of `devtools:session_info()` used to perform the analyses in the publication.

Session info ------------------------------------------------------------------------


## Checkpoint for reproducibility
To rerun all the code with packages as they existed on CRAN at time of our analyses we recommend using the `checkpoint` package, and running this code prior to the analysis:

```{r}
checkpoint("2017-08-17")
```
