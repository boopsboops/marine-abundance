# marine-abundance
 
Final data and code for the marine species abundances and climate change paper "Climate change drives poleward increases and equatorward declines in abundance of marine species".

Reuben A. Hastings, Louise A. Rutterford, Jennifer J. Freer, Rupert A. Collins, Stephen D. Simpson and Martin J. Genner.


### Contents

* **`Code/`** - R scripts to run analyses.
    - `Rcode_CC_abun_2019.R` - full data and data subset models and visualisation
    - `Rcode_temp_time_association.R` - time and temperature association model


* **`Data/`** - Data generated from the analyses.
    - `All_dat.csv` - Full dataset analysed in study
    - `location_timeframe_only_dat.csv` - Location and timeframe for temperature association 
    - `Multi_sp_dat.csv` - All multispecies studies only 
    - `No_repeat_dat.csv` - Spatially thinned data (repeat species, locations dropped)
    - `r.val_only_dat.csv` - All studies with reported r/R2 values 
    - `Sig_only_dat.csv` - All studies with reported significance values 
    
    
* **`Data/`** - Data used for the analyses.
    - `Final_data.csv` - All studies and species data for full study
    - `GLOBAL_annual_mean_cell_wlocation_long_valid.csv.gz` - Global lat/lon cell annual mean temperature - HadISST
        - Title: Monthly version of HadISST sea surface temperature component
        - Description: HadISST 1.1 monthly average sea surface temperature
        - Institution: Met Office Hadley Centre
        - Source: HadISST
        - Reference: Rayner, N. A., Parker, D. E., Horton, E. B., Folland, C. K., Alexander, L. V., Rowell, D. P., Kent, E. C., Kaplan, A.  Global analyses of sea surface temperature, sea ice, and night marine air temperature since the late nineteenth century J. Geophys. Res.Vol. 108, No. D14, 4407 [10.1029/2002JD002670](http://dx.doi.org/10.1029/2002JD002670)
        - Conventions: CF-1.0
        - History: 6/9/2019 converted to netcdf from pp format
        - Supplementary_information: Updates and supplementary information will be available from [http://www.metoffice.gov.uk/hadobs/hadisst](http://www.metoffice.gov.uk/hadobs/hadisst)
        - Comment: Data restrictions: for academic research use only. Data are Crown copyright see [http://www.opsi.gov.uk/advice/crown-copyright/copyright-guidance/index.htm](http://www.opsi.gov.uk/advice/crown-copyright/copyright-guidance/index.htm)
    - `species_occurrence_records.csv.gz` - Raw occurrence records for the 342 study species as downloaded from GBIF via modestR on 02/02/2018
        -  Two species (_Pomatomus saltatrix_ and _Caretta caretta_) were divided in to Northern (N) and Southern (S) hemisphere groups
        -  These records were further subject to screening and checking protocols as described in the manuscript.
        -  Contributing datasets are: [10.15468/dl.bogm0t](https://doi.org/10.15468/dl.bogm0t) (species A-H); [10.15468/dl.fsf7br](https://doi.org/10.15468/dl.fsf7br) (species I-R); and [10.15468/dl.mqu3hu](https://doi.org/10.15468/dl.mqu3hu) (species S-Z).