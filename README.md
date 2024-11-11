# Overview

This repository contains code written by AM Willson for the PalEON project. The purpose is to develop climate and soil drivers for various projects on a standard 8 x 8 km grid matching the grid of vegetation data products from the PalEON project. There is one set of soil drivers and two sets of climate drivers, one for the historical period from the late 1800s and one for the modern period from the early 2000s. The code is broken into two sections, one for soil variables and one for climate variables.

# License

This repository holds an MIT License, as described in the LICENSE file.

# Software versions

This repository was built in the R environment using R version 4.4.2.

# Package versions

- `dplyr` v. 1.1.4
- `fields` v. 16.3
- `GGally` v. 2.2.1
- `ggplot2` v. 3.5.1
- `raster` v. 3.6.26
- `sf` v. 1.0.19
- `sfheaders` v. 0.4.4
- `soilDB` v. 2.8.4
- `terra` v. 1.7.78
- `tidyr` v. 1.3.1

All packages can be installed using the install_packages.R script. When possible, the specific version of the package will be installed.

# Directory structure

## data

**Raw** data and **processed** data products from processing data for use in other repositories. The **raw** subdirectory is empty because the contents are large. The **raw** will be provided upon request. All the created files in the **processed** directory are provided, allowing for comarison between any products replicated and our original outputs. These can also be used to directly recreate any figures.

### raw: inputs to this workflow

- soils: folder containing soil drivers that were downloaded and formatted for gSSURGO
    - gssurgo_average_030_700m.RData: dataframe with soil covariates that are still in the 700 x 700 m grid from downloading from gSSURGO
    - gssurgo_floodplain_030_700m.RData: dataframe with floodplain covariate still in the 700 x 700 m grid from downloading from gSSURGO

### processed: processed data for use in PalEON analyses

- climate: processed climate products aggregated over time and to the 8 x 8 km grid
    - gridded_climate_modern.RData: modern (1995-2015) gridded climate estimates
    - gridded_climate.RData: historcal (1895-1915) gridded climate estimates
- soils: processed soil products aggregated to the 8 x 8 km grid
    - gridded_soil.RData: gridded soil estimates (soil texture, soil calcium carbonate concentration, soil available water content, and fraction of each grid cell in a floodplain)

### SDM_bigdata/PRISM/: raw PRISM downloads stored on external drive

- PRISM_ppt_stable_4kmM2_189501_198012_bil/: folder containing .bil files with PRISM estimates of total monthly precipitation. Only files for months between 1895-1915 were kept
- PRISM_tmax_stable_4kmM3_189501_198012_bil/: folder containing .bil files with PRISM estimates of maximum monthly temperature. Only files for months between 1895-1915 were kept
- PRISM_tmean_stable_4kmM3_189501_198012_bil/: folder containing .bil files with PRISM estimates of mean monthly temperature. Only files for months between 1895-1915 were kept
- PRISM_tmin_stable_4kmM3_189501_198012_bil/: folder containing .bil files with PRISM estimates of minimum monthly temperature. Only files for months between 1895-1915 were kept
- PRISM_vpdmax_stable_4kmM3_189501_198012_bil/: folder containing .bil files with PRISM estimates of maximum monthly vapor pressure deficit. Only files for months between 1895-1915 were kept
- climate_points.RData: intermediate data product with PRISM climate data in dataframe format
- climate_summary.RData: intermediate data product with PRISM climate aggregated over time in dataframe format

### SDM_bigdata/PRISM_modern/: raw PRISM downloads for modern period stored on external drive

- PRISM_ppt_stable_4kmM3_198101_202403_bil/: folder containing .bil files with PRISM estimates of total monthly precipitation. Only files for months between 1995-2015 were kept
- PRISM_tmax_stable_4kmM3_198101_202403_bil/: folder containing .bil files with PRISM estimates of maximum monthly temperature. Only files for months between 1995-2015 were kept
- PRISM_tmean_stable_4kmM3_198101_202403_bil/: folder containing .bil files with PRISM estimates of mean monthly temperature. Only files for months between 1995-2015 were kept
- PRISM_tmin_stable_4kmM3_198101_202403_bil/: folder containing .bil files with PRISM estimates of minimum monthly temperature. Only files for months between 1995-2015 were kept
- PRISM_vpdmax_stable_4kmM3_198101_202403_bil/: folder containing .bil files with PRISM estimates of maximum monthly vapor pressure deficit. Only files for months between 1995-2015 were kept
- climate_points.RData: intermediate data product with PRISM climate data in dataframe format
- climate_summary.RData: intermediate data product with PRISM climate aggregated over time in dataframe format

## scripts

Code for downloading and processing data. The code is broken into two sections. The first section (**1.soils**) downloads, formats, and aggregates the soil variable estimates from gSSURGO to the 8 x 8 km PalEON grid. The second section (**2.climate**) formats and aggregates the climate variable estimtates from PRISM over time and to the 8 x 8 km PalEON grid. The directory additionaly includes two helper files (**install_packages.R** and **1.soils/define_bounds.R**) for installing required packages, including versions, and dividing states into subsections for downloading soil data, respectively.

### 1.soils

- **1.1.download_gSSURGO_soils.R**: Downloading data for each state in the Upper Midwest region of interest. Data processing has to proceed directly from download or you get error "external pointer is not valid." Note that this step was originaly performed in spring 2024. Updates to the gSSURGO data products have created issues to the data product. This step should NOT be rerun and saved over the existing output, which is the correct data.
    - Inputs: none
    - Outputs:
        - data/raw/soils/gssurgo_average_030_700m.RData: Soil variables for 0-30 cm depth for all fie states in our geographic domain. Used in 1.3.gridded_soils.R
- **1.2.download_gSSURGO_floodplain.R**: Downloading data on the presence of floodplain from gSSURGO. This is the same as the gSSURGO soil routine, except you need a specific method = "Dominant Component (Category)" because it's a categorical variable. Note that this step was originally performed in spring 2024. Updates to the gSSURGO data products have created issues to the data product. This step should NOT be rerun and saved over the existing output, which is the correct data.
    - Inputs: none
    - Outputs:
        - data/raw/soils/gssurgo_floodplain_030_700m.RData: Floodplain variable for 0-30 cm depth for all five stats in the geographic domain. Used in 1.3.gridded_soils.R
- **1.3.gridded_soils.R**: Aggregating gSSURGO soil variables to PalEON grid
    - Inputs:
        - data/raw/soils/gssurgo_average_030_700m.RData: Dataframe of soil  variables on 700 x 700 m grid
        - data/raw/soils/gssurgo_floodplain_030_700m.RData: Dataframe of floodplain presence variable on 700 x 700 m grid
        - ~/Google Drive 2/environ-veg-prediction/data/processed/FIA/gridded_all_plots.RData: Grid cells in the standard PalEON grid from modern time period. This is from the repository https://github.com/amwillson/environ-veg-prediction/
        - ~/Google Drive 2/environ-veg-prediction/data/processed/PLS/gridded_fcomp_density.RData: Grid cells in the standard PalEON grid from the historical time period. This is from the repository https://github.com/amwillson/environ-veg-prediction/
    - Outputs:
        - data/processed/soils/gridded_soil.RData: Dataframe of soil variables aggregated to the PalEON 8 x 8 km grid. This is exported to the repository https://github.com/amwillson/environ-veg-prediction/ and additionally used for other PalEON products requiring gridded soil products
- **define_bounds.R**: helper file for dividing states into even sections for easier downloading of gSSURGO data

### 2.climate

- **2.1.process_climate.R**: Preparing climate drivers downloaded from PRISM. Cliamte estimates downloaded from PRISM website in February 2024. Formatting into dataframe and doing plotting checks.
    - Inputs:
        - bil files in /Volumes/FileBackup/SDM_bigdata/PRISM/PRISM_ppt_stable_4kmM2_189501_198012_bil/: PRISM estimates of total monthly precipitation for the period 1895-1915
        - .bil files in /Volumes/FileBackup/SDM_bigdata/PRISM/PRISM_tmean_stable_4kmM3_189501_198012_bil/: PRISM estimates of mean monthly temperature for the period 1895-1915
        - .bil files in /Volumes/FileBackup/SDM_bigdata/PRISM/PRISM_tmax_stable_4kmM3_189501_198012_bil/: PRISM estimates of maximum monthly temperature for the period 1895-1915
        - .bil files in /Volumes/FileBackup/SDM_bigdata/PRISM/PRISM_tmin_stable_4kmM3_189501_198012_bil/: PRISM estimates of minimum monthly temperature for the period 1895-1915
        - .bil files in /Volumes/FileBackup/SDM_bigdata/PRISM/PRISM_vpdmax_stable_4kmM3_189501_198012_bil/: PRISM estimates of maximum monthly vapor pressure deficit for the period 1895-1915
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/PRISM/climate_points.RData: Monthly estimates of climate drivers in the native scale of PRISM data. This is not used. It is save so that I don't have to re-run the steps of loading in the data because it takes a long time to convert to points
        - /Volumes/FileBackup/SDM_bigdata/PRISM/climate_summary.RData: Temporal summaries of climate variables over months of the year and years in this time period, still in the native PRISM resolution. Used in 2.2.gridded_climate.R
- **2.2.gridded_climate.R**: Aggregating climate estimates to PalEON grid
    - Inputs:
        - /Volumes/FileBackup/SDM_bigdata/PRISM/climate_summary.RData: Dataframe with temporal climate summaries at native PRISM resolution
        - ~/Google Drive 2/environ-veg-prediction/data/processed/FIA/gridded_all_plots.RData: Dataframe with 8 x 8 km grid cells in the modern time period. This is from the repository https://github.com/amwillson/environ-veg-prediction/
        - ~/Google Drive 2/environ-veg-prediction/data/processed/PLS/gridded_fcomp_density.RData: Dataframe with 8 x 8 km grid cells in the historic time period. This is from the repository https://github.com/amwillson/environ-veg-prediction/
    - Outputs:
        - data/processed/climate/gridded_climate.Rdata: Dataframe of climate estimates aggregated to the PalEON 8 x 8 km grid. Used in the repository https://github.com/amwillson/environ-veg-prediction/ and for other PalEON projects
- **2.3.process_modern_climate.R**: Preparing climate drivers downloaded from PRISM for the modern period. Climate estimates downloaded from PRISM website in October 2024. Formatting into dataframe and doing plotting checks.
    - Inputs:
        - .bil files in /Volumes/FileBackup/SDM_bigdata/PRISM_modern/PRISM_ppt_stable_4kmM3_198101_202403_bil/: PRISM estimates of total monthly precipitation for the period 1995-2015
        - .bil files in /Volumes/FileBackup/SDM_bigdata/PRISM_modern/PRISM_tmean_stable_4kmM3_198101_202403_bil/: PRISM estimates of mean monthly temperature for the period 1995-2015
        - .bil files in /Volumes/FileBackup/SDM_bigdata/PRISM_modern/PRISM_tmax_stable_4kmM3_198101_202403_bil/: PRISM estimates of maximum monthly temperature for the period 1995-2015
        - .bil files in /Volumes/FileBackup/SDM_bigdata/PRISM_modern/PRISM_tmin_stable_4kmM3_198101_202403_bil/: PRISM estimates of minimum monthly temperature for the period 1995-2015
        - .bil files in /Volumes/FileBackup/SDM_bigdata/PRISM_modern/PRISM_vpdmax_stable_4kmM3_198101_202403_bil/: PRISM estimates of maximum monhly vapor pressure deficit for the period 1995-2015
    - Outputs:
        - /Volumes/FileBackup/SDM_bigdata/PRISM_modern/climate_points.RData: Monthly estimates of modern climate drivers in the native scale of PRISM data. This is not used. It is saved so that I don't have to re-run the steps of loading in the data because it takes a long time to convert to points
        - /Volumes/FileBackup/SDM_bigdata/PRISM_modern/climate_summary.RData: Temporal summaries of modern climate variables over months of the year and years in this time period, still in the PRISM native resolution. Uesd in 2.4.gridded_modern_climate.
- **2.4.gridded_modern_climate.R**: Aggregating modern climate estimates to PalEON grid
    - Inputs:
        - /Volumes/FileBackup/SDM_bigdata/PRISM_modern/climate_summary.RData: Dataframe with modern temporal climate summaries at native PRISM resolution
        - ~/Google Drive 2/environ-veg-prediction/data/processed/FIA/gridded_all_plots.RData: Dataframe with 8 x 8 km grid cells in the modern time period. This is from the repository https://github.com/amwillson/environ-veg-prediction/
        - ~/Google Drive 2/environ-veg-prediction/data/processed/PLS/gridded_fcomp_density.RData: Dataframe with 8 x 8 km grid cells in teh historic time period. This is from the repository https://github.com/amwillson/environ-veg-prediction/
    - Outputs:
        - data/processed/climate/gridded_climate_modern.Rdata: Dataframe of modern climate estimates aggregated to the PalEON 8 x 8 km grid. Used in the repository https://github.com/amwillson/environ-veg-prediction/ and for other PalEON projects
        
### Other code

- install_packages.R: helper file for installing packages, with package versions noted