**ClimAVA‑SWE Dataset**

ClimAVA‑SWE: A High‑Resolution CMIP6‑Based Snow Water Equivalent Dataset for the Western United States
The ClimAVA‑SWE dataset provides high‑resolution (4 km) daily snow water equivalent (SWE) projections for the western United States derived from 14 CMIP6 General Circulation Models (GCMs). The dataset includes projections for three Shared Socioeconomic Pathways (SSP245, SSP370, and SSP585) and is intended to support climate impact, hydrologic, water‑resources, and adaptation studies in snow‑dominated regions.
ClimAVA‑SWE is produced using the Spatial Interactions Downscaling (SPID) method, a machine‑learning‑based approach designed to ensure physically consistent and spatially detailed downscaling. SPID captures relationships between coarse‑resolution spatial climate patterns and fine‑scale pixel‑level variability. For each grid cell, two Random Forest models—one for the accumulation period and one for the ablation period—are trained using high‑resolution reference data as the predictand and neighboring pixels from spatially resampled reference data as predictors. These trained models are then applied to bias‑corrected CMIP6 outputs to generate downscaled SWE projections. The resulting dataset preserves realistic spatial patterns and robustly represents extreme snow conditions.


**Repository purpose**

This GitHub repository provides the scripts, configuration files, and documentation required to ensure transparency and reproducibility of the ClimAVA‑SWE dataset.
The full gridded dataset itself is archived separately in a public data repository and is referenced from this repository.


**Repository structure:**

ClimAVA‑SWE/

├── README.md

├── Scripts/

│   ├── 1.downloading/

│   ├── 2.reference_process/

│   ├── 3.subset_cmip6/

│   ├── 4.bias_correction/

│   ├── 5.resample/

│   ├── 6.validation/

│   ├── 7.downscaling/

│   └── 8.figures/

├── csv_files/

├── guides/

└── shapefiles/



**Directory descriptions**


Scripts/
Contains the complete ClimAVA‑SWE data‑production workflow, organized into sequential processing steps from data acquisition through bias correction, spatial resampling, downscaling, validation, and figure generation.

csv_files/
Tabular control and configuration files defining model lists, processing periods, and validation subsets used by the scripts.

guides/
Supporting documentation and auxiliary materials related to dataset usage and interpretation.

shapefiles/
Spatial reference and boundary files used for subsetting, resampling, and visualization tasks.
