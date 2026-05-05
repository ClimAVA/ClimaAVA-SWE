The ClimAVA_SWE dataset provides high-resolution (4 km) future climate projections derived from 14 CMIP6 General Circulation Models (GCMs). It focuses on Snow Water Equivalent (SWE) and includes projections for three Shared Socioeconomic Pathways (SSP245, SSP370, and SSP585) at a daily temporal scale. The initial release of ClimAVA_SWE covers the entire western United States.
ClimAVA_SWE is produced using the newly developed Spatial Interactions Downscaling (SPID) method, which ensures high-quality downscaling through advanced machine learning techniques.
SPID captures the relationship between large-scale spatial patterns at GCM resolution and fine-scale pixel values. For each pixel, two Random Forest models (one for the accumulation period and one for the ablation period) were trained using fine-resolution reference data as the predictand, and nine neighboring pixels from a spatially resampled (coarser) version of the reference data as predictors.
These trained models are then applied to bias-corrected GCM data to generate the downscaled projections. The resulting dataset maintains strong climate realism and effectively represents extreme events.

ClimAVA-SWE/
│

├── README.md
│

├── scripts/

│     ├── 1.downloading/

│     ├── 2.reference_process/

│     ├── 3.subset_cmip6/

│     ├── 4.bias_correction/

│     ├── 5.resample/

│     ├── 6.validation/

│     ├── 7.downscaling/

│     └── 8.figures/

│
├── csv_files.zip

├── guides.zip

└── shapefiles.zip

