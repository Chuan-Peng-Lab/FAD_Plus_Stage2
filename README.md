# Assessing the Measurement Invariance of Free will and Determinism Plus Scale Across Four Languages (Stage2 RR)

## Authors

-   [Siqi Duan]()
-   [Chenghao Zhou]()
-   [Yixin Gong]()
-   [Zenan Dou]()
-   [Jingguang Li]()
-   [Hu Chuan-Peng](huchuanepng.com)

## Stage 1 RR protocol

<https://osf.io/umhvp>

Preprint: https://osf.io/nk358

## Folder structure of this repo

- Note: the code here can be run locally, but need to be re-checked by external rater.

`STEP0-preprocessing.R`: preprocessing all data;

`STEP1-FAD-PLUS_Analyses_combined.R`: all analyses.

Folder: `3_2_3_SAVE_points` - cleaned datasets and results tables

## Use docker image for macOS

1. Pull the docker image from `hcp4715/rdock` or build the docker image using `Dockerfile`
   pull the docker image: `docker pull hcp4715/rdock:fadpls`
   build the docker image: 
   - for apple chip machine: `docker buildx build -t hcp4715/rdock:fadpls -f Dockerfile.arm64 .`
   - for intel chip machine: `docker buildx build  -t hcp4715/rdock:fadpls_amd64 -f Dockerfile.amd64 .`
2. using the following code to run the container:
   ```
   # for machine with apple chips
   docker run -e PASSWORD=fadpls --cpus=4 --rm -p 8787:8787 -v ${PWD}:/home/rstudio/work hcp4715/rdock:fadpls

   or

   # for machine with intel chips
   docker run -e PASSWORD=fadpls --cpus=4 --rm -p 8787:8787 -v ${PWD}:/home/rstudio/work hcp4715/rdock:fadpls_amd64
   ```