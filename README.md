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

## Folder structure of this repo

`STEP0-retest-MLOC-match.R`: merge the data for the retest MLOC CHN dataset;

`STEP1-CHN-cleaning.R`: preprocess the newly collected CHN data, calculations of FAD_Plus scores & basic analysis;

`STEP2-ENG-FRN-JPN-cleaning`: preprocessing ENG, FRN, and JPN data, calculations of FAD_Plus scores & basic analysis; Note the data from these languages are already cleaned in the previous stage;

`STEP3-FAD-PLUS_Analysis`: all analyses as reported in our stage 2 RR report.

Folder: "3_2_3_SAVE_points" - cleaned datasets and results tables


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