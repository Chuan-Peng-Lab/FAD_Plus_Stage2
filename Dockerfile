###### uncomment the code below, you can use the latest verse
# I fixed this image to use 4.3.1
# https://hub.docker.com/layers/rocker/rstudio/4.3.1/images/sha256-91f68963b3fa85628b55acebb6783b42ae865bb5c967f9f0ca435c16e5a3d762?context=explore
# docker pull rocker/rstudio:4.3.1
# ARG BASE_CONTAINER=rocker/rstudio:4.3.1
# FROM $BASE_CONTAINER
FROM --platform=linux/arm64 rocker/rstudio:4.3.1

# install lib dependencies for tidyverse
RUN apt-get update && apt-get install -y \
    libfontconfig1-dev \
    zlib1g-dev \
    libharfbuzz-dev \
    libfribidi-dev \ 
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    cmake \
    libudunits2-dev \
    libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libgit2-dev

# install r packages from tsinghua's mirror: https://mirrors.tuna.tsinghua.edu.cn/CRAN/
# you may also use other mirrors: https://cran.r-project.org/mirrors.html

RUN R -e "install.packages('pacman', repos = 'https://mirrors.tuna.tsinghua.edu.cn/CRAN/')"
RUN R -e "install.packages('here', repos = 'https://mirrors.tuna.tsinghua.edu.cn/CRAN/')"
RUN R -e "install.packages('tidyverse', repos = 'https://mirrors.tuna.tsinghua.edu.cn/CRAN/')"
RUN R -e "install.packages('data.table', repos = 'https://mirrors.tuna.tsinghua.edu.cn/CRAN/')"
RUN R -e "install.packages('psych', repos = 'https://mirrors.tuna.tsinghua.edu.cn/CRAN/')"
RUN R -e "install.packages('car', repos = 'https://mirrors.tuna.tsinghua.edu.cn/CRAN/')"
# reinstall Matrix to the latest version
RUN R -e "install.packages('Matrix', repos = 'https://mirrors.tuna.tsinghua.edu.cn/CRAN/')"
RUN R -e "install.packages(c('CTT','ltm', 'mirt', 'lordif'), repos = 'https://mirrors.tuna.tsinghua.edu.cn/CRAN/')"
RUN R -e "install.packages(c('lavaan','semPlot','semTools'), repos = 'https://mirrors.tuna.tsinghua.edu.cn/CRAN/')"
RUN R -e "install.packages('biocManager', repos = 'https://mirrors.tuna.tsinghua.edu.cn/CRAN/')"
RUN R -e "install.packages(c('foreach', 'parallel', 'iterators', 'doParallel'), repos = 'https://mirrors.tuna.tsinghua.edu.cn/CRAN/')"
RUN R -e "install.packages(c('BiocManager', 'Biobase'), repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('NMF', version = "0.27", repos = 'http://cran.us.r-project.org')"
# RUN R -e "install.packages('cmdstanr', repos = c('https://mc-stan.org/r-packages/', getOption('repos')))"

# install r package from github
# RUN R -e "install.packages('devtools', repos = 'http://cran.us.r-project.org')"
# RUN R -e "devtools::install_github('hauselin/hausekeep')"
# RUN R -e "devtools::install_github('yuki-961004/yukiSH')"
# RUN R -e "devtools::install_github('yuki-961004/yukiBP@cdb5b0764438306aefe2687b7c1993a3e8059161')"

# RUN R -e "install.packages('tinytex', repos = 'http://cran.us.r-project.org')"
# RUN R -e "tinytex::install_tinytex()"
# RUN R -e "install.packages('papaja', repos = 'http://cran.us.r-project.org')"

# install cmdstanr
# RUN mkdir -p /home/rstudio/.cmdstanr
# ENV PATH="/home/rstudio/.cmdstanr:${PATH}"
# RUN R -e "cmdstanr::install_cmdstan(dir = '/home/rstudio/.cmdstanr', cores = 4)"


# add data
# COPY /example/Script_example.Rmd /home/rstudio/example/
# COPY /example/Script_example.r /home/rstudio/example/
# COPY /example/df_example.csv /home/rstudio/example/
