FROM rocker/tidyverse:latest

LABEL Description="This image is used to develop and run Dynamic Programming Research" \
      Author="ACME Products" \
      Version="1.0" \ 
      Maintainer="John-Craig Borman"

RUN Rscript -e "install.packages('RcppAlgos', dependencies = TRUE);" \
   && Rscript -e "install.packages('dygraphs', dependencies = TRUE);" \
   && Rscript -e "install.packages('lubridate', dependencies = TRUE);" \
   && Rscript -e "install.packages('quantmod', dependencies = TRUE);" \
   && Rscript -e "install.packages('PortfolioAnalytics', dependencies = TRUE);" \
   && Rscript -e "install.packages('PerformanceAnalytics', dependencies = TRUE);" \
   && Rscript -e "install.packages('doSNOW', dependencies = TRUE);" \
   && Rscript -e "install.packages('rmakrdown', dependencies = TRUE);"

# Expose for RStudio-Server
EXPOSE 8787
