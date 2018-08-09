
FROM rocker/tidyverse:latest

RUN Rscript -e "install.packages('RcppAlgos', dependencies = TRUE);" \
   && Rscript -e "install.packages('dygraphs', dependencies = TRUE);" \
   && Rscript -e "install.packages('lubridate', dependencies = TRUE);" \
   && Rscript -e "install.packages('quantmod', dependencies = TRUE);" \
   && Rscript -e "install.packages('PortfolioAnalytics', dependencies = TRUE);" \
   && Rscript -e "install.packages('PerformanceAnalytics', dependencies = TRUE);" \
   && Rscript -e "install.packages('rmakrdown', dependencies = TRUE);"
