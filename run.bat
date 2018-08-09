docker pull rocker/tidyverse
docker run --rm -it -d -p 8787:8787 -v "//c/Users/J-C Borman/Github/":/home/rstudio/Github/ rocker/tidyverse
start chrome --new-window "http://localhost:8787"