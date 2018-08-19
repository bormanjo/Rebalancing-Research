docker build -t dp_research .
docker run --rm -it -d -p 8787:8787 -v "//c/Users/J-C Borman/Github/":/home/rstudio/Github/ dp_research
start chrome --new-window "http://localhost:8787"