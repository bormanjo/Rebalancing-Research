docker build -t dp_research .
docker run -it -d -p 8787:8787 -v "%cd%":/home/rstudio/Github/ dp_research
start chrome --new-window "http://localhost:8787"