# project3
ST 558 Project 3

1) Brief Description of the App and its purpose

2) List of packages needed to run the app

list.of.packages <- c("shiny","ggmap")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){library(x,character.only=TRUE)}) 

3) line of code that would install all packages used

4) Shiny::runGithub code
shiny::runGitHub('project3', 'dberger2','project3', ref = "main")