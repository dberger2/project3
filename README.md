# project3
ST 558 Project 3

1) Brief Description of the App and its purpose:

This app explores data about the quality of coffee.  The data source stems from the Coffee Quality Institute and was found on kabble.  The app allows the user to conduct data exploration looking at numeric and graphical summaries from the dataset.  The app also allows the user to construct three models by chosing the variable of interest.  Of note, the response variable, Total.Cup.Points, is hard coded.  The data set is a bit tricky to work with, so taking this step does make the modeling process more smooth and less error prone.  The final tab allows the user to explore the dataset itself and download a CSV file.    

2) List of packages needed to run the app:

library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(caret)
library(readr)
library(dplyr)
library(purrr)
library(DT)

3) Line of code that will install all packages used:

install.packages(c("shiny", "shinydashboard", "shinythemes", "tidyverse", "ggplot2", "caret", "readr", "dplyr", "purrr", "DT",))

4) Line of code to run the application:

shiny::runGitHub('project3', 'dberger2','project3', ref = "main")