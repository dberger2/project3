#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(rmarkdown)
library(caret)
library(readr)
library(dplyr)
library(corrplot)
library(purrr)
c <- read_csv("coffee_data.csv") %>% select(c(-Lot.Number,-ICO.Number, -...1)) %>% mutate(Owner=coalesce(Owner, Farm.Name)) %>% mutate(Region=coalesce(Region, Country.of.Origin)) 

# Define UI for application that draws a histogram

ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage("Coffee Data",
                tabsetPanel(
                  
#############################About Tab
                    tabPanel("About",
                             sidebarPanel("Links to Coffee Institue"),
                             br(),
                             
                             mainPanel(h3("This data set comes from the", a("caret package", href="https://topepo.github.io/caret/"), "- originally from the UCI machine learning repository"),
                             )
                    ),
                    
#########################################Data Exploration Tab
                    tabPanel("Data Exploration",
                             sidebarPanel(
                                 h3("Select the type of summary you would like to see"),
                                 selectInput("var", strong("Select the Variable to Explore"),
                                              c("Country.of.Origin",
                                                "Aroma",
                                                "Flavor",
                                                "Aftertaste",
                                                "Acidity",
                                                "Body",
                                                "Balance",
                                                "Uniformity",
                                                "Clean.Cup",
                                                "Sweetness",
                                                "Cupper.Points",
                                                "Total.Cup.Points",
                                                "Moisture",
                                                "altitude_low_meters",
                                                "altitude_high_meters",
                                                "altitude_mean_meters",
                                                "Species", 
                                                "Number.of.Bags",
                                                "Region", 
                                                "Number.of.Bags",
                                                "Harvest.Year", 
                                                "Variety",
                                                "Color")),
                                 
                                 #adding numeric input to change labels angles
                                 conditionalPanel(condition = "input.var == 'Country.of.Origin' |
                                                     input.var == 'Region' |
                                                     input.var == 'Harvest.Year' |
                                                     input.var == 'Variety'",
                                                  numericInput("wordangle", "Change angle of words for readability if needed",value = 0, min = 0, max = 90, step = 15),
                                 ),
                                 
                                 #select plot type
                                 h3("Select the type of plot"),
                                 radioButtons("plot", "Plot Type:",
                                             c("Box Plot",
                                               "Barchart",
                                               "Variable Correlation",
                                               "Density Plot",
                                               "Histogram")),
                                            
                                                 
                                    #adding slider for histogram or barchart
                                    conditionalPanel(condition = "input.plot == 'Histogram' | input.plot == 'Barchart'",
                                              sliderInput("boxhist", "Select Range",value = c(10,20), min = 0, max = 100),
                                    ),
                                    
                                  
                             ),
                                    
                             mainPanel("This is the Main",
                                       h4("Summary of the variable"),
                                       verbatimTextOutput("sum"),
                                       plotOutput("plot"),
                                       conditionalPanel(condition = "input.plot == 'Variable Correlation'",
                                                        h4("This is the variable correlation with the repsonse variable total.cup.points"),
                                                        verbatimTextOutput("corr")
                                       )
                             )
                                                      ),
##########################################################Modeling Tab
                    tabPanel("Modeling",
                             #will include 3 side tabs (modeling info
                             #model fitting, Prediction)
                             #selectize input multiple = true
                             sidebarPanel("This is the side"),
                             mainPanel(
                                tabsetPanel(
                                  tabPanel("Modeling Informaion"),
                                  tabPanel("Model Fitting"),
                                  tabPanel("Prediction")
                               )
                             )
                    ),
                    
####################################################################Data Tab
                    #refer to this https://mastering-shiny.org/action-dynamic.html#dynamic-filter
                    tabPanel("Data",
                             sidebarPanel("This is the side"),
                             mainPanel("This is the Main"),
                    )
                    )
                    )
)
                    
                    
                
                


