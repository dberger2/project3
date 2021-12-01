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
library(DT)
library(randomForest)
library(varImp)
c <- read_csv("coffee_data.csv") %>% mutate(across(where(is.character),as_factor)) %>% select(c(-Lot.Number,-ICO.Number, -...1)) %>% mutate(Owner=coalesce(Owner, Farm.Name)) %>% mutate(Region=coalesce(Region, Country.of.Origin)) %>%
  rename(coo = Country.of.Origin,
         var = Variety,
         pm = Processing.Method) %>%
  mutate(coo = str_replace_all(coo, " ", "_"),
         coo = str_replace_all(coo, "\\(", ""),
         coo = str_replace_all(coo, "\\)", ""),
         var = str_replace_all(var, " ", "_"),
         pm = str_replace_all(pm, " ", "_"),
         pm = str_replace_all(pm, "_\\/", ""),
         pm = str_replace_all(pm, "-", ""))

model_data <- c %>% mutate(across(where(is.character),as_factor)) %>%
  filter(is.na(coo) == FALSE,
         is.na(var) == FALSE,
         is.na(Owner) == FALSE)

# Define UI for application that draws a histogram

ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage("Coffee Data",
                tabsetPanel(
                  
#############################About Tab
                    tabPanel("About",
                             #sidebarPanel("Links to Coffee Institue"),
                             br(),
                             
                             mainPanel(h3("This data set comes from the", a("caret package", href="https://topepo.github.io/caret/"), "- originally from the UCI machine learning repository"),
                             )
                    ),
                    
#########################################Data Exploration Tab
                    tabPanel("Data Exploration",
                             sidebarPanel(
                                 h3("Select the type of summary you would like to see"),
                                 #select variables
                                 varSelectInput("var", strong("Select the Variable to Explore"),
                                                c,
                                 ),
                                 
                                 #adding numeric input to change labels angles
                                  numericInput("wordangle", "Change angle of words for readability if needed",value = 0, min = 0, max = 90, step = 45)
                                 ,
                                 
                                 #select plot type
                                 h3("Select the type of plot"),
                                 radioButtons("plot", "Plot Type:",
                                             c("Box Plot",
                                               "Barchart",
                                               "Variable Correlation",
                                               "Density Plot",
                                               "Histogram")),
                                            
                                                 
                                    #adding slider for histogram or barchart
                                    conditionalPanel(condition = "input.plot == 'Histogram'",
                                              sliderInput("hist", "Select Bins", value = 0, min = 0, max = 100),
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
                             #sidebarPanel("This is the side"),
                             mainPanel(
                                tabsetPanel(
                                  #Tab for Modeling Information
                                  tabPanel("Modeling Informaion",
                                    h5("You should include some type of math type in the explanation, you’ll need to include mathJax"),
                                  ),
                                  #Tab Panel for model fitting
                                  tabPanel("Model Fitting",
                                           sidebarPanel(
                                           #slider inputs for choosing training / test data
                                           h4("Select the size of the trianing and test set"),
                                           sliderInput(inputId = "train", label = "Select Train Data Set", min = 0, max = 1, value = 1 - 0.1, step = 0.1),
                                           sliderInput(inputId = "test", label = "Select Test Data Set", min = 0, max = 1, value = 0.1, step = 0.1),
                                           #Variable selection numeric
                                           varSelectInput("varseln", "Select Numeric Variables", select_if(model_data, is.numeric), selected = NULL, multiple = TRUE,
                                                          selectize = TRUE, width = NULL, size = NULL),
                                           #Variable selection categorical
                                           varSelectInput("varselc", "Select Categorical Variables", select_if(model_data, is.factor), selected = NULL, multiple = TRUE,
                                                          selectize = TRUE, width = NULL, size = NULL),
                                           #cross validation select
                                           sliderInput("cv", "Select Level of Cross Validation", min = 0, max = 10, value = 5, step = 1),
                                           #repeat selection
                                           sliderInput("rpt", "Select # of Repeats", min = 0, max = 10, value = 3, step = 1),
                                           #mtry range for Random
                                           sliderInput("mtry", "Select mtry range for Random Forest Model", min = 1, max = 15, value = 7, step = 1, dragRange = FALSE),
                                           
                                           actionButton("go", "Fit Models")
                                ),
                                mainPanel(
                                  h5("Random Forest Summary"),
                                  verbatimTextOutput("rfp"),
                                  h5("Random Forest plot"),
                                  plotOutput("rf"),
                                  h5("lm summary"),
                                  verbatimTextOutput("lm"),
                                  h5("Classification Tree Summary"),
                                  verbatimTextOutput("clt"),
                                  h5("test outputs"),
                                  #verbatim outputs
                                  
                                )
                                  ),
                                  #Tab Panel for prediction
                                  tabPanel("Prediction")
                               )
                             )
                    ),
                    
####################################################################Data Tab
                    #refer to this https://mastering-shiny.org/action-dynamic.html#dynamic-filter
                    tabPanel("Data",
                             sidebarPanel("This is the side",
                                          #check boxes to select columns to view
                                          checkboxGroupInput("show_vars", "Columns in the Coffee Dataset to show:",
                                                             names(c), selected = names(c)),
                                          #button to download CSV file
                                          downloadButton("downloadData", "Download")
                    ),
                             mainPanel("This is the Main",
                              dataTableOutput("datat")
                             )

                    )
                    )
)
)


                    
                    
                
                


