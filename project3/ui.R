#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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

#Data for Data Exploration
c <- read_csv("coffee_data.csv") %>% mutate(across(where(is.character),as_factor)) %>% select(c(-Lot.Number,-ICO.Number, -...1)) %>% mutate(Owner=coalesce(Owner, Farm.Name)) %>% mutate(Region=coalesce(Region, Country.of.Origin))

#Data for modeling
c2 <- c %>% rename(coo = Country.of.Origin,
         var = Variety,
         pm = Processing.Method) %>%
  mutate(coo = str_replace_all(coo, " ", "_"),
         coo = str_replace_all(coo, "\\(", ""),
         coo = str_replace_all(coo, "\\)", ""),
         var = str_replace_all(var, " ", "_"),
         pm = str_replace_all(pm, " ", "_"),
         pm = str_replace_all(pm, "_\\/", ""),
         pm = str_replace_all(pm, "-", ""))

model_data <- c2 %>% mutate(across(where(is.character),as_factor)) %>%
  filter(is.na(coo) == FALSE,
         is.na(var) == FALSE,
         is.na(Owner) == FALSE)

# Define UI for application that draws a histogram

ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage("Exploration and Modeling of Coffee Data",
                tabsetPanel(
                  
#############################About Tab
                    tabPanel("About",
                             br(),
                             
                             mainPanel(
                               #Briefly discuss the data and its source - providing a link to more information about the data ∗ 
                               h3("Welcome!  This app allows an exploration of data related to coffee.  The data set comes from the", a("Coffee Quality Institute", href="https://www.coffeeinstitute.org/"), "and was ultimately sourced from a post on Kabble.  The data contains a mix of numerical and categorical variables.  In the data set, you can find information on where coffee was sourced from geographically, different attributes related to things such as flavor, taste, aroma, and even what altitude the where coffee was grown."),
                               br(),
                               h3("The data set is rather large containing over 41 variables and approximately 1300 rows.  That being said, there is much to explore and learn about!  Of note, the dataset is a bit tricky to work with, based on that, the", strong("Total.Cup.Points"), "variable has been hard coded as the response variable for the modeling and some of the data exploration."),
                               br(),
                               #Tell the user the purpose of each tab (page) of the app
                               h3(strong("You will see three tabs to explore in the app:")),
                               br(),
                               h4(strong("Data Exploration Tab")," – This tab allows the user to create numerical and graphical summaries of all the variables in the data set."),
                               br(),
                               h4(strong("Modeling Tab")," – This tab allows the user to learn more about the models the app will create.  Additionally, the user can select the variables for the models, addition parameters, and see predictions based on the test data."),
                               br(),
                               h4(strong("Data Tab"), " – This tab allows the user to view the dataset in a table, select rows and columns, and generate a csv file to download."),
                               br(),
                               #Include a picture related to the data (for instance, if the data was about the world wildlife fund, you might include a picture of their logo)
                               img(src = "CQI.png", height = 140, width = 400)
                               ,
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
                                               "Density Plot")),
                             ),
                                    
                             mainPanel(
                                       h4("Numeric Summary of Selected Variable"),
                                       verbatimTextOutput("sum"),
                                       br(),
                                       h4("Graphical Summary of Selected Variable"),
                                       plotOutput("plot"),
                                       conditionalPanel(condition = "input.plot == 'Variable Correlation'",
                                                        h4("This is the variable correlation with the repsonse variable total.cup.points"),
                                                        verbatimTextOutput("corr")
                                       )
                             )
                                                      ),
##########################################################Modeling Tab
                    tabPanel("Modeling",
                             
                             mainPanel(
                                tabsetPanel(
                                  #Tab for Modeling Information
                                  tabPanel("Modeling Informaion",
                                           h3(strong("There are three modeling approaches that are used in the app:")),
                                           br(),
                                           h4("1) Linear Regression Model"),
                                           h4("2) Classification Tree"),
                                           h4("3) Random Forest"),
                                           br(),
                                           #the benefits of each, and the drawbacks of each. 
                                           h3(strong("Each of these models has their respective pros and cons:")),
                                           br(),
                                           h4(strong("Multiple Linear Regression"), "models are relatively easy to implement.  The underlying theory is not too complex, and it is not demanding in terms of computational power required to run the model.  Some drawbacks for models of this type are in the real world it is unlikely to have perfectly linearly separable datasets.  The models can also be prone to underfitting real world scenarios and may underperform compared to machine learning or more advanced algorithms."),
                                           withMathJax(),
                                           uiOutput('ex1'),
                                           br(),
                                           h4(strong("Classification Tree"), "models allow for interpretability.  There is no need for feature scaling, and these models work on both linear and non-linear problems.  The drawbacks on these models are they can be prone to poor results if the underlying dataset is small.  Additionally, overfitting can easily occur.  One element of classification trees are the Gini impurity which is a measure of how often a randomly chosen element form the set would be incorrectly labeled if it was randomly labeled according to the distribution labels in the subset.  The Gini impurity can be computed by the equation below"),
                                           withMathJax(),
                                           uiOutput('ex2'),
                                           br(),
                                           h4(strong("Random Forest"), "models are powerful and accurate.  They generally have good performance on many types of problems to include non-linear ones.  Challenges with models of this type are there is little to no interpretability.  The model can easily overfit and the number of trees must be chosen manually.  One element of random forests is bagging.  After training, predictions for unseens samples x' can be made be averaging the predictions for all the individual regression trees on x' as noted by the equation below."),
                                           withMathJax(),
                                           uiOutput('ex3'),
                                           br(),
                                           
                                  ),
                                  #Tab Panel for model fitting
                                  tabPanel("Model Fitting",
                                           sidebarPanel(
                                           #slider inputs for choosing training / test data
                                            h4("Select the size of the trianing and test set"),
                                            sliderInput(inputId = "train", label = "Select Train Data Set", min = 0, max = 1, value = 1 - 0.1, step = 0.1),
                                             sliderInput(inputId = "test", label = "Select Test Data Set", min = 0, max = 1, value = 0.1, step = 0.1),
                                           #Variable selection numeric
                                           varSelectInput("varseln", "Select Variables for Models", select_if(model_data, is.numeric), selected = NULL, multiple = TRUE,
                                                          selectize = TRUE, width = NULL, size = NULL),
                                           #cross validation select
                                           sliderInput("cv", "Select Level of Cross Validation", min = 0, max = 10, value = 5, step = 1),
                                           #repeat selection
                                           sliderInput("rpt", "Select Number of Repeats", min = 0, max = 10, value = 3, step = 1),
                                           #mtry range for Random
                                           sliderInput("mtry", "Select mtry range for Random Forest Model", min = 1, max = 20, value = c(1,7), step = 1, dragRange = TRUE),
                                           
                                           actionButton("go", "Fit Models")
                                ),
                                #mainpanel for model fitting tab
                              mainPanel(
                                fluidPage(
                                  fluidRow(
                                    box(width=8,
                                          h5("Random Forest Plot"),
                                         plotOutput("rf"),
                                         ),
                                    
                                    box(width=4,
                                        h5("Random Forest Summary"),
                                         verbatimTextOutput("rfp"),
                                         ),
                                  ),
                                  
                                  fluidRow(
                                    box(width=6,
                                         h5("Linear Regression Model Summary"),
                                         verbatimTextOutput("lm"),
                                         ),
                                    box(width=6,
                                         h5("Classification Tree Summary"),
                                         verbatimTextOutput("clt")
                                         )
                                  ),
                                  fluidRow(
                                    h5("Test Results"),
                                    column(4,
                                           h6("Linear Model Test Results"),
                                           verbatimTextOutput("lmt"),
                                           ),
                                    column(4,
                                           h6("Classification Tree Test Results"),
                                           verbatimTextOutput("ctt"),
                                           ),
                                    column(4,
                                           h6("Random Forest Test Results"),
                                           verbatimTextOutput("rft"),
                                           )
                                    )
                                )
                                  
                              )
                              ),
                                  
                                
                                  #Tab Panel for prediction
                                  tabPanel("Prediction",
                                           selectInput("predm", "Select Model to Predict Based on Variable Selections from Model Fitting Tab:",
                                                       c("Linear Regression Model",
                                                         "Classification Tree",
                                                         "Random Forest")),
                                  h5("Model Prediction Plot"),
                                  plotOutput("modp"),
                                  h5 ("Mean Abosolute Error for Test Set"),
                                  verbatimTextOutput("mae"),
                                  
                                
                               
                             )
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
                              DT::DTOutput("datat")
                             )

                    )
                    )
)
)


                    
                    
                
                


