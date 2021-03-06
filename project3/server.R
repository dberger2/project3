#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(caret)
library(readr)
library(dplyr)
library(purrr)
library(DT)
library(randomForest)

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


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # code for plots associated with radio buttons - Ui input id = plot
    output$plot <- renderPlot({
        
        #code for barchart selection
        if(input$plot == "Barchart"){
            ggplot(data = c, aes_string(x = input$var)) + geom_bar(aes_string(fill = input$var), position = "dodge")+ scale_fill_discrete(name = "")+ guides(x= guide_axis(angle=input$wordangle)) 
        }
        
        #code for Variable Correlation
        else if(input$plot == "Variable Correlation"){
            ggplot(data = c, aes_string(y = "Total.Cup.Points", x = input$var)) + geom_point(aes(color = Total.Cup.Points), position = "jitter") + geom_smooth(formula = y ~ x, method = "loess") + guides(x= guide_axis(angle=input$wordangle))
        }
        
        #code for density plot
        else if(input$plot == "Density Plot"){
        ggplot(c, aes_string(x=input$var)) + 
            geom_density(color="darkblue", fill="lightblue") +
                guides(x= guide_axis(angle=input$wordangle))
        }
        
       #code for boxplot
        else if(input$plot == "Box Plot"){
            ggplot(c, aes_string(x=input$var, y="Total.Cup.Points", fill=input$var)) + 
                geom_boxplot(alpha=0.3) +
                theme(legend.position="none") +
                coord_flip(ylim = c(67, 90))
        }
    })
        
        
    # Code for stat summary based on variable selected
    output$sum <- renderPrint({
        
        summary(c[,input$var])
        
        })
    
    #code to add correlation value for Variable Correlation Plot
    output$corr <- renderPrint({
        
        cor(x=c$Total.Cup.Points, y=c[,as.character(input$var)])
        
    })
    
########################### Modeling Tab

##########################Model Fitting Tab
    
### Modeling Info Tab
    
    output$ex1 <- renderUI({
      withMathJax(
        helpText('Multiple Linear Regression Model Equation $$y_{i}=\\beta_{0}+\\beta_{1}{x}_{i1}+\\cdots+\\beta_{p}x_{ip}+\\varepsilon_{i}$$'))
    })
    output$ex2 <- renderUI({
      withMathJax(
        helpText('Gini Impurity for Classification Tree $$\\sum_{k \\neq i} p_{k}=1-p_{i}$$')
      )
    })
    
    output$ex3 <- renderUI({
      withMathJax(
        helpText('Bagging $$\\hat {f} = \\frac {1}{B}\\sum_{b=1}^{B} f_{b}(x^`)$$'))
    })
    
# Test / Train Data slider inputs
    
    # when test changes, update train
    observeEvent(input$test,  {
        updateSliderInput(session = session, inputId = "train", value = 1 - input$test)
    })
    
    # when train changes, update test
    observeEvent(input$train,  {
        updateSliderInput(session = session, inputId = "test", value = 1 - input$train)
    })
    
# Create Data Partition for all models based on slider inputs
    
    ctrainindex <- eventReactive(input$go, {
        createDataPartition(model_data$Total.Cup.Points, p = input$train, list = FALSE)
    })
    
    ctrain <- reactive({model_data[ctrainindex(), ]})
    
    
    ctest <- reactive({model_data[-ctrainindex(), ]})
    
    
# Linear Regression Model 
    
        cFit1 <- eventReactive(input$go, {
    train(as.formula(paste("Total.Cup.Points~",paste(c(input$varseln), collapse="+"))), data = ctrain(), method = "lm",
                  trControl = trainControl(method = "cv", number = input$cv),
                   na.action = na.pass,
                   preProcess = c("center", "scale"))
 
})

        
   #Linear Regression Model Summary 
    output$lm <- renderPrint({
    (cFit1())
    })
    

# Classification Tree
    
    ctree <- eventReactive(input$go, {
        train(as.formula(paste("Total.Cup.Points~",paste(c(input$varseln), collapse="+"))), data = ctrain(),
                 method = "rpart",
                 preProcess = c("center", "scale"),
                 trControl = trainControl(method = "repeatedcv", repeats = input$rpt,
                                          number = input$cv),
                 na.action = na.roughfix,
                 tuneGrid = expand.grid(cp = (.interaction.depth = seq(0, .1, by = .001))))
    })
    
# Classification Tree Summary      
    output$clt <- renderPrint({
        (ctree())
    })
    
#Random Forest Model

    rfFit <- eventReactive(input$go, {
        train(as.formula(paste("Total.Cup.Points~",paste(c(input$varseln), collapse="+"))), data = ctrain(), method = "rf",
                   preProcess = c("center", "scale"),
                   trControl = trainControl(method = "cv", number = input$cv),
                   tuneGrid = expand.grid(mtry = (input$mtry[1]):(input$mtry[2])))
      
      })
    
    
    #Random Forest Summary
    output$rfp <- renderPrint({
        (rfFit())
    })
    
    #Random Forest Model Plot
    output$rf <- renderPlot({
      ggplot(rfFit())
    })
    
    #Test results
    output$lmt <- renderPrint({
      lm1Results()
    })
    
    output$ctt <- renderPrint({
      ctreeResults()
    })
    
    output$rft <- renderPrint({
      rfResults()
    })
      

#test results for linear regression model
    
    lmpred <- reactive({predict(cFit1(), newdata = ctest())})
    lm1Results <- reactive({postResample(lmpred(), obs = ctest()$Total.Cup.Points)})

#test results for classification tree model
    
    ctPred <- reactive({predict(ctree(), newdata = ctest())})
    ctreeResults <- reactive({postResample(ctPred(), obs = ctest()$Total.Cup.Points)})

#test results for Random Forest Model
    
    rfPred <- reactive({predict(rfFit(), newdata = ctest())})
    rfResults <- reactive({postResample(rfPred(), obs = ctest()$Total.Cup.Points)})
    
    
#################################Prediction Tab
#plots for each model    
    output$modp <- renderPlot({
      
      #plot for linear regression model
      if(input$predm == "Linear Regression Model"){
      
      ggplot(ctest(), aes(lmpred(), ctest()$Total.Cup.Points)) + geom_point(colour = "tan3") + 
      geom_abline(slope = 1, intercept = 0) +
      theme_bw() + ggtitle("Test Set Predictions for Total.Cup.Points", 
                           subtitle = "Final Linear Regression Model") + 
      labs(x = "Prediction", y = "Actual")
        
      }
      #plot for classification tree
      else if(input$predm == "Classification Tree"){
        ggplot(ctest(), aes(ctPred(), ctest()$Total.Cup.Points)) + geom_point(colour = "tan3") + 
          geom_abline(slope = 1, intercept = 0) +
          theme_bw() + ggtitle("Test Set Predictions for Total.Cup.Points", 
                               subtitle = "Final Classification Tree Model") + 
          labs(x = "Prediction", y = "Actual")
        
      }
      #plot for random forest
      else if(input$predm == "Random Forest"){
        ggplot(ctest(), aes(rfPred(), ctest()$Total.Cup.Points)) + geom_point(colour = "tan3") + 
          geom_abline(slope = 1, intercept = 0) +
          theme_bw() + ggtitle("Test Set Predictions for Total.Cup.Points", 
                               subtitle = "Final Random Forest Model") + 
          labs(x = "Prediction", y = "Actual")
        
      }
      
    })
  #MAE for each model plot  
  output$mae <- renderPrint({
    
    #MAE for linear regression model
    if(input$predm == "Linear Regression Model"){
      
      MAE(ctest()$Total.Cup.Points, lmpred())
      
    }
    #MAE for classification tree
    else if(input$predm == "Classification Tree"){
      
      MAE(ctest()$Total.Cup.Points, ctPred())
      
    }
    #MAE for Random forest
    else if(input$predm == "Random Forest"){
      
      MAE(ctest()$Total.Cup.Points, rfPred())
      
    }

  })
    
################################Data Tab
    
    #reactive expression for the data
    d <- reactive(c)
 
    # choose columns to display

    d2 <- reactive(d()[sample(nrow(d()), 1000), ])
    
    #rendered datatable
    output$datat <- DT::renderDataTable({
        DT::datatable(d2()[, input$show_vars, drop = FALSE], filter = "top")
    })
    
    
    #Downloadable csv of selected dataset
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("Coffee Data", ".csv", sep="")
        },
        
        content = function(file) {
            write.csv(d2()[ ,input$show_vars, drop = FALSE], file, row.names = FALSE)
        }
    )
    
   
    
    
}

    
    
    
    
    
