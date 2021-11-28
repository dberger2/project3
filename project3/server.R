#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinythemes)
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

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # code for plots associated with radio buttons - Ui input id = plot
    output$plot <- renderPlot({
        
        #code for barchart selection
        if(input$plot == "Barchart"){
            ggplot(data = c, aes_string(x = input$var)) + geom_bar(aes_string(fill = as.factor(input$var)), position = "dodge") + labs(x = "Coffee" [input$var], y = "Count", title="Distribution of" [input$var]) + scale_fill_discrete(name = "")+ guides(x= guide_axis(angle=input$wordangle))
        }
        
        #code for Variable Correlation
        else if(input$plot == "Variable Correlation"){
            ggplot(data = c, aes_string(y = "Total.Cup.Points", x = input$var)) + geom_point(aes(color = Total.Cup.Points), position = "jitter") + geom_smooth(formula = y ~ x, method = "loess") + labs(x = "Flavor", y = "Total Cup Points", title="Correlation of Total Cup Points to Flavor") + guides(x= guide_axis(angle=input$wordangle))
        }
        
        #code for density plot
        else if(input$plot == "Density Plot"){
        ggplot(c, aes_string(x=input$var)) + 
            geom_density(color="darkblue", fill="lightblue") +
                guides(x= guide_axis(angle=input$wordangle))
        }
        
        #code for Histogram
        else if(input$plot == "Histogram"){
            ggplot(c, aes_string(x=input$var)) + 
                geom_histogram(bins = input$boxhist, color="darkgreen", fill="green", binwidth = 3) + 
                guides(x= guide_axis(angle=input$wordangle))
        }
        
        #code for boxplot
        else if(input$plot == "Box Plot"){
            ggplot(c, aes_string(x=input$var, y="Total.Cup.Points", fill=input$var)) + 
                geom_boxplot(alpha=0.3) +
                theme(legend.position="none") +
                coord_flip(ylim = c(67, 90))
        }
        
        #code for dynamic slider for boxplot and barchart UI id = boxhist
        #observeEvent(input$boxhist, {
        #    updateSliderInput(inputId = "n", min = input$var)
        #})  
        #observeEvent(input$max, {
         #   updateSliderInput(inputId = "n", max = input$var)
        #})
        
    })
        
        
    # Code for stat summary based on variable selected
    output$sum <- renderPrint({
        
        summary(c[,input$var])
        
        })
    
    #code to add correlation value for Variable Correlation Plot
    output$corr <- renderPrint({
        
        cor(x=c$Total.Cup.Points, y=c[,as.character(input$var)])
        
    })
    
    
    
}

    
    
    
    
    # code for digits
    #d <- input$digit
    #tab[ ,3] <- round(tab[ ,3], digits = d)

