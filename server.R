#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(caret)
library(randomForest)

#
# Defines the Random Forest model and predictor for 'mpg' in the 'mtcars' dataset.
#
#
# Setting up for Random Forest predictor.
#

# Initializing data. 'mtcars' dataset is used. 
data("mtcars")

# To show structure in UI
dataStructure <- capture.output(str(mtcars))

# Setting up the random generator seed.
set.seed(210915) # my son's birthday :-)

# Defining custom training controls with cross validation.
customTrainControl <- trainControl(method = "cv", number = 10)

# Building Random Forest model function. 
# in order to regenerate the model when the user change parameters in the UI.
# The goal of this model is to predict 'mpg' (miles per gallon) using the rest
# of the variables.
carsRandomForestModelBuilder <- function() {
  
  return(
    train(
      mpg ~ ., 
      data = mtcars,
      method = "rf",
      trControl = customTrainControl
    )
  )
  
}

# Predictor function.  It will be invoked 'reactively'.
randomForestPredictor <- function(model, parameters) {
  
  prediction <- predict(
    model,
    newdata = parameters
  )
  
  return(prediction)
  
}

#
# Setting up Shiny Server
#
shinyServer(
  
  function(input, output, session) {
    
    # To show new lines in the browser
    decoratedDataStructure <- paste0(dataStructure, collapse = "<br/>")
    output$dataStructure <- renderText({decoratedDataStructure})
    
    # Builds "reactively" the prediction.
    predictMpg <- reactive({
      
      carToPredict <- data.frame(
        cyl = input$cyl, 
        disp = input$disp, 
        hp = input$hp, 
        drat = input$drat, 
        wt = input$wt, 
        qsec = input$qsec, 
        vs = as.numeric(input$vs), 
        am = as.numeric(input$am), 
        gear = input$gear, 
        carb = input$carb)
      
      randomForestPredictor(carsRandomForestModelBuilder(), carToPredict)
      
    })
    
    output$prediction <- renderTable({
      predictMpg()
    })
    
  }
  
)
