#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fixedPage(
  fixedRow(
    column(8,
           titlePanel("Next Word Prediction"),
           
           wellPanel(
             textInput("text_input", "Write your input below", width = "1000px")
           ),
           
           wellPanel(
             selectInput("prediction", "Predictions from most confident to least *", "", multiple=TRUE, selectize=FALSE),
             "* You can click on a prediction to use it"
           ),
           
           wellPanel(
             a(href = "https://github.com/refik/capstone", "Source code on Github"), " - Refik Türkeli"
           )
    )
  )
))
