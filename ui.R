
### UI module (UI.R)

library(shiny)

shinyUI( pageWithSidebar(
        # Application title
        headerPanel("Human Activity Recognition using Smartphone data "),
        
        sidebarPanel(
                selectInput("Model", "Choose Prediction Model:", choices = c('Decision Trees','Naive Bayes'
                                                                             , "Support Vector Machine (svm)"
                                                                             ,'Random Forest')),
                numericInput('testId', 'Choose the test case for prediction', 1, min = 1, max = 50, step = 1),
                submitButton('Submit')
                
        ),
        mainPanel(
                h3('Results of prediction'),
                h4('The test case chosen was'),
                verbatimTextOutput("inputValue"),
                h4('The prediction model chosen was'),
                verbatimTextOutput("dataModel"),
                h4('Which resulted in a prediction of '),
                verbatimTextOutput("prediction"),
                h4('The correct outcome is '),
                verbatimTextOutput("rightPrediction"),
                h4("Confusion Matrix for the Chosen Model"),
                verbatimTextOutput("ConfMatrix")
                , h4("Accuracy of Models on Training vs Test Data VS TestingSet Data"),
                 verbatimTextOutput("accuracy"),
                h4("Performance of Decision Trees on Test Data"),
                verbatimTextOutput("performancerpart"),
                h4("Performance of Naive Bayes on Test Data"),
                verbatimTextOutput("performancenaivebayes"),
                h4("Performance of Support Vector Machines on Test Data"),
                verbatimTextOutput("performancesvm"),
                h4("Performance of Random Forest on Test Data"),
                verbatimTextOutput("performancerf")
        ) 
        
) )






