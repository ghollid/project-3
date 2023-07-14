library(shiny)
library(rmarkdown)
library(tidyverse)
library(ggplot2)
library(caret)
library(DT)

setwd("C:/Users/Sarah/Documents/project-3")
# read in dataset
GradData <- read.csv(file="data.csv",sep=';')

# Define UI 
fluidPage(
  
  
  tabsetPanel(
    tabPanel("About",
             h2("Purpose"),
             "The purpose of this apps is to produce exploratory data analysis on a dataset containing information on the graduation status of students at a higher education institution.",
             "We hope to explore what factors may influence a students' retention and eventual graduation from a University.  We hope to produce a model that will predict graduation or ",
             "dropout status.",
             br(),
             br(),
             imageOutput("image1"),
             br(),
             br(),br(),
             h2("Data and Source"),
             "The data contains 4,424 observations with demographic and other information including the target variable - graduate or dropout status.",
             "This data was taken from the UCI Machine Learning Repostory.",
             h2("Tab Purposes"),
             h4("Data Exploration"),
             "The Data Exploration panel contains numerical and graphical summaries intended to provide a basic first look on the dataset.",
             "It includes ___ and ____. Insert whatever you end up creating for the data exploration panel here",
             h4("Modeling"),
             "The Modeling panel contains the production of alinear regression model, classification tree, and a random forest model.  It contains subpanels, one for Modeling Info that",
             "contains information on each modeling approach and their benefits and drawbacks.  The next subpanel is the Model Fitting panel, where the user",
             "can select settings for each model to be fit, as well as decide how much data to train/test on.  The user can select what variables each model contains and summaries and fit",
             "statistics will be reported and results compared to the test set.",
             ''
             ),
             
    tabPanel("Data Exploration",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("summary","Plot or Summary Reported",
                              choices=c("Boxplots","Contingency Table","Scatterplot")),
                 conditionalPanel(condition="input.summary=='Boxplots'",
                    radioButtons('vars',"Select One Variable",
                      choiceNames=c("Unemployment Rate","Inflation Rate","Age at Enrollment","Admission Grade","Previous Qualification Grade","GDP"),
                      choiceValues=c("Unemployment.rate","Inflation.rate","Age.at.enrollment","Admission.grade","Previous.qualification..grade.","GDP"))),
                 conditionalPanel(condition="input.summary=='Contingency Table'",
                    radioButtons('vars2',"Select One Variable",
                      choiceNames=c("Marital Status","Application Order","Daytime/Evening Attendance","International?","Educational Special Needs","Tuition and Fees Up to Date?","Gender","Scholarship holder?"),
                      choiceValues=c("Marital.status","Application.order","Daytime.evening.attendance.","International","Educational.special.needs","Tuition.fees.up.to.date","Gender","Scholarship.holder")))
           
              ),
             mainPanel(conditionalPanel(condition="input.summary=='Boxplots'",plotOutput("plot")),
                       conditionalPanel(condition="input.summary=='Contingency Table'",tableOutput("table"))
                       ))),
    tabPanel("Modeling",checkboxInput("modeling","Click Here for Additional Panels"),"You will fit three supervised learning models. Depending on your response you’ll
             fit a multiple linear regression or generalized linear regression model, regression or classification
             tree, and a random forest model. This page should have three tabs to it."),
    conditionalPanel(condition="input.modeling==1",
      tabPanel("Modeling Info","You should explain these three modeling approaches, the benefits of each,
                  and the drawbacks of each. You should include some type of math type in the explanation
                    (you’ll need to include mathJax)."
      ) ,
       tabPanel("Model Fitting","You’ll split your data into a training and test set. Give the user the ability to choose the
                        proportion of data used in each.
                      · The user should have functionality for choosing model settings for each model. For all
                        models, they should be able to select the variables used. Cross validation should be used
                        for selecting models on the training set where appropriate.
                      · When the user is ready they should be able to press a button and fit all three models on
                        the training data.
                      · Fit statistics (such as RMSE) on the training data should be reported for each model
                      along with appropriate summaries about the model (for instance, summary() run on your
                      lm() or glm() fit, a plot showing the variable importance from the random forest model,
                      etc.).
                      · The models should be compared on the test set and appropriate fit statistics reported"),
    tabPanel("Prediction","You should give the user a way to use one of the models for prediction. That
                        is, they should be able to select the values of the predictors and obtain a prediction for the
                        response.")
            ,
    
    tabPanel("Data","The user should be able to
               ∗ Scroll through the data set
               ∗ Subset this data set (rows and columns)
               ∗ Save the (possibly subsetted) data as a file (.csv is fine but whatever you’d like)")
             )
)
)

    


