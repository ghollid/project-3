library(shiny)
library(rmarkdown)
library(tidyverse)
library(ggplot2)
library(caret)
library(DT)

setwd("C:/Users/Sarah/Documents/project-3")
# read in dataset
GradData <- read.csv(file="data.csv",sep=';')
# creating a version of this data with variables that make sense
GradDataV2 <- GradData
for(i in 1:nrow(GradData)){
  # marital.status
  if(GradData[i,1]==1){
    GradDataV2[i,1]<-"Single"
  }
  else if(GradData[i,1]==2){
    GradDataV2[i,1] <- "Married"
  }
  else if(GradData[i,1]==3){
    GradDataV2[i,1] <- "Widower"
  }
  else if(GradData[i,1]==4){
    GradDataV2[i,1] <- "Divorced"
  }
  else if(GradData[i,1]==5){
    GradDataV2[i,1] <- "Facto Union"
  }
  else if(GradData[i,1]==6){
    GradDataV2[i,1] <- "Legally Separated"
  }
  # Daytime/Evening Attendance
  if(GradData[i,5]==1){
    GradDataV2[i,5]<- "Daytime"
  }
  else{GradDataV2[i,5] <- "Evening"}
  # International
  if(GradData[i,21]==1){
    GradDataV2[i,21] <- 'Yes'
  }
  else{GradDataV2[i,21]<- 'No'}
  # Educational Special Needs
  if(GradData[i,15]==1){
    GradDataV2[i,15] <- 'Yes'
  }
  else{GradDataV2[i,15] <- 'No'}
  # Tuition and Fees up to date
  if(GradData[i,17]==1){
    GradDataV2[i,17] <- 'Yes'
  }
  else{GradDataV2[i,17] <- 'No'}
  # Gender
  if(GradData[i,18]==1){
    GradDataV2[i,18] <- 'Male'
  }
  else{GradDataV2[i,18] <- 'Female'}
  # Scholarship holder
  if(GradData[i,19]==1){
    GradDataV2[i,19]<- 'Yes'
  }
  else{GradDataV2[i,19]<-'No'}
}

## removing enrolled students
GradDataV2 <- subset(GradDataV2, Target!='Enrolled')

# Define UI 
fluidPage(
  
  
  tabsetPanel(
    tabPanel("About",
             h2("Purpose"),
             "The purpose of this apps is to produce exploratory data analysis on a dataset containing information on the graduation status of students at a higher education institution.",
             "We hope to explore what factors may influence a student's retention and eventual graduation from a University.  The user is able to produce three models that will predict graduation or ",
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
             "It includes box plots, contingency tables, and mosaic plots, and allows the user to select their desired variable to explore.",
             h4("Modeling"),
             "The Modeling panel contains the production of a generalized linear regression model, classification tree, and a random forest model.  It contains subpanels, one for Modeling Info that",
             "contains information on each modeling approach and their benefits and drawbacks.  The next subpanel is the Model Fitting panel, where the user",
             "can select settings for each model to be fit, as well as decide how much data to train/test on.  The user can select what variables each model contains and summaries and fit",
             "statistics will be reported and results compared to the test set.  The final subpanel is the Prediction subpanel which allows the users to select a previously created model and use that model for prediction.",
             '',
             h4('Data'),
             "The Data panel allows the user to view, subset, and save the dataset used in creation of the models.",
             br(),
             br()
             
             ),
             
    tabPanel("Data Exploration",
             h2("Data Exploration"),
             sidebarLayout(
               sidebarPanel(
                 radioButtons("summary","Plot or Summary Reported",
                              choices=c("Boxplots","Contingency Tables","Mosaic Plots")),
                 conditionalPanel(condition="input.summary=='Boxplots'",
                    radioButtons('vars',"Select One Variable",
                      choiceNames=c("Unemployment Rate","Inflation Rate","Age at Enrollment","Admission Grade","Previous Qualification Grade","GDP"),
                      choiceValues=c("Unemployment.rate","Inflation.rate","Age.at.enrollment","Admission.grade","Previous.qualification..grade.","GDP"))),
                 conditionalPanel(condition="input.summary=='Contingency Tables'",
                    radioButtons('vars2',"Select One Variable",
                      choiceNames=c("Marital Status","Application Order (0 = First Choice, 9 = Last Choice)","Daytime or Evening Attendance","International?","Educational Special Needs","Tuition and Fees Up to Date?","Gender","Scholarship holder?"),
                      choiceValues=c("Marital.status","Application.order","Daytime.evening.attendance.","International","Educational.special.needs","Tuition.fees.up.to.date","Gender","Scholarship.holder"))),
                 conditionalPanel(condition="input.summary=='Mosaic Plots'",
                      radioButtons('vars3',"Select One Variable",
                      choiceNames=c("Daytime or Evening Attendance","International?","Educational Special Needs","Tuition and Fees Up to Date?","Gender","Scholarship holder?"),
                      choiceValues=c("Daytime.evening.attendance.","International","Educational.special.needs","Tuition.fees.up.to.date","Gender","Scholarship.holder"))),
                 ),
             mainPanel(conditionalPanel(condition="input.summary=='Boxplots'",plotOutput("plot")),
                       conditionalPanel(condition="input.summary=='Contingency Tables'",tableOutput("table")),
                       conditionalPanel(condition="input.summary=='Mosaic Plots'",plotOutput("plot2"))

                       ))),
    tabPanel("Modeling",
             h2("Modeling"),
          h4("This panel allows for the exploration of three supervised learning models."),
          "Select the model info button to read about each modeling approach, their benefits, and drawbacks.",
          br(),
          "Select the model fitting button to fit a generalized linear regression model, a classification tree, and a random forest.",
          br(),
          "Select the prediction button to use one of the models for prediction.",
          br(),
          br(),
        radioButtons("modeling",'Select an Option',choices=c("Modeling Info","Model Fitting","Prediction")),
      conditionalPanel(condition="input.modeling=='Modeling Info'",
        sidebarLayout(
          sidebarPanel(h3("Modeling Info"),
                       radioButtons("mod","Select a Model",choices=c("Generalized Linear Regression","Classification Tree","Random Forest"))),
              mainPanel(
                conditionalPanel(condition="input.mod=='Generalized Linear Regression'",
                  h3("Generalized Linear Regression"),
                  "Generalized Linear Models are a class of regression models very similar to normal linear regression models.  However, they are a parametric modeling technique that",
                  "makes assumptions about the distribution of the data.  They allow for the estimation of non-linear relationships.  They do this by assuming a different distribution than",
                  "normal for the data.  For example, to explore binary classification data (such as the data explored in this project), they would assume a Bernoulli distribution (logistic regression).",
                  "For Real-Valued data, they would assume a Gaussian distribution.  Additionally, for Count-data, they would assume a Poisson distribution.",
                  "We will go into more detail on logistic regression and the estimation of classification data.",
                  br(),
                  br(),
                  "In logistic regression, the following equations are used, rather than the typical",
                  br(),
                  "Y = Beta_0 + Beta_1X1 + Beta_2X2...etc.",
                  br(),
                  br(),
                   "Logit(pi) = 1/(1+ exp(-pi))",
                  br(),
                  "ln(pi/(1-pi)) = Beta_0 + Beta_1*X_1 + … + B_k*X_k",
                  br(),
                  br(),
                  "In these equations, Logit(pi) is the response variable and each x represents independent variables.  The beta parameters are most commonly estimated using maximum likelihood estimation,",
                  "testing different values to pick the best fit.  These many iterations create the log likelihood function and logistic regression maximizes this to find the 'best' estimate of the parameters.",
                  "After determining the best esimate, conditional probabilities are calculated for each observation.  The log of these values is taken and summed together to find the predicted probability.",
                  "In binary classification (such as the classification in this app), a probability below 0.5 will predict '0' and above 0.5 will predict '1'.",
                  br(),
                  h4('Advantages and Disadvantages'),
                  'The generalized linear model has many advantages, some of which are listed below: ',
                  br(),
                  '1. Generalized Linear Models allow for a wide range of potential relationships between',
                  "   response and predictor variables (Gaussian, Binomial, Poisson).",
                  br(),
                  "2. Generalized Linear Models are often robust to outliers as they allow for non-normal",
                  "   distributions.",
                  br(),
                  "3. Generalized Linear Models can be compared using various fit criteria, including AIC,",
                  "   BIC, or RMSE.",
                  br(),
                  br(),
                  "As with any modeling technique, depending on what the endgoal of the analysis is, there",
                  "may be disadvantages, such as those listed below: ",
                  br(),
                  "1. You must be careful about the assumptions your choice of distribution implies about the",
                  "   response variable, as they may not always hold.",
                  br(),
                  "2. Choosing the correct distribution can be difficult, and an incorrect selection may result",
                  "   in incorrect predictions.",
                  br(),
                  "3. Generalized Linear Models may be prone to overfitting if a model has too many predictor",
                  "   variables.",
                br(),
                br()
                  ),
                conditionalPanel(condition="input.mod=='Classification Tree'","cstree",
                                 h3("Generalized Linear Regression"),
                                 "Generalized Linear Models are a class of regression models very similar to normal linear regression models.  However, they are a parametric modeling technique that",
                                 "makes assumptions about the distribution of the data.  They allow for the estimation of non-linear relationships.  They do this by assuming a different distribution than",
                                 "normal for the data.  For example, to explore binary classification data (such as the data explored in this project), they would assume a Bernoulli distribution (logistic regression).",
                                 "For Real-Valued data, they would assume a Gaussian distribution.  Additionally, for Count-data, they would assume a Poisson distribution.",
                                 "We will go into more detail on logistic regression and the estimation of classification data.",
                                 br(),
                                 br(),
                                 "In logistic regression, the following equations are used, rather than the typical",
                                 br(),
                                 "Y = Beta_0 + Beta_1X1 + Beta_2X2...etc.",
                                 br(),
                                 br(),
                                 "Logit(pi) = 1/(1+ exp(-pi))",
                                 br(),
                                 "ln(pi/(1-pi)) = Beta_0 + Beta_1*X_1 + … + B_k*X_k",
                                 br(),
                                 br(),
                                 "In these equations, Logit(pi) is the response variable and each x represents independent variables.  The beta parameters are most commonly estimated using maximum likelihood estimation,",
                                 "testing different values to pick the best fit.  These many iterations create the log likelihood function and logistic regression maximizes this to find the 'best' estimate of the parameters.",
                                 "After determining the best esimate, conditional probabilities are calculated for each observation.  The log of these values is taken and summed together to find the predicted probability.",
                                 "In binary classification (such as the classification in this app), a probability below 0.5 will predict '0' and above 0.5 will predict '1'.",
                                 br(),
                                 h4('Advantages and Disadvantages'),
                                 'The generalized linear model has many advantages, some of which are listed below: ',
                                 br(),
                                 '1. Generalized Linear Models allow for a wide range of potential relationships between',
                                 "   response and predictor variables (Gaussian, Binomial, Poisson).",
                                 br(),
                                 "2. Generalized Linear Models are often robust to outliers as they allow for non-normal",
                                 "   distributions.",
                                 br(),
                                 "3. Generalized Linear Models can be compared using various fit criteria, including AIC,",
                                 "   BIC, or RMSE.",
                                 br(),
                                 br(),
                                 "As with any modeling technique, depending on what the endgoal of the analysis is, there",
                                 "may be disadvantages, such as those listed below: ",
                                 br(),
                                 "1. You must be careful about the assumptions your choice of distribution implies about the",
                                 "   response variable, as they may not always hold.",
                                 br(),
                                 "2. Choosing the correct distribution can be difficult, and an incorrect selection may result",
                                 "   in incorrect predictions.",
                                 br(),
                                 "3. Generalized Linear Models may be prone to overfitting if a model has too many predictor",
                                 "   variables.",
                                 br(),
                                 br())
                     ,
                conditionalPanel(condition="input.mod=='Random Forest'","rf")
              )
      )) ,
       conditionalPanel(condition="input.modeling=='Model Fitting'",
            sidebarLayout(
              sidebarPanel(h4("Model Fitting"),
                           "Here, you may choose variables and fit each model type, returning fit statistics and summaries for each model",
                           numericInput("prop","Pick Proportion of Data for Training Set",value=.80,min=.01,max=.99),
                           checkboxGroupInput('varsm','Pick Variables for Models',
                          choiceNames=c("Marital Status","Application Mode","Application Order","Course","Daytime or Evening Attendance?","Previous Qualification","Previous Qualification Grade","Nationality","Mother's Qualification","Father's Qualification","Mother's Occupation","Father's Occupation","Admission Grade","Displaced?","Educational Special Needs?","Debtor?","Tuition and Fees Up to Date?","Gender","Scholarship Holder?","Age at Enrollment","International?","1st Semester Grades","2nd Semester Grades","Unemployment Rate","Inflation Rate","GDP"),
                          choiceValues=c("Marital.status","Application.mode","Application.order","Course","Daytime.evening.attendance.","Previous.qualification","Previous.qualification..grade.","Nacionality","Mother.s.qualification","Father.s.qualification","Mother.s.occupation","Father.s.occupation","Admission.grade","Displaced","Educational.special.needs","Debtor","Tuition.fees.up.to.date","Gender","Scholarship.holder","Age.at.enrollment","International","Curricular.units.1st.sem..grade.","Curricular.units.2nd.sem..grade.","Unemployment.rate","Inflation.rate","GDP"))
                          , actionButton('submit',"Submit Model")),
            mainPanel(h4("Generalized Linear Model Output"),
                      tableOutput("modfit1"),
                      h5("Results from Prediction with Test Set"),
                      tableOutput("modfit12"),
                      h4("Classification Tree Output"),
                      h5("Boosted Tree Fit"),
                      plotOutput("bfitplot"),
                      h5("Results from Prediction with Test Set"),
                      tableOutput("bfitresults"),
                      h4("Random Forest Output"),
                      plotOutput("rforestp"),
                      h5("Results from Prediction with Test Set"),
                      tableOutput("rforestt")
                      
                      ))),
    conditionalPanel(condition="input.modeling=='Prediction'",
            sidebarLayout(
              sidebarPanel("Prediction"),
            mainPanel("Prediction","You should give the user a way to use one of the models for prediction. That
                        is, they should be able to select the values of the predictors and obtain a prediction for the
                        response."))),
    )
            ,
    
    tabPanel("Data","The user should be able to
               ∗ Scroll through the data set
               ∗ Subset this data set (rows and columns)
               ∗ Save the (possibly subsetted) data as a file (.csv is fine but whatever you’d like)")
             
)
)



    


