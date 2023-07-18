library(shiny)
library(rmarkdown)
library(tidyverse)
library(ggplot2)
library(caret)
library(gbm)
library(randomForest)
library(DT)

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
  # displaced
  if(GradData[i,14]==1){
    GradDataV2[i,14] <- 'Yes'
  }
  else{GradDataV2[i,14] <- 'No'}
  # debtor
  if(GradData[i,16]==1){
    GradDataV2[i,16] <- 'Yes'
  }
  else{GradDataV2[i,16]<-'No'}
}

## removing enrolled students
GradDataV2 <- subset(GradDataV2, Target!='Enrolled')

## 'class' variables as factors and not numeric
GradDataV2$Course <- as.factor(GradDataV2$Course)
GradDataV2$Application.mode <- as.factor(GradDataV2$Application.mode)
GradDataV2$Application.order <- as.factor(GradDataV2$Application.order)
GradDataV2$Previous.qualification <- as.factor(GradDataV2$Previous.qualification)
GradDataV2$Nacionality<- as.factor(GradDataV2$Nacionality)
GradDataV2$Mother.s.qualification <- as.factor(GradDataV2$Mother.s.qualification)
GradDataV2$Mother.s.occupation <- as.factor(GradDataV2$Mother.s.occupation)
GradDataV2$Father.s.qualification <- as.factor(GradDataV2$Father.s.qualification)
GradDataV2$Father.s.occupation<- as.factor(GradDataV2$Father.s.occupation)
GradDataV2$Displaced <- as.factor(GradDataV2$Displaced)
GradDataV2$Debtor <- as.factor(GradDataV2$Debtor)

## numeric variables as numeric and not character
GradDataV2$Previous.qualification..grade. <- as.numeric(GradDataV2$Previous.qualification..grade.)
GradDataV2$Admission.grade <- as.numeric(GradDataV2$Admission.grade)
GradDataV2$Age.at.enrollment <- as.numeric(GradDataV2$Age.at.enrollment)
GradDataV2$Curricular.units.1st.sem..credited. <- as.numeric(GradDataV2$Curricular.units.1st.sem..credited.)
GradDataV2$Curricular.units.1st.sem..enrolled. <- as.numeric(GradDataV2$Curricular.units.1st.sem..enrolled.)
GradDataV2$Curricular.units.1st.sem..evaluations.<- as.numeric(GradDataV2$Curricular.units.1st.sem..evaluations.)
GradDataV2$Curricular.units.1st.sem..grade.<- as.numeric(GradDataV2$Curricular.units.1st.sem..grade.)
GradDataV2$Curricular.units.1st.sem..without.evaluations. <- as.numeric(GradDataV2$Curricular.units.1st.sem..without.evaluations.)
GradDataV2$Curricular.units.2nd.sem..credited.<- as.numeric(GradDataV2$Curricular.units.2nd.sem..credited.)
GradDataV2$Curricular.units.2nd.sem..enrolled. <- as.numeric(GradDataV2$Curricular.units.2nd.sem..enrolled.)
GradDataV2$Curricular.units.2nd.sem..evaluations.<- as.numeric(GradDataV2$Curricular.units.2nd.sem..evaluations.)
GradDataV2$Curricular.units.2nd.sem..approved. <- as.numeric(GradDataV2$Curricular.units.2nd.sem..approved.)
GradDataV2$Curricular.units.2nd.sem..grade. <- as.numeric(GradDataV2$Curricular.units.2nd.sem..grade.)
GradDataV2$Curricular.units.2nd.sem..without.evaluations. <- as.numeric(GradDataV2$Curricular.units.2nd.sem..without.evaluations.)
GradDataV2$Unemployment.rate <- as.numeric(GradDataV2$Unemployment.rate)
GradDataV2$Inflation.rate <- as.numeric(GradDataV2$Inflation.rate)
GradDataV2$GDP <- as.numeric(GradDataV2$GDP)

## making lists for UI
mode <- list('1','2','5','7','10','15','16','17','18','26','27','39','42','43','44','51','53','57')
names(mode) <- c('1st Phase General Contingent',"Ordinance No. 612/94",'1st Phase - Special Contingent (Azores Island)','Holders of other  higher courses','Ordinance No. 854-B/99','International Student (bachelor)','1st phase - Special Continent (Madeira Island)','2nd Phase - General Contingent','3rd Phase - General Contingent','Ordinance No. 533-A/99, item b2 (Different Plan)','Ordinance No. 533-A/99, item b3 (Other institution)','Over 23 years old','Transfer','Change of course','Technological Specialization Diploma Holders','Change of Institution/Course','Short Cycle Diploma Holders','Change of Institution/Course (International)')

class <- list('33','171','8014','9003','9070','9085','9119','9130','9147','9238','9254','9500','9556','9670','9773','9853','9991')
names(class) <- c("Biofuel Production Technologies","Animation and Multimedia Design","Social Service (evening attendance)","Agronomy","Communication Design","Verterinary Nursing","Informatics Engineering","Equiniculture","Management","Social Service","Tourism","Nursing","Oral Hygiene","Advertising and Marketing Management","Journalism and Communication","Basic Education","Management (evening attendance)")

pqal <- list('1','2','3','4','5','6','9','10','12','14','15','19','38','39','40','42','43')
names(pqal) <- c('Secondary Education',"Higher Education - Bachelor's Degree",'High Education - degree',"Higher Education - Master's",'Higher Education - Doctorate','Frequency of Higher Education','12th Year of Schooling - not completed','11th Year of Schooling - not completed','Other - 11th Year of Schooling','10th Year of Schooling','10th Year of Schooling - Not Completed','Basic Education 3rd Cycle (9th/10th/11th year) or equiv.','Basic Education - 2nd Cycle (6th/7th/8th year) or equiv.','Technological Specialization Course','High Education - degree (1st Cycle)','Professional Higher Technical Course','Higher Education - Master (2nd Cycle)')

nat <- list('1','2','6','11','13','14','17','21','22','24','25','26','32','41','62','100','101','103','105','108','109')
names(nat) <- c('Portuguese','German','Spanish','Italian','Dutch','English','Lithuanian','Angolan','Cape Verdean','Guinean','Mozambican','Santomean','Turkish','Brazilian','Romanian','Moldova (Republic of)','Mexian','Ukrainian','Russian','Cuban','Colombian')

mqal <- list('1','2','3','4','5','6','9','10','11','12','14','18','19','22','26','27','29','30','34','35','36','37','38','39','40','41','42','43','44')
names(mqal) <- c('Secondary Education - 12th Year of Schooling or Eq.',"Higher Education - Bachelor's Degree",'Higher Education - Degree',"Higher Education - Master's",'Higher Education - Doctorate','Frequency of Higher Education','12th Year of Schooling - Not Completed','11th Year of Schooling - Not Completed','7th Year (Old)','Other - 11th Year of Schooling','10th Year of Schooling','General Commerce Course','Basic Education 3rd Cycle (9th/10th/11th Year) or Equiv.','Technical Professional Course','7th Year of Schooling','2nd Cycle of the General High School Course','9th Year of Schooling - Not Completed','8th Year of Schooling','Unknown',"Can't Read or Write",'Can read without having a 4th year of schooling','Basic Education 1st Cycle (4th/5th Year) or Equiv.','Basic Education 2nd Cycle (6th/7th/8th Year) or Equiv.','Technological Specialization Course','Higher Education - degree (1st cycle)','Specialized Higher Studies Course','Professional Higher Technical Course',"Higher Education - Master (2nd Cycle)",'Higher Education - Doctorate (3rd Cycle)')

pqal <- list('1','2','3','4','5','6','9','10','11','12','13','14','18','19','20','22','25','26','27','29','30','31','33','34','35','36','37','38','39','40','41','42','43','44')
names(pqal) <- c('Secondary Education - 12th Year of Schooling or Eq.',"Higher Education - Bachelor's Degree","Higher Education - Degree","Higher Education - Master's",'Higher Education - Doctorate','Frequency of Higher Education','12th Year of Schooling - Not Completed','11th Year of Schooling - Not Completed','7th Year (Old)','Other - 11th Year of Schooling','2nd Year Complementary High School Course','10th Year of Schooling','General Commerce Course','Basic Education 3rd Cycle (9th/10th/11th Year) or Equiv.','Complementary High School Course','Technical Professional Course','Complementary High School Course - Not Concluded','7th Year of Schooling','2nd Cycle of the General High School Course','9th Year of Schooling - Not Completed','8th Year of Schooling','General Course of Administration and Commerce','Supplementary Accounting and Administration','Unknown',"Can't read or write",'Can read without having a 4th year of schooling','Basic Education 1st Cycle (4th/5th year) or Equiv.','Basic Education - 2nd Cycle (6th/7th/8th Year) or Equiv.','Technological Specialization Course','Higher Education - degree (1st Cycle)','Specialized Higher Studies Course','Professional Higher Technical Course','Higher Education - Master (2nd Cycle)','Higher Education - Doctorate (3rd Cycle)')

mocc <- list('0','1','2','3','4','5','6','7','8','9','10','90','99','122','123','125','131','132','134','141','143','144','151','152','153','171','173','175','191','192','193','194')
names(mocc)<-c('Student','Representatives of the Legislative Power and Executive Bodies, Directors, Directors and Executive Managers','Specialists in Intellectual and Scientific Activities','Intermediate Level Technicians and Professions','Adminnistrative Staff','Personal Services, Security, and Safety Workers and Sellers','Farmers and Skilled Workers in Agriculture, Fisheries, and Forestry','Skilled Workers in Industry, Construction, and Craftsmen','Installation and Machine Operators and Assembly Workers','Unskilled Workers','Armed Forces Profesions','Other Situation ','(blank)','Health Professionals','Teachers','Specialists in Information and Communication Technologies','Intermediate Level Science and Engineeringn Technicians and Professions','Technicians and Professionals of Intermediate Level of Health','Intermediate Level Technicians from Legal, Social, Sports, Cultural, and Similar Services','Office Workers, Secretaries, in General and Data Processing Operators','Data, Accounting, Statistical, Financial Services, and Registry-related Operators','Other Administrative Support Staff','Personal Service Workers','Sellers','Personal Care Workers and the like','Skilled Construction Workes and the like, Except Electricians','Skilled Workers in Printing, Precision Instrument Manufacturing, Jewelers, Artisans, and the like','Workers in Food Processing, Woodworking, Clothing, and Other Industries and Crafts','Cleaning Workers','Unskilled Workers in Agriculture, Animal Production, Fisheries, and Forestry','Unskilled Workers in Extractive Industry, Construction, Manufacturing, and Transport','Meal Preparation Assistants')

pocc <- list('0','1','2','3','4','5','6','7','8','9','10','90','99','101','102','103','112','114','121','122','123','124','131','132','134','135','141','143','144','151','152','153','154','161','163','171','172','174','175','181','182','183','192','193','194','195')
names(pocc) <- c('Student','Representatives of the Legislative Power and Executive Bodies, Directors, Directors and Executive Managers','Specialists in Intellectual and Scientific Activities','Intermediate Level Technicians and Professions','Administrative Staff','Personal Services, Security, and Safety Workers and Sellers','Farmers and Skilled Workers in Agriculture, Fisheries, and Forestry','Skilled Workers in Industry, Construction, and Craftsmen','Installation and Machine Operators and Assembly Workers','Unskilled Workers','Armed Forces Professions','Other Situation','(blank)','Armed Forces Officers','Armed Forces Sergeants','Other Armed Forces personnel','Directors of Administrative and Commercial Services','Hotel, Catering, Trade and Other Services Directors','Specialists in the Physical Sciences, Mathematics, Engineering and Related Techniques','Health Professionals','Teachers','Specialists in Finance, Accounting, Administrative Organization, Public and Commercial Relations','Intermediate Level Science and Engineering Technicians and Professions','Technicians and Professinals of Intermediate Level of Health','Intermediate Level Technicians from Legal, Social, Sports, Cultural, and Similar Services','Information and Communication Technology Technicians','Office Workers, Secretaries in General and Data Processing Operators','Data, Accounting, Statistical, Financial Services, and Registry-related Operators','Other Administrative Support Staff','Personal Service Workers','Sellers','Personal Care Workers and the like','Protection and Security Services Personnel','Market-oriented Farmers and Skilled Agricultural and Animal Production Workers','Farmers, Livestock Keepers, Fishermen, Hunters and Gatherers, Subsistence','Skilled Construction Workers and the like, Except Electricians','Skilled Workers in Metallurgy, Metalworking, and Similar','Skilled Workers in Electricity and Electronics','Workers in Food Processing, Woodworking, Clothing, and Other Industries and Crafts','Fixed Plant and Machine Operators','Assembly Workers','Vehicle Drivers and Mobile Equipment Operators','Unskilled Workers in Agriculture, Animal Production, Fisheries and Forestry','Unskilled Workers in Extractive Industry, Construction, Manufacturing and Transport','Meal Preparation Assistants','Street Vendors (except food) and Street Service Providers')



# Define UI 
ui <- fluidPage(
  
  
  tabsetPanel(
    tabPanel("About",
             h2("Purpose"),
             "The purpose of this app is to produce exploratory data analysis on a dataset containing information on the graduation status of students at a higher education institution.",
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
                                  conditionalPanel(condition="input.mod=='Classification Tree'",
                                                   h3("Classification Trees"),
                                                   "Classification Trees are used to classify/predict group membership.  They divide the predictor space up into regions, with different predictions for each region.",
                                                   "The most prevalent class in a region is usually the result of prediction.  These are fit similarly to regression trees, with recursive binary splitting.  For every",
                                                   "possible  value of each predictor, the Gini Index or Deviance is found and minimized using the equations below.",
                                                   br(),
                                                   br(),
                                                   "Gini Index: 2p(1-p)",
                                                   br(),
                                                   "Deviance: -2plog(p)-2(1-p)log(1-p)",
                                                   br(),
                                                   br(),
                                                   "Both of these values will be small if p is near 0 or 1 (meaning the node classifies well).",
                                                   "Once the first split is chosen, the process will be repeated to determine the subsequent splits.  Generally, a large tree with many nodes is grown and then pruned back,",
                                                   "so that the data is not overfit.  This increases bias but decreases variance, which will hopefully improve prediction overall.",
                                                   "The specific tree method discussed here is boosting, which is the slow training of trees.  This method averages across trees, which results in a lost in interpretability,",
                                                   "but a gain in prediction.  In this method, trees are grown sequentially, with each subsequent tree being",
                                                   "grown on a modified version of the original data, and predictions updating as the trees are grown.  ",
                                                   br(),
                                                   br(),
                                                   h4('Advantages and Disadvantages'),
                                                   'Classification trees have many advantages, some of which are listed below: ',
                                                   br(),
                                                   '1. Simple to understand and interpret output',
                                                   br(),
                                                   "2. Predictors do not need to be scaled",
                                                   br(),
                                                   "3. No statistical assumptions are necessary",
                                                   br(),
                                                   "4. Built in variable selection",
                                                   br(),
                                                   br(),
                                                   "As with any modeling technique, depending on what the endgoal of the analysis is, there",
                                                   "may be disadvantages, such as those listed below: ",
                                                   br(),
                                                   "1. Small changes in data can vastly change tree",
                                                   br(),
                                                   "2. Greedy algorithm (recursive binary splitting) necessary",
                                                   br(),
                                                   "3. Usually need to prune",
                                                   br(),
                                                   br())
                                  ,
                                  conditionalPanel(condition="input.mod=='Random Forest'",
                                                   h3("Random Forest"),
                                                   "Random forests are similar to bagging in the sense that multiple trees (i.e., a 'forest'') are created from bootstrap samples of data, with the outcome",
                                                   "of these trees averaged.  However, this method uses a random subset of predictors for each boostrap sample/tree fit, rather than using every predictor, as in bagging.",
                                                   "This in turn makes the trees less correlated with one another, which allows for a greater reduction in variation once the outcome of the trees are averaged.",
                                                   "By randomly selecting a subset of predictors, one or two good predictors will not dominate the tree fits.  Typically, m = sqrt(p) or m=p/3 predictors are randomly",
                                                   "selected.  This method is a type of classification tree and, as a result, uses the same equations to calculate data splits, as seen below:",
                                                   br(),
                                                   br(),
                                                   "Gini Index: 2p(1-p)",
                                                   br(),
                                                   "Deviance: -2plog(p)-2(1-p)log(1-p)",
                                                   br(),
                                                   br(),
                                                   "Both of these values will be small if p is near 0 or 1 (meaning the node classifies well).",
                                                   "Once the first split is chosen, the process will be repeated to determine the subsequent splits.  Generally, a large tree with many nodes is grown and then pruned back,",
                                                   "so that the data is not overfit." ,
                                                   br(),
                                                   br(),
                                                   h4('Advantages and Disadvantages'),
                                                   'Classification trees have many advantages, some of which are listed below: ',
                                                   br(),
                                                   '1. Simple to understand and interpret output',
                                                   br(),
                                                   "2. Predictors do not need to be scaled",
                                                   br(),
                                                   "3. No statistical assumptions are necessary",
                                                   br(),
                                                   "4. Built in variable selection",
                                                   br(),
                                                   br(),
                                                   "As with any modeling technique, depending on what the endgoal of the analysis is, there",
                                                   "may be disadvantages, such as those listed below: ",
                                                   br(),
                                                   "1. Small changes in data can vastly change tree",
                                                   br(),
                                                   "2. Greedy algorithm (recursive binary splitting) necessary",
                                                   br(),
                                                   "3. Usually need to prune",
                                                   br(),
                                                   br())
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
                                          DTOutput('coeff1'),
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
                                sidebarPanel(h3("Prediction"),
                                             numericInput("prop2","Pick Proportion of Data for Training Set",value=.80,min=.01,max=.99),
                                             radioButtons("pred","Select a Model for Prediction",choices=c("Generalized Linear Regression","Classification Tree","Random Forest")),
                                             h4("Pick Variables for Models"),
                                             checkboxInput('marpred','Marital Status'),
                                             checkboxInput('appmode',"Application Mode"),
                                             checkboxInput('appord',"Application Order"),
                                             checkboxInput('course',"Course"),
                                             checkboxInput('dteatten',"Daytime or Evening Attendance"),
                                             checkboxInput('prevq','Previous Qualification'),
                                             checkboxInput('prevqg','Previous Qualification Grade'),
                                             checkboxInput('nac','Nationality'),
                                             checkboxInput('mq',"Mother's Qualification"),
                                             checkboxInput('fq',"Father's Qualification"),
                                             checkboxInput('mo',"Mother's Occupation"),
                                             checkboxInput('fo',"Father's Occupation"),
                                             checkboxInput('ag','Admission Grade'),
                                             checkboxInput('dp','Displaced'),
                                             checkboxInput('esn','Educational Special Needs'),
                                             checkboxInput('deb','Debtor'),
                                             checkboxInput('tuit','Tuition and Fees Up to Date'),
                                             checkboxInput('gen','Gender'),
                                             checkboxInput('schol','Scholarship Holder'),
                                             checkboxInput('age','Age At Enrollment'),
                                             checkboxInput('int','International'),
                                             checkboxInput('cfsc',"Curricular Units 1st Semester (Credited)"),
                                             checkboxInput('cfse','Curricular Units 1st Semester (Enrolled)'),
                                             checkboxInput('cfsev','Curricular Units 1st Semester (Evaluations)'),
                                             checkboxInput('cfsa','Curricular Units 1st Semester (Approved'),
                                             checkboxInput('cfsg','Curricular Units 1st Semester Grade'),
                                             checkboxInput('cfswoe','Curricular Units 1st Semester (Without Evaluation'),
                                             checkboxInput('cssc','Curricular Units 2nd Semester (Credited)'),
                                             checkboxInput('csse','Curricular Units 2nd Semester (Enrolled)'),
                                             checkboxInput('cssev','Curricular Units 2nd Semester (Evaluations)'),
                                             checkboxInput('cssa','Curricular Units 2nd Semester (Approved)'),
                                             checkboxInput('cssg','Curricular Units 2nd Semester Grade'),
                                             checkboxInput('csswoe','Curricular Units 2nd Semester (Without Evaluation'),
                                             checkboxInput('ur','Unemployment Rate'),
                                             checkboxInput('ir','Inflation Rate'),
                                             checkboxInput('gdp','GDP')
                                             
                                             
                                ),
                                mainPanel(
                                  conditionalPanel(condition="input.marpred==1", 
                                                   selectInput("marstat","Select Marital Status",choices=c("Single","Married","Widower","Divorced","Facto Union","Legally Separated"))),
                                  conditionalPanel(condition="input.appmode==1",
                                                   selectInput('appstat','Select Application Mode',choices=mode)),
                                  conditionalPanel(condition="input.appord==1",
                                                   selectInput("appordpred","Select Application Order",choices=c("0",'1','2','3','4','5','6','7','8','9'))),
                                  conditionalPanel(condition="input.course==1",
                                                   selectInput('coursep',"Select Course",choices=class)),
                                  conditionalPanel(condition="input.dteatten",
                                                   selectInput('dtep',"Daytime or Evening Attendance",choices=c("Daytime","Evening"))),
                                  conditionalPanel(condition="input.prevq",
                                                   selectInput('prevqp',"Previous Qualification",choices=pqal)),
                                  conditionalPanel(condition='input.prevqg',
                                                   numericInput('prevqgp','Previous Qualification Grade',100,min=0,max=200)),
                                  conditionalPanel(condition="input.nac==1",
                                                   selectInput('nacp',"Nationality",choices=nat)),
                                  conditionalPanel(condition='input.mq==1',
                                                   selectInput('mqp',"Mother's Qualification",choices=mqal)),
                                  conditionalPanel(condition='input.fq==1',
                                                   selectInput('fqp',"Father's Qualification",choices=pqal)),
                                  conditionalPanel(condition='input.mo==1',
                                                   selectInput("mop","Mother's Occupation",choices=mocc)),
                                  conditionalPanel(condition='input.fo==1',
                                                   selectInput('fop',"Father's Occupation",choices=pocc)),
                                  conditionalPanel(condition='input.ag==1',
                                                   numericInput('agp','Admission Grade',100,min=0,max=200)),
                                  conditionalPanel(condition='input.dp==1',
                                                   selectInput('dpp','Displaced',choices=c('Yes','No'))),
                                  conditionalPanel(condition='input.esn==1',
                                                   selectInput('esnp','Educational Special Needs',choices=c('Yes','No'))),
                                  conditionalPanel(condition='input.deb==1',
                                                   selectInput('debp','Debtor',choices=c('Yes','No'))),
                                  conditionalPanel(condition='input.tuit==1',
                                                   selectInput('tuitp','Tuition and Fees Up to Date',choices=c('Yes','No'))),
                                  conditionalPanel(condition='input.gen==1',
                                                   selectInput('genp','Gender',choices=c("Male","Female"))),
                                  conditionalPanel(condition='input.schol==1',
                                                   selectInput('scholp','Scholarship Holder',choices=c('Yes','No'))),
                                  conditionalPanel(condition='input.age==1',
                                                   numericInput('agep','Age',18,min=0,max=72,step=1)),
                                  conditionalPanel(condition='input.int==1',
                                                   selectInput('intp','International',choices=c('Yes','No'))),
                                  conditionalPanel(condition='input.cfsc==1',
                                                   numericInput('cfscp','Curricular Units 1st Semester (Credited)',12,min=0,max=20,step=1)),
                                  conditionalPanel(condition='input.cfse==1',
                                                   numericInput('cfsep','Curricular Units 1st Semester (Enrolled)',12,min=0,max=26,step=1)),
                                  conditionalPanel(condition="input.cfsev==1",
                                                   numericInput('cfsevp','Curricular Units 1st Semester (Evaluations)',13,min=0,max=26,step=1)),
                                  conditionalPanel(condition='input.cfsa==1',
                                                   numericInput('csfap','Curricular Units 1st Semester (Approved)',13,min=0,max=26,step=1)),
                                  conditionalPanel(condition='input.cfsg==1',
                                                   numericInput('cfsgp','Curricular Units 1st Semester Grade',14,min=0,max=20,step=1)),
                                  conditionalPanel(condition='input.cfswoe==1',
                                                   numericInput('cfswoep','Curricular Units 1st Semester (Without Evaluations)',0,min=0,max=12,step=1)),
                                  conditionalPanel(condition='input.cssc==1',
                                                   numericInput('csscp','Curricular Units 2nd Semester (Credited)',12,min=0,max=20,step=1)),
                                  conditionalPanel(condition='input.csse==1',
                                                   numericInput('cssep','Curricular Units 2nd Semester (Enrolled)',12,min=0,max=26,step=1)),
                                  conditionalPanel(condition="input.cssev==1",
                                                   numericInput('cssevp','Curricular Units 2nd Semester (Evaluations)',13,min=0,max=26,step=1)),
                                  conditionalPanel(condition='input.cssa==1',
                                                   numericInput('csap','Curricular Units 2nd Semester (Approved)',13,min=0,max=26,step=1)),
                                  conditionalPanel(condition='input.cssg==1',
                                                   numericInput('cssgp','Curricular Units 2nd Semester Grade',14,min=0,max=20)),
                                  conditionalPanel(condition='input.csswoe==1',
                                                   numericInput('csswoep','Curricular Units 2nd Semester (Without Evaluations)',0,min=0,max=12,step=1)),
                                  conditionalPanel(condition='input.ur==1',
                                                   numericInput('urp','Unemployment Rate',10,min=7,max=17)),
                                  conditionalPanel(condition='input.ir==1',
                                                   numericInput('irp','Inflation Rate',1, min=-0.9,max=4)),
                                  conditionalPanel(condition='input.gdp==1',
                                                   numericInput('gdpp','GDP',0,min=-4.06,max=3.51)),
                                  actionButton('submit2','Submit Prediction'),
                                  DTOutput('predglm'),
                                  DTOutput('predboost'),
                                  DTOutput('predrf')
                                  
                                ))),
    )
    ,
    
    tabPanel("Data",
             br(),
             "View the dataset used in this app below.  Choose variables to subset the columns, and 
             choose the numbers of rows (observations) you would like to view.",
             "To download the data you choose, press the download button.",
             sidebarLayout(
               sidebarPanel(h4("Pick Variables for Dataset"),
                            checkboxInput('marpred2','Marital Status',value=TRUE),
                            checkboxInput('appmode2',"Application Mode",value=TRUE),
                            checkboxInput('appord2',"Application Order",value=TRUE),
                            checkboxInput('course2',"Course",value=TRUE),
                            checkboxInput('dteatten2',"Daytime or Evening Attendance",value=TRUE),
                            checkboxInput('prevq2','Previous Qualification',value=TRUE),
                            checkboxInput('prevqg2','Previous Qualification Grade',value=TRUE),
                            checkboxInput('nac2','Nationality',value=TRUE),
                            checkboxInput('mq2',"Mother's Qualification",value=TRUE),
                            checkboxInput('fq2',"Father's Qualification",value=TRUE),
                            checkboxInput('mo2',"Mother's Occupation",value=TRUE),
                            checkboxInput('fo2',"Father's Occupation",value=TRUE),
                            checkboxInput('ag2','Admission Grade',value=TRUE),
                            checkboxInput('dp2','Displaced',value=TRUE),
                            checkboxInput('esn2','Educational Special Needs',value=TRUE),
                            checkboxInput('deb2','Debtor',value=TRUE),
                            checkboxInput('tuit2','Tuition and Fees Up to Date',value=TRUE),
                            checkboxInput('gen2','Gender',value=TRUE),
                            checkboxInput('schol2','Scholarship Holder',value=TRUE),
                            checkboxInput('age2','Age At Enrollment',value=TRUE),
                            checkboxInput('int2','International',value=TRUE),
                            checkboxInput('cfsc2',"Curricular Units 1st Semester (Credited)",value=TRUE),
                            checkboxInput('cfse2','Curricular Units 1st Semester (Enrolled)',value=TRUE),
                            checkboxInput('cfsev2','Curricular Units 1st Semester (Evaluations)',value=TRUE),
                            checkboxInput('cfsa2','Curricular Units 1st Semester (Approved)',value=TRUE),
                            checkboxInput('cfsg2','Curricular Units 1st Semester Grade',value=TRUE),
                            checkboxInput('cfswoe2','Curricular Units 1st Semester (Without Evaluation',value=TRUE),
                            checkboxInput('cssc2','Curricular Units 2nd Semester (Credited)',value=TRUE),
                            checkboxInput('csse2','Curricular Units 2nd Semester (Enrolled)',value=TRUE),
                            checkboxInput('cssev2','Curricular Units 2nd Semester (Evaluations)',value=TRUE),
                            checkboxInput('cssa2','Curricular Units 2nd Semester (Approved)',value=TRUE),
                            checkboxInput('cssg2','Curricular Units 2nd Semester Grade',value=TRUE),
                            checkboxInput('csswoe2','Curricular Units 2nd Semester (Without Evaluation)',value=TRUE),
                            checkboxInput('ur2','Unemployment Rate',value=TRUE),
                            checkboxInput('ir2','Inflation Rate',value=TRUE),
                            checkboxInput('gdp2','GDP',value=TRUE),
                            checkboxInput('tar','Target',value=TRUE),
                            h4('Pick Number of Observations'),
                            numericInput('rows','Number of Rows',value=3630,min=1,max=3630),
                            downloadButton('download','Download')
               ),
               mainPanel(DTOutput('dt')
                         
                         
               )
             )
             
             
    )
  )
)


set.seed(1234)


# Define server logic 
server <- function(input, output, session) {
  ## Code for image in About tab
  output$image1 <- renderImage({
    width<- "80%"
    height<- "20%"
    list(src = "www/cu-grad.jpg",
         width = width,
         height = "auto",
         align = 'center'
    )
  }, deleteFile = FALSE)
  
  ## Numerical and graphical summaries tab
  ## Boxplots
  output$plot <- renderPlot({
    if(input$summary=='Boxplots'){
      var <- input$vars
      Target <- "Target"
      ggplot(GradDataV2, aes_string(x=Target, y=var)) + 
        geom_boxplot(outlier.colour="red", outlier.shape=8,
                     outlier.size=4)}
    
    
  })
  # contingency tables output
  output$table <- renderTable({
    if(input$summary=='Contingency Tables'){
      if(input$vars2=='Marital.status'){
        table(GradDataV2$Marital.status,GradDataV2$Target)
      }
      else if(input$vars2=='Application.order'){
        table(GradDataV2$Application.order,GradDataV2$Target)
      }
      else if(input$vars2=='International'){
        table(GradDataV2$International,GradDataV2$Target)
      }
      else if(input$vars2=='Educational.special.needs'){
        table(GradDataV2$Educational.special.needs, GradDataV2$Target)
      }
      else if(input$vars2=='Tuition.fees.up.to.date'){
        table(GradDataV2$Tuition.fees.up.to.date,GradDataV2$Target)
      }
      else if(input$vars2=='Gender'){
        table(GradDataV2$Gender,GradDataV2$Target)
      }
      else if(input$vars2=='Scholarship.holder'){
        table(GradDataV2$Scholarship.holder,GradDataV2$Target)
      }
      else if(input$vars2=='Daytime.evening.attendance.'){
        table(GradDataV2$Daytime.evening.attendance.,GradDataV2$Target)
      }
    }
  })
  # mosaic plots output
  output$plot2 <- renderPlot({
    if(input$vars3=='International'){
      International <- table(GradDataV2$International,GradDataV2$Target)
      mosaicplot(International)
    }
    else if(input$vars3=='Educational.special.needs'){
      EducationalSpecialNeeds <- table(GradDataV2$Educational.special.needs, GradDataV2$Target)
      mosaicplot(EducationalSpecialNeeds)
    }
    else if(input$vars3=='Tuition.fees.up.to.date'){
      TuitionUpToDate <- table(GradDataV2$Tuition.fees.up.to.date,GradDataV2$Target)
      mosaicplot(TuitionUpToDate)
    }
    else if(input$vars3=='Gender'){
      Gender <- table(GradDataV2$Gender,GradDataV2$Target)
      mosaicplot(Gender)
    }
    else if(input$vars3=='Scholarship.holder'){
      ScholarshipHolder <- table(GradDataV2$Scholarship.holder,GradDataV2$Target)
      mosaicplot(ScholarshipHolder)
    }
    else if(input$vars3=='Daytime.evening.attendance.'){
      AttendanceTime <- table(GradDataV2$Daytime.evening.attendance.,GradDataV2$Target)
      mosaicplot(AttendanceTime)
    }
  })
  # main panel stuff
  # generalized linear model 
  # first splitting data between training and test set
  # Divide data into training and test sets
  # needs to be updated when the button clicks 
  
  
  observeEvent(input$submit,{
    output$modfit1 <-renderTable({
      
      train <- sample(1:nrow(GradDataV2), size = nrow(GradDataV2)*input$prop)
      test <- setdiff(1:nrow(GradDataV2), train)
      
      # training and testing subsets
      datTrain1 <- GradDataV2[train, ]
      datTest1 <- GradDataV2[test, ]
      
      isolate(
        if(!is.null(input$varsm)){
          vars <- input$varsm
          vars2 <- str_c(vars,collapse='+')
          formula <- paste('Target ~',vars2)
          form2 <- as.formula(formula)
          mrm <- train(form2,
                       data=datTrain1,
                       method="glm",
                       preProcess = c("center", "scale"), 
                       trControl = trainControl(method = "cv", number = 5))
          Results <- as.data.frame(mrm$results)
          Results <- Results[,-1]
        })
    })
  })
  
  observeEvent(input$submit,{
    output$coeff1 <-renderDT({
      
      train <- sample(1:nrow(GradDataV2), size = nrow(GradDataV2)*input$prop)
      test <- setdiff(1:nrow(GradDataV2), train)
      
      # training and testing subsets
      datTrain1 <- GradDataV2[train, ]
      datTest1 <- GradDataV2[test, ]
      
      
      isolate(
        if(!is.null(input$varsm)){
          withProgress(message='Running GLM',{
            vars <- input$varsm
            vars2 <- str_c(vars,collapse='+')
            formula <- paste('Target ~',vars2)
            form2 <- as.formula(formula)
            mrm <- train(form2,
                         data=datTrain1,
                         method="glm",
                         preProcess = c("center", "scale"), 
                         trControl = trainControl(method = "cv", number = 5))
            t <- summary(mrm)
            tab <- as.data.frame(t$coefficients)
            tab<- tibble::rownames_to_column(tab, "row_names")
            colnames(tab)[1] <- ''
            tab
          })
        })
    })
  })
  
  ## predicting and RMSE
  observeEvent(input$submit,{
    output$modfit12 <-
      renderTable({
        train <- sample(1:nrow(GradDataV2), size = nrow(GradDataV2)*input$prop)
        test <- setdiff(1:nrow(GradDataV2), train)
        
        # training and testing subsets
        datTrain1 <- GradDataV2[train, ]
        datTest1 <- GradDataV2[test, ]
        
        isolate(
          if(!is.null(input$varsm)){
            withProgress(message='Running GLM',{
              vars <- input$varsm
              vars2 <- str_c(vars,collapse='+')
              formula <- paste('Target ~',vars2)
              form2 <- as.formula(formula)
              mrm <- train(form2,
                           data=datTrain1,
                           method="glm",
                           preProcess = c("center", "scale"), 
                           trControl = trainControl(method = "cv", number = 5))
              #Run on test data
              GLM.predict2 <- predict(mrm, newdata = datTest1)
              
              #Obtain RMSE from test set, which will be used to compare to other models later 
              # but printed out for now 
              GLM.compare2 <- postResample(GLM.predict2, obs=datTest1$Target)
              RMSE <- as.data.frame(GLM.compare2)
              RMSE2 <- RMSE          
              RMSE2<- tibble::rownames_to_column(RMSE2, "row_names")
              colnames(RMSE2)[1] <- ''
              RMSE2
            })
          })
      })
  })
  
  ## classification tree - boosting
  observeEvent(input$submit,{
    output$bfitplot <-
      renderPlot({
        train <- sample(1:nrow(GradDataV2), size = nrow(GradDataV2)*input$prop)
        test <- setdiff(1:nrow(GradDataV2), train)
        
        # training and testing subsets
        datTrain1 <- GradDataV2[train, ]
        datTest1 <- GradDataV2[test, ]
        ## will make a plot and a table here
        isolate(
          if(!is.null(input$varsm)){
            withProgress(message='Running Classification Tree',{
              vars <- input$varsm
              vars2 <- str_c(vars,collapse='+')
              formula <- paste('Target ~',vars2)
              form2 <- as.formula(formula)
              boostFit <- train(form2, data=datTrain1, method="gbm",
                                preProcess=c("center","scale"),
                                trControl=trainControl(method='cv',number=5),
                                tuneGrid = expand.grid(n.trees=seq(25,200,50),
                                                       interaction.depth=seq(1,4,1),
                                                       shrinkage=0.1,
                                                       n.minobsinnode=10))
              
              # plot fit
              plot(boostFit) 
            })
          })
      })
  })
  
  ## run bfit model on test data
  observeEvent(input$submit,{
    output$bfitresults <-
      renderTable({
        train <- sample(1:nrow(GradDataV2), size = nrow(GradDataV2)*input$prop)
        test <- setdiff(1:nrow(GradDataV2), train)
        
        # training and testing subsets
        datTrain1 <- GradDataV2[train, ]
        datTest1 <- GradDataV2[test, ]
        
        isolate(
          if(!is.null(input$varsm)){
            withProgress(message='Running Classification Tree',{
              vars <- input$varsm
              vars2 <- str_c(vars,collapse='+')
              formula <- paste('Target ~',vars2)
              form2 <- as.formula(formula)
              boostFit <- train(form2, data=datTrain1, method="gbm",
                                preProcess=c("center","scale"),
                                trControl=trainControl(method='cv',number=5),
                                tuneGrid = expand.grid(n.trees=seq(25,200,50),
                                                       interaction.depth=seq(1,4,1),
                                                       shrinkage=0.1,
                                                       n.minobsinnode=10))
              #Run on test data
              boostFit.predict <- predict(boostFit, newdata = datTest1)
              
              #Obtain RMSE from test set, which will be used in automated comparison
              boostFit.compare <- postResample(boostFit.predict, obs = datTest1$Target)
              RMSEf <- as.data.frame(boostFit.compare)
              RMSE2f <- RMSEf          
              RMSE2f<- tibble::rownames_to_column(RMSE2f, "row_names")
              colnames(RMSE2f)[1] <- ''
              RMSE2f})})
      })
  })
  
  ## random forest output
  #Create model
  observeEvent(input$submit,{
    output$rforestp <-
      renderPlot({
        train <- sample(1:nrow(GradDataV2), size = nrow(GradDataV2)*input$prop)
        test <- setdiff(1:nrow(GradDataV2), train)
        
        # training and testing subsets
        datTrain1 <- GradDataV2[train, ]
        datTest1 <- GradDataV2[test, ]
        
        isolate(
          if(!is.null(input$varsm)){
            withProgress(message='Running Random Forest',{
              vars <- input$varsm
              vars2 <- str_c(vars,collapse='+')
              formula <- paste('Target ~',vars2)
              form2 <- as.formula(formula)
              random.forest.fit <- train(form2, data = datTrain1,
                                         method = "rf",
                                         preProcess = c("center", "scale"),
                                         trControl = trainControl(method = "cv", number = 5),
                                         tuneGrid = data.frame(mtry = 1:37))
              
              # plot fit
              plot(random.forest.fit)
              
            })
          })
      })
  })
  
  observeEvent(input$submit,{
    output$rforestt <-
      renderTable({
        train <- sample(1:nrow(GradDataV2), size = nrow(GradDataV2)*input$prop)
        test <- setdiff(1:nrow(GradDataV2), train)
        
        # training and testing subsets
        datTrain1 <- GradDataV2[train, ]
        datTest1 <- GradDataV2[test, ]
        
        isolate(
          if(!is.null(input$varsm)){
            withProgress(message='Running Random Forest',{
              vars <- input$varsm
              vars2 <- str_c(vars,collapse='+')
              formula <- paste('Target ~',vars2)
              form2 <- as.formula(formula)
              random.forest.fit <- train(form2, data = datTrain1,
                                         method = "rf",
                                         preProcess = c("center", "scale"),
                                         trControl = trainControl(method = "cv", number = 5),
                                         tuneGrid = data.frame(mtry = 1:37))
              
              #Run on test data
              random.forest.predict <- predict(random.forest.fit, newdata = datTest1)
              
              #Obtain RMSE from test set, which will be used in automated comparison
              random.forest.compare <- postResample(random.forest.predict,
                                                    obs = datTest1$Target)
              RMSErff <- as.data.frame(random.forest.compare)
              RMSErf2f <- RMSErff
              RMSErf2f<- tibble::rownames_to_column(RMSErf2f, "row_names")
              colnames(RMSErf2f)[1] <- ''
              RMSErf2f })
          })
      })
  })
  
  ## prediction tab
  ## 37 variables
  # Generalized Linear Model Output
  
  observeEvent(input$submit2,{
    output$predglm <-renderDT({
      
      isolate(
        if(input$pred=='Generalized Linear Regression'){
          
          df <- matrix()
          if(input$marpred==1){
            df <- cbind(df,Marital.status=input$marstat)
          }
          if(input$appmode==1){
            df <- cbind(df,Application.mode=input$appstat)
          }
          if(input$appord==1){
            df <- cbind(df,Application.order=input$appordpred)
          }
          if(input$course==1){
            df <- cbind(df,Course=input$coursep)
          }
          if(input$dteatten==1){
            df <- cbind(df,Daytime.evening.attendance.=input$dtep)
          }
          if(input$prevq==1){
            df <- cbind(df,Previous.qualification=input$prevqp)
          }
          if(input$prevqg==1){
            df <- cbind(df,Previous.qualification..grade.=input$prevqgp)
          }
          if(input$nac==1){
            df <- cbind(df,Nacionality = input$nacp)
          }
          if(input$mq==1){
            df <- cbind(df,Mother.s.qualification=input$mqp)
          }
          if(input$fq==1){
            df <- cbind(df,Father.s.qualification = input$fqp)
          }
          if(input$mo==1){
            df <- cbind(df,Mother.s.occupation = input$mop)
          }
          if(input$fo==1){
            df <- cbind(df,Father.s.occupation= input$fop)
          }
          if(input$ag==1){
            df <- cbind(df,Admission.grade = input$agp)
          }
          if(input$dp==1){
            df <- cbind(df,Displaced =input$dpp)
          }
          if(input$esn==1){
            df <- cbind(df,Educational.special.needs= input$esnp)
          }
          if(input$deb==1){
            df <- cbind(df,Debtor =input$debp)
          }
          if(input$tuit==1){
            df <- cbind(df,Tuition.fees.up.to.date = input$tuitp)
          }
          if(input$gen==1){
            df <- cbind(df,Gender= input$genp)
          }
          if(input$schol==1){
            df <- cbind(df,Scholarship.holder= input$scholp)
          }
          if(input$age==1){
            df <- cbind(df,Age.at.enrollment= input$agep)
          }
          if(input$int==1){
            df <- cbind(df,International= input$intp)
          }
          if(input$cfsc==1){
            df <- cbind(df,Curricular.units.1st.sem..credited.= input$cfscp)
          }
          if(input$cfse==1){
            df <- cbind(df,Curricular.units.1st.sem..enrolled. = input$cfsep)
          }
          if(input$cfsev==1){
            df <- cbind(df,Curricular.units.1st.sem..evaluations. = input$cfsevp)
          }
          if(input$cfsa==1){
            df <- cbind(df,Curricular.units.1st.sem..approved.=input$cfsap)
          }
          if(input$cfsg==1){
            df <- cbind(df,Curricular.units.1st.sem..grade. = input$cfsgp)
          }
          if(input$cfswoe==1){
            df <- cbind(df,Curricular.units.1st.sem..without.evaluations. = input$cfswoep)
          }
          if(input$cssc==1){
            df <- cbind(df,Curricular.units.2nd.sem..credited. = input$csscp)
          }
          if(input$csse==1){
            df <- cbind(df,Curricular.units.2nd.sem..enrolled. = input$cssep)
          }
          if(input$cssev==1){
            df <- cbind(df,Curricular.units.2nd.sem..evaluations. = input$cssevp)
          }
          if(input$cssa==1){
            df <- cbind(df,Curricular.units.2nd.sem..approved. = input$csap)
          }
          if(input$cssg==1){
            df <- cbind(df,Curricular.units.2nd.sem..grade. = input$cssgp)
          }
          if(input$csswoe==1){
            df <- cbind(df,Curricular.units.2nd.sem..without.evaluations. = input$csswoep)
          }
          if(input$ur==1){
            df <- cbind(df,Unemployment.rate = input$urp)
          }
          if(input$ir==1){
            df <- cbind(df,Inflation.rate =input$irp)
          }
          if(input$gdp==1){
            df <- cbind(df,GDP = input$gdpp)
          }
          
          
          train <- sample(1:nrow(GradDataV2), size = nrow(GradDataV2)*input$prop2)
          test <- setdiff(1:nrow(GradDataV2), train)
          
          # training and testing subsets
          datTrain1 <- GradDataV2[train, ]
          datTest1 <- GradDataV2[test, ]
          
          isolate(
            if(!is.null(df)){
              withProgress(message='Predicting GLM',{
                vars <- colnames(df)
                for(i in 1:length(vars)){
                  if(is.numeric(GradDataV2[1,vars[i]])){
                    vars[i] <- paste('as.numeric(',vars[i],')')
                  }
                }
                vars2 <- str_c(vars,collapse='+')
                formula <- paste('Target ~',vars2)
                form2 <- as.formula(formula)
                mrm <- train(form2,
                             data=datTrain1,
                             method="glm",
                             preProcess = c("center", "scale"),
                             trControl = trainControl(method = "cv", number = 5))
                prediction<- predict(mrm,df,interval='prediction')
                prediction <- as.data.frame(prediction)
                prediction
              })
            })
        })
    })
  })
  
  # Boosting output
  observeEvent(input$submit2,{
    output$predboost <-renderDT({
      
      isolate(
        if(input$pred=='Classification Tree'){
          
          df <- matrix()
          if(input$marpred==1){
            df <- cbind(df,Marital.status=input$marstat)
          }
          if(input$appmode==1){
            df <- cbind(df,Application.mode=input$appstat)
          }
          if(input$appord==1){
            df <- cbind(df,Application.order=input$appordpred)
          }
          if(input$course==1){
            df <- cbind(df,Course=input$coursep)
          }
          if(input$dteatten==1){
            df <- cbind(df,Daytime.evening.attendance.=input$dtep)
          }
          if(input$prevq==1){
            df <- cbind(df,Previous.qualification=input$prevqp)
          }
          if(input$prevqg==1){
            df <- cbind(df,Previous.qualification..grade.=input$prevqgp)
          }
          if(input$nac==1){
            df <- cbind(df,Nacionality = input$nacp)
          }
          if(input$mq==1){
            df <- cbind(df,Mother.s.qualification=input$mqp)
          }
          if(input$fq==1){
            df <- cbind(df,Father.s.qualification = input$fqp)
          }
          if(input$mo==1){
            df <- cbind(df,Mother.s.occupation = input$mop)
          }
          if(input$fo==1){
            df <- cbind(df,Father.s.occupation= input$fop)
          }
          if(input$ag==1){
            df <- cbind(df,Admission.grade = input$agp)
          }
          if(input$dp==1){
            df <- cbind(df,Displaced =input$dpp)
          }
          if(input$esn==1){
            df <- cbind(df,Educational.special.needs= input$esnp)
          }
          if(input$deb==1){
            df <- cbind(df,Debtor =input$debp)
          }
          if(input$tuit==1){
            df <- cbind(df,Tuition.fees.up.to.date = input$tuitp)
          }
          if(input$gen==1){
            df <- cbind(df,Gender= input$genp)
          }
          if(input$schol==1){
            df <- cbind(df,Scholarship.holder= input$scholp)
          }
          if(input$age==1){
            df <- cbind(df,Age.at.enrollment= input$agep)
          }
          if(input$int==1){
            df <- cbind(df,International= input$intp)
          }
          if(input$cfsc==1){
            df <- cbind(df,Curricular.units.1st.sem..credited.= input$cfscp)
          }
          if(input$cfse==1){
            df <- cbind(df,Curricular.units.1st.sem..enrolled. = input$cfsep)
          }
          if(input$cfsev==1){
            df <- cbind(df,Curricular.units.1st.sem..evaluations. = input$cfsevp)
          }
          if(input$cfsa==1){
            df <- cbind(df,Curricular.units.1st.sem..approved.=input$cfsap)
          }
          if(input$cfsg==1){
            df <- cbind(df,Curricular.units.1st.sem..grade. = input$cfsgp)
          }
          if(input$cfswoe==1){
            df <- cbind(df,Curricular.units.1st.sem..without.evaluations. = input$cfswoep)
          }
          if(input$cssc==1){
            df <- cbind(df,Curricular.units.2nd.sem..credited. = input$csscp)
          }
          if(input$csse==1){
            df <- cbind(df,Curricular.units.2nd.sem..enrolled. = input$cssep)
          }
          if(input$cssev==1){
            df <- cbind(df,Curricular.units.2nd.sem..evaluations. = input$cssevp)
          }
          if(input$cssa==1){
            df <- cbind(df,Curricular.units.2nd.sem..approved. = input$csap)
          }
          if(input$cssg==1){
            df <- cbind(df,Curricular.units.2nd.sem..grade. = input$cssgp)
          }
          if(input$csswoe==1){
            df <- cbind(df,Curricular.units.2nd.sem..without.evaluations. = input$csswoep)
          }
          if(input$ur==1){
            df <- cbind(df,Unemployment.rate = input$urp)
          }
          if(input$ir==1){
            df <- cbind(df,Inflation.rate =input$irp)
          }
          if(input$gdp==1){
            df <- cbind(df,GDP = input$gdpp)
          }
          
          
          train <- sample(1:nrow(GradDataV2), size = nrow(GradDataV2)*input$prop2)
          test <- setdiff(1:nrow(GradDataV2), train)
          
          # training and testing subsets
          datTrain1 <- GradDataV2[train, ]
          datTest1 <- GradDataV2[test, ]
          
          isolate(
            if(!is.null(df)){
              withProgress(message='Predicting Classification Tree',{
                vars <- colnames(df)
                for(i in 1:length(vars)){
                  if(is.numeric(GradDataV2[1,vars[i]])){
                    vars[i] <- paste('as.numeric(',vars[i],')')
                  }
                }
                vars2 <- str_c(vars,collapse='+')
                formula <- paste('Target ~',vars2)
                form2 <- as.formula(formula)
                boostFit <- train(form2, data=datTrain1, method="gbm",
                                  preProcess=c("center","scale"),
                                  trControl=trainControl(method='cv',number=5),
                                  tuneGrid = expand.grid(n.trees=seq(25,200,50),
                                                         interaction.depth=seq(1,4,1),
                                                         shrinkage=0.1,
                                                         n.minobsinnode=10))
                prediction<- predict(boostFit,df,interval='prediction')
                prediction <- as.data.frame(prediction)
                prediction
              })
            })
        })
    })
  })
  
  # Random Forest Output
  observeEvent(input$submit2,{
    output$predrf <-renderDT({
      
      isolate(
        if(input$pred=='Random Forest'){
          
          df <- matrix()
          if(input$marpred==1){
            df <- cbind(df,Marital.status=input$marstat)
          }
          if(input$appmode==1){
            df <- cbind(df,Application.mode=input$appstat)
          }
          if(input$appord==1){
            df <- cbind(df,Application.order=input$appordpred)
          }
          if(input$course==1){
            df <- cbind(df,Course=input$coursep)
          }
          if(input$dteatten==1){
            df <- cbind(df,Daytime.evening.attendance.=input$dtep)
          }
          if(input$prevq==1){
            df <- cbind(df,Previous.qualification=input$prevqp)
          }
          if(input$prevqg==1){
            df <- cbind(df,Previous.qualification..grade.=input$prevqgp)
          }
          if(input$nac==1){
            df <- cbind(df,Nacionality = input$nacp)
          }
          if(input$mq==1){
            df <- cbind(df,Mother.s.qualification=input$mqp)
          }
          if(input$fq==1){
            df <- cbind(df,Father.s.qualification = input$fqp)
          }
          if(input$mo==1){
            df <- cbind(df,Mother.s.occupation = input$mop)
          }
          if(input$fo==1){
            df <- cbind(df,Father.s.occupation= input$fop)
          }
          if(input$ag==1){
            df <- cbind(df,Admission.grade = input$agp)
          }
          if(input$dp==1){
            df <- cbind(df,Displaced =input$dpp)
          }
          if(input$esn==1){
            df <- cbind(df,Educational.special.needs= input$esnp)
          }
          if(input$deb==1){
            df <- cbind(df,Debtor =input$debp)
          }
          if(input$tuit==1){
            df <- cbind(df,Tuition.fees.up.to.date = input$tuitp)
          }
          if(input$gen==1){
            df <- cbind(df,Gender= input$genp)
          }
          if(input$schol==1){
            df <- cbind(df,Scholarship.holder= input$scholp)
          }
          if(input$age==1){
            df <- cbind(df,Age.at.enrollment= input$agep)
          }
          if(input$int==1){
            df <- cbind(df,International= input$intp)
          }
          if(input$cfsc==1){
            df <- cbind(df,Curricular.units.1st.sem..credited.= input$cfscp)
          }
          if(input$cfse==1){
            df <- cbind(df,Curricular.units.1st.sem..enrolled. = input$cfsep)
          }
          if(input$cfsev==1){
            df <- cbind(df,Curricular.units.1st.sem..evaluations. = input$cfsevp)
          }
          if(input$cfsa==1){
            df <- cbind(df,Curricular.units.1st.sem..approved.=input$cfsap)
          }
          if(input$cfsg==1){
            df <- cbind(df,Curricular.units.1st.sem..grade. = input$cfsgp)
          }
          if(input$cfswoe==1){
            df <- cbind(df,Curricular.units.1st.sem..without.evaluations. = input$cfswoep)
          }
          if(input$cssc==1){
            df <- cbind(df,Curricular.units.2nd.sem..credited. = input$csscp)
          }
          if(input$csse==1){
            df <- cbind(df,Curricular.units.2nd.sem..enrolled. = input$cssep)
          }
          if(input$cssev==1){
            df <- cbind(df,Curricular.units.2nd.sem..evaluations. = input$cssevp)
          }
          if(input$cssa==1){
            df <- cbind(df,Curricular.units.2nd.sem..approved. = input$csap)
          }
          if(input$cssg==1){
            df <- cbind(df,Curricular.units.2nd.sem..grade. = input$cssgp)
          }
          if(input$csswoe==1){
            df <- cbind(df,Curricular.units.2nd.sem..without.evaluations. = input$csswoep)
          }
          if(input$ur==1){
            df <- cbind(df,Unemployment.rate = input$urp)
          }
          if(input$ir==1){
            df <- cbind(df,Inflation.rate =input$irp)
          }
          if(input$gdp==1){
            df <- cbind(df,GDP = input$gdpp)
          }
          
          
          train <- sample(1:nrow(GradDataV2), size = nrow(GradDataV2)*input$prop2)
          test <- setdiff(1:nrow(GradDataV2), train)
          
          # training and testing subsets
          datTrain1 <- GradDataV2[train, ]
          datTest1 <- GradDataV2[test, ]
          
          isolate(
            if(!is.null(df)){
              withProgress(message='Predicting Random Forest',{
                vars <- colnames(df)
                for(i in 1:length(vars)){
                  if(is.numeric(GradDataV2[1,vars[i]])){
                    vars[i] <- paste('as.numeric(',vars[i],')')
                  }
                }
                vars2 <- str_c(vars,collapse='+')
                formula <- paste('Target ~',vars2)
                form2 <- as.formula(formula)
                random.forest.fit <- train(form2, data = datTrain1,
                                           method = "rf",
                                           preProcess = c("center", "scale"),
                                           trControl = trainControl(method = "cv", number = 5),
                                           tuneGrid = data.frame(mtry = 1:37))
                prediction<- predict(random.forest.fit,df,interval='prediction')
                prediction <- as.data.frame(prediction)
                prediction
              })
            })
        })
    })
  })
  
  ## Data Tab - User can scroll through data, subset (rows and cols) and save data (possibly subset)
  data <- reactive({
    df <- c("Student")
    GradDataOut <- GradDataV2
    GradDataOut<- tibble::rownames_to_column(GradDataOut, "row_names")
    colnames(GradDataOut)[1] <- 'Student'
    
    if(input$marpred2==1){
      df <- c(df,"Marital.status")
    }
    if(input$appmode2==1){
      df <- c(df,'Application.mode')
    }
    if(input$appord2==1){
      df <- c(df,'Application.order')
    }
    if(input$course2==1){
      df <- c(df,'Course')
    }
    if(input$dteatten2==1){
      df <- c(df,'Daytime.evening.attendance.')
    }
    if(input$prevq2==1){
      df <- c(df,'Previous.qualification')
    }
    if(input$prevqg2==1){
      df <- c(df,'Previous.qualification..grade.')
    }
    if(input$nac2==1){
      df <- c(df,'Nacionality')
    }
    if(input$mq2==1){
      df <- c(df,'Mother.s.qualification')
    }
    if(input$fq2==1){
      df <- c(df,'Father.s.qualification')
    }
    if(input$mo2==1){
      df <- c(df,'Mother.s.occupation')
    }
    if(input$fo2==1){
      df <- c(df,'Father.s.occupation')
    }
    if(input$ag2==1){
      df <- c(df,'Admission.grade')
    }
    if(input$dp2==1){
      df <- c(df,'Displaced')
    }
    if(input$esn2==1){
      df <- c(df,'Educational.special.needs')
    }
    if(input$deb2==1){
      df <- c(df,'Debtor')
    }
    if(input$tuit2==1){
      df <- c(df,'Tuition.fees.up.to.date')
    }
    if(input$gen2==1){
      df <- c(df,'Gender')
    }
    if(input$schol2==1){
      df <- c(df,'Scholarship.holder')
    }
    if(input$age2==1){
      df <- c(df,'Age.at.enrollment')
    }
    if(input$int2==1){
      df <- c(df,'International')
    }
    if(input$cfsc2==1){
      df <- c(df,'Curricular.units.1st.sem..credited.')
    }
    if(input$cfse2==1){
      df <- c(df,'Curricular.units.1st.sem..enrolled.')
    }
    if(input$cfsev2==1){
      df <- c(df,'Curricular.units.1st.sem..evaluations.')
    }
    if(input$cfsa2==1){
      df <- c(df,'Curricular.units.1st.sem..approved.')
    }
    if(input$cfsg2==1){
      df <- c(df,'Curricular.units.1st.sem..grade.')
    }
    if(input$cfswoe2==1){
      df <- c(df,'Curricular.units.1st.sem..without.evaluations.')
    }
    if(input$cssc2==1){
      df <- c(df,'Curricular.units.2nd.sem..credited.')
    }
    if(input$csse2==1){
      df <- c(df,'Curricular.units.2nd.sem..enrolled.')
    }
    if(input$cssev2==1){
      df <- c(df,'Curricular.units.2nd.sem..evaluations.')
    }
    if(input$cssa2==1){
      df <- c(df,'Curricular.units.2nd.sem..approved.')
    }
    if(input$cssg2==1){
      df <- c(df,'Curricular.units.2nd.sem..grade.')
    }
    if(input$csswoe2==1){
      df <- c(df,'Curricular.units.2nd.sem..without.evaluations.')
    }
    if(input$ur2==1){
      df <- c(df,'Unemployment.rate')
    }
    if(input$ir2==1){
      df <- c(df,'Inflation.rate')
    }
    if(input$gdp2==1){
      df <-c(df,'GDP')
    }
    
    if(!is.null(df)){
      vars <- df
      dt <- subset(GradDataOut, select=vars)
      
    }
    dt <- dt[1:input$rows,]
    
  })
  
  output$dt <-renderDT({
    
    data()
  })
  
  # Downloadable csv of selected dataset ----
  output$download <- downloadHandler(
    filename = function() {
      paste('GradData',".csv", sep = "")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui,server)

