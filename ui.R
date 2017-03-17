source("Read_diagnosis.R",echo=TRUE)
library(shiny)
library(googleVis)




shinyUI (pageWithSidebar(

  # Application title
  headerPanel("Detection of Diseases And predicting the disease"),
  shinyUI(fluidPage(
    # Sidebar with a slider input for number of bins
    sidebarPanel(
      selectInput('Gas',"which Disease would you make to use for prediction?",
                  choices=c("Carbonmonoxide"= "2.1","Nitricoxide"="21" ,"Abnormal Exhale value of Carbonmonoxide" ="29.3", selected="29.3"),
      sliderInput('Exhalation level', "In which level the gas components exceed the'disease prediction level?",
                  20, min=0.1, max=210, step=1, sep=","),
      sliderInput('Disease classifier', "Set the multiplier on the Disease classifier, listed as '(DC)'. NOTE: Leave the slider on the value of '0.01' to predict based on the value of the current disease classifier:",
                  1, min=.1, max=2, step=.01, sep=","),
      checkboxGroupInput("Predictors", "Choose any of the following predictors to include in your customized disease classification. It is recommended to leave the variable, 'CarbonMonoxide' checked. (There may be an error to predict prices if you do not use all historic data or if the ratios were not found in the dataset):"),
      choices=c("AbnormalExhalelevelofCarbonmonoxide"="2.2","AbnormalExhaleLevelofGas"="29.3","AbnormalExhalelevelofVOC"="11", selected="2.2",sep=','),
      selectInput('Data Set', "Choose which Data is to be viewed", choices=c("AbnormalExhalelevelofCarbonmonoxide"="AEC","AbnormalExhalelevelofGas"="AEG","AbnormalExhalelevelofVOC"="AEV"), selected="AEC",sep=','),
      submitButton(text="Analyze")
    )
                ),


    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table",
                 h4("Data Set"),
                 dataTableOutput("data_table")),
        tabPanel("Disease Classification",
                 plotOutput("regression_Plot"),
                 dataTableOutput("Disease_Classification")),
        tabPanel("Linear Analyzer",
                 h5("May take a couple seconds to load."),
                 plotOutput("regression_plot2"),
                 h5("Using the following variables that you selected, listed below is the 95% disease prediction interval for the gas content and voc present (fit=Predicted gas level; lwr=Lower concentration Interval; upr=Upper concentration Interval):"),
                 verbatimTextOutput("prediction"),
                 h5("Regression Analysis:"),
                 verbatimTextOutput("summary")),
        tabPanel("DiseaseRatioAnalysis",
                 h4("GoogleVis Motion Chart API to analyze predicted disease and facet plot to compare ratios side by side."),
                 htmlOutput("plot_table"),
                 plotOutput("facet_plot"))
      ),


          plotOutput("dist_plot Output")))
        )
   ))



