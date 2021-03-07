library(shiny)
druglist <- unique(data$drugName)
##(druglist)
ui <- shinyUI(
    
      pageWithSidebar(
        headerPanel("Drug Effectiveness Prediction"),
        sidebarPanel(
          selectizeInput(inputId = 'drugname',
                         label = 'Drug Name',
                         choices = druglist,
                         selected = NULL,
                         multiple = FALSE, # allow for multiple inputs,
                         options = list(create = FALSE)),
          sliderInput("rating","Please select rating: ",
                      min = 1, max = 10, value = 10, step = 1),
          textAreaInput(
            inputId = 'review',
            label = 'Your Review',
            value = "",
            width = NULL,
            height = NULL,
            cols = 10,
            rows = 5,
            placeholder = 'Review',
            resize = NULL
          ),
          selectInput(
            inputId='model',
            label='Select Method',
            choices=c('Method1','Method2','Method3','Method4'),
            selected = 'SVM',
            multiple = FALSE,
            selectize = TRUE,
            width = NULL,
            size = NULL
          ),
          submitButton(text = "Apply", icon = NULL, width = NULL)
        ),
        # mainPanel("Results",
        #     plotOutput('plot')          
        # )
        
        mainPanel(
                  #tableOutput("outtext"),
                  tableOutput("outtable"),
                  tableOutput("sentiment"),
                  tableOutput("condition"),
                  plotOutput('plot') 
                  )
      )
      
    )