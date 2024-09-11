
library(stringr)
library(readr)
library(dplyr)
library (teal)

### 

file_name <- read_csv(file.choose(), col_types = cols(Timestamp = col_datetime(format = "%Y-%m-%d %H:%M:%OS"))) 

df = file_name

df <- df[apply(df!=0, 1, all) ,]

###

app <- init(
  data = teal_data(file_name = df),
  modules = list(
    module(
      label = "LC9084 histogram",
      server = function(input, output, session, data) {
        updateSelectInput(session = session,
                          inputId = "var",
                          choices = names(data()[["file_name"]])[1:14])
        
        output$hist <- renderPlot({
          req(input$var)
          hist(x = data()[["file_name"]][[input$var]])
        })
      },
      ui = function(id) {
        ns <- NS(id)
        list(
          selectInput(inputId = ns("var"),
                      label = "Column name",
                      choices = NULL),
          plotOutput(outputId = ns("hist"))
        )
      }
    )
  )
)

### 


shinyApp(app$ui, app$server)
