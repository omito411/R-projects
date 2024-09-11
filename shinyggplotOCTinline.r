
library(stringr)
library(readr)
library(shiny)
library(shinyjs)
library(scales)
library(shinydashboard)
library(shinyscreenshot)
library(ggplot2)


rm(list = ls())
graphics.off()

### 

file_name <- read_csv(file.choose(), col_types = cols(Timestamp = col_datetime(format = "%Y-%m-%d %H:%M:%OS"))) 

df = file_name

df <- df[apply(df!=0, 1, all) ,]

### 

ui <- shinyUI(
  dashboardPage(
    header = dashboardHeader(),
    sidebar = dashboardSidebar(),
    body = dashboardBody(
      useShinyjs(),
      tags$head(tags$script(
        'var dimension = [0, 0];
        $(document).on("shiny:connected", function(e) {
          dimension[0] = window.innerWidth;
          dimension[1] = window.innerHeight;
          Shiny.onInputChange("dimension", dimension);
        });
        $(window).resize(function(e) {
          dimension[0] = window.innerWidth;
          dimension[1] = window.innerHeight;
          Shiny.onInputChange("dimension", dimension);
        });'
              
      )),
      fluidRow(
        column(6,
          #width = 6,
          fluidRow(
            box("Plot", width = 12, background = "aqua")
          )
        ),
        column(6,
          #width = 6,
          fluidRow(
            box(width = 12, height = "auto", infoBoxOutput("Result"), background = "aqua")
          ),
          fluidRow(
            column(width = 12, selectInput(inputId = "x", label = "X Axis", choices = names(df), selected = "Timestamp"),
                       selectInput(inputId = "y", label = "Y Axis", choices = names(df), selected = "MeanCoatingThickeness1")
          ))
      ),
      fluidRow(
        column(width = 12, actionButton("result1","Generate Result"),
               screenshotButton(label = "Capture entire page"),
               screenshotButton(selector = "#analysis", label = 'Capture plot'),
               downloadButton('downloadPlot','Download Plot'),
               plotOutput("analysis", width = "auto",))
        )))))

-
server <- function(input,output,session){
  output$Result <- renderValueBox({
    valueBox(
      paste("LC9084"),"Result",
    )
  }) 
  addClass(selector = "body", class = "sidebar-collapse")
  data <- reactiveValues()
 
  observeEvent(input$result1,{
    data$plot <- ggplot(data = df, aes_string(x = input$x, y = input$y)) + 
      geom_point(colour = "blue", size = 0.5)+
      scale_color_manual(values = c("#3C6E71", "#70AE6E", "#BEEE62")) +
      scale_y_continuous(breaks = c(0,20,40,60,80,100,120,140,160,180,200))+
      scale_x_datetime(breaks = breaks_width("1 hour"),labels=date_format("%H:%M")) +
      theme(axis.text.x=element_text(size=8, angle = 90),
            axis.text.y=element_text(size=8),
            axis.title=element_text(size=10))+
      theme(legend.position = "none")})
  
  output$analysis  <- renderPlot({  data$plot })
  
  output$downloadPlot <- downloadHandler(
    filename = function(){paste("input$analysis",Sys.Date(), '.pdf',sep='')},
    content = function(file){
      ggsave(file,plot=data$plot, width = 11, height = 4, dpi = 300, units = "in")
    }
  )
}
shinyApp(ui, server)
