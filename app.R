options(shiny.maxRequestSize=30*1024^2)            
library(shiny)
library(DT)
library(psych)
library(shinydashboard)
library(shinythemes)

ui<- dashboardPage(
  dashboardHeader(title = "Rshiny : EDA"),
  dashboardSidebar(),
  dashboardBody(fluidPage(theme = shinytheme("cerulean"),
  titlePanel("Exploratory Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file', 'Choose CSV File',
                accept=c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv')),
      uiOutput('select'),
      textOutput("selected_var"),
      helpText("Notes:"),
      helpText("1.Multivariate Analysis tab will take some time"),
      helpText("2.Use less amount of data in csv format")),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Data",
                           DT::dataTableOutput('Data')),
                  tabPanel("Summary", verbatimTextOutput("summary")) ,  
                  tabPanel("Summary Plot ",plotOutput("plot")),
                  tabPanel("Univariate Analysis", plotOutput("plot1")),
                  tabPanel("Multivariate Analysis", plotOutput("plot2")))
          )
       )
    )
  )
)

server <- function(input,output,session){
  Data <- reactive({
    inFile <- input$file
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath,header = TRUE)
    data
  })
  output$Data <- DT::renderDataTable({
    DT::datatable(Data(),options = list(scrollX = TRUE,pageLength = 5))       
  })
  output$summary <- renderPrint({
    summary(Data())
  })
  output$select <- renderUI({
    df <- Data()
    selectInput("variable", "Variable:",names(df))
  })
  output$plot <- renderPlot({
    df <- Data()
    df <- df[,input$variable]
   boxplot(df,col="blue")
  })
 output$plot1 <- renderPlot({
    df <- Data()
    df <- df[,input$variable]
    hist(df,col="green")
  })
 output$plot2 <- renderPlot({
   df <- Data()
   pairs.panels(df,gap=0)
 })
}
shinyApp(ui,server)