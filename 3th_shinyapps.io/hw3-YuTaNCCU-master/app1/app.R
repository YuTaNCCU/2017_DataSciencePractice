library(shiny)
library(ggplot2)
#setwd("~/Desktop/app1")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("106356013 Yu Ta"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(

      selectInput(inputId = "X", label = strong("Select X"),
                  choices = c(1,2,3,4),
                  selected = "1"),
      
      selectInput(inputId = "Y", label = strong("Select Y"),
                  choices = c(1,2,3,4),
                  selected = "2")
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "log.ir"',
        helpText("Display 10 records by default.", 
                 br(),
                 "Click the column header to sort a column.")
        
      ),
      conditionalPanel(
        'input.dataset === "ir.pca"',
        helpText("Display 10 records by default.", 
                 br(),
                 "Click the column header to sort a column.")
      ),
      conditionalPanel(
        'input.dataset === "iris"',
        checkboxGroupInput("show_vars", "Columns in log.ir to show:",
                           names(iris), selected = names(iris))
      )
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("iris", DT::dataTableOutput("mytable3")),
        tabPanel("log.ir", DT::dataTableOutput("mytable1")),
        tabPanel("ir.pca", DT::dataTableOutput("mytable2"))
      )
    )
  )
  
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  data(iris)
  log.ir <- log(iris[, 1:4])
  ir.species <- iris[, 5]
  ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)

  
  output$distPlot <- renderPlot({
    if( as.integer(input$X) != as.integer(input$Y)) {
      library(ggbiplot)
      ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species , 
               ellipse = TRUE, circle = TRUE , choices=c(as.integer(input$X),as.integer(input$Y))  ) +
        scale_color_discrete(name = '') +
        theme(legend.direction = 'horizontal', legend.position = 'top')
    }else{
      plot(1:1, 1:1,xlab=paste("PC",as.integer(input$X) ,seq=""),ylab=paste("PC",as.integer(input$X) ,seq=""))
      library(showtext)
      showtext.begin()
      text(1, 1.2, "Please Select Two ", cex = 2, col="skyblue")
      text(1, 0.9, "Different Principle Components", cex = 2, col="skyblue")
      text(1, 1.1, "請選擇兩個", cex = 2, col="skyblue")
      text(1, 0.8, "不同的主成分", cex = 2, col="skyblue")
    }
    
  })
  
  
  # choose columns to display
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(log.ir, options = list(lengthMenu = c(10,20,50,100,150), pageLength = 10))
  })
  
  # sorted columns are colored now because CSS are attached to them
  a <- as.data.frame(ir.pca[5])
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(a, options = list(lengthMenu = c(10,20,50,100,150), pageLength = 10))
  })
  
  # customize the length drop-down menu; display 5 rows per page by default
  output$mytable3 <- DT::renderDataTable({
    DT::datatable(iris[, input$show_vars, drop = FALSE])
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)