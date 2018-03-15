library(shiny)
#setwd("~/Desktop/app1")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("106356013 Yu Ta"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      #sliderInput(inputId = "bins",
       #           label = "Number of bins:",
        #          min = 5,
          #        max = 45,
         #         value = 30),
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
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    #x    <- faithful$waiting
   # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    #hist(x, breaks = bins, col = "skyblue", border = "white",
     #    xlab = "Waiting time to next eruption (in mins)",
       #  main = "Histogram of waiting times")
    
    data(iris)
    log.ir <- log(iris[, 1:4])
    ir.species <- iris[, 5]
    ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
    
    library(ggbiplot)
    ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species , 
                  ellipse = TRUE, circle = TRUE , choices=c(as.integer(input$X),as.integer(input$Y))  ) +
      scale_color_discrete(name = '') +
      theme(legend.direction = 'horizontal', legend.position = 'top')
  
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)