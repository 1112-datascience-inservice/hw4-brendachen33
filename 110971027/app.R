library(shiny)
library(ggbiplot)
library(FactoMineR)
library(factoextra)
#library(knitr)
library(shinyjs)
library(DT)

# 檢查 ggplot2 套件是否已安裝  
library(ggplot2)  

# 檢查 ggrepel 套件是否已安裝  
library(ggrepel) 

#ui <- fluidPage( tabsetPanel(
#  tabPanel("tab 1", "contents"),
#  tabPanel("tab 2", "contents"),
#  tabPanel("tab 3", "contents")))

# Define UI for app that draws a histogram ----
ui = fluidPage(
  useShinyjs(),
  navbarPage(title = "陳韻清 碩專二 110971027",
    tabPanel("PCA", 
             tabsetPanel(
               tabPanel("PCA Plot",
                        tags$div(
                          tags$h2("PCA"), 
                          tags$h4("x axis"), 
                          actionButton("xaPC1", "PC1",class = "btn-info",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                          actionButton("xaPC2", "PC2",class = "btn-info",style="color: #fff; background-color: #649170; border-color: #4e9c60"),
                          actionButton("xaPC3", "PC3",class = "btn-info",style="color: #fff; background-color: #9e4c6a; border-color: #a04e4a"),
                          actionButton("xaPC4", "PC4", class = "btn-info",style="color: #fff; background-color: #ffd700; border-color: #ffd700"),
                          tags$h4("y axis"), 
                          actionButton("yaPC1", "PC1", class = "btn-info",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                          actionButton("yaPC2", "PC2", class = "btn-info",style="color: #fff; background-color: #649170; border-color: #4e9c60"),
                          actionButton("yaPC3", "PC3", class = "btn-info",style="color: #fff; background-color: #9e4c6a; border-color: #a04e4a"),
                          actionButton("yaPC4", "PC4", class = "btn-info",style="color: #fff; background-color: #ffd700; border-color: #ffd700")
                        ),
                        tags$div(
                          plotOutput(outputId = "distPlot")
                        )
               ),
               tabPanel("result data",
                        tags$div(
                          DTOutput('tbl')
                        )
                ),
               tabPanel("input data",
                        tags$div(
                          DTOutput('tb2')
                        )
               )
    )
  ),
  tabPanel("CA",
          tabsetPanel(
           tabPanel("CA Plot", 
                    tags$h2("CA"), 
                    # Input: Slider for the number of bins ----
                    tags$div(
                      sliderInput(inputId = "ncp",
                                  label = "centers(k):",
                                  min = 3,
                                  max = 10,
                                  value = 3)
                    ),
                    tags$div(
                      plotOutput(outputId = "ca_plot")
                    )
           ),
           tabPanel("extended results", 
                    tags$h2("extended results"),
                    tags$div(
                      sliderInput(inputId = "ncp2",
                                  label = "centers(k):",
                                  min = 3,
                                  max = 10,
                                  value = 3)
                    ),
                    tags$div(
                      verbatimTextOutput("tb3")
                    )
            )
          )
  )
    #tabPanel("iris Data", "contents")
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  v <- reactiveValues(data = NULL, x_axis = 1, y_axis = 2)
  observe({
    hide("xaPC2")
    hide("yaPC1")
  })
  
  observeEvent(input$xaPC1, {
    v$x_axis <- 1
    hide("yaPC1")
    show("yaPC2")
    show("yaPC3")
    show("yaPC4")
  })
  
  observeEvent(input$xaPC2, {
    v$x_axis <- 2
    show("yaPC1")
    hide("yaPC2")
    show("yaPC3")
    show("yaPC4")
  })  
  
  observeEvent(input$xaPC3, {
    v$x_axis <- 3
    show("yaPC1")
    show("yaPC2")
    hide("yaPC3")
    show("yaPC4")
  }) 
  
  observeEvent(input$xaPC4, {
    v$x_axis <- 4
    show("yaPC1")
    show("yaPC2")
    show("yaPC3")
    hide("yaPC4")
  })

  observeEvent(input$yaPC1, {
    v$y_axis <- 1
    hide("xaPC1")
    show("xaPC2")
    show("xaPC3")
    show("xaPC4")
  })
  
  observeEvent(input$yaPC2, {
    v$y_axis <- 2
    show("xaPC1")
    hide("xaPC2")
    show("xaPC3")
    show("xaPC4")
  })  
  
  observeEvent(input$yaPC3, {
    v$y_axis <- 3
    show("xaPC1")
    show("xaPC2")
    hide("xaPC3")
    show("xaPC4")
  }) 
  
  observeEvent(input$yaPC4, {
    v$y_axis <- 4
    show("xaPC1")
    show("xaPC2")
    show("xaPC3")
    hide("xaPC4")
  })    
  
  data(iris)
  # log transform 
  log.ir <- log(iris[, 1:4])
  ir.species <- iris[, 5]
  
  # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
  ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
  PC <- as.data.frame(ir.pca$x)
  
  # Perform Correspondence Analysis
  ir.ca <- CA(iris[, 1:4], graph = FALSE)
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species, choices = c(v$x_axis,v$y_axis))
    g <- g + scale_color_discrete(name = '')
    g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
    print(g)
  })
  
  output$tbl = renderDT(
    PC, options = list(lengthChange = FALSE)
  )
  
  output$tb2 = renderDT(
    log.ir, options = list(lengthChange = TRUE)
  )
 
  output$ca_plot <- renderPlot({
    k <- input$ncp
    model <- kmeans(iris[, 1:4], centers = k)
    cmodel3 <- CA(table(iris$Species, model$cluster), graph = FALSE)
    fviz_ca_biplot(cmodel3, map ="colgreen", arrow = c(FALSE, TRUE),
                   repel = TRUE)
  }) 

  output$tb3 <- renderPrint({
    k <- input$ncp2
    model <- kmeans(iris[, 1:4], centers = k)
    cmodel3 <- CA(table(iris$Species, model$cluster), graph = FALSE)
    #print(model$cluster)
    summary(cmodel3)
  })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)