library(shiny)

# UI 定义
ui <- fluidPage(
  titlePanel("My Shiny Application"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("num", "Choose a number", 
                  min = 1, max = 100, value = 50)  # 滑块输入
    ),
    mainPanel(
      plotOutput("plot")  # 显示直方图
    )
  )
)

# Server 定义
server <- function(input, output) {
  output$plot <- renderPlot({
    hist(rnorm(input$num), 
         main = "Histogram", 
         xlab = "Values")  # 直方图
  })
}

# 启动应用
shinyApp(ui = ui, server = server)

