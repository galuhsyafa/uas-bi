library(shiny)
library(shinydashboard)
library(lmtest)
library(car)

# Create a data frame from the given data
data <- data.frame(
  month = factor(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
  x1 = c(150000, 160000, 170000, 180000, 190000, 200000, 210000, 220000, 230000, 240000, 250000, 260000),
  x2 = c(8000, 9500, 10000, 10500, 11000, 9000, 11500, 12000, 12500, 13000, 14000, 15000),
  x3 = c(5.0, 4.5, 4.8, 4.6, 5.1, 4.7, 4.9, 5.0, 5.2, 5.3, 5.4, 5.5),
  x4 = c(8.5, 8.2, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 8.7, 8.8, 8.9, 9.0),
  x5 = c(20000, 22000, 25000, 23000, 30000, 28000, 27000, 35000, 40000, 45000, 50000, 60000),
  y = c(120, 150, 160, 165, 180, 170, 190, 210, 230, 250, 300, 350)
)

# Estimate multiple linear regression model
model <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = data)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Sales Prediction Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Prediction", tabName = "prediction", icon = icon("line-chart")),
      menuItem("Interaction Test", tabName = "interaction", icon = icon("area-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "prediction",
              fluidPage(
                titlePanel("Sales Prediction"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("input_x1", "Number of Visitors", choices = unique(data$x1), selected = unique(data$x1)[1]),
                    selectInput("input_x2", "Number of Transactions", choices = unique(data$x2), selected = unique(data$x2)[1]),
                    selectInput("input_x3", "Average Items per Transaction", choices = unique(data$x3), selected = unique(data$x3)[1]),
                    selectInput("input_x4", "Customer Satisfaction Rating", choices = unique(data$x4), selected = unique(data$x4)[1]),
                    selectInput("input_x5", "Number of Online Ads", choices = unique(data$x5), selected = unique(data$x5)[1]),
                    selectInput("input_interaction_var1", "Variable 1 for Interaction", choices = colnames(data)[-c(1,7)], selected = colnames(data)[-c(1,7)][1]),
                    selectInput("input_interaction_var2", "Variable 2 for Interaction", choices = colnames(data)[-c(1,7)], selected = colnames(data)[-c(1,7)][2])
                  ),
                  mainPanel(
                    h4("Predicted Sales:"),
                    verbatimTextOutput("output_prediction"),
                    HTML("<hr />"),
                    h4("Summary:"),
                    verbatimTextOutput("output_summary"),
                    HTML("<hr />"),
                    h4("Assumptions:"),
                    verbatimTextOutput("output_assumptions"),
                    HTML("<hr />"),
                    h4("Coefficients Test:"),
                    verbatimTextOutput("output_coefficients_test"),
                    HTML("<hr />"),
                    h4("RMSE:"),
                    verbatimTextOutput("output_rmse")
                  )
                )
              )
      ),
      tabItem(tabName = "interaction",
              fluidPage(
                titlePanel("Interaction Test"),
                mainPanel(
                  h4("Interaction Test Results:"),
                  verbatimTextOutput("output_interaction_test")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  output$output_prediction <- renderText({
    new_data <- data.frame(
      x1 = as.numeric(input$input_x1),
      x2 = as.numeric(input$input_x2),
      x3 = as.numeric(input$input_x3),
      x4 = as.numeric(input$input_x4),
      x5 = as.numeric(input$input_x5)
    )
    predicted_sales <- predict(model, newdata = new_data)
    paste0("Estimated Sales: $", round(predicted_sales, 2))
  })
  
  output$output_summary <- renderPrint({
    summary(model)
  })
  
  # Residual normality test
  residuals <- residuals(model)
  shapiro_test <- shapiro.test(residuals)
  
  # Homoskedasticity test
  white_test <- lmtest::bptest(model)
  
  # Independence test using VIF
  vif_values <- car::vif(model)
  independence_test <- all(vif_values < 10)
  
  output$output_assumptions <- renderText({
    paste("Shapiro-Wilk Test for Normality:", "\n", "p-value =", shapiro_test$p.value, "\n\n",
          "White's Test for Homoskedasticity:", "\n", "p-value =", white_test$p.value, "\n\n",
          "VIF Test for Independence:", "\n", "All VIF values < 10:", independence_test, "\n\n")
  })
  
  # Individual coefficient significance test
  coef_test <- lmtest::coeftest(model)
  
  output$output_coefficients_test <- renderPrint({
    coef_test
  })
  
  # Calculate and display RMSE
  output$output_rmse <- renderText({
    predicted_sales <- predict(model, newdata = data)
    rmse <- sqrt(mean((data$y - predicted_sales)^2))
    paste0("RMSE: ", round(rmse, 2))
  })
  
  # Interaction test between two variables selected by the user
  interact_formula <- reactive({
    as.formula(paste("y ~ x1 + x2 + x3 + x4 + x5 +", input$input_interaction_var1, "*", input$input_interaction_var2))
  })
  
  model_interact <- reactive({
    lm(interact_formula(), data = data)
  })
  
  interaction_test <- reactive({
    anova(model, model_interact())
  })
  
  output$output_interaction_test <- renderPrint({
    interaction_test()
  })
}

# Run Shiny app
shinyApp(ui, server)
