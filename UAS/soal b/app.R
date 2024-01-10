# Panggil library yang diperlukan
library(shiny)
library(shinydashboard)
library(shinyjs)
library(readr)
library(dplyr)
library(ggplot2)

# Fungsi untuk membaca dan memproses file CSV
read_and_process_csv <- function(file_path) {
  # Membaca file CSV dengan pemisah ";"
  data <- readr::read_csv2(file_path)
  
  # Memproses nama kolom
  colnames(data) <- c("Ad.Placement", "Left.Sidebar", "Center.Page", "Right.Sidebar")
  
  # Mengubah data menjadi tipe numerik
  data[, 2:4] <- lapply(data[, 2:4], as.numeric)
  
  return(data)
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Analisis Rasio Klik Tayang"),
  dashboardSidebar(
    fileInput("file", "Unggah File CSV", accept = ".csv"),
    selectInput("location", "Pilih Lokasi Iklan",
                choices = c("Left.Sidebar", "Center.Page", "Right.Sidebar"),
                selected = "Left.Sidebar")
  ),
  dashboardBody(
    useShinyjs(),
    fluidRow(
      box(title = "Visualisasi Rasio Klik Tayang",
          plotOutput("bar_plot", height = 400)
      ),
      box(title = "Hasil Analisis",
          verbatimTextOutput("summary_output")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Memuat data dari file yang diunggah
  data <- reactive({
    req(input$file)
    read_and_process_csv(input$file$datapath)
  })
  
  # Visualisasi Rasio Klik Tayang
  output$bar_plot <- renderPlot({
    ggplot(data(), aes(x = Ad.Placement, y = get(input$location))) +
      geom_bar(stat = "identity", fill = "blue") +
      labs(title = paste("Rasio Klik Tayang -", input$location),
           x = "Ad.Placement", y = "Rasio Klik Tayang")
  })
  
  # Analisis Statistik dan Output Summary Anova
  output$summary_output <- renderPrint({
    # Melakukan analisis statistik dengan Anova
    anova_result <- aov(data()[[input$location]] ~ data()$Ad.Placement)
    
    # Menampilkan hasil summary Anova
    summary(anova_result)
  })
}

# Run aplikasi Shiny
shinyApp(ui, server)
