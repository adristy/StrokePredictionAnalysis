library(shiny)
library(shinydashboard)
library(shinythemes)
library(dashboardthemes)
library(DT)
library(ggplot2)
library(dplyr)
library(reshape2)
library(corrplot)
library(caret)
library(randomForest)
library(plotly)
library(rsconnect)
library(echarts4r)
library(tidyr)
library(scales)


#setwd("D:/Kuliah/SEM 7/Datmin/Final Project")
df_raw = read.csv("healthcare-dataset-stroke-data.csv")
df_bersih = read.csv("datastrokeclean.csv")
colnames(df_bersih) = c("Gender", "Age", "Hypertension", "Heart Disease", "Married",
                        "Work Type", "Residence Type", "Glucose", 
                        "BMI","Smoking Status", "Stroke")
df_bersih = df_bersih %>%
  mutate(`Hypertension` = recode(`Hypertension`,
                                 "Yes" = "Iya",
                                 "No" = "Tidak"),
         `Heart Disease` = recode(`Heart Disease`,
                                  "Yes" = "Iya",
                                  "No" = "Tidak"))
df_encoded = read.csv("resampled_data.csv")
head(df_bersih)

# random forest untuk prediksi
train_index <- createDataPartition(df_encoded$Stroke, p = 0.8, list = FALSE)
train_data <- df_encoded[train_index, ]
test_data <- df_encoded[-train_index, ]
train_data$Stroke <- as.factor(train_data$Stroke)
test_data$Stroke <- as.factor(test_data$Stroke)
train_data$Hypertension <- as.factor(train_data$Hypertension)
test_data$Hypertension <- as.factor(test_data$Hypertension)
train_data$Heart.Disease <- as.factor(train_data$Heart.Disease)
test_data$Heart.Disease <- as.factor(test_data$Heart.Disease)

# Model formula
formula <- Stroke ~ Age + Hypertension + Average.Glucose.Level + BMI + Heart.Disease
tuneGrid <- expand.grid(mtry = c(2, 3, 4, 5))
set.seed(123)
rf_model <- train(formula, data = train_data, method = "rf", tuneGrid = tuneGrid, trControl = trainControl(method = "cv"))

# Define UI
ui <- navbarPage(
  title = div(
    style = "display: flex; align-items: center; height: 70px; padding: 0px; margin-bottom: 20px;",
    div(
      style = "display: flex; align-items: center;",
      imageOutput("logo", inline = TRUE),
      span(style = "margin-left: 5px; font-size: 22px; font-weight: bold; color: #5e81ac; margin-bottom: 30px", "Stroke Analysis")
    )
    )
  ,
  #theme = shinythemes::shinytheme("flatly"),
  tags$head(
    tags$style(tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;500;600&display=swap"),
    HTML("
      @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@400;600&display=swap');

      /* General Body Styling */
      body {
        font-family: 'Poppins', sans-serif; 
        background-color: #f9f9f9;
      }

      /* Navbar Styling */
      .navbar {
        position: sticky;
        top: 0;
        z-index: 1000;
        height: 70px; /* Tinggi navbar */
        align-items: center; /* Semua elemen sejajar vertikal */
        justify-content: flex-start; /* Semua elemen berada di kiri */
        padding: 0px; /* Tambahkan padding horizontal */
        box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); /* Shadow untuk navbar */
        background-color: #ffffff; /* Warna latar navbar */
      }
      .navbar-header {
        align-items: center; /* Logo dan teks sejajar vertikal */
      }
      .navbar-nav {
        margin-top: 15px;
        margin-left: auto; /* Geser menu ke kanan */
        list-style: none; /* Hilangkan bullet point */
        padding-left: 0; /* Hilangkan padding default */
      }

      .navbar-nav > li > a {
        color: #5e81ac !important; /* Warna teks menu */
        font-weight: normal; /* Tidak bold */
        padding: 10px 15px; /* Tambahkan padding menu */
        border-radius: 20px; /* Bentuk lonjong */
        transition: all 0.3s ease-in-out; /* Animasi smooth */
        text-decoration: none; /* Hilangkan garis bawah */
      }

      .navbar-nav > li > a:hover {
        background-color: #eceff4 !important; /* Background saat hover */
        color: #5e81ac !important; /* Warna teks tetap */
      }

      .navbar-nav > li.active > a {
        background-color: #5e81ac !important; /* Background biru untuk menu aktif */
        color: #ffffff !important; /* Teks putih */
        font-weight: bold !important; /* Bold teks */
        border-radius: 20px; /* Bentuk lonjong */
      }
      
      /* Card Styling */
      .card {
        border-radius: 15px; 
        border: 1px solid #d8dee9; 
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        margin-bottom: 20px;
      }
      .card-header {
        background-color: #5e81ac; 
        color: white; 
        font-weight: bold; 
        font-size: 16px;
        border-radius: 15px 15px 0 0;
        padding: 15px;
      }
      .card-body {
        margin-top: -10px;
        padding: 20px;
      }

      /* Button Styling */
      .custom-btn {
        background-color: #5e81ac;
        border-color: #5e81ac;
        border-radius: 8px;
        font-weight: bold;
        font-size: 14px;
      }
      .custom-btn:hover {
        background-color: #4c566a;
        border-color: #4c566a;
      }

      /* Form Inputs */
      .form-control {
        border-radius: 8px;
      }

      /* Title and Alignment */
      .card-title {
        text-align: center;
        font-size: 20px;
        margin-bottom: 20px;
        font-weight: bold;
      }
      .small-box {
        border-radius: 10px;
        height: 90px; /* Sesuaikan tinggi */
        padding: 10px 15px; /* Sesuaikan padding */
        display: flex;
        flex-direction: column;
        justify-content: flex-end; /* Teks turun ke bawah */
        position: relative;
        overflow: hidden;
    }
    
    .small-box .inner {
        position: relative; /* Posisi relatif untuk teks */
        z-index: 2; /* Pastikan teks di atas ikon */
        text-align: left; /* Sesuaikan teks */
        padding-bottom: 10px; /* Kurangi padding bawah */
    }
    
    .small-box .inner h3 {
        font-size: 30px; /* Ukuran angka besar */
        margin: 0 0 0 0; /* Tambahkan margin atas untuk menurunkan angka */
        font-weight: bold;
        z-index: 3;
    }
    
    .small-box .inner p {
        font-size: 18px; /* Ukuran subtitle */
        margin: 0; /* Hilangkan margin default */
        z-index: 2;
    }
    
    .small-box .icon-large {
        font-size: 70px; /* Ukuran ikon lebih besar */
        position: absolute; /* Posisi absolut untuk bebas mengatur lokasi */
        top: 5px; /* Geser ikon ke atas */
        right: 10px; /* Geser ikon ke kanan */
        color: rgba(0, 0, 0, 0.1); /* Warna hitam transparan 50% */
        z-index: 1; /* Ikon berada di bawah teks */
    }

          
      .small-box.bg-red { background-color: #dd4b39 !important; color: #fff !important; }
      .small-box.bg-green { background-color: #00a65a !important; color: #fff !important; }
      .small-box.bg-yellow { background-color: #f39c12 !important; color: #fff !important; }
      .small-box.bg-blue { background-color: #0073b7 !important; color: #fff !important; }
      .small-box.bg-aqua { background-color: #00c0ef !important; color: #fff !important; }
      .small-box.bg-purple { background-color: #605ca8 !important; color: #fff !important; }
      .small-box.bg-light-blue { background-color: #3c8dbc !important; color: #fff !important; }

    "))
  ),
  
  # Home Page
  tabPanel("About",
           fluidPage(
             div(
               class = "card",
               div(class = "card-header", "Prediksi Risiko Stroke : Analisis dan Faktor-Faktor Penyebabnya"),
               div(
                 class = "card-body",
                 h2(style = "text-align: left;font-weight: bold;", class = "card-title", "Apa Itu Stroke?"),
                 div(
                   style = "display: flex; justify-content: center;;margin-bottom: 30px",
                   tags$iframe(
                     width = "800", height = "450",  # Ukuran video diperbesar
                     src = "https://www.youtube.com/embed/EsshJLm5CN8", 
                     frameborder = "0", 
                     allow = "accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", 
                     allowfullscreen = TRUE
                   )
                 ),
                 p(HTML("<b>Stroke adalah salah satu penyebab utama kematian dan kecacatan di seluruh dunia.</b> Dengan 
                   memahami faktor-faktor yang berkontribusi terhadap risiko stroke, kita dapat memberikan 
                   pencegahan lebih dini dan meningkatkan kualitas hidup pasien. Dalam proyek ini, kami melakukan 
                   analisis prediksi stroke dengan menggunakan dataset medis yang mengidentifikasi berbagai faktor 
                   risiko yang mempengaruhi terjadinya stroke.")),
                 p(HTML("Stroke terjadi ketika pasokan darah ke otak terganggu, yang dapat disebabkan oleh dua hal utama yakni 
                   <b>penyumbatan pembuluh darah</b> (stroke iskemik) atau <b>pecahnya pembuluh darah</b> (stroke hemoragik). Kondisi ini 
                   dapat menyebabkan kerusakan otak yang serius, gangguan pada fungsi motorik, bicara, bahkan kematian. 
                   Oleh karena itu, memahami faktor-faktor penyebab stroke sangat penting untuk pengelolaan kesehatan yang lebih baik.")),
                 # Menambahkan Tujuan Proyek dengan judul <h2>
                 h2(style = "text-align: left;font-weight: bold;", class = "card-title","Tujuan Proyek"),
                 p(HTML("Proyek ini bertujuan untuk menganalisis <b>faktor-faktor risiko dan membangun model prediksi stroke</b> berdasarkan 
                        data medis. Kami menggunakan dataset yang berisi informasi terkait usia, tekanan darah, hipertensi, riwayat 
                        penyakit jantung, berat serta tinggi badan, dan pola hidup lainnya.")),
                 
                 # Langkah-langkah proyek
                 h2(style = "text-align: left;font-weight: bold;", class = "card-title", "Langkah Analisis"),
                 p("Berikut adalah langkah analisis yang dilakukan pada proyek ini"),
                 tags$ol(
                   tags$li(HTML("<b>Pre-Processing Data:</b> Mengatasi data yang hilang dan melakukan konversi variabel.")),
                   tags$li(HTML("<b>Eksplorasi Data (EDA):</b> Meneliti data untuk mengidentifikasi hubungan antar variabel.")),
                   tags$li(HTML("<b>Seleksi Variabel:</b> Menentukan variabel-variabel yang berpengaruh.")),
                   tags$li(HTML("<b>Pembangunan Model Prediksi:</b> Menggunakan teknik pembelajaran mesin untuk membangun model yang dapat memprediksi risiko stroke.")),
                   tags$li(HTML("<b>Evaluasi Model:</b> Mengukur akurasi dan performa model dengan berbagai metrik seperti akurasi, precision, recall, dan F1-score.")),
                   tags$li(HTML("<b>Memprediksi Data Baru:</b> Menggunakan model untuk memprediksi data baru yang belum pernah dilihat sebelumnya."))
                 )
             ))
           )
  ),
  
  # About Page
  tabPanel("Summary",
           fluidPage(
             tabsetPanel(
               # Tab pertama: Statistik
               tabPanel("Dashboard",
                        fluidPage(
                          style = "margin-top: 10px;",
                          # Row 1: Filter Stroke dan Metrics
                          fluidRow(
                            box(
                              class = "card",
                              width = 3,
                              div(class = "card-header", "Filter Stroke", style="height: 35px; display: flex; align-items: center; "),
                              div(
                                class = "card-body",
                                style = "
                                  display: flex; 
                                  height: 50px; /* Sesuaikan tinggi area putih */
                                  padding: 5px;
                                  margin-top: 5px;
                                  border: none;
                                ",
                                status = "primary",
                                solidHeader = TRUE,
                                selectInput("filter", label = NULL, choices = c("All", "Yes", "No"))
                              )
                            ),
                            valueBoxOutput(outputId = "jumlah_pasien", width = 3),
                            valueBoxOutput(outputId = "rata2_usia", width = 3),
                            valueBoxOutput(outputId = "rata2_bmi", width = 3)
                          ),
                          fluidRow(style = "margin-top: 20px;",
                            box(
                              width = 4,
                              style = "
                                background-color: white; /* Background color */
                                border-radius: 15px; /* Rounded corners */
                                box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); /* Soft shadow */
                                padding: 5px; /* Adjust padding */
                                border: 1px solid #d8dee9; /* Thin border */
                                display: flex;
                                justify-content: center; /* Center the content */
                                align-items: center; /* Vertically center the content */
                                height: 300px;
                              ",
                              div(
                              style = "
                                  width: 100%; 
                                  height: 100%; 
                                  display: flex; 
                                  justify-content: center; 
                                  align-items: center; 
                                ",  
                                # Center the gauge chart inside the box
                                plotlyOutput("gaugeChart", width = "85%", height = "230px")  # Ensure proportional size
                              )
                            ),
                            box(
                              width = 8,
                              style = "
                                background-color: white; /* Background color */
                                border-radius: 15px; /* Rounded corners */
                                box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); /* Soft shadow */
                                padding: 5px; /* Adjust padding */
                                border: 1px solid #d8dee9; /* Thin border */
                                display: flex;
                                justify-content: center; /* Center the content */
                                align-items: center; /* Vertically center the content */
                                height: 300px;
                              ",
                              plotOutput("histogramAge", height = "300px") # Output histogram
                            )
                            ),
                          fluidRow(style = "margin-top: 20px;",
                              box(width = 4,
                                style = "
                                background-color: white; /* Background color */
                                border-radius: 15px; /* Rounded corners */
                                box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); /* Soft shadow */
                                padding: 5px; /* Adjust padding */
                                border: 1px solid #d8dee9; /* Thin border */
                                display: flex;
                                justify-content: center; /* Center the content */
                                align-items: center; /* Vertically center the content */
                                height: 300px;
                              ", plotOutput("hipertensi", height = "300px")),
                            box(width = 4,
                                style = "
                                background-color: white; /* Background color */
                                border-radius: 15px; /* Rounded corners */
                                box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); /* Soft shadow */
                                padding: 5px; /* Adjust padding */
                                border: 1px solid #d8dee9; /* Thin border */
                                display: flex;
                                justify-content: center; /* Center the content */
                                align-items: center; /* Vertically center the content */
                                height: 300px;
                              ", plotOutput("heartDisease", height = "300px")),
                            box(width = 4,
                                style = "
                                background-color: white; /* Background color */
                                border-radius: 15px; /* Rounded corners */
                                box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); /* Soft shadow */
                                padding: 5px; /* Adjust padding */
                                border: 1px solid #d8dee9; /* Thin border */
                                display: flex;
                                justify-content: center; /* Center the content */
                                align-items: center; /* Vertically center the content */
                                height: 300px;
                              ",
                                echarts4rOutput("gender", height = "300px"))
                          ),
                          fluidRow(style = "margin-top: 20px;margin-bottom: 20px;",
                              box(width = 7,
                                style = "
                                background-color: white; /* Background color */
                                border-radius: 15px; /* Rounded corners */
                                box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); /* Soft shadow */
                                padding: 5px; /* Adjust padding */
                                border: 1px solid #d8dee9; /* Thin border */
                                display: flex;
                                justify-content: center; /* Center the content */
                                align-items: center; /* Vertically center the content */
                                height: 300px;
                              ",
                                plotOutput("glucose", height = "300px")),
                            box(width = 5,
                                style = "
                                background-color: white; /* Background color */
                                border-radius: 15px; /* Rounded corners */
                                box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); /* Soft shadow */
                                padding: 5px; /* Adjust padding */
                                border: 1px solid #d8dee9; /* Thin border */
                                display: flex;
                                justify-content: center; /* Center the content */
                                align-items: center; /* Vertically center the content */
                                height: 300px;
                              ",
                                plotOutput("bmi_plot", height = "300px"))
                          )
                          )
  ),
                          
               
               # Tab kedua: Model
            tabPanel("Model",
                     fluidPage(
                       fluidRow(
                         div(
                           class = "card", style = "margin-top: 10px;",
                           div(
                             class = "card-header", 
                             h4("Best Model: Random Forest Classification", style = "font-weight: bold;")
                           ),
                           div(
                             class = "card-body",
                             p("Model Random Forest dipilih karena menghasilkan performa terbaik dibandingkan dengan model lainnya, seperti XG Boost, Regresi Logistik, dan Decision Tree."),
                             br(),
                             h5("Rincian Performa Model:", style = "font-weight: bold;"),
                             tags$ul(
                               tags$li("Accuracy: 0.9909574468085106"),
                               tags$li("Sensitivity: 1.0"),
                               tags$li("Specificity: 0.9815618221258134"),
                               tags$li("ROC AUC: 1.0")
                             ),
                             br(),
                             fluidRow(
                               # Gambar Confusion Matrix
                               column(
                                 width = 6,
                                 box(
                                   width = 12,
                                   style = "
                                   background-color: white; 
                                   border-radius: 15px; 
                                   box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); 
                                   padding: 10px; 
                                   border: 1px solid #d8dee9;
                                   text-align: center;
                                   height: 400px;",
                                   h5("Confusion Matrix", style = "margin-bottom: 10px; font-weight: bold;"),
                                   imageOutput("confusionMatrix", width = "100%", height = "auto")
                                 )
                               ),
                               # Gambar ROC Curve
                               column(
                                 width = 6,
                                 box(
                                   width = 12,
                                   style = "
                                   background-color: white; 
                                   border-radius: 15px; 
                                   box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); 
                                   padding: 10px; 
                                   border: 1px solid #d8dee9;
                                   text-align: center;
                                   height: 400px;",
                                   h5("ROC Curve", style = "margin-bottom: 10px; font-weight: bold;"),
                                   imageOutput("rocCurve", width = "100%", height = "auto")
                                 )
                               )
                             )
                           )
                         )
                       )
                     ))
           )
  )),
  
  # Prediction Page
  tabPanel("Prediction",
           fluidPage(
             div(
               class = "card",
               div(class = "card-header", "Prediksi Resiko Stroke"),
               div(
                 class = "card-body",
                 fluidRow(
                   column(3,
                          numericInput("input_age", "Usia:", value = NULL, min = 0, max = 120),
                          selectInput("input_hypertension", "Riwayat Hipertensi:", choices = c("Tidak" = 0, "Ya" = 1)),
                          numericInput("input_avg_glucose", "Gula Darah:", value = NULL, min = 0, max = 300),
                          selectInput("input_heart.disease", "Riwayat Penyakit Jantung:", choices = c("Tidak" = 0, "Ya" = 1)),
                          numericInput("height", "Tinggi Badan (cm):", value = NULL, min = 100, max = 250),
                          numericInput("weight", "Berat Badan (kg):", value = NULL, min = 30, max = 200),
                          actionButton("button_pred", "Prediksi Resiko")
                   ),
                   column(9,
                          uiOutput("hasil_pred"),
                          uiOutput("recommendation")
                   )
                 )
               )
             )
           )
  ),
  tabPanel("Dataset",
           fluidPage(
             div(
               class = "card",
               div(class = "card-header", "About Dataset"),
               div(
                 class = "card-body",
                 h2(style = "text-align: left;font-weight: bold;", class = "card-title", "Stroke Prediction Dataset"),
                 p(HTML("Dataset Stroke Prediction yang tersedia di Kaggle berisi data medis yang bertujuan untuk memprediksi risiko stroke 
                        pada individu berdasarkan berbagai faktor risiko. Dataset ini mencakup 5110 data dengan 12 fitur yang meliputi 
                        demografis, medis, dan gaya hidup pasien, seperti usia, jenis kelamin, tekanan darah tinggi, riwayat penyakit jantung, 
                        status pernikahan, tingkat glukosa rata-rata, indeks massa tubuh (BMI), dan status merokok. Label target dalam dataset 
                        ini adalah apakah pasien mengalami stroke atau tidak. Dengan menggunakan dataset ini, kita dapat membangun model 
                        prediksi untuk mengidentifikasi individu yang berisiko tinggi terkena stroke, yang sangat berguna dalam upaya 
                        pencegahan dan penanganan dini. Faktor-faktor risiko seperti hipertensi, diabetes, dan pola hidup yang tidak 
                        sehat tercatat dalam dataset ini, memungkinkan analisis lebih dalam tentang penyebab utama stroke. Dataset ini dapat diakses di 
                        <a href='https://www.kaggle.com/datasets/fedesoriano/stroke-prediction-dataset' target='_blank'>link ini</a>.")),
                 DTOutput("table1")
               )
             )
           )
  ),
  tabPanel("Author",
           fluidPage(
             div(
               style = "position: relative; height: 100vh; width: 100%;",
               
               # Gambar Author di kiri bawah
               div(
                 style = "position: absolute; top: 5%; left: 0;",
                 imageOutput("authorImage", width = "300px")
               ),
               
               # Tulisan di kanan
               div(
                 style = "position: absolute; top: 20%; right: 0; transform: translateY(-50%); padding-right: 20px; text-align: right;",
                 h3(style = "font-weight: bold; font-size: 25px; margin: 0;", "Data Mining dan Visualisasi B - 5003211082"),
                 h1(style = "font-weight: bold; font-size: 80px; color: #6A8EC8; margin: 10px 0;", "Adristy Rizki")
               ),
               
               # Contact Me di kanan bawah
               div(
                 style = "position: absolute; top: 65%; right: 30px; text-align: right;",
                 h4(style = "font-weight: bold;margin-bottom: 10px;", "Contact Me!"),
                 div(
                   style = "display: flex; justify-content: flex-end; gap: 10px;",
                   
                   # Gmail
                   tags$a(
                     href = "mailto:adristyrizki6@gmail.com", target = "_blank",
                     imageOutput("gmailLogo", inline = TRUE, width = "20px")
                   ),
                   
                   # LinkedIn
                   tags$a(
                     href = "https://www.linkedin.com/in/adristy-rizki-fahriyah-2846aa237/", target = "_blank",
                     imageOutput("linkedinLogo", inline = TRUE, width = "20px")
                   ),
                   
                   # Instagram
                   tags$a(
                     href = "https://www.instagram.com/adristyrizki", target = "_blank",
                     imageOutput("instagramLogo", inline = TRUE, width = "20px")
                   )
                 )
               )
             )
           )
  )
)

# Define Server
server <- function(input, output, session) {
  # data table
  output$table1 <- renderDT({
    df_raw <- read.csv("healthcare-dataset-stroke-data.csv")
    DT::datatable(df_raw, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Function to calculate BMI from height and weight
  calculate_bmi <- function(height, weight) {
    height_m <- height / 100  # Convert height from cm to meters
    bmi <- weight / (height_m^2)
    return(bmi)
  }
  
  # Reactive expression for prediction
  hasil_prediksi <- reactive({
    req(input$input_age, input$input_hypertension, input$input_avg_glucose, input$height, input$weight, input$input_heart.disease)
    
    # Calculate BMI based on height and weight
    bmi_value <- calculate_bmi(input$height, input$weight)
    
    # Prepare data for prediction
    new_data <- data.frame(
      Age = input$input_age,
      Hypertension = as.factor(input$input_hypertension),
      Average.Glucose.Level = input$input_avg_glucose,
      BMI = bmi_value,
      Heart.Disease = as.factor(input$input_heart.disease),
      Stroke = NA  
    )
    
    # Make prediction using the Random Forest model
    prediksi <- predict(rf_model, newdata = new_data)
    
    # Return prediction result
    ifelse(prediksi == 1, "Anda berisiko terkena stroke.", "Anda tidak berisiko terkena stroke.")
  })
  
  # Output prediction result with style for text color, size, and additional recommendations
  observeEvent(input$button_pred, {
    result <- hasil_prediksi()
    
    # Generate the recommendation and video link
    recommendation <- if (result == "Anda berisiko terkena stroke.") {
      tags$div(
        tags$br(),
        style = "font-size: 18px; font-weight: bold;",
        "Rekomendasi : ",
        tags$br(),
        tags$div(
          style = "font-size: 14px; font-weight: normal; color: black;",
          p("Anda diprediksi terkena stroke, sehingga : "),
          tags$ul(
            tags$li("Segera konsultasikan dengan dokter untuk evaluasi lebih lanjut."),
            tags$li("Rutin berolahraga untuk menjaga kesehatan jantung dan pembuluh darah."),
            tags$li("Makan sehat dengan mengurangi konsumsi garam, lemak jenuh, dan gula berlebih."),
            tags$li("Mengurangi stres melalui teknik relaksasi seperti meditasi dan yoga.")
          )
        ),
        tags$br(),
        tags$div(
          style = "font-size: 16px; font-weight: bold; color: red;",
          "Segera Konsultasikan ke dokter apabila anda memiliki gejala berikut!"
        ),
        tags$br(),
        tags$div(
          style = "position: relative; padding-bottom: 56.25%; height: 0; overflow: hidden; max-width: 100%;",
          tags$iframe(
            src = "https://www.youtube.com/embed/2Z_zKxSqgCo", 
            style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; border: none;", 
            allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture", 
            allowfullscreen = TRUE
          )
        )
      )
    } else {
      tags$div(
        tags$br(),
        style = "font-size: 18px; font-weight: bold;",
        "Rekomendasi : ",
        tags$div(
          style = "font-size: 14px; font-weight: normal; color: black;",
          p("Meskipun anda tidak beresiko terkena stroke, namun tetap penting untuk : "),
          tags$ul(
            tags$li("Tetap menjaga gaya hidup sehat dengan olahraga teratur."),
            tags$li("Makan makanan bergizi seperti buah, sayur, dan biji-bijian."),
            tags$li("Lakukan pemeriksaan kesehatan rutin untuk mendeteksi potensi masalah kesehatan.")
          )
        ),
        tags$br(),
        tags$div(
          style = "font-size: 16px; font-weight: bold; color: red;",
          "Segera Konsultasikan ke dokter apabila anda memiliki gejala berikut!"
        ),
        tags$br(),
        tags$div(
          style = "position: relative; padding-bottom: 56.25%; height: 0; overflow: hidden; max-width: 100%;",
          tags$iframe(
            src = "https://www.youtube.com/embed/2Z_zKxSqgCo", 
            style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; border: none;", 
            allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture", 
            allowfullscreen = TRUE
          )
        )
      )
    }
    
    
    # Render the result and recommendation UI
    output$hasil_pred <- renderUI({
      if (result == "Anda berisiko terkena stroke.") {
        tags$div(
          style = "font-size: 20px; font-weight: bold; color: white; background-color: red; padding: 10px; border-radius: 10px;text-align: center;",
          result
        )
      } else {
        tags$div(
          style = "font-size: 20px; font-weight: bold; color: white; background-color: green; padding: 10px; border-radius: 10px;text-align: center;",
          result
        )
      }
    })
    
    # Add the recommendation and video link below the result
    output$recommendation <- renderUI({
      recommendation
    })
  })
  
  output$logo <- renderImage({
    list(src="www/stroke logo.png", height = "40px", style = "margin-right: 0px; margin-bottom: 30px")
  },deleteFile = F)
  
  # Gambar Author
  output$authorImage <- renderImage({
    filename <- "www/Profile.png"  # Pastikan file ada di folder 'www'
    list(
      src = filename,
      contentType = "image/png",
      width = 650  # Sesuaikan ukuran lebar (px)
    )
  }, deleteFile = FALSE)
  
  # Logo Gmail
  output$gmailLogo <- renderImage({
    filename <- "www/Gmail.png"  # Path ke ikon Gmail
    list(
      src = filename,
      contentType = "image/png",
      width = 40,  # Ukuran lebar
      height = 40  # Ukuran tinggi
    )
  }, deleteFile = FALSE)
  
  # Logo LinkedIn
  output$linkedinLogo <- renderImage({
    filename <- "www/Linkedin.png"  # Path ke ikon LinkedIn
    list(
      src = filename,
      contentType = "image/png",
      width = 40,  # Ukuran lebar
      height = 40  # Ukuran tinggi
    )
  }, deleteFile = FALSE)
  
  # Logo Instagram
  output$instagramLogo <- renderImage({
    filename <- "www/Instagram.png"  # Path ke ikon Instagram
    list(
      src = filename,
      contentType = "image/png",
      width = 40,  # Ukuran lebar
      height = 40  # Ukuran tinggi
    )
  }, deleteFile = FALSE)
  
  # CHART
  metrik <- reactive({
    if (input$filter == "All") {
      # Jika filter adalah "All", ambil seluruh df_bersih
      df_bersih
    } else {
      # Jika filter bukan "All", lakukan filter berdasarkan nilai `input$filter`
      df_bersih %>%
        filter(`Stroke` == input$filter)
    }
  })
  
  output$jumlah_pasien <- renderValueBox({
    valueBox(
      value = nrow(metrik()),
      subtitle = "Jumlah Pasien",
      icon = icon("users"),
      color = "green"
    )
  })
  
  output$rata2_usia <- renderValueBox({
    valueBox(
      value = round(mean(metrik()$Age, na.rm = TRUE)),
      subtitle = "Rata-rata Usia",
      icon = icon("calendar-alt"),
      color = "yellow"
    )
  })
  
  output$rata2_bmi <- renderValueBox({
    valueBox(
      value = round(mean(metrik()$Glucose, na.rm = TRUE), 2),
      subtitle = "Rata-rata Gula Darah",
      icon = icon("heartbeat"),
      color = "red"
    )
  })
  
  # Hitung persentase Stroke
  percentage_stroke <- reactive({
    # Ubah nilai Stroke menjadi 1 untuk "Yes" dan 0 untuk "No"
    filtered_data <- metrik() %>%
      mutate(Stroke = if_else(Stroke == "Yes", 1, 0))
    
    stroke_sum <- sum(filtered_data$Stroke, na.rm = TRUE)  # Jumlah Stroke (1)
    total_data <- nrow(filtered_data)  # Total data
    
    if (total_data == 0) {  # Cegah pembagian dengan nol
      return(0)
    } else {
      return((stroke_sum / total_data) * 100)  # Hitung persentase
    }
  })
  
  # Render Gauge Chart
  output$gaugeChart <- renderPlotly({
    gauge_value <- percentage_stroke()  # Dapatkan nilai persentase
    
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = gauge_value,
      number = list(
        suffix = "%", 
        font = list(
          family = "Poppins", 
          size = 40, 
          color = "black"
        )
      ), # Font angka
      title = list(
        text = "<b>Persentase Kasus <br>Pasien Stroke</b>", 
        font = list(
          family = "Poppins", 
          size = 20, 
          color = "black"
        )
      ), # Judul bold dengan font Poppins
      gauge = list(
        axis = list(
          visible = FALSE, # Hilangkan label angka pada sumbu
          range = list(0, 100), # Rentang 0-100%
          tickwidth = 0, # Hilangkan garis sumbu
          tickcolor = "rgba(0,0,0,0)" # Transparan
        ),
        bar = list(
          color = "white", # Warna bar utama
          thickness = 0.2  # Ketebalan bar
        ),
        bgcolor = "rgba(0,0,0,0)", # Hilangkan latar belakang gauge
        borderwidth = 0, # Hilangkan border di area gauge
        steps = list(
          list(range = c(0, 25), color = "rgba(92,107,192,255)"),
          list(range = c(25, 50), color = "rgba(125,137,205,255)"),
          list(range = c(50, 75), color = "rgba(157,166,217,255)"), 
          list(range = c(75, 100), color = "rgba(190,196,230,255)")
        ),
        threshold = list(
          line = list(color = "black", width = 3), # Garis batas threshold
          thickness = 0.75,
          value = gauge_value # Nilai untuk threshold
        )
      ),
      domain = list(x = c(0, 1), y = c(0, 1)) # Membatasi ukuran gauge
    ) %>% layout(
      margin = list(t = 50, b = 10, l = 10, r = 10), # Tambahkan margin atas untuk jarak dari judul
      paper_bgcolor = "rgba(0, 0, 0, 0)",  # Transparent background for the overall chart
      plot_bgcolor = "rgba(0, 0, 0, 0)",   # Transparent background for the plot area
      width = NULL,  # Allow Plotly to dynamically adjust width
      height = NULL  # Allow Plotly to dynamically adjust height
    )
  })
  
  # Render histogram untuk usia pasien
  output$histogramAge <- renderPlot({
    ggplot(metrik(), aes(x = Age)) + # Gunakan data yang di-filter dari metrik()
      geom_histogram(
        binwidth = 3, # Rentang usia per bin
        fill = "#6A8EC8", # Warna histogram
        color = "white" # Warna garis tepi bin
      ) +
      labs(
        y = NULL, # Label sumbu y
        x = NULL, # Label sumbu x
        title = "Persebaran Usia Pasien", # Judul grafik
        family = "Poppins"
      ) +
      theme_minimal(base_family = "Poppins") + # Gunakan font Poppins
      theme(
        plot.title = element_text(
          size = 20,
          face = "bold", 
          hjust = 0.5, 
          margin = ggplot2::margin(b = 10) # Tambahkan jarak bawah judul
        ),
        axis.title = element_text(size = 10), # Ukuran label sumbu
        axis.text = element_text(size = 9), # Ukuran angka sumbu
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        plot.margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)  # Adjust plot margins
      )
  })
  
  output$bmi_plot <- renderPlot({
    # Kategorisasi BMI berdasarkan nilai
    data <- metrik() %>%
      mutate(
        BMI_Category = factor(case_when(
          BMI < 18.5 ~ "Underweight",
          BMI >= 18.5 & BMI <= 24.9 ~ "Normal",
          BMI >= 25 & BMI <= 29.9 ~ "Overweight",
          BMI >= 30 ~ "Obese"
        ), levels = c("Underweight", "Normal", "Overweight", "Obese")),  # Set the factor levels for BMI
        
        Stroke = factor(Stroke, levels = c("Yes", "No"))  # Set the factor levels for Stroke (Yes on top)
      )
    
    # Membuat plot menggunakan ggplot2
    ggplot(data, aes(x = BMI_Category, fill = Stroke)) +
      geom_bar(stat = "count", position = "stack") +
      scale_fill_manual(
        values = c("No" = "#6A8EC8", "Yes" = "#4A628A"),
        name = "Stroke"
      ) +
      labs(
        title = "Distribusi BMI Pasien",
        x = NULL,
        y = NULL
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 0.5, color = "black", size = 10),  # Keep x labels horizontal
        plot.title = element_text(
          size = 16, 
          face = "bold", 
          hjust = 0.5, 
          family = "Poppins"  # Ensure Poppins font
        ),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        plot.margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)  # Adjust plot margins
      )
  })
  
  # Render histogram untuk glukosa
  output$glucose <- renderPlot({
    ggplot(metrik(), aes(x = Glucose)) + # Gunakan data yang di-filter dari metrik()
      geom_histogram(
        binwidth = 20, # Rentang usia per bin
        fill = "#9DA6D9", # Warna histogram
        color = "white" # Warna garis tepi bin
      ) +
      labs(
        y = NULL, # Label sumbu y
        x = NULL, # Label sumbu x
        title = "Persebaran Kadar Gula Darah Pasien", # Judul grafik
        family = "Poppins"
      ) +
      theme_minimal(base_family = "Poppins") + # Gunakan font Poppins
      theme(
        plot.title = element_text(
          size = 16,
          face = "bold", 
          hjust = 0.5, 
          margin = ggplot2::margin(b = 10) # Tambahkan jarak bawah judul
        ),
        axis.title = element_text(size = 10), # Ukuran label sumbu
        axis.text = element_text(size = 9), # Ukuran angka sumbu
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        plot.margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)  # Adjust plot margins
      )
  })
  ## Output Gender
  output$gender <- renderEcharts4r({
    # Filter data sesuai input yang diberikan
    data <- metrik()
    
    # Hitung jumlah laki-laki dan perempuan
    gender_data <- data %>%
      group_by(Gender) %>%
      tally() %>%
      mutate(Persen = round(n / sum(n) * 100, 2)) %>%
      mutate(Gender = factor(Gender, levels = c("Male", "Female"))) %>%  # Atur urutan Male dulu
      arrange(Gender)
    
    # Gender yang akan digunakan di chart
    gender <- data.frame(
      gender = gender_data$Gender,
      value = gender_data$Persen,
      path = c('path://M18.2629891,11.7131596 L6.8091608,11.7131596 C1.6685112,11.7131596 0,13.032145 0,18.6237673 L0,34.9928467 C0,38.1719847 4.28388932,38.1719847 4.28388932,34.9928467 L4.65591984,20.0216948 L5.74941883,20.0216948 L5.74941883,61.000787 C5.74941883,65.2508314 11.5891201,65.1268798 11.5891201,61.000787 L11.9611506,37.2137775 L13.1110872,37.2137775 L13.4831177,61.000787 C13.4831177,65.1268798 19.3114787,65.2508314 19.3114787,61.000787 L19.3114787,20.0216948 L20.4162301,20.0216948 L20.7882606,34.9928467 C20.7882606,38.1719847 25.0721499,38.1719847 25.0721499,34.9928467 L25.0721499,18.6237673 C25.0721499,13.032145 23.4038145,11.7131596 18.2629891,11.7131596 M12.5361629,1.11022302e-13 C15.4784742,1.11022302e-13 17.8684539,2.38997966 17.8684539,5.33237894 C17.8684539,8.27469031 15.4784742,10.66467 12.5361629,10.66467 C9.59376358,10.66467 7.20378392,8.27469031 7.20378392,5.33237894 C7.20378392,2.38997966 9.59376358,1.11022302e-13 12.5361629,1.11022302e-13',
               'path://M28.9624207,31.5315864 L24.4142575,16.4793596 C23.5227152,13.8063773 20.8817445,11.7111088 17.0107398,11.7111088 L12.112691,11.7111088 C8.24168636,11.7111088 5.60080331,13.8064652 4.70917331,16.4793596 L0.149791395,31.5315864 C-0.786976655,34.7595013 2.9373074,35.9147532 3.9192135,32.890727 L8.72689855,19.1296485 L9.2799493,19.1296485 C9.2799493,19.1296485 2.95992025,43.7750224 2.70031069,44.6924335 C2.56498417,45.1567684 2.74553639,45.4852068 3.24205501,45.4852068 L8.704461,45.4852068 L8.704461,61.6700801 C8.704461,64.9659872 13.625035,64.9659872 13.625035,61.6700801 L13.625035,45.360657 L15.5097899,45.360657 L15.4984835,61.6700801 C15.4984835,64.9659872 20.4191451,64.9659872 20.4191451,61.6700801 L20.4191451,45.4852068 L25.8814635,45.4852068 C26.3667633,45.4852068 26.5586219,45.1567684 26.4345142,44.6924335 C26.1636859,43.7750224 19.8436568,19.1296485 19.8436568,19.1296485 L20.3966199,19.1296485 L25.2043926,32.890727 C26.1862111,35.9147532 29.9105828,34.7595013 28.9625083,31.5315864 L28.9624207,31.5315864 Z M14.5617154,0 C17.4960397,0 19.8773132,2.3898427 19.8773132,5.33453001 C19.8773132,8.27930527 17.4960397,10.66906 14.5617154,10.66906 C11.6274788,10.66906 9.24611767,8.27930527 9.24611767,5.33453001 C9.24611767,2.3898427 11.6274788,0 14.5617154,0 L14.5617154,0 Z'))
    
    gender %>%
      e_charts(gender) %>%
      e_x_axis(
        splitLine = list(show = FALSE),
        axisTick = list(show = FALSE),
        axisLine = list(show = FALSE),
        axisLabel = list(show = FALSE)
      ) %>%
      e_y_axis(
        max = 100,
        splitLine = list(show = FALSE),
        axisTick = list(show = FALSE),
        axisLine = list(show = FALSE),
        axisLabel = list(show = FALSE)
      ) %>%
      e_color(color = c('#4A628A', '#eee')) %>%
      e_pictorial(value, symbol = path, z = 10, name = 'realValue',
                  symbolBoundingData = 100, symbolClip = TRUE) %>%
      e_pictorial(value, symbol = path, name = 'background',
                  symbolBoundingData = 100) %>%
      e_labels(
        position = "bottom",  # Label berada di bawah ikon
        offset = c(0, 10),  # Jarak ke bawah
        textStyle = list(
          fontSize = 12, 
          fontFamily = 'Poppins', 
          fontWeight = 'bold',
          color = '#4A628A'
        ),
        formatter = "{@[1]}%"
      ) %>%
      e_legend(show = FALSE) %>%
      e_title(
        text = "Distribusi Jenis Kelamin",
        textStyle = list(
          fontSize = 14,  # Ukuran judul lebih kecil
          fontFamily = 'Poppins'
        ),
        left = "center",  # Posisi judul di tengah
        top = "5%"  # Judul lebih ke atas
      ) %>%
      e_grid(left = "12%", right = "12%", top = "15%", bottom = "12%")
  })
  
  ## Heart Disease
  output$heartDisease <- renderPlot({
    # Data
    data <- metrik()
    
    # Hitung jumlah dan persentase Heart Disease
    heartDisease_count <- data %>%
      group_by(`Heart Disease`) %>%
      summarise(n = n()) %>%
      mutate(Persen = n / sum(n))  # Hitung persentase
    
    # Siapkan data untuk chart
    dataa <- heartDisease_count %>%
      rename(Kategori = `Heart Disease`) %>%  # Ubah nama kolom
      mutate(Kategori = ifelse(Kategori == "Iya", "Heart Disease", "No Heart Disease")) %>%
      mutate(x = 3)  # Tambahkan kolom untuk polar chart
    
    # Warna dan pengaturan font
    highlight_color <- "#4A628A"  # Warna untuk Heart Disease
    font_family <- "Poppins"
    
    # Label untuk persentase Heart Disease
    label_pie <- percent(dataa$Persen[dataa$Kategori == "Heart Disease"], accuracy = 0.01)
    
    # Plot menggunakan ggplot2
    ggplot(dataa, aes(x = x, y = Persen, fill = Kategori)) +
      ggtitle("Persentase Heart Disease",
              subtitle = "Jumlah Pasien Dengan Riwayat Penyakit Jantung (%)") +
      geom_col() +
      coord_polar(theta = "y") +
      scale_fill_manual(values = c(highlight_color, "#D3D3D3")) +
      xlim(c(0.2, 3.5)) +
      theme_void() +
      theme(
        legend.position = "none",
        plot.title = element_text(family = font_family, face = "bold", size = 18, hjust = 0.5),
        plot.subtitle = element_text(family = font_family, face = "plain", size = 12, hjust = 0.5),
        text = element_text(family = font_family)
      ) +
      annotate(
        geom = "text",
        family = font_family,
        label = label_pie,
        fontface = "bold",
        color = highlight_color,
        size = 12,
        x = 0.3,
        y = 0
      )
  })
  
  ## Hypertention
  output$hipertensi <- renderPlot({
    # Data
    data <- metrik()
    
    # Hitung jumlah dan persentase Heart Disease
    hipertensi_count <- data %>%
      group_by(Hypertension) %>%
      summarise(n = n()) %>%
      mutate(Persen = n / sum(n))  # Hitung persentase
    
    # Siapkan data untuk chart
    dataa <- hipertensi_count %>%
      rename(Kategori = Hypertension) %>%  # Ubah nama kolom
      mutate(Kategori = ifelse(Kategori == "Iya", "Hypertension", "No Hypertension")) %>%
      mutate(x = 3)  # Tambahkan kolom untuk polar chart
    
    # Warna dan pengaturan font
    highlight_color <- "#9DA6D9"  # Warna untuk Heart Disease
    font_family <- "Poppins"
    
    # Label untuk persentase Heart Disease
    label_pie <- percent(dataa$Persen[dataa$Kategori == "Hypertension"], accuracy = 0.01)
    
    # Plot menggunakan ggplot2
    ggplot(dataa, aes(x = x, y = Persen, fill = Kategori)) +
      ggtitle("Persentase Hipertensi",
              subtitle = "Jumlah Pasien Dengan Riwayat Hipertensi (%)") +
      geom_col() +
      coord_polar(theta = "y") +
      scale_fill_manual(values = c(highlight_color, "#D3D3D3")) +
      xlim(c(0.2, 3.5)) +
      theme_void() +
      theme(
        legend.position = "none",
        plot.title = element_text(family = font_family, face = "bold", size = 18, hjust = 0.5),
        plot.subtitle = element_text(family = font_family, face = "plain", size = 12, hjust = 0.5),
        text = element_text(family = font_family)
      ) +
      annotate(
        geom = "text",
        family = font_family,
        label = label_pie,
        fontface = "bold",
        color = highlight_color,
        size = 12,
        x = 0.3,
        y = 0
      )
  })
  
  output$confusionMatrix <- renderImage({
    list(
      src = "www/cfmat.png",  # Ganti dengan path file Confusion Matrix
      contentType = "image/png",
      alt = "Confusion Matrix",
      width = 400
    )
  }, deleteFile = FALSE)
  
  output$rocCurve <- renderImage({
    list(
      src = "www/roc.png",  # Ganti dengan path file ROC Curve
      contentType = "image/png",
      alt = "ROC Curve",
      width = 400
    )
  }, deleteFile = FALSE)
  
}

# Run Application
shinyApp(ui, server)

