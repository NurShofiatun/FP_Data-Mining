library(dplyr)
library(shiny)
library(bslib)
library(bsicons)
library(ggplot2)
library(tidyr)
library(shinydashboard)
library(lubridate)
library(shinythemes)
library(DT)
library(plotly)
library(vcd)
library(reshape2)
library(randomForest)
library(caret)
library(ROSE)

data = read.csv("https://raw.githubusercontent.com/NurShofiatun/FP_Data-Mining/main/hotel.csv")
#View(data)
data$arrival_datetime <- as.Date(data$arrival_datetime, format = "%Y-%m-%d")
#str(data)
#colnames(data)
# Menggunakan unique() untuk melihat nilai unik dalam kolom
#unique(data$market_segment_type)

# Menghitung jumlah kemunculan setiap tanggal
jumlah_reservation <- data %>%
  group_by(arrival_datetime) %>%
  summarise(jumlah = n(), .groups = "drop")

# Summary Stat
sumReservation <- sum(jumlah_reservation$jumlah)
sumVisitors <- sum(data$no_of_adults + data$no_of_children)
maxVisitors <- max(data$no_of_adults + data$no_of_children)
meanPrice <- mean(data$avg_price_per_room)
maxPrice <- max(data$avg_price_per_room)
sumReq <- sum(data$no_of_special_requests)
meanLeadTime <- mean(data$lead_time)
# Menghitung proporsi
proporsi <- prop.table(table(data$required_car_parking_space)) * 100
# Mengambil kategori dengan proporsi tertinggi
kategori_tertinggi <- names(proporsi)[which.max(proporsi)]
# Menghitung proporsi
segmenmarket <- prop.table(table(data$market_segment_type)) * 100
# Mengambil kategori dengan proporsi tertinggi
kategori <- names(segmenmarket)[which.max(proporsi)]

# Fungsi untuk menghitung jumlah pengunjung berdasarkan arrival_datetime
hitung_jumlah_pengunjung <- function(data) {
  data %>%
    group_by(arrival_datetime) %>%
    summarise(
      total_adults = sum(no_of_adults, na.rm = TRUE),
      total_children = sum(no_of_children, na.rm = TRUE),
      total_visitors = sum(no_of_adults, na.rm = TRUE) + sum(no_of_children, na.rm = TRUE)
    ) %>%
    gather(key = "kategori", value = "jumlah", total_adults, total_children) %>%
    ungroup()
}

# Fungsi untuk menghitung jumlah pengunjung berdasarkan tahun
hitung_jumlah_pengunjung_tahun <- function(data) {
  data %>%
    mutate(year = as.integer(format(arrival_datetime, "%Y"))) %>%
    group_by(year, month) %>%
    summarise(
      total_adults = sum(no_of_adults, na.rm = TRUE),
      total_children = sum(no_of_children, na.rm = TRUE),
      total_visitors = sum(no_of_adults, na.rm = TRUE) + sum(no_of_children, na.rm = TRUE)
    ) %>%
    gather(kategori, jumlah, total_adults, total_children) %>%
    ungroup()
}
hitung_jumlah_pengunjung_tahun(data)

# Booking Status
hitung_persentase_booking_status <- function(data) {
  data %>%
    group_by(arrival_datetime, booking_status) %>%
    summarise(total_bookings = n()) %>%
    group_by(arrival_datetime) %>%
    ungroup()
}

hitung_persentase_market(data)

# Booking Status
hitung_persentase_market <- function(data) {
  data %>%
    group_by(arrival_datetime, market_segment_type) %>%
    summarise(total_bookings = n()) %>%
    group_by(arrival_datetime) %>%
    ungroup()
}

# Pilih hanya variabel numerik
numeric_vars <- sapply(data, is.numeric)
numeric_cols <- colnames(data)[numeric_vars]

# Dataset baru untuk model random forest
dataset <- data %>%
  select(booking_status, type_of_meal_plan, required_car_parking_space, room_type_reserved, 
         market_segment_type, repeated_guest, no_of_children, no_of_weekend_nights, no_of_week_nights,
         lead_time, month, day, no_of_previous_cancellations, no_of_previous_bookings_not_canceled, 
         no_of_special_requests)

# Memisahkan fitur dan target
features <- dataset %>% select(-booking_status)
target <- dataset$booking_status

# Mengonversi variabel kategori menjadi faktor
dataset$type_of_meal_plan <- factor(dataset$type_of_meal_plan)
dataset$required_car_parking_space <- factor(dataset$required_car_parking_space)
dataset$room_type_reserved <- factor(dataset$room_type_reserved)
dataset$market_segment_type <- factor(dataset$market_segment_type)
dataset$repeated_guest <- factor(dataset$repeated_guest)
dataset$booking_status <- factor(dataset$booking_status)

# Melakukan oversampling pada data training
set.seed(123)
data_balanced <- ROSE(booking_status ~ ., data = dataset, seed = 123)$data

# Melatih model Random Forest
model <- randomForest(booking_status ~ ., data = data_balanced)


# UI
ui <- dashboardPage(
  dashboardHeader(title = "Hotel Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data", tabName = "data", icon = icon("table"),
               menuSubItem("Deskripsi Data", tabName = "deskData"),
               menuSubItem("Dataset", tabName = "data"),
               menuSubItem("Summary Statistic", tabName = "sumStat")
      ),
      menuItem("Visualization", tabName = "visual", icon = icon("chart-bar")),
      menuItem("Predict", tabName = "prediksi", icon = icon("magic"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                box(title = "Selamat Datang", width = 12, solidHeader = TRUE, status = "primary",
                    img(src = "https://raw.githubusercontent.com/NurShofiatun/FP_Data-Mining/main/image%20hotel.jpg", style = "width: 100%; height: auto;")
                )
              ),
              fluidRow(
                box(HTML("<p style='font-size: 24px;'>Dashboard Reservasi Hotel</p>"),
                    width = 12, solidHeader = TRUE, status = "primary",
                    HTML("<p style= 'font-size: 20px;'>Dashboard ini menampilkan deskripsi data, dataset, summary statistik, visualisasi, dan prediksi status pemesanan hotel</p>"),
                    HTML("<p style='font-size: 18px;'>Oleh : Nur Shofiatun & Putri Ayu Desita</p>")
                )
              )
      ),
      tabItem(tabName = "deskData",
              fluidRow(
                box(title = "Deskripsi Variabel dalam Dataset Hotel", width =12, solidHeader = TRUE, status = "primary",
                    HTML("
              <p>Data Reservasi Hotel merupakan data yang berisi variabel-variabel yang berpotensi untuk melihat atau memprediksi perilaku konsumen dalam memutuskan reservasi hotel. Dataset ini berisi informasi tentang berbagai aspek pemesanan hotel. Berikut adalah deskripsi dari beberapa variabel yang tersedia dalam dataset:</p>
              <ul>
                <li><b>Booking_ID:</b> Pengenal unik dari setiap pemesanan</li>
                <li><b>no_of_adults:</b> Jumlah orang dewasa</li>
                <li><b>no_of_children:</b> Jumlah anak-anak</li>
                <li><b>no_of_weekend_nights:</b> Jumlah malam akhir pekan (Sabtu atau Minggu) tamu menginap atau memesan untuk menginap di hotel</li>
                <li><b>no_of_week_nights:</b> Jumlah malam minggu (Senin hingga Jumat) tamu menginap atau memesan untuk menginap di hotel</li>
                <li><b>type_of_meal_plan:</b> Jenis paket makanan yang dipesan oleh pelanggan</li>
                <li><b>required_car_parking_space:</b> Apakah pelanggan membutuhkan tempat parkir mobil? (0 - Tidak, 1- Ya)</li>
                <li><b>room_type_reserved:</b> Jenis kamar yang dipesan oleh pelanggan. Nilai-nilai tersebut dikodekan (dikodekan) oleh INN Hotels.</li>
                <li><b>lead_time:</b> Jumlah hari antara tanggal pemesanan dan tanggal kedatangan</li>
                <li><b>arrival_year:</b> Tahun tanggal kedatangan</li>
                <li><b>arrival_month:</b> Bulan tanggal kedatangan</li>
                <li><b>arrival_date:</b> Tanggal bulan</li>
                <li><b>market_segment_type:</b> Penunjukan segmen pasar.</li>
                <li><b>repeated_guest:</b> Apakah pelanggan merupakan tamu berulang? (0 - Tidak, 1- Ya)</li>
                <li><b>no_of_previous_cancellations:</b> Jumlah pemesanan sebelumnya yang dibatalkan oleh pelanggan sebelum pemesanan saat ini</li>
                <li><b>no_of_previous_bookings_not_cancelled:</b> Jumlah pemesanan sebelumnya yang tidak dibatalkan oleh pelanggan sebelum pemesanan saat ini</li>
                <li><b>avg_price_per_room:</b> Harga rata-rata per hari pemesanan; harga kamar bersifat dinamis. (dalam euro)</li>
                <li><b>no_of_special_requests:</b> Jumlah total permintaan khusus yang dibuat oleh pelanggan (mis. lantai yang tinggi, pemandangan dari kamar, dll.)</li>
                <li><b>booking_status:</b> Bendera yang menunjukkan apakah pemesanan dibatalkan atau tidak.</li>
              </ul>
            ")
                )
              )
      ),
      tabItem(tabName = "data",
              fluidRow(
                box(title = "Dataset", width = 12, solidHeader = TRUE, status = "primary",
                    dataTableOutput("table")
                )
              )
      ),
      tabItem(tabName = "sumStat",
              fluidRow(
                sidebarPanel(
                  selectInput("variable", "Pilih Variabel:", 
                              choices = colnames(data), selected = colnames(data)[1])
                ),
                mainPanel(
                  h3("Statistik Deskriptif Utama"),
                  verbatimTextOutput("summaryStats")
                )
              ),
              fluidRow(
                box(title = "Summary Statistic", width = 12, solidHeader = TRUE, status = "primary",
                    valueBoxOutput("sumReservation"),
                    valueBoxOutput("sumVisitors"),
                    valueBoxOutput("maxVisitors"),
                    valueBoxOutput("meanPrice"),
                    valueBoxOutput("maxPrice"),
                    valueBoxOutput("sumReq"),
                    valueBoxOutput("meanLeadTime"),
                    valueBoxOutput("parkir_percent"),
                    valueBoxOutput("market")
                )
              )
      ),
      tabItem(tabName = "visual",
              fluidRow(
                box(
                  title = "Date range input",
                  status = "primary",
                  solidHeader = TRUE,
                  dateRangeInput("dates", "Select dates")
                )
              ),
              fluidRow(
                box(
                  title = "Pengunjung Hotel",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  valueBoxOutput("total2", width = 4),
                  valueBoxOutput("dateStatusCancel", width = 4),
                  valueBoxOutput("dateStatusnonCancel", width = 4)
                )
              ),
              fluidRow(
                box(
                  title = "Jumlah Pengunjung Adults vs Children",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("plotVisitors2"),
                  tableOutput("filteredTableRange")
                )
              ),
              fluidRow(
                box(
                  title = "Tipe Makanan yang Dipesan", 
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  tableOutput("tableMeal"),
                  plotlyOutput("pieChart3")
                )
              ),
              fluidRow(
                box(
                  title = "Tipe Kamar yang Dipesan",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  tableOutput("tableRoom"),
                  plotlyOutput("pieChart4")
                )
              ),
              fluidRow(
                box(
                  title = "Jumlah Rata-rata Harga per Ruangan",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("chartPriceDate")
                )
              ),
              fluidRow(
                box(
                  title = "Segmen Market",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  valueBoxOutput("marketOnline"),
                  valueBoxOutput("marketOffline"),
                  valueBoxOutput("marketCorporate"),
                  valueBoxOutput("marketAviation"),
                  valueBoxOutput("marketComplementary")
                )
              ),
              fluidRow(
                box(
                  title = "Jumlah Pemesanan dari Waktu ke Waktu",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("timeSeriesPlot")
                )
              ),
              fluidRow(
                box(
                  title = "Scatter Plot Lead Time vs. Harga Rata-rata per Kamar",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("scatterPlot")
                )
              ),
              fluidRow(
                box(
                  title = "Distribusi Harga Rata-rata per Kamar",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("boxPlot")
                )
              ),
              fluidRow(
                box(
                  title = "Select Variable",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  checkboxGroupInput("vars", "Numeric Variables", 
                                     choices = numeric_cols, 
                                     selected = numeric_cols[1:3])
                )
              ),
              fluidRow(
                box(
                  title = "Korelasi Antar Variabel",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("heatmap")
                )
              )
      ),
      tabItem(tabName = "prediksi",
              fluidRow(
                box(title = "Memprediksi Perilaku Konsumen dalam Memutuskan Reservasi Hotel", 
                    width = 12, solidHeader = TRUE, status = "primary",
                    selectInput("type_of_meal_plan", "Type of Meal Plan:", choices = levels(dataset$type_of_meal_plan)),
                    selectInput("required_car_parking_space", "Required Car Parking Space:", choices = levels(dataset$required_car_parking_space)),
                    selectInput("room_type_reserved", "Room Type Reserved:", choices = levels(dataset$room_type_reserved)),
                    selectInput("market_segment_type", "Market Segment Type:", choices = levels(dataset$market_segment_type)),
                    selectInput("repeated_guest", "Repeated Guest:", choices = levels(dataset$repeated_guest)),
                    numericInput("no_of_children", "Number of Children:", value = 0, min = 0, max = 3),
                    numericInput("no_of_weekend_nights", "Number of Weekend Nights:", value = 0, min = 0, max = 3),
                    numericInput("no_of_week_nights", "Number of Week Nights:", value = 1, min = 1, max = 5),
                    numericInput("lead_time", "Lead Time:", value = 1, min = 1, max = 365),
                    numericInput("month", "Month:", value = 1, min = 1, max = 12),
                    numericInput("day", "Day:", value = 1, min = 1, max = 31),
                    numericInput("no_of_previous_cancellations", "Number of Previous Cancellations:", value = 0, min = 0, max = 5),
                    numericInput("no_of_previous_bookings_not_canceled", "Number of Previous Bookings Not Canceled:", value = 0, min = 0, max = 10),
                    numericInput("no_of_special_requests", "Number of Special Requests:", value = 0, min = 0, max = 5),
                    actionButton("predict", "Predict")
                )
              ),
              fluidRow(
                box(
                  title = "Hasil Prediksi dengan Menggunakan Model Random Forest",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  tableOutput("probabilities"),
                  textOutput("prediction")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Dataset
  output$table <- renderDataTable({
    datatable(data, options = list(
      pageLength = 10,
      ordering = TRUE,
      scrollX = TRUE,
      autoWidth = TRUE
    ))
  })
  
  # Statistik deskriptif
  output$summaryStats <- renderPrint({
    summary(data[[input$variable]])
  })
  
  # Calculate summary statistics
  output$sumReservation <- renderValueBox({
    valueBox(round(sumReservation, 6), "Jumlah Reservasi Hotel", icon = icon("chart-pie"), color = "purple")
  })
  
  output$sumVisitors <- renderValueBox({
    valueBox(round(sumVisitors, 6), "Jumlah Pengunjung", icon = icon("chart-line"), color = "purple")
  })
  
  output$maxVisitors <- renderValueBox({
    valueBox(maxVisitors, "Max Pengunjung", icon = icon("chart-area"), color = "purple")
  })
  
  output$meanPrice <- renderValueBox({
    valueBox(round(meanPrice, 6), "Average Price", icon = icon("chart-line"), color = "purple")
  })
  
  output$maxPrice <- renderValueBox({
    valueBox(maxPrice, "Max Harga", icon = icon("chart-area"), color = "purple")
  })
  
  output$sumReq <- renderValueBox({
    valueBox(sumReq, "Total Permintaan Khusus", icon = icon("chart-pie"), color = "purple")
  })
  
  output$meanLeadTime <- renderValueBox({
    valueBox(round(meanLeadTime, 6), "Average Lead Time", icon = icon("chart-pie"), color = "purple")
  })
  
  output$parkir_percent <- renderValueBox({
    valueBox(kategori_tertinggi, "Permintaan Ruang Parkir Mobil", icon = icon("car"), color = "purple")
  })
  
  output$market <- renderValueBox({
    valueBox(kategori, "Segmen Market", icon = icon("store"), color = "purple")
  })
  
  # Jumlah Pengunjung
  # Reactive untuk pengelompokan data berdasarkan arrival_datetime
  jumlah_pengunjung_date <- reactive({
    hitung_jumlah_pengunjung(data)
  })
  
  # Filter data 
  filteredDataRange_age <- reactive({
    req(input$dates)
    jumlah_pengunjung_date() %>%
      filter(arrival_datetime >= as.Date(input$dates[1]) & arrival_datetime <= as.Date(input$dates[2]))
  })
  
  # ValueBox
  output$total2 <- renderValueBox({
    total_visitors <- filteredDataRange_age() %>% 
      summarise(total_visitors = sum(total_visitors, na.rm = TRUE), .groups = "drop") %>%
      pull(total_visitors)
    
    valueBox(
      formatC(total_visitors, format = "d", big.mark = ","),
      "Total Pengunjung",
      icon = icon("users"),
      color = "aqua"
    )
  })
  
  
  #Booking Status
  # Reactive untuk pengelompokan data berdasarkan tanggal
  jumlah_booking_status <- reactive({
    hitung_persentase_booking_status(data)
  })
  
  # Filter data berdasarkan rentang tanggal
  filteredDataRange_booking <- reactive({
    req(input$dates)
    jumlah_booking_status() %>%
      filter(arrival_datetime >= as.Date(input$dates[1]) & arrival_datetime <= as.Date(input$dates[2]))
  })
  
  # ValueBox untuk persentase Confirmed
  output$dateStatusCancel <- renderValueBox({
    filtered_data_canceled <- filteredDataRange_booking() %>%
      filter(booking_status == 'Canceled')
    
    total_canceled <- nrow(filtered_data_canceled)
    total_bookings <- nrow(filteredDataRange_booking())
    percent_canceled <- total_canceled / total_bookings * 100
    
    valueBox(
      sprintf("%.2f%%", percent_canceled),
      "Persentase Canceled",
      icon = icon("times-circle", class = "fa-sm", lib = "font-awesome"),
      color = "blue"
    )
  })
  
  # ValueBox untuk persentase Cancelled
  output$dateStatusnonCancel <- renderValueBox({
    filtered_data_non_canceled <- filteredDataRange_booking() %>%
      filter(booking_status == 'Not_Canceled')
    
    total_nonCanceled <- nrow(filtered_data_non_canceled)
    total_bookings <- nrow(filteredDataRange_booking())
    percent_nonCanceled <- total_nonCanceled / total_bookings * 100
    
    valueBox(
      sprintf("%.2f%%", percent_nonCanceled),
      "Persentase Non Canceled",
      icon = icon("check"),
      color = "blue"
    )
  })
  
  # Bar-Chart jumlah pengunjung
  output$plotVisitors2 <- renderPlot({
    plotData <- filteredDataRange_age()
    
    ggplot(plotData, aes(x = arrival_datetime, y = total_visitors, fill = kategori)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        x = "Tanggal", y = "Jumlah Pengunjung") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Room and Meal Type
  output$tableMeal <- renderTable({
    req(input$dates)
    mealDataRange <- data %>%
      filter(arrival_datetime >= as.Date(input$dates[1]) & arrival_datetime <= as.Date(input$dates[2]))
    as.data.frame(table(mealDataRange$type_of_meal_plan))
  })
  
  output$tableRoom <- renderTable({
    req(input$dates)
    roomDataRange <- data %>%
      filter(arrival_datetime >= as.Date(input$dates[1]) & arrival_datetime <= as.Date(input$dates[2]))
    as.data.frame(table(roomDataRange$room_type_reserved))
  })
  
  output$pieChart3 <- renderPlotly({
    req(input$dates)
    mealDataRange <- data %>%
      filter(arrival_datetime >= as.Date(input$dates[1]) & arrival_datetime <= as.Date(input$dates[2]))
    meal <- as.data.frame(table(mealDataRange$type_of_meal_plan))
    
    plot_ly(
      meal,
      labels = ~Var1,
      values = ~Freq,
      type = 'pie',
      textinfo = 'label+percent',
      insidetextorientation = 'radial'
    ) %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      )
  })
  
  output$pieChart4 <- renderPlotly({
    req(input$dates)
    roomDataRange <- data %>%
      filter(arrival_datetime >= as.Date(input$dates[1]) & arrival_datetime <= as.Date(input$dates[2]))
    room <- as.data.frame(table(roomDataRange$room_type_reserved))
    
    plot_ly(
      room,
      labels = ~Var1,
      values = ~Freq,
      type = 'pie',
      textinfo = 'label+percent',
      insidetextorientation = 'radial'
    ) %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      )
  })
  
  # Price 
  filteredDataPrice <- reactive({
    req(input$dates)
    data %>%
      filter(arrival_datetime >= as.Date(input$dates[1]) & arrival_datetime <= as.Date(input$dates[2])) %>%
      group_by(arrival_datetime) %>%
      summarise(total_harga = sum(avg_price_per_room), .groups = "drop")
  })
  
  output$chartPriceDate <- renderPlot({
    ggplot(filteredDataPrice(), aes(x = arrival_datetime, y = total_harga)) +
      geom_bar(stat = "identity", fill = "red") +
      labs(
        x = "Date",
        y = "Total Price") +
      theme_minimal()
  }) 
  
  # Reactive untuk pengelompokan data berdasarkan tanggal
  jumlah_market <- reactive({
    hitung_persentase_market(data)
  })
  
  # Filter data berdasarkan rentang tanggal
  filteredDataMarket <- reactive({
    req(input$dates)
    jumlah_market() %>%
      filter(arrival_datetime >= as.Date(input$dates[1]) & arrival_datetime <= as.Date(input$dates[2]))
  })
  
  # ValueBox untuk persentase Confirmed
  output$marketOnline <- renderValueBox({
    filtered_data_online <- filteredDataMarket() %>%
      filter(market_segment_type == 'Online')
    
    frekuensi_online <- nrow(filtered_data_online)
    total_segmen <- nrow(filteredDataMarket())
    percent_online <- (frekuensi_online / total_segmen) * 100
    
    valueBox(
      sprintf("%.2f%%", percent_online),
      "Segmen Market Online",
      icon = icon("store"),
      color = "green"
    )
  })
  
  # ValueBox untuk persentase Cancelled
  output$marketOffline <- renderValueBox({
    filtered_data_offline <- filteredDataMarket() %>%
      filter(market_segment_type == 'Offline')
    
    frekuensi_offline <- nrow(filtered_data_offline)
    total_segmen <- nrow(filteredDataMarket())
    percent_offline <- (frekuensi_offline / total_segmen) * 100
    
    valueBox(
      sprintf("%.2f%%", percent_offline),
      "Segmen Market Offline",
      icon = icon("store"),
      color = "green"
    )
  })
  
  # ValueBox untuk persentase Cancelled
  output$marketCorporate <- renderValueBox({
    filtered_data_corporate <- filteredDataMarket() %>%
      filter(market_segment_type == 'Corporate')
    
    frekuensi_corporate <- nrow(filtered_data_corporate)
    total_segmen <- nrow(filteredDataMarket())
    percent_corporate <- (frekuensi_corporate / total_segmen) * 100
    
    valueBox(
      sprintf("%.2f%%", percent_corporate),
      "Segmen Market Corporate",
      icon = icon("store"),
      color = "green"
    )
  })
  
  # ValueBox untuk persentase Cancelled
  output$marketAviation <- renderValueBox({
    filtered_data_aviation <- filteredDataMarket() %>%
      filter(market_segment_type == 'Aviation')
    
    frekuensi_aviation <- nrow(filtered_data_aviation)
    total_segmen <- nrow(filteredDataMarket())
    percent_aviation <- (frekuensi_aviation / total_segmen) * 100
    
    valueBox(
      sprintf("%.2f%%", percent_aviation),
      "Segmen Market Aviation",
      icon = icon("store"),
      color = "green"
    )
  })
  
  # ValueBox untuk persentase Cancelled
  output$marketComplementary <- renderValueBox({
    filtered_data_complementary <- filteredDataMarket() %>%
      filter(market_segment_type == 'Complementary')
    
    frekuensi_complementary <- nrow(filtered_data_complementary)
    total_segmen <- nrow(filteredDataMarket())
    percent_complementary <- (frekuensi_complementary / total_segmen) * 100
    
    valueBox(
      sprintf("%.2f%%", percent_complementary),
      "Segmen Market Complementary",
      icon = icon("store"),
      color = "green"
    )
  })
  
  filteredData <- reactive({
    data %>%
      filter(arrival_datetime >= as.Date(input$dates[1]) & arrival_datetime <= as.Date(input$dates[2]))
    return(data)
  })
  
  output$timeSeriesPlot <- renderPlot({
    data <- filteredData()
    
    ggplot(data, aes(x = arrival_datetime)) +
      geom_line(stat = "count", color = "blue") +
      labs(
        x = "Tanggal Kedatangan",
        y = "Jumlah Pemesanan") +
      theme_minimal()
  }) 
  
  output$boxPlot <- renderPlot({
    data <- filteredData()
    
    ggplot(data, aes(x = booking_status, y = avg_price_per_room)) +
      geom_boxplot(fill = "blue", color = "black") +
      labs(
        x = "Status Pemesanan",
        y = "Harga Rata-rata per Kamar") +
      theme_minimal()
  })
  
  filteredDataScatter <- reactive({
    req(input$dates)
    data %>%
      filter(arrival_datetime >= as.Date(input$dates[1]) & arrival_datetime <= as.Date(input$dates[2]))
    data <- data %>%
      filter(is.finite(lead_time) & is.finite(avg_price_per_room))
    return(data)
  })
  
  output$scatterPlot <- renderPlot({
    data <- filteredDataScatter()
    
    ggplot(data, aes(x = lead_time, y = avg_price_per_room)) +
      geom_point(color = "blue") +
      labs(
        x = "Lead Time (days)",
        y = "Harga Rata-rata per Kamar") +
      theme_minimal()
  })
  
  # HEATMAP 
  filtered_DataCorr <- reactive({
    req(input$dates)
    data %>%
      filter(arrival_datetime >= as.Date(input$dates[1]) & arrival_datetime <= as.Date(input$dates[2])) %>%
      select(input$vars)
  })
  
  correlation_matrix <- reactive({
    # Filter hanya variabel numerik
    numeric_vars <- sapply(filtered_DataCorr(), is.numeric)
    selected_data <- filtered_DataCorr()[, numeric_vars, drop = FALSE]
    
    # Menghitung matriks korelasi
    cor(selected_data, use = "complete.obs")
  })
  
  output$heatmap <- renderPlot({
    corr_melt <- melt(correlation_matrix())
    
    ggplot(data = corr_melt, aes(Var1, Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "yellow", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1, 1), space = "Lab", 
                           name="Correlation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 12, hjust = 1)) +
      coord_fixed()
  })
  
  # Prediksi
  observeEvent(input$predict, {
    new_data <- data.frame(
      type_of_meal_plan = factor(input$type_of_meal_plan, levels = levels(dataset$type_of_meal_plan)),
      required_car_parking_space = factor(input$required_car_parking_space, levels = levels(dataset$required_car_parking_space)),
      room_type_reserved = factor(input$room_type_reserved, levels = levels(dataset$room_type_reserved)),
      market_segment_type = factor(input$market_segment_type, levels = levels(dataset$market_segment_type)),
      repeated_guest = factor(input$repeated_guest, levels = levels(dataset$repeated_guest)),
      no_of_children = input$no_of_children,
      no_of_weekend_nights = input$no_of_weekend_nights,
      no_of_week_nights = input$no_of_week_nights,
      lead_time = input$lead_time,
      month = input$month,
      day = input$day,
      no_of_previous_cancellations = input$no_of_previous_cancellations,
      no_of_previous_bookings_not_canceled = input$no_of_previous_bookings_not_canceled,
      no_of_special_requests = input$no_of_special_requests
    )
    
    prediction <- predict(model, new_data)
    probabilities <- predict(model, new_data, type = "prob")
    
    output$prediction <- renderText({
      if (is.na(prediction)) {
        "The predicted booking status is NA, which means the input values might not match the levels in the training data."
      } else {
        paste("The predicted booking status is:", prediction)
      }
    })
    
    output$probabilities <- renderTable({
      probabilities
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
