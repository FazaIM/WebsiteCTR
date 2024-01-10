library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)

# Load data ad placement
ads <- data.frame(
  Day = 1:10,
  Left_Sidebar = c(2.5, 2.7, 2.8, 2.6, 3, 2.4, 2.9, 2.5, 2.6, 2.7),
  Center_Page = c(3.8, 3.5, 4, 3.7, 3.9, 3.6, 4.1, 3.4, 3.8, 3.9),
  Right_Sidebar = c(3.1, 2.9, 3, 3.2, 3.3, 2.8, 3.4, 3.1, 3.2, 3.5)
)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Website CTR"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Input Data",
        tabName = "menu_1",
        icon = icon("circle-down")
      ),
      menuItem(
        text = "ANOVA Analysis",
        tabName = "menu_2",
        icon = icon("desktop")
      ),
      menuItem(
        text = "Introduction",
        tabName = "menu_intro",
        icon = icon("info")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "menu_1",
        fluidPage(
          h2(tags$b("Input Ad Placement Data")),
          fluidRow(
            box(
              title = "Add New Data",
              width = 4,
              solidHeader = TRUE,
              numericInput("input_x1", "Left Sidebar Data :", value = 2.5),
              numericInput("input_x2", "Center Page Data :", value = 4),
              numericInput("input_x3", "Right Sidebar Data :", value = 3),
              actionButton("addBtn", "Add Data")
            ),
            box(
              status = "primary",
              headerPanel("Ad Placement Data"),
              solidHeader = TRUE,
              br(),
              fluidRow(
                column(
                  width = 12,
                  DT::dataTableOutput("data_table", height = "350px")
                ),
                column(
                  width = 10,
                  align = "left",
                  actionButton("deleteBtn", "Delete Selected Data")
                )
              ),
              width = 8,
              height = "620px"
            )
          )
        )
      ),
      tabItem(
        tabName = "menu_2",
        fluidPage(
          h2(tags$b("ANOVA Analysis")),
          fluidRow(
            column(
              width = 8,
              verbatimTextOutput("anova_summary"),
              column(
                width = 12,
                tags$p("Dengan menggunakan tingkat kepercayaan 95%, jika nilai p < 0,05, maka setidaknya terdapat satu perbedaan yang signifikan dalam rata-rata CTR berdasarkan penempatan iklan di situs web. Selain itu, jika nilai p > 0,05, maka tidak ada perbedaan signifikan dalam rata-rata CTR berdasarkan penempatan iklan di situs web.",
                       style = "color:black; font-size: 18px; text-align:justify;")
              )
            )
          ),
          h2(tags$b("Visualization of CTR Performance")),
          plotOutput("boxplot_ctr"),
          plotOutput("barplot_ctr")  # Tambahkan elemen plotOutput untuk bar plot
        )
      ),
      tabItem(
        tabName = "menu_intro",
        fluidPage(
          h2(tags$b("Introduction")),
          tags$p("Di era digital ini, informasi dan alat praktis ada di ujung jari kita. Namun, dengan begitu banyaknya hal tersedia, terutama dalam hal pemasaran digital, terkadang sulit untuk mengoptimalkan semuanya. Di sinilah Alat Interaktif untuk Menganalisis Efektivitas Strategi Penempatan Produk masuk berperan."),
          tags$p("Aplikasi ini dapat membantu dalam:"),
          tags$ul(
            tags$li("Uji Coba Penempatan Iklan: Masukkan data CTR Anda untuk tiga lokasi berbeda (sidebar kiri, pusat, sidebar kanan) dan lihat kinerja masing-masing lokasi dengan cepat dan mudah."),
            tags$li("Analisis Statistik: Aplikasi ini melakukan keajaiban di latar belakang, menjalankan analisis statistik canggih untuk menentukan apakah ada perbedaan signifikan dalam CTR berdasarkan penempatan iklan."),
            tags$li("Visualisasi Data: Hasilnya disajikan dalam visualisasi yang jelas dan menarik, seperti bar plot atau box plot, sehingga Anda dapat dengan mudah memahami tren dan kinerja CTR."),
            tags$li("Interpretasi Cepat: Dapatkan ringkasan ringkas dari hasil analisis, termasuk nilai p dan interpretasi apakah hasilnya secara statistik signifikan pada tingkat signifikansi 0,05.")
          ),
          tags$p("Aplikasi ini juga diharapkan akan memberi manfaat:"),
          tags$ul(
            tags$li("Optimalkan Penempatan Iklan: Buat keputusan berdasarkan data tentang penempatan iklan mana yang mendorong klik terbanyak, sehingga Anda dapat memaksimalkan ROI kampanye Anda."),
            tags$li("Hemat Waktu dan Uang: Hindari menebak-nebak dan coba-coba dengan strategi penempatan iklan. Alat ini memberi Anda wawasan yang dapat ditindaklanjuti dalam hitungan menit, menghemat waktu dan uang Anda."),
            tags$li("Tingkatkan Konversi: Ketika iklan ditempatkan secara optimal, pengguna lebih cenderung mengklik dan terlibat, pada akhirnya mendorong lebih banyak konversi dan penjualan."),
            tags$li("Keterampilan Pemasaran Digital Lebih Tajam: Alat ini memperdalam pemahaman Anda tentang analitik klik dan memberi Anda keunggulan dalam memahami perilaku pengguna.")
          ),
          tags$p("Secara keseluruhan, Alat Interaktif untuk Menganalisis Efektivitas Strategi Penempatan Produk adalah investasi yang cerdas bagi siapa saja yang ingin meningkatkan strategi pemasaran digital mereka dan memaksimalkan hasil iklan. Ini adalah alat yang ampuh untuk membuat keputusan strategis berdasarkan data dan mengungguli persaingan.")
        )
      )
    )
  )
)

server <- function(input, output) {
  ads_reactive <- reactiveVal(ads)
  
  ad_placement_data <- reactiveVal(data.frame(
    ad_placement = rep(c("Left_Sidebar", "Center_Page", "Right_Sidebar"), each = 10),
    CTR = c(ads$Left_Sidebar, ads$Center_Page, ads$Right_Sidebar)
  ))
  
  anova_results <- reactiveVal(NULL)
  
  perform_anova <- function(data) {
    anova_result <- aov(CTR ~ ad_placement, data = data)
    return(list(
      summary = summary(anova_result)
    ))
  }
  
  observeEvent(input$addBtn, {
    new_data <- data.frame(
      Day = max(ads_reactive()$Day, na.rm = TRUE) + 1,
      Left_Sidebar = input$input_x1,
      Center_Page = input$input_x2,
      Right_Sidebar = input$input_x3
    )
    names(new_data) <- names(ads)
    ads_reactive(rbind(ads_reactive(), new_data))
    
    new_ad_placement_data <- data.frame(
      ad_placement = rep(c("Left_Sidebar", "Center_Page", "Right_Sidebar"), each = 1),
      CTR = c(new_data$Left_Sidebar, new_data$Center_Page, new_data$Right_Sidebar)
    )
    updated_ad_placement_data <- rbind(ad_placement_data(), new_ad_placement_data)
    ad_placement_data(updated_ad_placement_data)
  })
  
  observeEvent(input$deleteBtn, {
    selected_rows <- input$data_table_rows_selected
    
    ads_reactive(ads_reactive()[-selected_rows, , drop = FALSE])
    
    ad_placement_data(ad_placement_data()[-selected_rows, , drop = FALSE])
  })
  
  output$data_table <- DT::renderDataTable({
    DT::datatable(ads_reactive(), selection = 'multiple', options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$anova_summary <- renderPrint({
    anova_results(perform_anova(ad_placement_data()))
    anova_results()$summary
  })
  
  output$boxplot_ctr <- renderPlot({
    boxplot(CTR ~ ad_placement, data = ad_placement_data(), main = "CTR Performance by Ad Placement", ylab = "CTR")
  })
  
  output$barplot_ctr <- renderPlot({
    bar_data <- aggregate(CTR ~ ad_placement, data = ad_placement_data(), mean)
    barplot(bar_data$CTR, names.arg = bar_data$ad_placement, 
            main = "Average CTR by Ad Placement",
            ylab = "Average CTR",
            xlab = "Ad Placement",
            col = rainbow(nlevels(ad_placement_data()$ad_placement))
    )
  })
  
  observe({
    anova_results(perform_anova(ad_placement_data()))
    output$boxplot_ctr <- renderPlot({
      boxplot(CTR ~ ad_placement, data = ad_placement_data(), main = "CTR Performance by Ad Placement", ylab = "CTR")
    })
  })
}

shinyApp(ui, server)
