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