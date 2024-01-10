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