server <- function(input, output) {
  
  reac_driver_trips <- reactive(
    trajets_sample %>%
      filter(ENROLLED_VIN == input$vin) %>%
      select(time, VSS_DISTANCE) %>%
      mutate(time = as.numeric(time))
  )
  
  reac_info_driver <- reactive(
    info_drivers %>%
    filter(ENROLLED_VIN == input$vin)
  )
  
  output$hex_plot <- renderPlot(
    plot_hex(
      data = reac_driver_trips(),
      x = time,
      y = VSS_DISTANCE
    ) +
    labs(
      x = "Heure de la journée", 
      y = "Distance"
    )
  )
  
  output$scatter_plot <- renderPlot(
    plot_scatter(
      data = reac_driver_trips(),
      x = time,
      y = VSS_DISTANCE
    ) +
      labs(
        x = "Heure de la journée", 
        y = "Distance"
      )
  )
  
  output$histogram_distance <- renderPlot(
    plot_hist_2(
      data = reac_driver_trips(),
      x = VSS_DISTANCE,
      binwidth = 1,
      xlim = c(0, 100)
    )
  )
  
  output$histogram_time <- renderPlot(
    plot_hist_2(
      data = reac_driver_trips(),
      x = time,
      binwidth = 900, 
      xlim = c(0, 86400)
    ) +
    scale_x_time(breaks = seq(0, 86400, by = 3600), labels = 0:24, limits = c(0, 86400))
  )
  
  output$title_tabbox <- renderText(
    glue("Trajets du conducteur {input$vin}")
  )
  
  output$nb_trips <- renderValueBox(
    valueBox(
      nrow(reac_driver_trips()),
      "Nombre de trajets",
      color = "red"
    )
  )
  
  output$expo_years <- renderValueBox(
    valueBox(
      round(reac_info_driver()$expo_time, 2),
      "Années d'exposition",
      color = "aqua"
    )
  )
  
  output$expo_km <- renderValueBox(
    valueBox(
      round(reac_info_driver()$expo_distance),
      "Kilomètres parcourus",
      color = "orange"
    )
  )
  
  output$freq_time <- renderValueBox(
    valueBox(
      reac_info_driver()$freq_time,
      "Réclamations par année",
      color = "yellow"
    )
  )
  
  output$freq_dist <- renderValueBox(
    valueBox(
      round(reac_info_driver()$freq_distance, 2),
      "Réclamations par 100 000 km",
      color = "aqua"
    )
  )
  
  output$nb_recl <- renderValueBox(
    valueBox(
      round(reac_info_driver()$nb_claims, 2),
      "Nombre de réclamations",
      color = "green"
    )
  )
  
  output$histograms <- renderPlot(
    make_histograms(input$vin)
  )
  
  output$histograms_time <- renderPlot(
    make_histograms_time(input$vin)
  )
  
}
