server <- function(input, output) {
  
  output$scatter_clus <- renderPlot({
    driver <- trajets %>%
      filter(ENROLLED_VIN == input$vin) %>%
      select(time, VSS_DISTANCE) %>%
      mutate(time = as.numeric(time))
    
    driver_scaled <- scale(driver)

    d <- dist(driver_scaled, method = "euclidean")
    hc <- hclust(d, method = input$linkage)
    clusters <- cutree(hc, k = input$nb_clus)

    driver %<>%
      mutate(clusters = factor(clusters))
    
    plot_scatter(
      data = driver, 
      x = time, 
      y = VSS_DISTANCE, 
      color = clusters, 
      title = glue("Driver {input$vin}")
    )
  })
  
  
}