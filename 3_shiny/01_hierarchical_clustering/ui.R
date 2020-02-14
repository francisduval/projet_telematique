ui <- fluidPage(
  titlePanel("Agglomerative Hierarchical Clustering"),
  p(strong("Scaled data from year 2018, euclidean distance")),
  sidebarLayout(
    sidebarPanel(
      # Input: Number of clusters -----------------------------------------------------------------------------------------------
      numericInput(
        inputId = "nb_clus", 
        label = "Number of clusters", 
        value = 3,
        min = 2
      ),
      
      # Input: Select driver (or ENROLLED_VIN) ----------------------------------------------------------------------------------
      selectInput(
        inputId = "vin",
        label = "VIN",
        choices = unique(trajets$ENROLLED_VIN),
      ),
      
      # Input: Linkage used for hierarchical clustering -------------------------------------------------------------------------
      selectInput(
        inputId = "linkage",
        label = "Linkage",
        choices = c("single", "complete", "average", "centroid"),
      )
    ),
    
    mainPanel(
      # Output: Scatterplot with clusters ---------------------------------------------------------------------------------------
      plotOutput(outputId = "scatter_clus")
    )
  )
)