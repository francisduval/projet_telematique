header <- dashboardHeader(
  title = "Visualisation des scores de routine",
  titleWidth = 350
)

body <- dashboardBody(
  fluidRow(
    column(
      width = 3,
      selectInput(
        inputId = "vin",
        label = "Choisir le numéro de véhicule",
        choices = VINS,
        selectize = FALSE
      ),
     valueBoxOutput("nb_trips", width = NULL),
     valueBoxOutput("expo_years", width = NULL),
     valueBoxOutput("expo_km", width = NULL),
     valueBoxOutput("nb_recl", width = NULL),
     valueBoxOutput("freq_time", width = NULL),
     valueBoxOutput("freq_dist", width = NULL)
    ),
    column(
      width = 9,
      tabBox(
        title = textOutput("title_tabbox"),
        width = NULL,
        tabPanel(
          "Hexagones",
          box(
            width = NULL,
            plotOutput(outputId = "hex_plot")
          )
        ),
        tabPanel(
          "Nuage de points", 
          box(
            width = NULL,
            plotOutput(outputId = "scatter_plot")
          )
        ),
        tabPanel(
          "Histogramme (temps)", 
          box(
            width = NULL,
            plotOutput(outputId = "histogram_time")
          )
        ),
        tabPanel(
          "Histogramme (distance)", 
          box(
            width = NULL,
            plotOutput(outputId = "histogram_distance")
          )
        )
      ),
      
      tabBox(
        title = "Scores de routine",
        width = NULL,
        tabPanel(
          "Distance",
          box(
            width = NULL,
            plotOutput(outputId = "histograms")
          )
        ),
        tabPanel(
          "Temps de la journée", 
          box(
            width = NULL,
            plotOutput(outputId = "histograms_time")
          )
        )
      )
    )
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)
