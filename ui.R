shinyUI(pageWithSidebar(
  headerPanel("Soft Timing Logs Slicer and Dicer"),
  sidebarPanel(
    dateInput('date',
      label = 'Look at logs for:',
      value = Sys.Date()),
      uiOutput("mesurePicker"),
      selectInput("facet", "Stratifier par:", 
        c("-rien-","Canal","Portail","Poste_Travail","Cas_Gestion","Browser","OS")
      ),
      sliderInput("cutoff", "Plafond (%):", 
                min=10, max=100, value=90),
      sliderInput("alpha", "Transparence (%):", 
                min=0, max=100, value=5)),
  mainPanel(
    verbatimTextOutput("date"),
    plotOutput("plot"))
))

