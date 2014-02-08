shinyUI(pageWithSidebar(
  headerPanel("Soft Timing Logs Slicer and Dicer"),
  sidebarPanel(
    dateInput('date',
      label = 'Look at logs for:',
      value = Sys.Date()),
      uiOutput("drillControls")),
  mainPanel(
    verbatimTextOutput("date"),
    h3(textOutput("status")),
    plotOutput("plot"))
))

