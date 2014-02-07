shinyUI(pageWithSidebar(
  headerPanel("Soft Timing Logs Slicer and Dicer"),
  sidebarPanel(
    dateInput('date',
      label = 'Look at logs for:',
      value = Sys.Date())),
  mainPanel(
    verbatimTextOutput("date"),
    verbatimTextOutput("status"),
    plotOutput("plot"),
    tableOutput("view"))
))

