logs_by_date = if(exists("logs_by_date")) logs_by_date else new.env()

shinyServer(function(input, output, session) {

  baseDir = "dataFiles"

  loadLogsAsync <- reactiveGenerator(
    function(value) {
      key = strftime(input$date,"%Y%m%d")
      data = mget(key,envir=logs_by_date,ifnotfound=list(NA))[[1]]
      !is.data.frame(data)
    },
    function(value) {
      key = strftime(input$date,"%Y%m%d")
      data = mget(key,envir=logs_by_date,ifnotfound=list(NA))[[1]]
      if (!is.data.frame(data)) {
        data = getTimingsForDate("dataFiles",key)
        assign(key,data,envir=logs_by_date)
      }
      if (nrow(data)==0) "No files" else "Loaded"
    },
    "Loading"
  )

  output$date  <- renderText({
    if (input$date <= Sys.Date()) {
	as.character(input$date)
    } else {
	"We can't analyze future logs, alas."
    }
  })

  output$status  <- renderText(loadLogsAsync())

  output$view <- renderTable({
    key = strftime(input$date,"%Y%m%d")
    data = mget(key,envir=logs_by_date,ifnotfound=list(data.frame()))[[1]]
    head(data, n = 3)
  })

  output$plot <- renderPlot({
    key = strftime(input$date,"%Y%m%d")
    data = mget(key,envir=logs_by_date,ifnotfound=list(data.frame()))[[1]]
    ggplot(data[data$ID_Mesure=='SCR_OFFER_FAM',],aes(x=Creneau,y=Mesure/1000))+	geom_point(alpha=0.05)+	geom_smooth(method="lm")+	coord_cartesian(xlim=c(25000,75000),ylim=c(0,50))  
  })

})
