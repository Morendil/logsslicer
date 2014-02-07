logs_by_date = if(exists("logs_by_date")) logs_by_date else new.env()

shinyServer(function(input, output, session) {

  baseDir = "dataFiles"

  loadLogsAsync <- reactiveGenerator(
    function(value) {
      key = strftime(input$date,"%Y%m%d")
      !is.data.frame(mget(key,envir=logs_by_date,ifnotfound=c(NA)))
    },
    function(value) {
      key = strftime(input$date,"%Y%m%d")
      data = mget(key,envir=logs_by_date,ifnotfound=c(NA))
      if (is.character(data)) {
        return(data)
      }
      if (!is.data.frame(data)) {
        assign(key,"Loading",envir=logs_by_date)
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


})
