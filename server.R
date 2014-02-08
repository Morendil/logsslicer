shinyServer(function(input, output, session) {

  data <- reactive({
    loadData(input$date)
  })

  subset <- reactive({
    full = data()
    mesure = input$mesure
    if (nrow(full)==0) return(full)
    full[full$ID_Mesure==mesure,]
  })

  output$date  <- renderText({
    if (input$date < Sys.Date()) {
       as.character(input$date)
    } else {
       "We can't analyze future logs, alas."
    }
  })

  output$plot <- renderPlot({
    if (input$date >= Sys.Date()) return()
    cutoff = input$cutoff/100
    transp = input$alpha/100
    bywhat = input$facet
    if (nrow(data())==0) return()
    sub = subset()
    top = quantile(sub$Mesure/1000,probs=cutoff)
    plot = ggplot(sub,aes(x=Creneau,y=Mesure/1000))+
	ylab("Duree (secondes)")+	geom_point(alpha=transp)+	geom_smooth(method="lm")+	coord_cartesian(xlim=c(25000,75000),ylim=c(0,top))
    if (bywhat!="-rien-") {
	plot = plot+facet_grid(paste(bywhat,"~."))
    }
    print(plot)
  },
  height = function() {
    bywhat = input$facet
    if (input$date >= Sys.Date()) return()
    sub = subset()
    if (bywhat=="-rien-") {
	400
    } else 400 * length(levels(sub[,bywhat]))
  })

  output$mesurePicker <- renderUI({
    if (input$date >= Sys.Date()) return()
    if (nrow(data())==0) return()
    mesures <- levels(data()$ID_Mesure)
    selectInput("mesure", "Mesure:", as.list(mesures))
  })

})

library(stringr)
library(xts)
library(ggplot2)

loadData <- function(date) {
  key = strftime(date,"%Y%m%d")
  data = mget(key,envir=logs_by_date,ifnotfound=list(NA))[[1]]
  if (!is.data.frame(data)) {
    data = getTimingsForDate("dataFiles",key)
    assign(key,data,envir=logs_by_date)
    save(list=ls(logs_by_date),envir=logs_by_date,file="/tmp/lbd.RData")
  }
  data
}

getTimingsForDate <- function(baseDir,dateStr) {
  files <- list.files(baseDir,paste0("^",dateStr,"_.*_softcu_timings\\.csv$"))
  consolidated <- data.frame()
  for (f in files) {
      part <- read.table(paste0(baseDir,"/",f),sep=";",quote="\"")
      server <- paste(str_match(f,"(op)33ias([0-9]+)")[,c(2,3)],collapse="")
      part <- data.frame(rep(server,nrow(part)),part[,])
      consolidated <- rbind(part,consolidated)
  }
  if (nrow(consolidated)==0) return(consolidated)
  names(consolidated) = c("Serveur","Time","Creneau",
    "Libelle_Mesure","ID_Mesure","Mesure","Canal","Portail","Session",
    "User","PDV","Offre","Libelle_Offre","Cas_Gestion", "Poste_Travail",
    "Browser","OS")
  consolidated$Creneau = as.numeric(align.time(strptime(gsub(",",".",consolidated$Time),"%d/%m/%Y %H:%M:%OS"),n=60*5)) %% 86400
  consolidated
}

logs_by_date = new.env()
if (file.exists("/tmp/lbd.RData")) {
  result = load("/tmp/lbd.RData",envir=logs_by_date)
}
