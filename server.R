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

  output$status  <- renderText({
    if (input$date >= Sys.Date()) return()
    if (nrow(data())==0) "No data for this date"
  })

  output$drillControls <- renderUI({
    if (input$date >= Sys.Date()) return()
    if (nrow(data())==0) return()
    mesures <- levels(data()$ID_Mesure)
    list(
      selectInput("mesure", "Mesure:", as.list(mesures)),
      selectInput("facet", "Stratifier par:", 
        list("-rien-","Serveur","Canal","Portail","Poste_Travail",
        "Cas_Gestion","Browser","OS")
      ),
      sliderInput("cutoff", "Plafond (%):", 
                min=10, max=100, value=99),
      sliderInput("alpha", "Transparence (%):", 
                min=0, max=100, value=5))
  })

  output$plot <- renderPlot({
    if (input$date >= Sys.Date()) return()
    cutoff = input$cutoff/100
    transp = input$alpha/100
    bywhat = input$facet
    if (nrow(data())==0) return()
    sub = subset()
    top = quantile(sub$Mesure/1000,probs=cutoff)
    fmt = function(x){strftime(as.POSIXct(x,origin=Sys.Date()),"%H:%M")}
    plot = ggplot(sub,aes(x=Creneau,y=Mesure/1000))+
	ylab("Duree (secondes)")+
	xlab("Horaire")+
	scale_x_continuous(breaks=seq(0,86400,1800),labels=fmt)+
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+	geom_point(alpha=transp)+	geom_smooth(method="lm")+
	coord_cartesian(xlim=c(25201,71999),ylim=c(0,top))
    if (bywhat!="-rien-") {
	plot = plot+facet_grid(paste(bywhat,"~."))
    }
    print(plot)
  },
  height = function() {
    if (input$date >= Sys.Date()) return(0)
    if (nrow(data())==0) return(0)
    bywhat = input$facet
    sub = subset()
    if (bywhat=="-rien-") {
	400
    } else 300 * length(levels(sub[,bywhat]))
  })

})

library(stringr)
library(xts)
library(ggplot2)
library(scales)

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
  consolidated$Creneau = as.numeric(strptime(gsub(",",".",consolidated$Time),"%d/%m/%Y %H:%M:%OS")) %% 86400
  consolidated
}

logs_by_date = new.env()
if (file.exists("/tmp/lbd.RData")) {
  result = load("/tmp/lbd.RData",envir=logs_by_date)
}
