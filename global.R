library(stringr)
library(xts)

getTimingsForDate <- function(baseDir,dateStr) {
  files <- list.files(baseDir,paste0("^",dateStr,"_.*_softcu_timings\\.csv$"))
  consolidated <- data.frame()
  for (f in files) {
      part <- read.table(paste0(baseDir,"/",f),sep=";",quote="\"")
      server <- paste(str_match(f,"(op)33ias([0-9]+)")[,c(2,3)],collapse="")
print(server)
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

reactiveGenerator <- function(shouldContinue, doWork, initialValue) {
  started <- FALSE
  value <- initialValue
  reactive({
    if (!started) {
      started <<- TRUE
    } else {
      value <<- doWork(value)
    }
 
    if (shouldContinue(value)) {
      # It's reasonably safe to use a null session here because we're
      # in a reactive expression, which is lazily run, not an observer
      invalidateLater(10, session=NULL)
    }
 
    value
  })
}

