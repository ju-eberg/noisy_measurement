folge_fragen <- read.csv2(paste0("orig-data/folgefragen.csv"), colClasses = rep("character", 10), fileEncoding = "UTF-8")
folge_fragen$antwort.pos <- as.numeric(folge_fragen$antwort.pos)
folge_fragen$laufindexFolge <- as.numeric(folge_fragen$laufindexFolge)
folge_fragen$questionNumber <- as.numeric(folge_fragen$questionNumber)

############################################################
# Nutze Angaben zur beruflichen Stellung falls die entsprechende Folgefrage nicht beantwortet wurde.
# Idee: Wenn eine Folgefrage dem Anforderungsnivau x zugeordnet, dann können die gleichen Codes übernommen werden, selbst wenn die Folgefrage nicht gestellt wurde und nur das Anforderungsniveau bekannt ist.
############################################################

######## Bereite Folgefragen entsprechend auf
# nur Folgefragen zum Anforderungsniveau behalten
anforderungsniveauVonFolgefragen <- merge(folge_fragen, folge_fragen[folge_fragen$typ == "anforderungsniveau", c("id", "questionNumber")], by = c("id", "questionNumber"))
# anforderungsniveauVonFolgefragen <- folge_fragen[folge_fragen$id %in% folge_fragen[folge_fragen$typ == "anforderungsniveau", "id"],]
anforderungsniveauVonFolgefragen$anforderungsniveau <- substr(anforderungsniveauVonFolgefragen$antwort.kldb, 5,5)
# füge für jede ID jedes beliebige Anforderungsniveau (1,2,3,4,97,98) hinzu
anforderungsniveauVonFolgefragen <- merge(anforderungsniveauVonFolgefragen[anforderungsniveauVonFolgefragen$typ == "",], expand.grid(id = unique(anforderungsniveauVonFolgefragen$id), anforderungsniveau = c(1, 2, 3, 4, 97, 98)), by = c("id", "anforderungsniveau"), all.y = TRUE)
anforderungsniveauVonFolgefragen$antwort.kldb[is.na(anforderungsniveauVonFolgefragen$antwort.kldb)] <- "Code liegt für gewähltes AnfNiveau nicht vor / manuell prüfen"
anforderungsniveauVonFolgefragen$antwort.isco[is.na(anforderungsniveauVonFolgefragen$antwort.isco)] <- "Code liegt für gewähltes AnfNiveau nicht vor / manuell prüfen"
anforderungsniveauVonFolgefragen <- anforderungsniveauVonFolgefragen[, c("id", "anforderungsniveau", "antwort.kldb", "antwort.isco")]
anforderungsniveauVonFolgefragen <- anforderungsniveauVonFolgefragen[!duplicated(anforderungsniveauVonFolgefragen),]

# Funktion um bei gewählter Hilfskategorie & anforderungsniveau einen Code auszuwählen
getFinalToolCode <- function(auswahlHilfsklassifikation, anforderungsniveau, type = "kldb") {
  
  # Im folgenden geht man davon aus, dass anforderungsniveauVonFolgefragen für jede ID alle möglichen Anforderungsniveaus enthält
  if (type == "kldb") {
    code <- mapply(function(ausw, anf) unique(anforderungsniveauVonFolgefragen[anforderungsniveauVonFolgefragen$id == ausw & anforderungsniveauVonFolgefragen$anforderungsniveau == anf, "antwort.kldb"]),
                   auswahlHilfsklassifikation, anforderungsniveau
    )
  }
  
  if (type == "isco") {
    code <- mapply(function(ausw, anf) unique(anforderungsniveauVonFolgefragen[anforderungsniveauVonFolgefragen$id == ausw & anforderungsniveauVonFolgefragen$anforderungsniveau == anf, "antwort.isco"]),
                   auswahlHilfsklassifikation, anforderungsniveau
    )
  }
  
  code <- sapply(code, FUN = function(co) {
    if (length(co) == 0) return("Anforderungsniveau nicht erforderlich / Default Code übernehmen")
    if (length(co) == 1) return(co)
    if (length(co) > 1) return("Mehrere Matches, bitte manuell prüfen")
  })
  
  if (type == "isco") {
    code[auswahlHilfsklassifikation %in% c(1733)] <- "Bei auswahlHilfsklassifikation in 1733 ist der Code mithilfe des AnfNiveaus nicht eindeutig bestimmt, bitte manuell prüfen"
  }
  
  if (type == "kldb") {
    code[auswahlHilfsklassifikation %in% c(1748, 1749)] <- "Bei auswahlHilfsklassifikation in 1748, 1749 ist der Code mithilfe des AnfNiveaus nicht eindeutig bestimmt, bitte manuell prüfen"
  }
  
  code
}
