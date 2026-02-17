
#it's former file called load_Infas_Berufetool_Datenbankabzug_20190706_v2.R.R"

# the following variable needs to be set to use this script
path_to_data <- getwd()
# path_to_data <- "."

library(readxl)
library(data.table)
library(haven)
library(dplyr)

# Daten direkt aus dem Tool (erstellt mithilfe der Datei erzeuge_Infas_Datenbankabzug_20190706.R)
load(paste0(path_to_data, "/data/Infas_Berufetool_Datenbankabzug_20190706.RData"))
# Replikation der Vorhersagen im Tool (erstellt auf AWS, siehe erzeuge_Infas_Datenbankabzug_20190706.R)
load(paste0(path_to_data, "/data/sessionIdMitAntwortTextAngereichertMitPredictionData.RData"))

# Lade Folge-Fragen
folge_fragen <- read.csv2(paste0(path_to_data, "/data/folgefragen.csv"), colClasses = rep("character", 10), fileEncoding = "UTF-8")
folge_fragen$antwort.pos <- as.numeric(folge_fragen$antwort.pos)
folge_fragen$laufindexFolge <- as.numeric(folge_fragen$laufindexFolge)
folge_fragen$questionNumber <- as.numeric(folge_fragen$questionNumber)

infas_completed_interviews <- haven::read_dta(paste0(path_to_data, "/data/infas_7130_AuxCo Usability Test_Enddatensatz_realisierte Interviews_20190712_1.dta"))
infas_incomplete_interviews <- haven::read_dta(paste0(path_to_data, "/data/infas_7130_AuxCo Usability Test_Enddatensatz_unvollstaendige Interviews_20190712_1.dta"))

# Bei den completed Interviews die manuellen Codes ergänzen (auskommentiert weil alte Lieferung ohne Erst-/Zweitkodierung)
# infas_kodierungen <- haven::read_dta(paste0(path_to_data, "/infas_7130_AuxCo Usability Test_Enddatensatz_Berufscodierung_20190716_1.dta"))
# infas_kodierungen$InfasOrigKldbNum <- as.numeric(infas_kodierungen$kldb)
# infas_kodierungen$InfasOrigKldBStr <- as_factor(infas_kodierungen$kldb, levels = "both")
# infas_kodierungen$InfasOrigIscoNum <- as.numeric(infas_kodierungen$isco)
# infas_kodierungen$InfasOrigIscoStr <- as_factor(infas_kodierungen$isco, levels = "both")
# infas_completed_interviews <- merge(infas_completed_interviews, infas_kodierungen[, c("INTNR", "InfasOrigKldbNum", "InfasOrigKldBStr", "InfasOrigIscoNum", "InfasOrigIscoStr")], by = "INTNR")
# noch einmal das selbe laden mit neuer Lieferung
infas_kodierungen <- haven::read_dta(paste0(path_to_data, "/data/infas_7130_AuxCo Usability Test_Enddatensatz_Berufscodierung_20191017_2.dta"))
infas_kodierungen$InfasOrigKldbNum <- as.numeric(infas_kodierungen$kldb)
infas_kodierungen$InfasOrigKldBStr <- as_factor(infas_kodierungen$kldb, levels = "both")
infas_kodierungen$InfasOrigIscoNum <- as.numeric(infas_kodierungen$isco)
infas_kodierungen$InfasOrigIscoStr <- as_factor(infas_kodierungen$isco, levels = "both")
infas_kodierungen$INTERNR_neu <- as.numeric(infas_kodierungen$INTERNR)

infas_completed_interviews <- as.data.frame(infas_completed_interviews)
infas_completed_interviews <- merge(infas_completed_interviews, infas_kodierungen[, c("INTNR", "INTERNR_neu", "res1_kldb", "res2_kldb", "res3_kldb", "InfasOrigKldbNum", "InfasOrigKldBStr", "res1_isco", "res2_isco", "res3_isco", "interprob", "InfasOrigIscoNum", "InfasOrigIscoStr")], by = "INTNR")

# Lade interviewermerkmale
interviewermerkmale <- haven::read_dta(paste0(path_to_data, "/data/infas_7130_AuxCo Usability Test_Interviewermerkmale_20191017_2.dta"))
# setdiff(interviewermerkmale$INTERNR, infas_completed_interviews$INTERNR_neu) # 22 Interviewnummern liegen nicht im neuen Datensatz vor

# complete and incomplete interviews in einem Datensatz zusammenfügen
infas_completed_interviews[, "interviewCompleted"] <- TRUE
infas_incomplete_interviews[, "interviewCompleted"] <- FALSE
infas_incomplete_interviews[, "InfasOrigKldbNum"] <- NA
infas_incomplete_interviews[, "InfasOrigKldBStr"] <- NA
infas_incomplete_interviews[, "InfasOrigIscoNum"] <- NA
infas_incomplete_interviews[, "InfasOrigIscoStr"] <- NA
infas_incomplete_interviews[, "INTERNR_neu"] <- NA
infas_incomplete_interviews[, "res1_kldb"] <- NA
infas_incomplete_interviews[, "res2_kldb"] <- NA
infas_incomplete_interviews[, "res3_kldb"] <- NA
infas_incomplete_interviews[, "res1_isco"] <- NA
infas_incomplete_interviews[, "res2_isco"] <- NA
infas_incomplete_interviews[, "res3_isco"] <- NA
infas_incomplete_interviews[, "interprob"] <- NA

InterviewerBefragtenMatch <- rbind(infas_completed_interviews, as.data.frame(infas_incomplete_interviews))

# Zwischenstand der Befragten-Ids gematcht mit Interviewer-IDs (von Anfang Juni)
# InterviewerBefragtenMatch <- readxl::read_xlsx(paste0(path_to_data, "/InterviewerBefragtenMatch.xlsx"), sheet = 1, col_names = c("IntId", "userId", "gruppe"), skip = 1)

# füge Bedeutung der studId hinzu
resultOverview$userIdTyp <- NA
resultOverview$userIdTyp[resultOverview$studid == "12225be"] <- "Auffrischer"
resultOverview$userIdTyp[resultOverview$studid == "12225bf"] <- "Panel"
resultOverview$userIdTyp[resultOverview$studid == "12225DATENNACHTRAG"] <- "DATENNACHTRAG" # Aufgrund eines Bugs wurde die Info nicht im Tool gespeichert. Die Daten wurden erst später nachgetragen.

################
# Zwischenergebnisse zum Matching:
# alle Personen, die laut Infas das Tool durchlaufen haben sollen, haben wir auch in unserer Datenbank
setdiff(InterviewerBefragtenMatch$INTNR[InterviewerBefragtenMatch$tool == 1], substr(resultOverview[resultOverview$studid %in% c("12225be", "12225bf", "12225DATENNACHTRAG"), "sessionId"], 1, 8))
# aber umgekehrt haben wir 46 Personen, die das Tool nicht durchlaufen sollten. (vmtl. bei 12 davon wurde das Tool abgrochen, weil Befragte realisieren, dass sie doch nicht erwerbstätig sind??)
setdiff(substr(resultOverview[resultOverview$studid %in% c("12225be", "12225bf", "12225DATENNACHTRAG"), "sessionId"], 1, 8), InterviewerBefragtenMatch$INTNR[InterviewerBefragtenMatch$tool == 1])
# und 34 Personen tauchen überhaupt nicht in den Datensätzen von Infas auf. Birgit: "Für die restlichen 34 Fälle [,die das Interview vorzeitig beendet haben, MS] wurden keine Daten aus dem Interview in unser U-File geschrieben und können daher nicht ausgeliefert werden.
setdiff(substr(resultOverview[resultOverview$studid %in% c("12225be", "12225bf", "12225DATENNACHTRAG"), "sessionId"], 1, 8), InterviewerBefragtenMatch$INTNR)
## Außerdem fragt Oli zur der ID 23226399 nach Informationen, zu der wir aber nirgends etwas gespeichert haben.
#################

# delete all test cases from the data, only keep valid interviews
# validSessionIds <- resultOverview[resultOverview$studid %in% c("12225be", "12225bf", "12225DATENNACHTRAG"), "sessionId"]
### only keep Ids from first data delivery from infas (intentionally wrong)

validUserIds <- setdiff(InterviewerBefragtenMatch$INTNR, "13435853") # 13435853 ist bei den infas_incomplete_interviews enthalten, aber kommt aus der Testphase (studid = 12229be) -> löschen
validSessionIds <- union(answerSubmitted[answerSubmitted$userId %in% validUserIds, "sessionId"],
                         resultOverview[resultOverview$userId %in% validUserIds, "sessionId"])
# setdiff(validSessionIds, resultOverview[resultOverview$userId %in% validUserIds, "sessionId"])
# setdiff(validUserIds, substring(validSessionIds, 1, 8))

resultOverview <- resultOverview[resultOverview$sessionId %in% validSessionIds, ]
answerSubmitted <- answerSubmitted[answerSubmitted$sessionId %in% validSessionIds, ]
occupationsSuggested <- occupationsSuggested[occupationsSuggested$sessionId %in% validSessionIds, ]
toggleSubmitted <- toggleSubmitted[toggleSubmitted$sessionId %in% validSessionIds, ]

# # füge die Freitextantwort "Oder machen sie etwas anderes?" zu resultOverview hinzu, erfolgt unten aber nochmals eleganter
# etwas_anderes_text <- answerSubmitted[answerSubmitted$questId == "auswahl_hilfsklassifikation", c("sessionId", "textNoneSelected")]
# names(etwas_anderes_text)[2] <- "hilfsklassifikationAnderesText"
# resultOverview <- merge(resultOverview, etwas_anderes_text[!duplicated(etwas_anderes_text$sessionId, fromLast = TRUE),], all.x = TRUE, by = "sessionId") # duplicates (weil zurück-button geklickt wurde) raus, dann mergen
# rm(etwas_anderes_text)

########################################
# Codeschnipsel und ToDos
########################################
# merge mit Infas-Interviewerschlüssel
# wir haben 1379 Befragte, die das Tool durchlaufen sollten und das Interview beendet haben. Zusätzlich haben wir Befragte, die mit dem Tool angefangen haben und solche, die das Interview nicht beendet haben.
## Achtung: einige userIds sind mehrfach ins Tool gestartet, daher haben wir für 1484 sessionIds
# length(unique(infas_completed_interviews$INTNR[!is.na(InterviewerBefragtenMatch$tool)])) 
# length(unique(InterviewerBefragtenMatch$INTNR[!is.na(InterviewerBefragtenMatch$tool)]))  
# length(unique(resultOverview$sessionId))
# length(unique(answerSubmitted$userId))
# length(unique(c(resultOverview$userId, InterviewerBefragtenMatch$INTNR, answerSubmitted$userId)))

####
# transformiere answerSubmitted ins wide-Format (= eine Zeile pro session Id). Wenn mit dem Zurück-Button gearbeitet wurde, wird für jede Frage nur der Eintrag gespeichert, bei dem das Enddatum am kürzesten zurückliegt (= die endgültige Auswahl). Bei Nutzung des Zurück-Buttons können hier Angaben gespeichert sein, die aufgrund der Filterführung gar nicht hätten gestellt werden dürfen.
answerSubmittedWide <- dcast(data.table(answerSubmitted)[, tail(.SD[order(end, decreasing = FALSE)], 1),by = list(sessionId, questId)], 
                             sessionId ~ questId, 
                             value.var = c("antwortId", "antwortText", "textNoneSelected", "start", "end"))

# an resultOverview anhängen
resultOverview <- merge(resultOverview, answerSubmittedWide, by = "sessionId", all = TRUE)
resultOverview <- merge(resultOverview, infasPredictions, by.x = c("sessionId", "start_beruf_taetigkeit_text"), by.y = c("sessionId", "start"), all.x = TRUE, all.y = FALSE)
# resultOverview$userId[resultOverview$sessionId == "23433239_1554308854_7"] <- "23433239" # there is one person missing in person
resultOverview <- merge(resultOverview, InterviewerBefragtenMatch, by.x = "userId", by.y = "INTNR", all.x = TRUE, all.y = TRUE)


#### Small helpers
# Mache aus einem Data.table eine data.frame
# class(answerSubmittedWide) <- "data.frame"
# entferne zur besseren Übersichtlichkeit alle Spalten mit Bezug zu Follow-Up-Fragen
# answerSubmittedWide <- answerSubmittedWide[, !grepl("followUp", names(answerSubmittedWide))]

rm(answerSubmittedWide)

resultOverview <- data.table(resultOverview)


############################################################
# Nutze Angaben zur beruflichen Stellung falls die entsprechende Folgefrage nicht beantwortet wurde.
# Idee: Wenn eine Folgefrage dem Anforderungsnivau x zugeordnet, dann können die gleichen Codes übernommen werden, selbst wenn die Folgefrage nicht gestellt wurde und nur das Anforderungsniveau bekannt ist.
############################################################
# erzeuge variable zum Anforderungsniveau
resultOverview <- resultOverview %>% mutate(anforderungsniveauFromErfAusbildung = case_when(beco10 == 1 | beco10 == 2 ~ 1, # keine Ausbildung / Anlernausbildung
                                                                                            beco10 == 3 ~ 2, # abgeschlossene Berufsausbildung
                                                                                            beco10 == 5 ~ 3, # ein Fortbildungsabschluss, z.B. zum Meister oder Techniker
                                                                                            beco10 == 4 ~ 4, # ein Fachhochschul- oder Universitätsabschluss
                                                                                            beco10 == 97 | beco10 == 98 ~ 9))
resultOverview <- data.table(resultOverview)
# View(resultOverview[, c("beco10", "anforderungsniveauFromErfAusbildung")])

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
# Berufliche Stellung
# table(resultOverview$sozp12_neu)
# table(resultOverview$sozp12_01)

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

auxco_id_that_have_two_followups <- unique(folge_fragen[folge_fragen$questionNumber == 2,"id"])

# cbind(resultOverview$auswahlHilfsklassifikation, resultOverview$anforderungsniveauFromErfAusbildung, final = getFinalToolCode(resultOverview$auswahlHilfsklassifikation, resultOverview$anforderungsniveauFromErfAusbildung, type = "isco"))
#
############################################################
# remove duplicate user ids
############################################################
# IDs, bei denen das Tool mehrfach aufgerufen wurde
# hier muss man dann vermutlich per Hand durchgehen, welchen Eintrag man sich weiter anschaut.
# resultOverview[, .N, by = userId][N > 1]
duplicateUserIds <- resultOverview[, .N, by = userId][N > 1, userId]

# look at duplicates and plan what needs to be done
# View(resultOverview[userId %in% duplicateUserIds, list(sessionId, berufTaetigkeitText, nchar(berufTaetigkeitText), start_beruf_taetigkeit_text, end_beruf_taetigkeit_text, end_beruf_taetigkeit2_text, end_befragter_schwierigkeiten1,end_befragter_uebereinstimmung, end_warum_etwas_anderes )])
# Nicht sessionIds aus resultOverview stehen auch in answerSubmitted
# setdiff(resultOverview$sessionId, answerSubmitted$sessionId)

# remove sessionIds if its first verbal answer was NA or length 0
removeSessionsIds <- resultOverview[userId %in% duplicateUserIds & (berufTaetigkeitText %in% c("", "NA")) & sessionId != "13424733_1556130639_6" & sessionId != "13468003_1556645633_6" & sessionId != "23226399_1560885373_2", sessionId] #  UserId 13424733 und 13468003 hat 2-mal nichts gesagt, aber beim ersten Mal wurden ein paar Zeiten gespeichert -> ersten Eintrag 13424733_1556130639_6 behalten
# what to do with the remaining duplicate session IDs? Remove the ones which started earlier
duplicateUserIds <- resultOverview[!(sessionId %in% removeSessionsIds), .N, by = userId][N > 1, userId]
removeSessionsIds <- c(removeSessionsIds, resultOverview[userId %in% duplicateUserIds & sessionId != "23413313_1557767992_7", .SD[max(start_beruf_taetigkeit_text) != start_beruf_taetigkeit_text, sessionId], by = userId][, V1]) # 23413313_1557767992_7 ist kritisch, weil die Freitextantwort als letztes genannt wird.

# check that we remove the correct number of sessionIds
length(removeSessionsIds) == resultOverview[, .N, by = userId][N > 1][, sum(N) - .N]

resultOverview <- resultOverview[!(sessionId %in% removeSessionsIds), ]
answerSubmitted <- answerSubmitted[!(answerSubmitted$sessionId %in% removeSessionsIds), ]
occupationsSuggested <- occupationsSuggested[!(occupationsSuggested$sessionId %in% removeSessionsIds), ]
toggleSubmitted <- toggleSubmitted[!(toggleSubmitted$sessionId %in% removeSessionsIds), ]


##########################
# merge with Infas data
##########################



# check that both columns are identical
# resultOverview[antwortText_beruf_taetigkeit_text != antwortText | is.na(antwortText_beruf_taetigkeit_text) | is.na(antwortText), list(antwortText_beruf_taetigkeit_text, antwortText)]

# die folgenden Variablen sind immer NA:
# unique(resultOverview$textNoneSelected_beruf_taetigkeit_text)
# unique(resultOverview$textNoneSelected_beruf_taetigkeit2_text)
# unique(resultOverview$textNoneSelected_interviewer_schwierigkeiten1)
# resultOverview[!is.na(textNoneSelected_interviewer_schwierigkeiten1a), .(textNoneSelected_interviewer_schwierigkeiten1a, textNoneSelected_auswahl_hilfsklassifikation)] # BUG: hier wurden Daten doppelt gespeichert.
# unique(resultOverview$antwortText_interviewer_schwierigkeiten2)
# unique(resultOverview$antwortText_befragter_schwierigkeiten1)
# unique(resultOverview$textNoneSelected_befragter_schwierigkeiten1)
# unique(resultOverview$textNoneSelected_beruf_taetigkeit_kontrolltext)
# unique(resultOverview$antwortText_befragter_uebereinstimmung)
# unique(resultOverview$textNoneSelected_befragter_uebereinstimmung)
# unique(resultOverview$antwortText_warum_etwas_anderes)

# create an alternative file without times and followUp questions
# names(resultOverview) # find relevant variable names

# In der folgenden Tabelle sind hauptsächlich Ergebnisse gespeichert aus resultOverview und aus answerSubmitted 
# Die Angaben aus beiden Tabellen sollten identisch sein mit folgender Einschränkung: Wenn eine Antwort gespeichert wird, aber später über den Zurück-Button und geänderte Filterführung nicht aktualisiert wird, sollten  Unterschiede auftreten (nur in resultOverview ist die Filterführung berücksichtigt). Oder Unterschiede haben technische Probleme/Programmierfehler zur Ursache.
resultOverviewCompact <- resultOverview[,list(sessionId,
                                              userId,
                                              interviewerID = INTERNR,
                                              interviewerID_neu = INTERNR_neu,
                                              # Rahmenbedingungen
                                              # interviewerGruppe = c("0" = "sonstige", "1" = "conversational", "2" = "standardized")[gruppen],
                                              conversational, # Modus der Befragungssoftware (abgeleitet von der URL in resultOverview)
                                              berufstyp, # aktueller Beruf oder vergangener Beruf (abgeleitet von der URL in resultOverview)
                                              audiorecording, # Audiorecording verfügbar (abgeleitet von der URL in resultOverview)?
                                              userIdTyp, # abgeleitet von studId
                                              
                                              # from Infas
                                              toolStarted = tool, # 1 = Tool soll laut Filterführung starten, NA = nicht
                                              interviewCompleted, # Interview wurde bis zum Ende durchgeführt
                                    
                                              # Erste Freitextfrage
                                              berufTaetigkeitText, # von datenbank result overview
                                              antwortText_beruf_taetigkeit_text = ifelse(is.na(antwortText_beruf_taetigkeit_text), "NA", antwortText_beruf_taetigkeit_text), # von datenbank answer submitted und für Vorhersagen verwendet
                                              antwortId_beruf_taetigkeit_text = ifelse(antwortId_beruf_taetigkeit_text == "TRUE", "*** Keine Angabe checked", "*** Keine Angabe unchecked"),
                                              neutralTitleFromDictionaryUnused = neutral.title, # welcher neutral title wäre verwendet worden wenn wir den Coding index verwendet hätten?
                                              #                                              dictionaryCodedTitle, # verwendeter Neutral.title (hier immer NA, weil das feature deaktiviert war)
                                              Osa1111SumProb = Osa1111SumProb, # Wenn dieser Wert > 0.535 ist, wurde die Osa1111-Stringdistanz verwendet, ansonsten ... (Summe der Wahrscheinlichkeiten als Indikator dafür, dass ein Vorschlag zutrifft)
                                              SubstringSumProb = SubstringSumProb, # Wenn Osa1111SumProb <= 0.535 ist, wurde die Substring-Stringdistanz verwendet. Wenn SubstringSumProb > 0.02 wurden Tätigkeiten vorgeschlagen (auswahl_hilfsklassifikation), ansonsten Freitextfrage (beruf_taetigkeit2_text). Ursprünglicher optimierter Threshold war 0.618, aber um mehr schlechte Antworten vorzuschlagen, haben wir dies auf 0.02 reduziert.
                                              # FILTERKRITERIUM: Schlagen wir wahrscheinlich eine passende Tätigkeit vor? (= Osa1111SumProb > 0.535 | SubstringSumProb > 0.02)
                                              # IF ... :Nur gefüllt falls keine Angaben vorgeschlagen wurden:
                                              #                                              berufTaetigkeitText2 = berufTaetigkeitText2, # von datenbank result overview
                                              antwortText_beruf_taetigkeit2_text = ifelse(is.na(antwortText_beruf_taetigkeit2_text), "NA", antwortText_beruf_taetigkeit2_text), # von datenbank answer submitted
                                              antwortId_beruf_taetigkeit2_text = ifelse(antwortId_beruf_taetigkeit2_text == "TRUE", "*** Keine Angabe checked", "*** Keine Angabe unchecked"),
                                              # ELSE Nur gefüllt falls Tätigkeiten vorgeschlagen wurden:
                                              auswahlHilfsklassifikation, # von datenbank result overview
                                              antwortId_auswahl_hilfsklassifikation, # von datenbank answer submitted
                                              auswahlHilfsklassifikationText, # von datenbank result overview, Text zur Id in auswahlHilfsklassifikation
                                              antwortText_auswahl_hilfsklassifikation,  # von datenbank answer submitted, Text zur ID in antwortId_auswahl_hilfsklassifikation
                                              textNoneSelected_auswahl_hilfsklassifikation, # Bitte beschreiben Sie mir diese Tätigkeit genau. (wenn etwas anderes ausgewählt wurde)
                                              #                                               auswahlKldbDreistellig (nur verwendet, falls KldB-Kategorien 3-stellig vorgeschlagen werden anstelle der AuxCo)
                                              # FILTER für Folgefragen: Wenn laut antwortId_auswahl_hilfsklassifikation Folgefragen nötig sind
                                              # (die ausführlichen Ergebnisse aus answerSubmitted stehen in resultOverview, aber werden der Übersichtlichkeit halber hier sehr kurz aus datenbank result overview zusammengefasst.)
                                              followUp1Question, followUp1Answer, 
                                              followUp2Question, followUp2Answer,
                                              # KldB- und ISCO-Kodierungen (vorläufig, weil das Anforderungsniveau nicht abgefragt wurde und die folgenden Codes anhand mithilfe des Anforderungsniveaus (und der Führungstätigkeit?) noch aktualisiert werden muss.)
                                              kldb_tool_vorlaufig = kldb,
                                              isco_tool_vorlaufig = isco,
                                              
                                              anfniveau = anforderungsniveauFromErfAusbildung, # versuche die finalen Codes mithilfe des Anforderungsniveaus zu bestimmen
                                              auswahlHilfsklassifikationHasTwoFollowUps = auswahlHilfsklassifikation %in% auxco_id_that_have_two_followups,
                                              kldb_tool_mit_anfniveau = getFinalToolCode(auswahlHilfsklassifikation, anforderungsniveauFromErfAusbildung, type = "kldb"),
                                              isco_tool_mit_anfniveau = getFinalToolCode(auswahlHilfsklassifikation, anforderungsniveauFromErfAusbildung, type = "isco"),
                                              
                                              kldb_infas_num = InfasOrigKldbNum, # finale Kodierungen von Infas
                                              kldb_infas_str = InfasOrigKldBStr,
                                              isco_infas_num = InfasOrigIscoNum, 
                                              isco_infas_str = InfasOrigIscoStr,
                                              res1_kldb, res2_kldb, res3_kldb, # Erstkodierung, Zweitkodierung, Dirttkodierung/Problemfallbespr?chung KldB
                                              res1_isco, res2_isco, res3_isco, # Erstkodierung, Zweitkodierung, Dirttkodierung/Problemfallbespr?chung ISCO
                                              schluessel_kldb_zu_isco_genutzt = interprob, # Mittels Umsteigeschl?ssel KldB zu ISCO automatisch generiert 
                                               
                                              # FILTER: wenn "etwas anderes" ausgewählt wurde (= !(antwortId_auswahl_hilfsklassifikation %in% c("95", "99", "EMPTY")) ) wird keine Kontrollfrage gestellt. (vmtl. war bis 17.5. ein Bug enthalten: "EMPTY" fehlte in der Bedingung) Hier Kontrollfrage:
                                              berufTaetigkeitKontrolltext,  # von datenbank result overview
                                              antwortText_beruf_taetigkeit_kontrolltext = ifelse(is.na(antwortText_beruf_taetigkeit_kontrolltext), "NA", antwortText_beruf_taetigkeit_kontrolltext), # von datenbank answer submitted
                                              antwortId_beruf_taetigkeit_kontrolltext = ifelse(antwortId_beruf_taetigkeit_kontrolltext == "TRUE", "*** Keine Angabe checked", "*** Keine Angabe unchecked"),
                                              
                                              # Evaluationsfragen wenn Tätigkeiten vorgeschlagen wurden
                                              ## Interviewereinschätzung: Wie einfach oder schwierig war es für den Befragten, eine Tätigkeitsbeschreibung auszuwählen?
                                              interviewerSchwierigkeiten1 = c("1" = "sehr einfach", "2" = "eher einfach", "3" = "eher schwierig", "4" = "sehr schwierig", "99" = "*** keine sinnvolle Antwort möglich", "EMPTY" = "Keine Auswahl getroffen")[interviewerSchwierigkeiten1], # von datenbank result overview
                                              antwortId_interviewer_schwierigkeiten1 = c("1" = "sehr einfach", "2" = "eher einfach", "3" = "eher schwierig", "4" = "sehr schwierig", "99" = "*** keine sinnvolle Antwort möglich", "EMPTY" = "Keine Auswahl getroffen")[antwortId_interviewer_schwierigkeiten1], # von datenbank answer submitted
                                              ## Filter für die kommenden zwei Fragen: Wenn zuvor eher schwierig oder sehr schwierig ausgewählt wurde:
                                              # Interviewereinschätzung: Hat dies die Durchführung des Interviews erschwert?
                                              interviewerSchwierigkeiten1a = c("1" = "ja, deutlich", "2" = "ja, ein wenig", "3" = "nein, fast gar nicht", "4" = "nein, überhaupt nicht", "99" = "*** keine sinnvolle Antwort möglich", "EMPTY" = "Keine Auswahl getroffen")[interviewerSchwierigkeiten1a], # von datenbank result overview
                                              antwortId_interviewer_schwierigkeiten1a = c("1" = "ja, deutlich", "2" = "ja, ein wenig", "3" = "nein, fast gar nicht", "4" = "nein, überhaupt nicht", "99" = "*** keine sinnvolle Antwort möglich", "EMPTY" = "Keine Auswahl getroffen")[antwortId_interviewer_schwierigkeiten1a], # von datenbank answer submitted
                                              # Interviewereinschätzung: Welche spezifischen Probleme sind aufgetreten?
                                              interviewerSchwierigkeiten2Id = c("1" = "allgemeine Verständnisprobleme in Bezug auf die Aufgabe", "2" = "Verständnisprobleme in Bezug auf einzelne Antwortoptionen", "3" = "Verständnisprobleme in Bezug auf Erläuterungen zu einzelnen Antwortoptionen", "4" = "Probleme, die eigene Tätigkeit einzuordnen, weil keine Antwortoption passend war", "5" = "Probleme, die eigene Tätigkeit einzuordnen, weil mehrere Antwortoptionen passend waren", "6" = "Technische Probleme", "7" = "Andere Probleme, nämlich:", "99" = "*** keine sinnvolle Antwort möglich", "EMPTY" = "Keine Auswahl getroffen")[interviewerSchwierigkeiten2Id],
                                              antwortId_interviewer_schwierigkeiten2 = c("1" = "allgemeine Verständnisprobleme in Bezug auf die Aufgabe", "2" = "Verständnisprobleme in Bezug auf einzelne Antwortoptionen", "3" = "Verständnisprobleme in Bezug auf Erläuterungen zu einzelnen Antwortoptionen", "4" = "Probleme, die eigene Tätigkeit einzuordnen, weil keine Antwortoption passend war", "5" = "Probleme, die eigene Tätigkeit einzuordnen, weil mehrere Antwortoptionen passend waren", "6" = "Technische Probleme", "7" = "Andere Probleme, nämlich:", "99" = "*** keine sinnvolle Antwort möglich", "EMPTY" = "Keine Auswahl getroffen")[antwortId_interviewer_schwierigkeiten2],
                                              interviewerSchwierigkeiten2Text, textNoneSelected_interviewer_schwierigkeiten2,
                                              # Befragteneinschätzung: Wie einfach oder schwierig war die Auswahl einer Tätigkeitsbeschreibung, die auf Ihren Beruf zutrifft? War die Auswahl sehr einfach, eher einfach, eher schwierig oder sehr schwierig?
                                              befragterSchwierigkeiten1  = c("1" = "sehr einfach", "2" = "eher einfach", "3" = "eher schwierig", "4" = "sehr schwierig", "97" = "*** verweigert", "98" = "*** weiß nicht", "99" = "*** keine sinnvolle Antwort möglich", "EMPTY" = "Keine Auswahl getroffen")[befragterSchwierigkeiten1],
                                              antwortId_befragter_schwierigkeiten1 = c("1" = "sehr einfach", "2" = "eher einfach", "3" = "eher schwierig", "4" = "sehr schwierig", "97" = "*** verweigert", "98" = "*** weiß nicht", "99" = "*** keine sinnvolle Antwort möglich", "EMPTY" = "Keine Auswahl getroffen")[antwortId_befragter_schwierigkeiten1],
                                              # Befragteneinschätzung: In welchem Maße stimmt diese Tätigkeit mit Ihren tatsächlichen Tätigkeiten im Beruf überein? Ist die Übereinstimmung gering, mittel, groß oder sehr groß?
                                              befragterUebereinstimmung = c("1" = "geringe Übereinstimmung", "2" = "mittlere Übereinstimmung", "3" = "große Übereinstimmung", "4" = "sehr große Übereinstimmung", "97" = "*** verweigert", "98" = "*** weiß nicht", "99" = "*** keine sinnvolle Antwort möglich", "EMPTY" = "Keine Auswahl getroffen")[befragterUebereinstimmung],
                                              antwortId_befragter_uebereinstimmung = c("1" = "geringe Übereinstimmung", "2" = "mittlere Übereinstimmung", "3" = "große Übereinstimmung", "4" = "sehr große Übereinstimmung", "97" = "*** verweigert", "98" = "*** weiß nicht", "99" = "*** keine sinnvolle Antwort möglich", "EMPTY" = "Keine Auswahl getroffen")[antwortId_befragter_uebereinstimmung],
                                              # FILTER: Nur wenn der Befragte etwas anderes ausgewählt hat (= (antwortId_auswahl_hilfsklassifikation == "95" | (antwortId_auswahl_hilfsklassifikation == "EMPTY" & nchar(textNoneSelected_auswahl_hilfsklassifikation) > 1)) ) # BUG bis 17.5.: der Filter für EMPTY wurde erst dann eingebaut, vorher wurde diese Frage nur gestellt bei antwortId_auswahl_hilfsklassifikation == "95"
                                              # Befragteneinschätzung: Warum war keine der vorgeschlagenen Antwortoptionen passend?
                                              warumEtwasAnderesId = c("1" = "berufliche Tätigkeit nicht aufgeführt", "2" = "nur teilweise Übereinstimmung mit dem Beruf des Befragten", "3" = "mehrere Antwortoptionen plausibel", "4" = "Frage nicht verstanden", "5" = "Andere Gründe (bitte eintragen):", "97" = "*** verweigert", "98" = "*** weiß nicht", "99" = "*** keine sinnvolle Antwort möglich", "EMPTY" = "Keine Auswahl getroffen")[warumEtwasAnderesId],
                                              antwortId_warum_etwas_anderes = c("1" = "berufliche Tätigkeit nicht aufgeführt", "2" = "nur teilweise Übereinstimmung mit dem Beruf des Befragten", "3" = "mehrere Antwortoptionen plausibel", "4" = "Frage nicht verstanden", "5" = "Andere Gründe (bitte eintragen):", "97" = "*** verweigert", "98" = "*** weiß nicht", "99" = "*** keine sinnvolle Antwort möglich", "EMPTY" = "Keine Auswahl getroffen")[antwortId_warum_etwas_anderes],
                                              warumEtwasAnderesText, textNoneSelected_warum_etwas_anderes,
                                              
                                              # Zeiten (alle übernommen aus answerSubmitted) (ggf. sind alle Uhrzeiten genau 1h abweichend von der deutschen Uhrzeit)
                                              start_beruf_taetigkeit_text, end_beruf_taetigkeit_text, # Zeiterfassung in end_beruf_taetigkeit_text erfolgt bevor die Berechnungen erfolgen, ggf. daher kleine Verzögerung bis die nächste Frage startet
                                              start_beruf_taetigkeit2_text, end_beruf_taetigkeit2_text, 
                                              start_auswahl_hilfsklassifikation, end_auswahl_hilfsklassifikation,
                                              # an dieser Stelle liegt eine detaillierte Zeiterfassung für die Folgefragen vor, aber nicht in dieser Tabelle eingebaut.
                                              start_beruf_taetigkeit_kontrolltext, end_beruf_taetigkeit_kontrolltext,
                                              
                                              start_interviewer_schwierigkeiten1, end_interviewer_schwierigkeiten1,
                                              start_interviewer_schwierigkeiten1a, end_interviewer_schwierigkeiten1a,
                                              start_interviewer_schwierigkeiten2, end_interviewer_schwierigkeiten2,
                                              
                                              start_befragter_schwierigkeiten1, end_befragter_schwierigkeiten1, 
                                              start_befragter_uebereinstimmung, end_befragter_uebereinstimmung,
                                              start_warum_etwas_anderes, end_warum_etwas_anderes
                                              )]

# zeige die ausgewählten IDs aus der Hilfsklassifikation an, die kein passendes Anforderungsniveau enthalten
resultOverviewCompact[kldb_tool_mit_anfniveau == "Code liegt für gewähltes AnfNiveau nicht vor / manuell prüfen", .N, by = auswahlHilfsklassifikation]

# ausgewählte ID 1733 anhand der Folgefragen korrigieren
resultOverviewCompact[anfniveau == 3 & isco_tool_mit_anfniveau == "Bei auswahlHilfsklassifikation in 1733 ist der Code mithilfe des AnfNiveaus nicht eindeutig bestimmt, bitte manuell prüfen", isco_tool_mit_anfniveau := ifelse(followUp1Answer == "Elektrotechnik", "3113", "3114")]
resultOverviewCompact[anfniveau == 4 & isco_tool_mit_anfniveau == "Bei auswahlHilfsklassifikation in 1733 ist der Code mithilfe des AnfNiveaus nicht eindeutig bestimmt, bitte manuell prüfen", isco_tool_mit_anfniveau := ifelse(followUp1Answer == "Elektrotechnik", "2151", "2152")]
resultOverviewCompact[isco_tool_mit_anfniveau == "Bei auswahlHilfsklassifikation in 1733 ist der Code mithilfe des AnfNiveaus nicht eindeutig bestimmt, bitte manuell prüfen", isco_tool_mit_anfniveau := "Code liegt für gewähltes AnfNiveau nicht vor / manuell prüfen"]

# ausgewählte ID 1748 anhand der Folgefragen korrigieren (nicht getestet)
resultOverviewCompact[anfniveau == 3 & auswahlHilfsklassifikation == 1748 & kldb_tool_mit_anfniveau == "Bei auswahlHilfsklassifikation in 1748, 1749 ist der Code mithilfe des AnfNiveaus nicht eindeutig bestimmt, bitte manuell prüfen", kldb_tool_mit_anfniveau := ifelse(followUp1Answer == "Ja", "61203", "61283")]
resultOverviewCompact[anfniveau == 4 & auswahlHilfsklassifikation == 1748 & kldb_tool_mit_anfniveau == "Bei auswahlHilfsklassifikation in 1748, 1749 ist der Code mithilfe des AnfNiveaus nicht eindeutig bestimmt, bitte manuell prüfen", kldb_tool_mit_anfniveau := ifelse(followUp1Answer == "Ja", "61204", "61284")]
resultOverviewCompact[auswahlHilfsklassifikation == 1748 & kldb_tool_mit_anfniveau == "Bei auswahlHilfsklassifikation in 1748, 1749 ist der Code mithilfe des AnfNiveaus nicht eindeutig bestimmt, bitte manuell prüfen", kldb_tool_mit_anfniveau := "Code liegt für gewähltes AnfNiveau nicht vor / manuell prüfen"]

# ausgewählte ID 1749 anhand der Folgefragen korrigieren (nicht getestet)
resultOverviewCompact[anfniveau == 3 & auswahlHilfsklassifikation == 1749 & kldb_tool_mit_anfniveau == "Bei auswahlHilfsklassifikation in 1748, 1749 ist der Code mithilfe des AnfNiveaus nicht eindeutig bestimmt, bitte manuell prüfen", kldb_tool_mit_anfniveau := ifelse(followUp1Answer == "Ja", "61213", "61283")]
resultOverviewCompact[anfniveau == 4 & auswahlHilfsklassifikation == 1749 & kldb_tool_mit_anfniveau == "Bei auswahlHilfsklassifikation in 1748, 1749 ist der Code mithilfe des AnfNiveaus nicht eindeutig bestimmt, bitte manuell prüfen", kldb_tool_mit_anfniveau := ifelse(followUp1Answer == "Ja", "61214", "61284")]
resultOverviewCompact[auswahlHilfsklassifikation == 1749 & kldb_tool_mit_anfniveau == "Bei auswahlHilfsklassifikation in 1748, 1749 ist der Code mithilfe des AnfNiveaus nicht eindeutig bestimmt, bitte manuell prüfen", kldb_tool_mit_anfniveau := "Code liegt für gewähltes AnfNiveau nicht vor / manuell prüfen"]

## Überprüfe ggf. Identität von Textangaben in resultOverview und answerSubmitted
# Weder gründlich noch vollständig durchgeführt
# answerSubmitted und resultsOverview stimmen nicht immer überein
# View(resultOverviewCompact[antwortText_beruf_taetigkeit_text != berufTaetigkeitText, list(sessionId, antwortText_beruf_taetigkeit_text, berufTaetigkeitText)])

# resultOverview[antwortText_beruf_taetigkeit2_text != berufTaetigkeitText2 | is.na(antwortText_beruf_taetigkeit_text) | is.na(antwortText), list(antwortText_beruf_taetigkeit2_text, berufTaetigkeitText2)]
# resultOverview[auswahlHilfsklassifikation != antwortId_auswahl_hilfsklassifikation, list(auswahlHilfsklassifikation, antwortId_auswahl_hilfsklassifikation)]
# table(resultOverviewCompact$antwortId_beruf_taetigkeit2_text)
# table(resultOverview$antwortId_beruf_taetigkeit_kontrolltext)
# table(resultOverview$antwortId_beruf_taetigkeit_text)

########################
## checks
# make sure all userIds are valid
!any(is.na(resultOverviewCompact$userId))
# count for how many userIds from Infas we dont have any data
resultOverviewCompact[is.na(sessionId),.N]

# no userIds from interviewer training?
nrow(resultOverviewCompact[is.na(userIdTyp) & !is.na(sessionId)]) == 0


# save(resultOverviewCompact, file = "output/data/resultOverviewCompactZwischenstandVom7Juli2019.RData")
