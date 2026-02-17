# this is a former file aufbereitung_02092019.R


library(data.table)
library(ggplot2)
library(beanplot)
library(DescTools)
library(haven)
library(lubridate)

Encoding(resultOverviewCompact$kldb_tool_mit_anfniveau) <- "UTF-8"
Encoding(resultOverviewCompact$isco_tool_mit_anfniveau) <- "UTF-8"


## Valide Interviews

#Interviews werden dann als valide betrachtet, wenn wir von infas eine SessionId bekommen haben und wenn das Interview abgeschlossen wurde

#Neue Variable zur Filterung valider Interviews

resultOverviewCompact[is.na(sessionId) | !interviewCompleted, valid := F]
resultOverviewCompact[!is.na(sessionId) & interviewCompleted, valid := T]



## Outcome erste Frage 

#Bei ID 23238108_1560963654_2 erfolgte ein Input, aber es wurde auch "Keine Angabe" ausgewaehlt". Im folgenden wird dieser Fall als "na" behandelt, er wurde aber von infas kodiert.



resultOverviewCompact[valid == TRUE & ((!is.na(start_beruf_taetigkeit_text) & berufTaetigkeitText == "" & antwortId_beruf_taetigkeit_text  == "*** Keine Angabe unchecked") | is.na(start_beruf_taetigkeit_text)), outcomefirstquestion:="breakoff"]




#N/a at first question:

resultOverviewCompact <- resultOverviewCompact[valid == TRUE & !is.na(start_beruf_taetigkeit_text) & antwortId_beruf_taetigkeit_text  == "*** Keine Angabe checked", outcomefirstquestion:="na"]



#Valid answer verbatim input (people who give job description)

resultOverviewCompact <- resultOverviewCompact[valid == TRUE & !is.na(start_beruf_taetigkeit_text) & !berufTaetigkeitText == "" & antwortId_beruf_taetigkeit_text  == "*** Keine Angabe unchecked", outcomefirstquestion:="valid"]



## Berufsart
# also removes the incomplete interviews from resultOverviewCompact



InterviewerBefragtenMatch <- as.data.table(infas_kodierungen)

InterviewerBefragtenMatch <- as_factor(InterviewerBefragtenMatch, only_labelled = T)

InterviewerBefragtenMatch[beco1 == "ja" | (!sozp09 == "Vollzeit erwerbstaetig (35 Wochenstunden und mehr)" & !sozp09 == "Teilzeit erwerbstaetig (15 bis unter 35 Wochenstunden)" & !sozp09 == "in Altersteilzeit, unabhaengig davon in welcher Phase" & !beco1 == "ja" & !beco2 == "ja"), berufsart := "Nebenerwerbstaetigkeit"]

InterviewerBefragtenMatch[beco1 == "nein" & beco2 == "ja", berufsart := "fruehere Erwerbstaetigkeit"]

InterviewerBefragtenMatch[sozp09 == "Vollzeit erwerbstaetig (35 Wochenstunden und mehr)" | sozp09 == "Teilzeit erwerbstaetig (15 bis unter 35 Wochenstunden)" | sozp09 == "in Altersteilzeit, unabhaengig davon in welcher Phase", berufsart := "derzeit teilzeit oder vollzeiterwerbstaetig"]

resultOverviewCompact[, userId := as.numeric(userId)]
resultOverviewCompact <- merge(resultOverviewCompact, InterviewerBefragtenMatch[, .(berufsart, INTNR, INTERNR)], by.y = c("INTNR", "INTERNR"), by.x = c("userId", "interviewerID_neu"))
rm(list="InterviewerBefragtenMatch")


## Generierung von Vorschlaegen


resultOverviewCompact[outcomefirstquestion == "valid" & (Osa1111SumProb <= 0.535 & SubstringSumProb <= 0.02), suggestionsgenerated := "no"]
resultOverviewCompact[outcomefirstquestion == "valid" & (Osa1111SumProb > 0.535 | SubstringSumProb > 0.02), suggestionsgenerated := "yes"]

#Urspruenglicher hoeherer Threshold fuer SubstringSumProb von 0.618

resultOverviewCompact[outcomefirstquestion == "valid" & (Osa1111SumProb <= 0.535 & SubstringSumProb <= 0.618), suggestionsgenerated_h := "no"]
resultOverviewCompact[outcomefirstquestion == "valid" & (Osa1111SumProb > 0.535 | SubstringSumProb > 0.618), suggestionsgenerated_h := "yes"]


# Auwahl von Taetigkeiten (detailliert)

# anderer Beruf

resultOverviewCompact[suggestionsgenerated =="yes" & auswahlHilfsklassifikation == "95" & !textNoneSelected_auswahl_hilfsklassifikation == "", auswahl :="ein anderer Beruf"]

resultOverviewCompact[suggestionsgenerated =="yes" & !auswahlHilfsklassifikation == "95" & !nchar(auswahlHilfsklassifikation) == 4 &!textNoneSelected_auswahl_hilfsklassifikation == "", auswahl := "Bedienungsfehler: kein anderer Beruf, aber Textinput"]



#Beruf ausgewaehlt 
resultOverviewCompact[suggestionsgenerated =="yes" & nchar(auswahlHilfsklassifikation) == 4 & textNoneSelected_auswahl_hilfsklassifikation == "", auswahl := "Beruf ausgewaehlt, kein Textinput"]


#Bedienungsfehler

resultOverviewCompact[suggestionsgenerated =="yes" & auswahlHilfsklassifikation == "95" & textNoneSelected_auswahl_hilfsklassifikation == "", auswahl :="Bedienungsfehler: ein anderer Beruf, aber kein Textinput"]

resultOverviewCompact[suggestionsgenerated =="yes" & nchar(auswahlHilfsklassifikation) == 4 & !textNoneSelected_auswahl_hilfsklassifikation == "",
                      auswahl := "Bedienungsfehler: Beruf ausgewaehlt, auch Textinput"]

#NA/Break-off

resultOverviewCompact[suggestionsgenerated =="yes" & auswahlHilfsklassifikation == "99",
                      auswahl := "Break-off 1"]
resultOverviewCompact[suggestionsgenerated =="yes" & auswahlHilfsklassifikation == "EMPTY" & textNoneSelected_auswahl_hilfsklassifikation == "",
                      auswahl := "Break-off 2"]
# Sonderfall: Hier wurden Vorschlaege berechnet, aber nicht angezeigt
resultOverviewCompact[sessionId == "13237093_1557247253_7", auswahl := "Abbruch bevor Taetigkeiten angezeigt werden"]




## Auswahl von Taetigkeiten (zusammengefasst)

#Fuer die Auswertungen habe ich die verschiedenen Auspraegungen teilweise zusammengefasst und zudem noch die Faelle hinzugefuegt, in denen keine Vorschlaege generiert wurden oder in denen kein valider Input bei der ersten Frage eingegeben wurde.


#Low Threshold

resultOverviewCompact[auswahl == "ein anderer Beruf" | auswahl == "Bedienungsfehler: kein anderer Beruf, aber Textinput", auswahl_com := "ein anderer Beruf"]
resultOverviewCompact[!suggestionsgenerated =="yes" , auswahl_com :="no suggestions generated"]
resultOverviewCompact[!outcomefirstquestion == "valid", auswahl_com :="no valid input"]
resultOverviewCompact[auswahl == "Beruf ausgewaehlt, kein Textinput", auswahl_com := "Beruf ausgewaehlt"]
resultOverviewCompact[auswahl == "NA" | auswahl == "Break-off 1" | auswahl == "Break-off 2" | auswahl == "Abbruch bevor Taetigkeiten angezeigt werden", auswahl_com := "NA/Break-off"]
resultOverviewCompact[auswahl == "Bedienungsfehler: ein anderer Beruf, aber kein Textinput" | auswahl == "Bedienungsfehler: Beruf ausgewaehlt, auch Textinput", auswahl_com := "Bedienungsfehler"]

#High Threshold

resultOverviewCompact[valid == T, outcome_high_ts := auswahl_com]
resultOverviewCompact[valid == T & suggestionsgenerated_h == "no", outcome_high_ts := "no suggestions generated if high ts"]



# Ergebnis manuelle Kodierung

# nocodeassigned bedeutet auch, dass von Infas "Hausfrau", "Rentner" oder Aehnliches kodiert wurde
resultOverviewCompact[nchar(kldb_infas_num) >= 4, outcome_manual_kldb := "code assigned"]
resultOverviewCompact[!nchar(kldb_infas_num) >= 4 | is.na(kldb_infas_num), outcome_manual_kldb := "no code assigned"]

resultOverviewCompact[nchar(isco_infas_num) >= 3, outcome_manual_isco := "code assigned"]
resultOverviewCompact[!nchar(isco_infas_num) >= 3 | is.na(isco_infas_num), outcome_manual_isco := "no code assigned"]


# Update des Outcomes fuer den Vergleich mit der manuellen Kodierung

# Update Outcome bei niedrigem Threshold

resultOverviewCompact[,outcome_tool_isco_lowth := auswahl_com]
resultOverviewCompact[,outcome_tool_kldb_lowth := auswahl_com]

resultOverviewCompact[auswahl_com == "Beruf ausgewaehlt" & (kldb_tool_mit_anfniveau == "Code liegt fuer gewaehltes AnfNiveau nicht vor / manuell pruefen" | kldb_tool_mit_anfniveau == "Mehrere Matches, bitte manuell pruefen" | (!auswahlHilfsklassifikation == "NA" & !followUp1Question == "NA" & followUp1Answer == "NA")), outcome_tool_kldb_lowth := "Beruf ausgewaehlt, aber Mismatch bei Folgefrage"]
resultOverviewCompact[auswahl_com == "Beruf ausgewaehlt" & (isco_tool_mit_anfniveau == "Code liegt fuer gewaehltes AnfNiveau nicht vor / manuell pruefen" | isco_tool_mit_anfniveau == "Mehrere Matches, bitte manuell pruefen" | (!auswahlHilfsklassifikation == "NA" & !followUp1Question == "NA" & followUp1Answer == "NA")), outcome_tool_isco_lowth := "Beruf ausgewaehlt, aber Mismatch bei Folgefrage"]

# Update Outcome bei hohem Threshold
resultOverviewCompact[,outcome_tool_isco_highth := outcome_high_ts]
resultOverviewCompact[,outcome_tool_kldb_highth := outcome_high_ts]

resultOverviewCompact[outcome_high_ts == "Beruf ausgewaehlt" & (kldb_tool_mit_anfniveau == "Code liegt fuer gewaehltes AnfNiveau nicht vor / manuell pruefen" | kldb_tool_mit_anfniveau == "Mehrere Matches, bitte manuell pruefen" | (!auswahlHilfsklassifikation == "NA" & !followUp1Question == "NA" & followUp1Answer == "NA")), outcome_tool_kldb_highth := "Beruf ausgewaehlt, aber Mismatch bei Folgefrage"]
resultOverviewCompact[outcome_high_ts == "Beruf ausgewaehlt" & (isco_tool_mit_anfniveau == "Code liegt fuer gewaehltes AnfNiveau nicht vor / manuell pruefen" | isco_tool_mit_anfniveau == "Mehrere Matches, bitte manuell pruefen" | (!auswahlHilfsklassifikation == "NA" & !followUp1Question == "NA" & followUp1Answer == "NA")), outcome_tool_isco_highth := "Beruf ausgewaehlt, aber Mismatch bei Folgefrage"]


## Vergleich manuelle Kodierung/Toolkodierung


# finaler Code mit dem Tool

resultOverviewCompact[,kldb_final := kldb_tool_vorlaufig][nchar(kldb_tool_mit_anfniveau) == 5, kldb_final := kldb_tool_mit_anfniveau]
resultOverviewCompact[,isco_final := isco_tool_vorlaufig][nchar(isco_tool_mit_anfniveau) == 4, isco_final := isco_tool_mit_anfniveau]

resultOverviewCompact[, kldb_final := as.numeric(kldb_final)]
resultOverviewCompact[, isco_final := as.numeric(isco_final)]


#-----------------Outcomes---------------------


# No code with tool
resultOverviewCompact[(valid == T & (!outcome_tool_isco_lowth == "Beruf ausgewaehlt" | (!auswahlHilfsklassifikation == "NA" & !followUp1Question == "NA" & followUp1Answer == "NA")) & nchar(isco_infas_num) >= 3) | sessionId == "13237093_1557247253_7", isco_agreement := "no code with tool"]
resultOverviewCompact[(valid == T & (!outcome_tool_kldb_lowth == "Beruf ausgewaehlt" | (!auswahlHilfsklassifikation == "NA" & !followUp1Question == "NA" & followUp1Answer == "NA")) & nchar(kldb_infas_num) >= 4) | sessionId == "13237093_1557247253_7", kldb_agreement := "no code with tool"]

#No coding by Infas
resultOverviewCompact[valid == T & outcome_tool_isco_lowth == "Beruf ausgewaehlt" & (nchar(isco_infas_num) < 3 | is.na(isco_infas_num)), isco_agreement := "no code manual coding"]
resultOverviewCompact[valid == T & outcome_tool_kldb_lowth == "Beruf ausgewaehlt" & (nchar(kldb_infas_num) < 4 | is.na(isco_infas_num)), kldb_agreement := "no code manual coding"]

#No coding by infas or by tool
resultOverviewCompact[valid == T & (!outcome_tool_isco_lowth == "Beruf ausgewaehlt" | (!auswahlHilfsklassifikation == "NA" & !followUp1Question == "NA" & followUp1Answer == "NA")) & (nchar(isco_infas_num) < 3 | is.na(isco_infas_num)), isco_agreement := "no code with tool, no code manual coding"]
resultOverviewCompact[valid == T & (!outcome_tool_kldb_lowth == "Beruf ausgewaehlt" | (!auswahlHilfsklassifikation == "NA" & !followUp1Question == "NA" & followUp1Answer == "NA")) & (nchar(kldb_infas_num) < 4 | is.na(kldb_infas_num)), kldb_agreement := "no code with tool, no code manual coding"]

# Uebereinstimmung (erste Stelle)
resultOverviewCompact[outcome_tool_kldb_lowth == "Beruf ausgewaehlt" & substring(kldb_infas_num,1,1) == substring(kldb_final,1,1) &  substring(kldb_infas_num,2,2) != substring(kldb_final,2,2) & nchar(kldb_infas_num) > 2, kldb_agreement := "erste Stelle"]
resultOverviewCompact[outcome_tool_isco_lowth == "Beruf ausgewaehlt" & substring(isco_infas_num,1,1) == substring(isco_final,1,1) &  substring(isco_infas_num,2,2) != substring(isco_final,2,2) & nchar(isco_infas_num) > 1, isco_agreement := "erste Stelle"]

# Uebereinstimmung (zweite Stelle)
resultOverviewCompact[outcome_tool_kldb_lowth == "Beruf ausgewaehlt" & substring(kldb_infas_num,1,2) == substring(kldb_final,1,2) &  substring(kldb_infas_num,3,3) != substring(kldb_final,3,3) & nchar(kldb_infas_num) > 2, kldb_agreement := "zweite Stelle"]
resultOverviewCompact[outcome_tool_isco_lowth == "Beruf ausgewaehlt" & substring(isco_infas_num,1,2) == substring(isco_final,1,2) &  substring(isco_infas_num,3,3) != substring(isco_final,3,3) & nchar(isco_infas_num) > 1, isco_agreement := "zweite Stelle"]

# Uebereinstimmung (dritte Stelle)
resultOverviewCompact[outcome_tool_kldb_lowth == "Beruf ausgewaehlt" & substring(kldb_infas_num,1,3) == substring(kldb_final,1,3) &  substring(kldb_infas_num,4,4) != substring(kldb_final,4,4) & nchar(kldb_infas_num) > 2, kldb_agreement := "dritte Stelle"]
resultOverviewCompact[outcome_tool_isco_lowth == "Beruf ausgewaehlt" & substring(isco_infas_num,1,3) == substring(isco_final,1,3) &  substring(isco_infas_num,4,4) != substring(isco_final,4,4)  & nchar(isco_infas_num) > 1, isco_agreement := "dritte Stelle"]

# Uebereinstimmung (vierte Stelle)
resultOverviewCompact[outcome_tool_kldb_lowth == "Beruf ausgewaehlt" & substring(kldb_infas_num,1,4) == substring(kldb_final,1,4) & ! substring(kldb_infas_num,5,5) == substring(kldb_final,5,5) & nchar(kldb_infas_num) > 2, kldb_agreement := "vierte Stelle"]
resultOverviewCompact[outcome_tool_isco_lowth == "Beruf ausgewaehlt" & substring(isco_infas_num,1,4) == substring(isco_final,1,4)  & nchar(isco_infas_num) > 1, isco_agreement := "vollstaendige Uebereinstimmung"]

# Uebereinstimmung (KldB: fuenfte Stelle)
resultOverviewCompact[outcome_tool_kldb_lowth == "Beruf ausgewaehlt" & substring(kldb_infas_num,1,5) == substring(kldb_final,1,5) & nchar(kldb_infas_num) > 2, kldb_agreement := "vollstaendige Uebereinstimmung"]

# keine Uebereinstimmung
resultOverviewCompact[outcome_tool_kldb_lowth == "Beruf ausgewaehlt" & substring(kldb_infas_num,1,1) != substring(kldb_final,1,1) & nchar(kldb_infas_num) > 2, kldb_agreement := "keine Uebereinstimmung auf erster Stelle"]
resultOverviewCompact[outcome_tool_isco_lowth == "Beruf ausgewaehlt" & substring(isco_infas_num,1,1) != substring(isco_final,1,1) & nchar(isco_infas_num) > 1, isco_agreement := "keine Uebereinstimmung auf erster Stelle"]

resultOverviewCompact[, kldb_agreement := factor(kldb_agreement, ordered = TRUE, levels = c("no code with tool", "no code manual coding", "no code with tool, no manual coding", "keine Uebereinstimmung auf erster Stelle", "erste Stelle", "zweite Stelle", "dritte Stelle", "vierte Stelle", "vollstaendige Uebereinstimmung"))]
resultOverviewCompact[, isco_agreement := factor(isco_agreement, ordered = TRUE, levels = c("no code with tool", "no code manual coding", "no code with tool, no manual coding", "keine Uebereinstimmung auf erster Stelle", "erste Stelle", "zweite Stelle", "dritte Stelle", "vollstaendige Uebereinstimmung"))]

#Uebereinstimmung bei hoeherem Threshold



resultOverviewCompact[, kldb_agreement_highth := kldb_agreement] 
resultOverviewCompact[, isco_agreement_highth := isco_agreement]

resultOverviewCompact[suggestionsgenerated_h == "no", kldb_agreement_highth := "no code with tool"] 
resultOverviewCompact[suggestionsgenerated_h == "no", isco_agreement_highth := "no code with tool"]

resultOverviewCompact[, kldb_agreement_highth := factor(kldb_agreement_highth, ordered = TRUE, levels = c("no code with tool", "no code manual coding", "no code with tool, no manual coding", "keine Uebereinstimmung auf erster Stelle", "erste Stelle", "zweite Stelle", "dritte Stelle", "vierte Stelle", "vollstaendige Uebereinstimmung"))]
resultOverviewCompact[, isco_agreement_highth := factor(isco_agreement_highth, ordered = TRUE, levels = c("no code with tool", "no code manual coding", "no code with tool, no manual coding", "keine Uebereinstimmung auf erster Stelle", "erste Stelle", "zweite Stelle", "dritte Stelle", "vollstaendige Uebereinstimmung"))]




#Prediction Probability des ausgewaehlten Berufs hinzufuegen


#Uebereinstimmung nach Guete des ausgewaehlten Vorschlags
occupationsSuggested <- as.data.table(occupationsSuggested)

resultOverviewCompact <- merge(resultOverviewCompact, occupationsSuggested[,.(sessionId, antwortId, antwortText, predProb, start)], by.y = c("sessionId", "antwortId", "start"), by.x = c("sessionId", "auswahlHilfsklassifikation", "start_auswahl_hilfsklassifikation"), all.x = T)

resultOverviewCompact[, predProb := as.numeric(predProb)]




## Zeit, die fuer Berufserfassung benoetigt wird



#1. no suggestions made: beruf_taetigkeit_text, beruf_taetigkeit_text2
resultOverviewCompact <- resultOverviewCompact[valid == T & (Osa1111SumProb <= 0.535 & SubstringSumProb <= 0.02), time1 := as.numeric(difftime(end_beruf_taetigkeit2_text, start_beruf_taetigkeit2_text, unit="secs"))]

#2. suggestions made: beruf_taetigkeit_text, auswahl_hilfsklassifikation 
resultOverviewCompact <- resultOverviewCompact[valid == T & (Osa1111SumProb > 0.535 & SubstringSumProb > 0.02) & !is.na(start_auswahl_hilfsklassifikation) & followUp1Question == "NA", time1 := as.numeric(difftime(end_auswahl_hilfsklassifikation, start_auswahl_hilfsklassifikation, unit="secs"))]

#3. follow-up question: beruf_taetigkeit_text, auswahl_hilfsklassifikation, followUp1Question 
resultOverviewCompact <- resultOverviewCompact[valid == T & (Osa1111SumProb > 0.535 & SubstringSumProb > 0.02) & !is.na(start_auswahl_hilfsklassifikation) & !followUp1Question == "NA", time1 := as.numeric(difftime(start_beruf_taetigkeit_kontrolltext, start_auswahl_hilfsklassifikation, unit="secs"))]

#Groups to compare
resultOverviewCompact[valid == T & (Osa1111SumProb <= 0.535 & SubstringSumProb <= 0.02), tooloutcome := "no suggestions"]

resultOverviewCompact[valid == T & (Osa1111SumProb > 0.535 | SubstringSumProb > 0.02) & (auswahlHilfsklassifikation == "95" | (!textNoneSelected_auswahl_hilfsklassifikation == "" & !nchar(auswahlHilfsklassifikation) == 4)), tooloutcome := "other occupation selected"]

resultOverviewCompact[valid == T & (Osa1111SumProb > 0.535 | SubstringSumProb > 0.02) & nchar(auswahlHilfsklassifikation) == 4 & textNoneSelected_auswahl_hilfsklassifikation == "" & followUp1Question == "NA", tooloutcome := "occupation selected, no follow up"]

resultOverviewCompact[valid == T & (Osa1111SumProb > 0.535 | SubstringSumProb > 0.02) & nchar(auswahlHilfsklassifikation) == 4 & textNoneSelected_auswahl_hilfsklassifikation == "" & !followUp1Question == "NA", tooloutcome := "occupation selected, follow up"]

#Zeiten hinzufügen
answerSubmitted <- as.data.table(answerSubmitted)
answerSubmitted_red <- answerSubmitted[, timediff := as.numeric(difftime(answerSubmitted[,end], answerSubmitted[,start]))]
answerSubmitted_red <- answerSubmitted_red[sessionId %in% resultOverviewCompact$sessionId]
answerSubmitted_red <- answerSubmitted_red[questId == "beruf_taetigkeit_text" |
                                       questId == "auswahl_hilfsklassifikation" |
                                       questId == "beruf_taetigkeit_kontrolltext" |
                                       questId == "beruf_taetigkeit2_text" | 
                                       grepl("followUp", questId)]
answerSubmitted_red <- answerSubmitted_red[grepl("followUp", questId), questId := "followUp"]

answerSubmitted_redwide <- dcast(data.table(answerSubmitted_red)[, tail(.SD[order(end, decreasing = FALSE)], 1),by = list(sessionId, questId)], 
                             sessionId ~ questId, 
                             value.var = "timediff")
names(answerSubmitted_redwide) <- c("sessionId", "time_auswahl.hilfsklassifikation", "time_beruf.taetigkeit2.text", "time_beruf.taetigkeit.kontrolltext", "time_beruf.taetigkeit.text", "time_followUp")

resultOverviewCompact <- merge(resultOverviewCompact, answerSubmitted_redwide, by = "sessionId", all.x = T)

resultOverviewCompact <- resultOverviewCompact[order(interviewerID, start_beruf_taetigkeit_text)][, rank_order := seq_len(.N), by = interviewerID]


# Correspondence with actual work
resultOverviewCompact[, factoruebereinstimmung := as.factor(befragterUebereinstimmung)]
levels(resultOverviewCompact$factoruebereinstimmung) <- c(NA,NA,"gering", "gross", NA, "mittel", "sehr gross")
resultOverviewCompact$factoruebereinstimmung <- ordered(resultOverviewCompact$factoruebereinstimmung, levels = c("sehr gross", "gross", "mittel", "gering"))

