library(readxl)
library(data.table)
library(haven)
library(dplyr)

## Load compact overview table
# load table: resultOverviewCompact
source("prog/load_data_resultOverviewCompactZwischenstandVom7Juli2019.R")

## Original data obtained from our database
# Load tables: toggleSubmitted, answerSubmitted, occupationsSuggested
source("prog/load_data_Infas_Berufetool_Datenbankabzug_20190706.R")

## Original data provided by infas
# load tables: infas_kodierungen, infas_completed_interviews, infas_incomplete_interviews, interviewermerkmale
source("prog/load_data_Enddatensatz_Berufscodierung_20190716_1.R")
source("prog/load_data_Enddatensatz_realisierte Interviews_20190712_1.R")
source("prog/load_data_Enddatensatz_unvollstaendige Interviews_20190712_1.R")
source("prog/load_data_Interviewermerkmale_20191017_2.R")

## Calculate additional variables for resultOverviewCompact (and some additional tables are created as a side effect)
source("prog/02_2_processing.R")

## More technical data and data from the auxiliary classification
# load tables: Hilfskategorien_data, abgrenzungen_sortiert_nach_kldb, hilfskategorien_kldb_mit_id, infasPredictions
# load list: infasDetailledCalculations
# load functions (can be ignored but maybe useful jointly with infasPredictions table): job_coding2, make.suggestions, mergeWithAuxiliaryClassificationForEvaluation
source("prog/load_data_sessionIdMitAntwortTextAngereichertMitPredictionData.R")

# load table: folge_fragen
# create table: anforderungsniveauVonFolgefragen
# create function: getFinalToolCode(auswahlHilfsklassifikation, anforderungsniveau, type = c("kldb", "isco")
source("prog/load_data_folgefragen.R")

# load table: kantar_auxco_kodierung
source("prog/load_data_kantar_neukodierung_MS2_ar3_restart.R")
