
# Daten direkt aus dem Tool (erstellt mithilfe der Datei erzeuge_Infas_Datenbankabzug_20190706.R)
load(paste0("orig-data/Infas_Berufetool_Datenbankabzug_20190706.RData"))

# table was created automatically within the tool and resultsOverviewCompact is a far a better version.
rm(list="resultOverview")
