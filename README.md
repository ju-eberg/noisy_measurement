# occupationMeasurement-infas-survey-data-sharing

## Getting started

To load the data in R, run *load_data_main.R.* This will load everything, except data in *behavior-coding-data.*

To understand the data, you should first know the occupation coding instrument, which was used in the 2019 telephone survey administrated by infas.

-   The current version of the instrument is available in the R package `occupationMeasurement` (<https://occupationmeasurement.github.io/occupationMeasurement/>). There is no need to install the whole package, you can simply run the demo at <https://occupationmeasurement.shinyapps.io/app_demo/>. The explanations in this demo are are highly recommended to understand what the instrument does. Take note of how this instrument finds appropriate ISCO-/KldB codes, and think about how manual coders would do occupation coding in this project.
-   To collect the data provided here, we used an earlier version of the instrument. The version in the R package is not exactly identical to the version that has been tested in the telephone survey with infas. The one that was used in the phone survey to collect the data can be viewed at <http://3.122.252.76/telephone-interviewing/?pwd=4rt54z7g544dtsg56&askFollowUp=aufsicht;spezialisierung;sonstige&berufstyp=aktuell&audiorecording=on&conversational=on&id=0&studid=Test> (with conversational interviewing turned on) and at <http://3.122.252.76/telephone-interviewing/?pwd=4rt54z7g544dtsg56&askFollowUp=aufsicht;spezialisierung;sonstige&berufstyp=aktuell&audiorecording=on&conversational=off&id=0&studid=Test> (with conversational interviewing turned off).
-   You can also look at the documention of the survey, available at *doc/infas_AuxCo-Usabilitytest_7130_Methodenbericht_20191125.pdf.* The appendix contains the complete questionnaire, including the different filters that describe when certain questions were skipped. The variables collected with the occupation coding instrument are called `tool1` - `tool10`. Note that we didn't ask a follow-up question about skill level in the telephone survey because all respondents were asked this question (variable `beco10`) from within the standard interviewing software outside the instrument.
-   It's probably also helpful to think through and understand the process of manual occupation coding, cf. page 23 in the *doc/infas_AuxCo-Usabilitytest_7130_Methodenbericht_20191125.pdf.*.

Once you have familiarized yourself with the survey, go on and have a look at the data. There is a codebook for the data available in the subfolder, *doc/codebook_compact.overview.file.xlsx*

The file *doc/expose_praxisprojekt_berufskodierung.pdf* provides some additional background and mentions several open research questions within the project.

Please don't hesitate to contact Malte Schierholz in case you have any questions. :-)
