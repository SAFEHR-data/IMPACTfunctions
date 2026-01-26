## code to prepare `DATASET` dataset goes here

# IMPACT_dictionary

## We should get the IMPACT Dictionary from a publicly published dataset, this will define the expected tables and column names that we cna check our impiort_dataset functions against.

    #IMPACT_dictionary <- get_from_online_source()

IMPACT_dictionary <- data.frame(table_name = c("Demographics", "ePOMS_AcuteKidneyInjuries", "ePOMS_AntibacAntifungalAdmins", "ePOMS_AntiemeticAdmins", "ePOMS_BloodProductTransfusions", "ePOMS_Bupivacaine", "ePOMS_CamICU", "ePOMS_CTHead", "ePOMS_Delirium", "ePOMS_Furosemide", "ePOMS_GCS", "ePOMS_IntravenousOpioids", "ePOMS_LeftRightPowerMismatch", "ePOMS_ReOperations", "ePOMS_Specialling", "ePOMS_SupplementaryO2", "ePOMS_SupplementaryO2Additional", "ePOMS_SuspiciousCardio", "ePOMS_Temperature", "ePOMS_Troponin", "IntraOpMar", "Nasogastric", "PCAs", "Phases", "PostOpDreaming", "PostOpMar", "PostOpMortality_CardioMed", "PostOpMortality_CardioObs", "PostOpMortality_FIO2", "PostOpMortality_InfectionCulture", "PostOpMortality_InfectionMed", "PostOpMortality_InfectionTemp", "PostOpMortality_OutputMl", "PostOpMortality_Reoperation", "PostOpMortality_Respiration", "PostOpMortality_Troponin", "PreBloods", "PreOpCTScans", "ProcedureNotes", "Procedures", "UrinaryCatheters"))

usethis::use_data(IMPACT_dictionary, overwrite = TRUE)
