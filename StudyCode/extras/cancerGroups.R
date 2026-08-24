cancer_types = list()

cancer_types[["respiratory"]] = list(
  types = c("larynx", "lung", "mesothelioma"),
  stratification = c("asthma","copd", "tobacco", "obesitycharybdis","cardiovascular_disease", "hypertension")
)

cancer_types[["digestive"]] = list(
  types = c("esophagus", "colorectal", "colon", "rectum", "anus", "stomach", "liver", "pancreatic", "gallbladder"),
  stratification = c("t2_diabetes","viral_hepatitis", "tobacco", "obesitycharybdis","cardiovascular_disease", "hypertension", "hpv")
)

cancer_types[["male"]] = list(
  types = c("penis", "prostate", "testis"), 
  stratification = c("hpv", "tobacco", "obesitycharybdis")
)

cancer_types[["female"]] = list(
  types = c("breast", "vulva", "vagina", "cervix", "corpus_uteri", "ovary"),
  stratification = c("hpv", "tobacco", "obesitycharybdis","menopausal_status", "depressive_disorder", "t2_diabetes")
)

cancer_types[["hematologic"]] = list(
  types = c("hodgkin", "non_hodgkin", "multiple_myeloma", "leukaemia"),
  stratification = c("hiv", "autoimmune_conditions", "obesitycharybdis","tobacco")
)

cancer_types[["oral"]] = list(
  types = c("lip_oral", "salivary_gland", "oropharynx", "nasopharynx", "hypopharynx"),
  stratification = c("hpv", "tobacco", "obesitycharybdis")
)

cancer_types[["urinary"]] = list(
  types = c("kidney", "bladder"),
  stratification = c("chronic_kidney_disease", "tobacco", "obesitycharybdis","hypertension")
)

cancer_types[["skin"]] = list(
  types = c("melanoma_skin", "nmsc_broad", "nmsc_narrow"),
  stratification = c("tobacco", "obesitycharybdis")
)

cancer_types[["other"]] = list(
  types = c("brain", "kaposi", "thyroid"),
  stratification = c("hiv", "tobacco", "obesitycharybdis")
)
