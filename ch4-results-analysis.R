library(MatchIt)
library(haven)
library(dplyr)
library(sandwich)
library(lmtest)
library(optmatch)
library(quickmatch)
library(Matching)
library(rgenoud)
library(miceadds)
library(writexl)
library(Hmisc)
library(gtsummary)

wd = ''
setwd(mac_wd)

wvs = read_dta('Data dan Kuesioner/WVS.dta')

df = wvs

# Filter data untuk exclude missing data dan ambil hanya orang bekerja krn sebagian besar observasinya orang kerja
df_filter = df %>%
  filter(q106>0) %>%
  filter(q107>0) %>%
  filter(q108>0) %>%
  filter(q109>0) %>%
  filter(q110>0) %>%
  filter(q135>0) %>%
  filter(q275>=0) %>%
  filter(q279>=1 & q279<=3) %>% # hanya ambil yang kerja aja, fulltime parttime dan wirausaha
  filter(q284>=0) %>%
  filter(q285>=0) %>%
  filter(q287>0) %>%
  filter(q288>0) %>%
  filter(q289==5) %>% # hanya muslim
  filter(qnm1>0) %>%
  filter(qnm2>0) %>%
  filter(qnm3>0) %>%
  filter(obs_h1>0)
  # filter(q158>0 | q159>0 | q160>0 | q161>0 | q162>0 | q163>0)

# Variable treatment

# Variable Treatment kuesioner mengenai Economic Equality Perspective
income_equality = df_filter['q106']
income_equality_binary = ifelse(income_equality>5, 1, 0)

ownership = df_filter['q107']
ownership_binary = ifelse(ownership>5, 1, 0)

government_role = df_filter['q108']
gov_role_binary = ifelse(government_role>5, 1, 0)

competitive = df_filter['q109']
competitive_binary = ifelse(competitive>5, 1, 0)

work_hard = df_filter['q110']
work_hard_binary = ifelse(work_hard>5, 1, 0)

# Variable Outcome
# Apakah bisa menerima pemimpin politik non-muslim?
presiden_nm = df_filter['qnm1']
president_binary = ifelse(presiden_nm<=2,1,0)

mayor_nm = df_filter['qnm2']
mayor_binary = ifelse(mayor_nm<=2, 1, 0)

legislatif_nm = df_filter['qnm3']
legislative_binary = ifelse(legislatif_nm<=2, 1, 0)

# Karakteristik Individu di dalam WVS

# Sex Q260 (string categorical variable)
sex = df_filter['q260']

# Age
age = df_filter['q262']

# WNI atau tidak
wni = df_filter['q269']

# Anggota rumah tangga
art = df_filter['q270']

# status pernikahan (marital status)
ms = df_filter['q273']

# status pendidikan detail
educ = df_filter['q275a']

# status pendidikan simple
educ2 = df_filter['q275']

# status pekerjaan
job = df_filter['q279']

# jenis pekerjaan
job_type = df_filter['q281']

# Pencari nafkah utama atau tidak?
main_worker = df_filter['q285']

# Tipe institusi bekerja
inst_type = df_filter['q284']

# kelas taraf hidup (kelompok atas, menengah atas dll)
class_life = df_filter['q287']

# Kelompok penghasilan
class_wage = df_filter['q288']

# Wilayah tinggal (Urban/Rural)
urban = df_filter['obs_h1']

# Kode Region/Provinsi
prov_code = df_filter['obs_n_cd']

# Combining dataframe
combined_df = cbind(president_binary,
                    mayor_binary,
                    legislative_binary,
                    income_equality_binary,
                    ownership_binary,
                    gov_role_binary,
                    competitive_binary,
                    work_hard_binary,
                    sex, age, art, ms, educ2,
                    job_type, class_life, class_wage,
                    main_worker, inst_type,
                    urban, prov_code)

colnames(combined_df) = c('president_response',
                          'mayor_response',
                          'legislative_response',
                          'income_equality_binary',
                          'ownership_binary',
                          'gov_role_binary',
                          'competitive_binary',
                          'work_hard_binary',
                          'sex', 'age', 'art', 'ms', 'educ_level', 'job_type',
                          'class_life', 'class_wage', 'main_worker', 'inst_type',
                          'urban', 'prov_code')

var.labels = c(president_response = 'President Response', mayor_response = 'Mayor Response', 
               legislative_response = 'Local Parliament Response',
               income_equality_binary = 'Income Equality Treatment', 
               ownership_binary = 'Private Ownership Treatment',
               gov_role_binary = 'Government Role Treatment', 
               competitive_binary = 'Competitive Treatment',
               work_hard_binary = 'Work Hard Treatment',
               sex = "Sex", age = 'Age', art = 'Household Member', ms = "Marital Status", educ_level = 'Education Level', job_type = 'Job Type',
               class_life = "Life Class", class_wage = "Wage Scale", main_worker = 'Main Worker Status', urban = "Urban", inst_type = 'Institution Type',
               prov_code = "Provinces Code")

label(combined_df) = as.list(var.labels[match(names(combined_df), names(var.labels))])
label(combined_df)


# Peform the PSM Analysis the check the balance of data
# Read the paper about PSM in R:
# https://cran.r-project.org/web/packages/MatchIt/vignettes/matching-methods.html#:~:text=Nearest%20neighbor%20matching%20(%20method%20%3D%20%22nearest%22%20)%2C%20optimal,distance%20matching%20implemented%20in%20MatchIt%20.

test_income = income_equality_binary ~ sex +  age + educ_level + job_type + art + class_wage + class_life + ms + urban + main_worker + inst_type
test_ownership = ownership_binary ~ sex +  age + educ_level + job_type + art + class_wage + class_life + ms + urban + main_worker + inst_type
test_gov_role = gov_role_binary ~ sex +  age + educ_level + job_type + art + class_wage + class_life + ms + urban + main_worker + inst_type
test_competitive = competitive_binary ~ sex +  age + educ_level + job_type + art + class_wage + class_life + ms + urban + main_worker + inst_type
test_work_hard = work_hard_binary ~ sex +  age + educ_level + job_type + art + class_wage + class_life + ms + urban + main_worker + inst_type

# Function to perform PSM and export the result as an excel file

PSM_treatment = function(treatment_name) {
  # treatment_name is a string: 'income equality';'private ownership';
  #                             'government role';'competition';'work hard'
  
  if (treatment_name=='income equality') {
    model_test = test_income
  } else if (treatment_name=='private ownership') {
    model_test = test_ownership
  } else if (treatment_name=='government role') {
    model_test = test_gov_role
  } else if (treatment_name=='competition') {
    model_test = test_competitive
  } else if (treatment_name=='work hard') {
    model_test = test_work_hard
  } else {
    print('Unrecognized treatment_name')
  }
  
  m.out = matchit(model_test,
                  data = combined_df, method = 'full', ratio = 1, distance = 'glm')
  summary_text = summary(m.out)
  list_excel = list(before_matching = data.frame(summary_text$sum.all, 
                                                 row_names = rownames(summary_text$sum.all)),
                    after_matching = data.frame(summary_text$sum.matched,
                                                row_names = rownames(summary_text$sum.matched)),
                    matching_summary = data.frame(summary_text$nn,
                                                  row_names = rownames(summary_text$nn)))
  
  # Filename to export as an excel file
  words = c('Tables and Graphs/Table Bab 4 Hasil Analisis/PSM Summary of ', treatment_name, '.xlsx')
  file_name_export = paste(words, collapse = "")
  
  write_xlsx(list_excel,
             file_name_export)
  
  return(m.out)
}

# Run the function of PSM_treatment
psm_income = PSM_treatment('income equality')
psm_ownership = PSM_treatment('private ownership')
psm_gov_role = PSM_treatment('government role')
psm_competition = PSM_treatment('competition')
psm_work_hard = PSM_treatment('work hard')



# After the matching, all the treatment variables are balanced in covariates
# Function to estimate the Weighted Logistic Regression and export the result as an html file
WLS_response = function(treatment_name) {
  # treatment_name is a string: 'income equality';'private ownership';
  #                             'government role';'competition';'work hard'
  
  # treatment_name = 'income equality'
  
  if (treatment_name=='income equality') {
    m.out = psm_income
    sm_president = president_response ~ income_equality_binary
    fm_president = president_response ~ income_equality_binary + sex +  age + educ_level + job_type + art + class_wage + class_life + ms + urban + main_worker + inst_type
    
    sm_mayor = mayor_response ~ income_equality_binary
    fm_mayor = mayor_response ~ income_equality_binary + sex +  age + educ_level + job_type + art + class_wage + class_life + ms + urban + main_worker + inst_type
    
    sm_legislative = legislative_response ~ income_equality_binary
    fm_legislative = legislative_response ~ income_equality_binary + sex +  age + educ_level + job_type + art + class_wage + class_life + ms + urban + main_worker + inst_type
    
    
  } else if (treatment_name=='private ownership') {
    m.out = psm_ownership
    sm_president = president_response ~ ownership_binary
    fm_president = president_response ~ ownership_binary + sex +  age + educ_level + job_type + art + class_wage + class_life + ms + urban + main_worker + inst_type
    
    sm_mayor = mayor_response ~ ownership_binary
    fm_mayor = mayor_response ~ ownership_binary + sex +  age + educ_level + job_type + art + class_wage + class_life + ms + urban + main_worker + inst_type
    
    sm_legislative = legislative_response ~ ownership_binary
    fm_legislative = legislative_response ~ ownership_binary + sex +  age + educ_level + job_type + art + class_wage + class_life + ms + urban + main_worker + inst_type
    
  } else if (treatment_name=='government role') {
    m.out = psm_gov_role
    sm_president = president_response ~ gov_role_binary
    fm_president = president_response ~ gov_role_binary + sex +  age + educ_level + job_type + art + class_wage + class_life + ms + urban + main_worker + inst_type
    
    sm_mayor = mayor_response ~ gov_role_binary
    fm_mayor = mayor_response ~ gov_role_binary + sex +  age + educ_level + job_type + art + class_wage + class_life + ms + urban + main_worker + inst_type
    
    sm_legislative = legislative_response ~ gov_role_binary
    fm_legislative = legislative_response ~ gov_role_binary + sex +  age + educ_level + job_type + art + class_wage + class_life + ms + urban + main_worker + inst_type
    
  } else if (treatment_name=='competition') {
    m.out = psm_competition
    sm_president = president_response ~ competitive_binary
    fm_president = president_response ~ competitive_binary + sex +  age + educ_level + job_type + art + class_wage + class_life + ms + urban + main_worker + inst_type
    
    sm_mayor = mayor_response ~ competitive_binary
    fm_mayor = mayor_response ~ competitive_binary + sex +  age + educ_level + job_type + art + class_wage + class_life + ms + urban + main_worker + inst_type
    
    sm_legislative = legislative_response ~ competitive_binary
    fm_legislative = legislative_response ~ competitive_binary + sex +  age + educ_level + job_type + art + class_wage + class_life + ms + urban + main_worker + inst_type
    
  } else if (treatment_name=='work hard') {
    m.out = psm_work_hard
    sm_president = president_response ~ work_hard_binary
    fm_president = president_response ~ work_hard_binary + sex +  age + educ_level + job_type + art + class_wage + class_life + ms + urban + main_worker + inst_type
    
    sm_mayor = mayor_response ~ work_hard_binary
    fm_mayor = mayor_response ~ work_hard_binary + sex +  age + educ_level + job_type + art + class_wage + class_life + ms + urban + main_worker + inst_type
    
    sm_legislative = legislative_response ~ work_hard_binary
    fm_legislative = legislative_response ~ work_hard_binary + sex +  age + educ_level + job_type + art + class_wage + class_life + ms + urban + main_worker + inst_type
    
  } else {
    print('Unrecognized treatment_name')
  }
  
  # Extract the match data and weight
  match_data = match.data(m.out)
  data_weight = match_data$weights
  
  # Weighted Logistic Regression
  # Short Model (SM)
  logit_sm_president = glm(sm_president,
                           data = match_data,
                           family = 'binomial',
                           weights = data_weight)
  
  logit_sm_mayor = glm(sm_mayor,
                       data = match_data,
                       family = 'binomial',
                       weights = data_weight)
  
  logit_sm_legislative = glm(sm_legislative,
                             data = match_data,
                             family = 'binomial',
                             weights = data_weight)
  
  # Full Model (FM)
  logit_fm_president = glm(fm_president,
                           data = match_data,
                           family = 'binomial',
                           weights = data_weight)
  
  logit_fm_mayor = glm(fm_mayor,
                       data = match_data,
                       family = 'binomial',
                       weights = data_weight)
  
  logit_fm_legislative = glm(fm_legislative,
                             data = match_data,
                             family = 'binomial',
                             weights = data_weight)
  
  # Using stargazer to export the regression table
  # Filename to export as an excel file
  words = c('Tables and Graphs/Table Bab 4 Hasil Analisis/Weighted Logistic Regression/Weighted Logistic Regression ', treatment_name, '.html')
  file_name_export = paste(words, collapse = "")
  
  dep.var.labels=c("Non-Moslem President","Non-Moslem President",
                   "Non-Moslem Bupati/Mayor", "Non-Moslem Bupati/Mayor",
                   'Non-Moslem Local Parliament', 'Non-Moslem Local Parliament')
  
  t1 = 
    tbl_regression(logit_sm_president, exponentiate = FALSE,
                   intercept = TRUE) %>%
    bold_p()
  t2 = 
    tbl_regression(logit_fm_president, exponentiate = FALSE,
                   intercept = TRUE) %>%
    bold_p()
  
  t3 = 
    tbl_regression(logit_sm_mayor, exponentiate = FALSE,
                   intercept = TRUE) %>%
    bold_p()
  t4 = 
    tbl_regression(logit_fm_mayor, exponentiate = FALSE,
                   intercept = TRUE) %>%
    bold_p()
  
  t5 = 
    tbl_regression(logit_sm_legislative, exponentiate = FALSE,
                   intercept = TRUE) %>%
    bold_p()
  
  t6 = 
    tbl_regression(logit_fm_legislative, exponentiate = FALSE,
                   intercept = TRUE) %>%
    bold_p()
  
  # merge tables
  tbl_merge_ex1 <-
    tbl_merge(
      tbls = list(t1, t2, t3, t4, t5, t6),
      tab_spanner = dep.var.labels
    ) %>%
    as_gt() %>%
    gt::gtsave(filename = file_name_export)
  
  return(match_data)
}

#Running the function WLS_response
wls_income = WLS_response('income equality')
wls_ownership = WLS_response('private ownership')
wls_gov_role = WLS_response('government role')
wls_competition = WLS_response('competition')
wls_work_hard = WLS_response('work hard')

# Logistic regression with cluster standard error
match_data = match.data(m.out)
model_test2 = binary_outcomes ~ binary_treatment
model_test3 = binary_outcomes ~ binary_treatment + sex +  age + educ_level + job_type + art + class_wage + class_life + ms + urban + main_worker + inst_type

logistic_model2 <- glm.cluster(model_test2,
                              data = match_data,
                              family = 'binomial',
                              cluster = 'prov_code')

logistic_model3 <- glm.cluster(model_test3,
                               data = match_data,
                               family = 'binomial',
                               cluster = 'prov_code')



summary(logistic_model2)
summary(logistic_model3)

sink('Tables and Graphs/Logit 1 competitive behavior on religion neighbor.rtf')
print(summary(logistic_model2))
sink()

sink('Tables and Graphs/Logit 2 competitive behavior on religion neighbor.rtf')
print(summary(logistic_model3))
sink()

