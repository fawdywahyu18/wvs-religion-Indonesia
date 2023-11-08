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

mac_wd = "KEPO"
backup_wd = 'KEPO'
onedrive = 'KEPO'
setwd(onedrive)

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
  # filter(q279>=1 & q279<=3) %>% # hanya ambil yang kerja aja, fulltime parttime dan wirausaha
  filter(q284>=0) %>%
  filter(q285>=0) %>%
  filter(q287>0) %>%
  filter(q288>0) %>%
  # filter(q289==5) %>% # hanya muslim
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
job_status = df_filter['q279']

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
combined_df = cbind(
                    income_equality_binary,
                    ownership_binary,
                    gov_role_binary,
                    competitive_binary,
                    work_hard_binary,
                    sex, age, art, ms, educ2, job_status,
                    job_type, class_life, class_wage,
                    main_worker, inst_type,
                    urban, prov_code)

colnames(combined_df) = c(
                          'income_equality_binary',
                          'ownership_binary',
                          'gov_role_binary',
                          'competitive_binary',
                          'work_hard_binary',
                          'sex', 'age', 'art', 'ms', 'educ_level', 'job_status', 'job_type',
                          'class_life', 'class_wage', 'main_worker', 'inst_type',
                          'urban', 'prov_code')

# Peform the PSM Analysis the check the balance of data
# Read the paper about PSM in R:
# https://cran.r-project.org/web/packages/MatchIt/vignettes/matching-methods.html#:~:text=Nearest%20neighbor%20matching%20(%20method%20%3D%20%22nearest%22%20)%2C%20optimal,distance%20matching%20implemented%20in%20MatchIt%20.

test_income = income_equality_binary ~ sex +  age + educ_level + job_type + art + class_wage + class_life + ms + urban + main_worker + inst_type + job_status
test_ownership = ownership_binary ~ sex +  age + educ_level + job_type + art + class_wage + class_life + ms + urban + main_worker + inst_type + job_status
test_gov_role = gov_role_binary ~ sex +  age + educ_level + job_type + art + class_wage + class_life + ms + urban + main_worker + inst_type + job_status
test_competitive = competitive_binary ~ sex +  age + educ_level + job_type + art + class_wage + class_life + ms + urban + main_worker + inst_type + job_status
test_work_hard = work_hard_binary ~ sex +  age + educ_level + job_type + art + class_wage + class_life + ms + urban + main_worker + inst_type + job_status

# Function to perform PSM and export the result as an excel file

PSM_treatment_full = function(treatment_name) {
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
  weight_full = match.data(m.out)$weights
  
  list_excel = list(before_matching = data.frame(summary_text$sum.all, 
                                                 row_names = rownames(summary_text$sum.all)),
                    after_matching = data.frame(summary_text$sum.matched,
                                                row_names = rownames(summary_text$sum.matched)),
                    matching_summary = data.frame(summary_text$nn,
                                                  row_names = rownames(summary_text$nn)),
                    data_weight = data.frame(weight_full))
  
  # Filename to export as an excel file
  words = c('Data Export WVS Mandiri/PSM Summary of ', treatment_name,' using full matching', '.xlsx')
  file_name_export = paste(words, collapse = "")
  
  write_xlsx(list_excel,
             file_name_export)
  
  return(m.out)
}

# Run the function of PSM_treatment
psm_income = PSM_treatment_full('income equality')
psm_ownership = PSM_treatment_full('private ownership')
psm_gov_role = PSM_treatment_full('government role')
psm_competition = PSM_treatment_full('competition')
psm_work_hard = PSM_treatment_full('work hard')



