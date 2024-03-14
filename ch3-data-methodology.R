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
library(gtsummary)
library(stargazer)

setwd("")

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
  filter(obs_h1>0) %>%
  filter(q171>0) %>%
  filter(q172>0)
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

# Agama
religious_events = df_filter['q171']
religious_worship = df_filter['q172']

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
                    urban, prov_code,
                    religious_events,
                    religious_worship)

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
                          'urban', 'prov_code', 'religious_events', 'religious_worship')

# Converting as categorical variable
combined_df$job_type = factor(combined_df$job_type)
combined_df$class_wage = factor(combined_df$class_wage)
combined_df$sex = factor(combined_df$sex)
combined_df$religious_events = factor(combined_df$religious_events)
combined_df$religious_worship = factor(combined_df$religious_worship)
combined_df$income_equality_binary = factor(combined_df$income_equality_binary)
combined_df$ownership_binary = factor(combined_df$ownership_binary)
combined_df$gov_role_binary = factor(combined_df$gov_role_binary)
combined_df$competitive_binary = factor(combined_df$competitive_binary)
combined_df$work_hard_binary = factor(combined_df$work_hard_binary)
combined_df$president_response = factor(combined_df$president_response)
combined_df$mayor_response = factor(combined_df$mayor_response)
combined_df$legislative_response = factor(combined_df$legislative_response)

# Summary table
# summarize the covariates data
table_basic <- 
  combined_df %>%
  tbl_summary(include = c(sex, age, art, ms, educ_level, job_type,
                          class_life, class_wage, main_worker, inst_type,
                          urban, religious_events, religious_worship)) %>%
  as_gt() %>%
  gt::gtsave(filename = "Tables and Graphs/Table Bab 3 Data and Methodology/table summary basic covariates.docx")

# Summarize the treatment data
table_treatment <-
  combined_df %>%
  tbl_summary(include= c(income_equality_binary, ownership_binary, gov_role_binary,
                         competitive_binary, work_hard_binary)) %>%
  as_gt() %>%
  gt::gtsave(filename = 'Tables and Graphs/Table Bab 3 Data and Methodology/table summary treatments.docx')

# Summarize the responses data
table_responses <-
  combined_df %>%
  tbl_summary(include= c(president_response, mayor_response, legislative_response)) %>%
  as_gt() %>%
  gt::gtsave(filename = 'Tables and Graphs/Table Bab 3 Data and Methodology/table summary responses.docx')


table1 <-
  tbl_summary(
    combined_df,
    include = c(sex, age, art, ms, job_type, class_life,
                president_response, mayor_response, legislative_response),
    by = binary_treatment, # split table by group
    missing = "no" # don't list missing data separately
  ) %>%
  add_n() %>% # add column with total number of non-missing observations
  add_p() %>% # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() %>%
  as_gt() %>%
  gt::gtsave(filename = "Hasil Analisis/table summary 1 dengan treatment income equality.html")

# Summary table 2
table2 <-
  tbl_summary(
    combined_df,
    include = c(educ_level, class_wage, main_worker, inst_type, urban,
                president_response, mayor_response, legislative_response),
    by = binary_treatment, # split table by group
    missing = "no" # don't list missing data separately
  ) %>%
  add_n() %>% # add column with total number of non-missing observations
  add_p() %>% # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() %>%
  as_gt() %>%
  gt::gtsave(filename = "Hasil Analisis/table summary 2 dengan treatment income equality.html")


