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
library(purrr)

wd = ""
setwd(wd)

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
competitive = df_filter['q109']
competitive_binary = ifelse(competitive>5, 1, 0)

set.seed(42)
competitive_binary_placebo = sample(competitive_binary)

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
                    competitive_binary,
                    competitive_binary_placebo,
                    sex, age, art, ms, educ2,
                    job_type, class_life, class_wage,
                    main_worker, inst_type,
                    urban, prov_code,
                    religious_events,
                    religious_worship)

colnames(combined_df) = c('president_response',
                          'mayor_response',
                          'legislative_response',
                          'competitive_binary',
                          'competitive_binary_placebo',
                          'sex', 'age', 'art', 'ms', 'educ_level', 'job_type',
                          'class_life', 'class_wage', 'main_worker', 'inst_type',
                          'urban', 'prov_code', 'religious_events', 'religious_worship')

var.labels = c(president_response = 'President Response', mayor_response = 'Mayor Response', 
               legislative_response = 'Local Parliament Response', 
               competitive_binary = 'Competitive Treatment',
               sex = "Sex", age = 'Age', art = 'Household Member', ms = "Marital Status", educ_level = 'Education Level', job_type = 'Job Type',
               class_life = "Life Class", class_wage = "Wage Scale", main_worker = 'Main Worker Status', urban = "Urban", inst_type = 'Institution Type',
               prov_code = "Provinces Code", religious_events = 'Religious Events Frequency', 
               religious_worship = 'Religious Worship Frequency',
               competitive_binary_placebo = 'Competitive Treatment Placebo')

label(combined_df) = as.list(var.labels[match(names(combined_df), names(var.labels))])
label(combined_df)

# Iterasi 500 kali

# Number of iterations
iterations <- 500

# Initialize vectors to store p-values for each iteration
p_values_president <- numeric(iterations)
p_values_mayor <- numeric(iterations)
p_values_legislative <- numeric(iterations)

set.seed(42)  # For reproducibility

for (i in 1:iterations) {
  # Print progress
  cat("Running iteration:", i, "out of", iterations, "\n")
  
  # Shuffle the competitive binary variable to create a new placebo treatment
  competitive_binary_placebo <- sample(combined_df$competitive_binary)
  
  # Add the new placebo treatment to the data
  combined_df$competitive_binary_placebo <- competitive_binary_placebo
  
  # Perform matching with the placebo treatment
  m.out <- matchit(competitive_binary_placebo ~ sex + age + educ_level + job_type + art + class_wage + class_life + ms + urban + main_worker + inst_type + religious_events + religious_worship,
                   data = combined_df, method = 'full', ratio = 1, distance = 'glm')
  
  # Extract matched data and weights
  match_data <- match.data(m.out)
  data_weight <- match_data$weights
  
  # Run weighted logistic regressions for each outcome variable with the placebo treatment
  logit_fm_president <- suppressWarnings(glm(fm_president, data = match_data, family = 'binomial', weights = data_weight))
  logit_fm_mayor <- suppressWarnings(glm(fm_mayor, data = match_data, family = 'binomial', weights = data_weight))
  logit_fm_legislative <- suppressWarnings(glm(fm_legislative, data = match_data, family = 'binomial', weights = data_weight))
  
  # Store p-values for the placebo treatment variable in each model
  p_values_president[i] <- coeftest(logit_fm_president, vcov = vcovHC(logit_fm_president, type = "HC3"))[2, 4]
  p_values_mayor[i] <- coeftest(logit_fm_mayor, vcov = vcovHC(logit_fm_mayor, type = "HC3"))[2, 4]
  p_values_legislative[i] <- coeftest(logit_fm_legislative, vcov = vcovHC(logit_fm_legislative, type = "HC3"))[2, 4]
}

# Combine p-values into a data frame for analysis
p_values_df <- data.frame(
  iteration = 1:iterations,
  p_val_president = p_values_president,
  p_val_mayor = p_values_mayor,
  p_val_legislative = p_values_legislative
)

# Histogram for President Model
hist(p_values_df$p_val_president, main = "Distribution of P-values for President Model (Placebo)", 
     xlab = "P-values", col = "skyblue", breaks = 20)

# Histogram for Mayor Model
hist(p_values_df$p_val_mayor, main = "Distribution of P-values for Mayor Model (Placebo)", 
     xlab = "P-values", col = "salmon", breaks = 20)

# Histogram for Legislative Model
hist(p_values_df$p_val_legislative, main = "Distribution of P-values for Legislative Model (Placebo)", 
     xlab = "P-values", col = "lightgreen", breaks = 20)

# Close the graphics device
dev.off()





