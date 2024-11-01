options(tigris_use_cache = TRUE)

install.packages("devtools")
install.packages("prism")
install.packages("raster")
install.packages("here")
install.packages("tidycensus")
install.packages("rmapshaper")
library(rmapshaper)
library(here)
library(reshape2) ##melting dataframes
library(dplyr) #data wrangling
library(raster) ##working with raster data
library(prism) ##prism data access
library(devtools)
library(data.table)
library(sf)
library(tidycensus)
library(purrr)
prism_set_dl_dir("~/prismtmp")

# download the PRISM data: ------- daily
get_prism_dailys(
  type = "tmean",
  minDate = "2010-01-01",
  maxDate = "2022-12-31",
  keepZip = FALSE
)
get_prism_dailys(
  type = "tmax",
  minDate = "2010-01-01",
  maxDate = "2022-12-31",
  keepZip = FALSE
)
get_prism_dailys(
  type = "tmin",
  minDate = "2010-01-01",
  maxDate = "2022-12-31",
  keepZip = FALSE
)
# download the PRISM data: ------- monthly
get_prism_monthlys(type = "tmean", year = 2010:2022, mon = 1:12, keepZip = FALSE)
jmean <- prism_archive_subset(
  "tmean", "monthly", mon = 1
)
pd_image(jmean[1])# 2010 Jan


# ___________________________________________________ 每个ct的daily change
pd_get_name(prism_archive_ls())
file_paths <- paste0("./Data/weighted_area_raster_census_tract_tmean_daily_", 2010:2019, ".rds")
la_tmean_summary <- map_dfr(file_paths, ~{
  ct_tmean_daily <- readRDS(.x)
  la_tmean_data <- ct_tmean_daily %>% filter(ct_id %in% la_city_tracts$GEOID)
  la_tmean_date_summary <- la_tmean_data %>%
    group_by(ct_id, date) %>%
    summarize(mean_tmean = mean(tmean, na.rm = TRUE)) 
  return(la_tmean_date_summary)
})
print(la_tmean_summary)

# ___________________________________________________
library(ggplot2)
overall_mean <- la_tmean_summary %>%
  group_by(date) %>%
  summarize(mean_tmean = mean(mean_tmean, na.rm = TRUE))

# 确保 `date` 列为日期格式
la_tmean_summary <- la_tmean_summary %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))
overall_mean <- overall_mean %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

# 绘制图形
ggplot() +
  # 每个 census tract 的变化线条（浅灰色）
  geom_line(data = la_tmean_summary, aes(x = date, y = mean_tmean, group = ct_id), color = "lightgray", alpha = 0.5) +
  # 所有 census tract 的每日平均温度（深灰色）
  geom_line(data = overall_mean, aes(x = date, y = mean_tmean), color = "red", size = 1) +
  labs(
    title = "各 Census Tract 的每日平均温度及所有 Tract 的每日均值",
    x = "日期",
    y = "平均温度 (tmean)"
  ) +
  theme_minimal()
# ___________________________________________________
temp_data <- temp_data %>%
  mutate(
    lower_bound = avg_tmean - sd_tmean,
    upper_bound = avg_tmean + sd_tmean
  )
# 绘制平均温度和误差带
ggplot(temp_data, aes(x = year, y = avg_tmean)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = 0.2) +
  labs(
    title = "2010-2022 年平均温度及波动范围",
    x = "年份",
    y = "平均温度 (tmean)"
  ) +
  theme_minimal()
#___________________________________________________


#___________________________________________________ population and age group data (S0101) - population, male, female, age_0_19, age_20_29, age_30_39, age_40_49, age_50_59, age_60_69, age_70_plus
years <- 2010:2019
all_years_data <- list()
for (year in years) {
  data_path <- paste0("./Data/S0101/ACSST5Y", year, ".S0101-Data.csv")
  yearly_data <- read.csv(data_path)
  
  yearly_data <- yearly_data %>%
    mutate(GEOID = sub("1400000US", "", GEO_ID)) %>%
    mutate(across(c(S0101_C01_001E, S0101_C02_001E, S0101_C03_001E,
                    S0101_C01_002E, S0101_C01_003E, S0101_C01_004E, S0101_C01_005E,
                    S0101_C01_006E, S0101_C01_007E, S0101_C01_008E, S0101_C01_009E,
                    S0101_C01_010E, S0101_C01_011E, S0101_C01_012E, S0101_C01_013E,
                    S0101_C01_014E, S0101_C01_015E, S0101_C01_016E, S0101_C01_017E,
                    S0101_C01_018E, S0101_C01_019E), as.numeric)) %>%
    mutate(
      year = year,  # Add year column
      population = S0101_C01_001E,
      male = S0101_C01_001E - S0101_C03_001E,
      female = S0101_C03_001E,
      age_0_19 = ifelse(
        year >= 2010 & year <= 2016, 
        (S0101_C01_002E + S0101_C01_003E + S0101_C01_004E + S0101_C01_005E) / 100 * population, 
        S0101_C01_002E + S0101_C01_003E + S0101_C01_004E + S0101_C01_005E
      ),
      age_20_29 = ifelse(
        year >= 2010 & year <= 2016, 
        (S0101_C01_006E + S0101_C01_007E) / 100 * population, 
        S0101_C01_006E + S0101_C01_007E
      ),
      age_30_39 = ifelse(
        year >= 2010 & year <= 2016, 
        (S0101_C01_008E + S0101_C01_009E) / 100 * population, 
        S0101_C01_008E + S0101_C01_009E
      ),
      age_40_49 = ifelse(
        year >= 2010 & year <= 2016, 
        (S0101_C01_010E + S0101_C01_011E) / 100 * population, 
        S0101_C01_010E + S0101_C01_011E
      ),
      age_50_59 = ifelse(
        year >= 2010 & year <= 2016, 
        (S0101_C01_012E + S0101_C01_013E) / 100 * population, 
        S0101_C01_012E + S0101_C01_013E
      ),
      age_60_69 = ifelse(
        year >= 2010 & year <= 2016, 
        (S0101_C01_014E + S0101_C01_015E) / 100 * population, 
        S0101_C01_014E + S0101_C01_015E
      ),
      age_70_plus = ifelse(
        year >= 2010 & year <= 2016, 
        (S0101_C01_016E + S0101_C01_017E + S0101_C01_018E + S0101_C01_019E) / 100 * population, 
        S0101_C01_016E + S0101_C01_017E + S0101_C01_018E + S0101_C01_019E
      )
    ) %>%
    dplyr::select(GEOID, NAME, year, population, male, female, age_0_19, age_20_29, age_30_39, age_40_49, age_50_59, age_60_69, age_70_plus)
  all_years_data[[as.character(year)]] <- yearly_data
}
combined_data <- bind_rows(all_years_data)
la_age_data <- combined_data %>%
  filter(GEOID %in% la_city_tracts$GEOID)
age_summary <- la_age_data %>%
  group_by(year) %>%
  summarise(
    mean_pop = mean(population, na.rm = TRUE),
    sd_pop = sd(population, na.rm = TRUE),
    mean_male = mean(male, na.rm = TRUE),
    sd_male = sd(male, na.rm = TRUE),
    mean_female = mean(female, na.rm = TRUE),
    sd_female = sd(female, na.rm = TRUE),
    mean_under_19 = mean(age_0_19, na.rm = TRUE),
    sd_under_19 = sd(age_0_19, na.rm = TRUE),
    mean_20_29 = mean(age_20_29, na.rm = TRUE),
    sd_20_29 = sd(age_20_29, na.rm = TRUE),
    mean_30_39 = mean(age_30_39, na.rm = TRUE),
    sd_30_39 = sd(age_30_39, na.rm = TRUE),
    mean_40_49 = mean(age_40_49, na.rm = TRUE),
    sd_40_49 = sd(age_40_49, na.rm = TRUE),
    mean_50_59 = mean(age_50_59, na.rm = TRUE),
    sd_50_59 = sd(age_50_59, na.rm = TRUE),
    mean_60_69 = mean(age_60_69, na.rm = TRUE),
    sd_60_69 = sd(age_60_69, na.rm = TRUE),
    mean_70_plus = mean(age_70_plus, na.rm = TRUE),
    sd_70_plus = sd(age_70_plus, na.rm = TRUE)
  )
# 显示结果
age_summary
write.csv(age_summary, file = "./Data/age_summary.csv", row.names = FALSE)
#___________________________________________________ end

#___________________________________________________ race
years <- 2010:2019
race_codes <- c("A", "B", "C", "D", "E", "F", "G")
race_names <- c("white", "black", "indian", "asian", "island", "other", "multi")
all_data_list <- list()
for (year in years) {
  yearly_race_data <- list()
  
  for (i in seq_along(race_codes)) {
    race_code <- race_codes[i]
    race_name <- race_names[i]
    data_path <- paste0("./Data/B01001A-G/ACSDT5Y", year, ".B01001", race_code, "-Data.csv")
    race_data <- read.csv(data_path)
    column_name <- paste0("B01001", race_code, "_001E")
    if (column_name %in% names(race_data)) {
      race_data <- race_data %>%
        mutate(GEOID = sub("1400000US", "", GEO_ID)) %>%
        mutate(across(all_of(column_name), ~as.numeric(.), .names = race_name)) %>% # 转换成数值
        mutate(year = year) %>%
        dplyr::select(GEOID, NAME, year, !!sym(race_name))  # 选择相关列
      yearly_race_data[[race_name]] <- race_data
    } else {
      warning(paste("Column", column_name, "not found in file for", race_name, "in year", year))
    }
  }
  combined_yearly_data <- reduce(yearly_race_data, function(x, y) full_join(x, y, by = c("GEOID", "NAME", "year")))
  all_data_list[[as.character(year)]] <- combined_yearly_data
}
combined_race_data <- bind_rows(all_data_list)
print(head(combined_race_data))
la_race_data <- combined_race_data %>%
  filter(GEOID %in% la_city_tracts$GEOID)
race_summary <- la_race_data %>%
  group_by(year) %>%
  summarise(
    mean_white = mean(white, na.rm = TRUE),
    sd_white = sd(white, na.rm = TRUE),
    mean_black = mean(black, na.rm = TRUE),
    sd_black = sd(black, na.rm = TRUE),
    mean_indian = mean(indian, na.rm = TRUE),
    sd_indian = sd(indian, na.rm = TRUE),
    mean_asian = mean(asian, na.rm = TRUE),
    sd_asian = sd(asian, na.rm = TRUE),
    mean_island = mean(island, na.rm = TRUE),
    sd_island = sd(island, na.rm = TRUE),
    mean_other = mean(other, na.rm = TRUE),
    sd_other = sd(other, na.rm = TRUE),
    mean_multi = mean(multi, na.rm = TRUE),
    sd_multi = sd(multi, na.rm = TRUE)
  )
race_summary
write.csv(race_summary, file = "./Data/race_summary.csv", row.names = FALSE)
#___________________________________________________ end

#___________________________________________________ income S1901
# 定义年份范围
years <- 2010:2019
all_income_data <- list()
for (year in years) {
  data_path <- paste0("./Data/S1901/ACSST5Y", year, ".S1901-Data.csv")
  yearly_data <- read.csv(data_path)
  yearly_data <- yearly_data %>%
    mutate(
      GEOID = sub("1400000US", "", GEO_ID),                 # 提取GEOID
      across(c(S1901_C01_002E, S1901_C01_003E, S1901_C01_004E,
               S1901_C01_005E, S1901_C01_006E, S1901_C01_007E, S1901_C01_008E,
               S1901_C01_009E, S1901_C01_010E, S1901_C01_011E, S1901_C01_012E), as.numeric), # 转换为数值
      year = year,                                          # 添加年份列
      #verylow_income = (S1901_C01_002E + S1901_C01_003E),
      #low_income = S1901_C01_004E + S1901_C01_005E,
      #medium_income = S1901_C01_006E + S1901_C01_007E,
      #high_income = S1901_C01_008E + S1901_C01_009E,
      #veryhigh_income = S1901_C01_010E + S1901_C01_011E,
      median_income = S1901_C01_012E
    ) %>%
    dplyr::select(GEOID, NAME, year, median_income)#verylow_income, low_income, medium_income, high_income, veryhigh_income, 
  all_income_data[[as.character(year)]] <- yearly_data
}
combined_income_data <- bind_rows(all_income_data)
la_income_data <- combined_income_data %>%
  filter(GEOID %in% la_city_tracts$GEOID)
income_summary <- la_income_data %>%
  group_by(year) %>%
  summarise(
    #mean_verylow = mean(verylow_income, na.rm = TRUE),
    #sd_verylow = sd(verylow_income, na.rm = TRUE),
    #mean_low = mean(low_income, na.rm = TRUE),
    #sd_low = sd(low_income, na.rm = TRUE),
    #mean_med = mean(medium_income, na.rm = TRUE),
    #sd_med = sd(medium_income, na.rm = TRUE),
    #mean_high = mean(high_income, na.rm = TRUE),
    #sd_high = sd(high_income, na.rm = TRUE),
    #mean_veryhigh = mean(veryhigh_income, na.rm = TRUE),
    #sd_veryhigh = sd(veryhigh_income, na.rm = TRUE),
    mean_median = mean(median_income, na.rm = TRUE),
    sd_median = sd(median_income, na.rm = TRUE)
  )
income_summary
write.csv(income_summary, file = "./Data/income_summary.csv", row.names = FALSE)
#___________________________________________________ end

#___________________________________________________ educational attainment S1501 - no high school
years <- 2010:2019
all_edu_data <- list()
for (year in years) {
  data_path <- paste0("./Data/S1501/ACSST5Y", year, ".S1501-Data.csv")
  yearly_data <- read.csv(data_path)
  yearly_data <- yearly_data %>%
    mutate(
      GEOID = sub("1400000US", "", GEO_ID),                 # 提取GEOID
      across(c(S1501_C01_002E), as.numeric), # 转换为数值
      year = year,                                          # 添加年份列
      no_highschool = S1501_C01_002E
    ) %>%
    dplyr::select(GEOID, NAME, year, no_highschool)
  all_edu_data[[as.character(year)]] <- yearly_data
}
combined_edu_data <- bind_rows(all_edu_data)
la_edu_data <- combined_edu_data %>%
  filter(GEOID %in% la_city_tracts$GEOID)
edu_summary <- la_edu_data %>%
  group_by(year) %>%
  summarise(
    mean_nohigh = mean(no_highschool, na.rm = TRUE),
    sd_nohigh = sd(no_highschool, na.rm = TRUE)
  )
edu_summary
write.csv(edu_summary, file = "./Data/edu_summary.csv", row.names = FALSE)
#___________________________________________________ end

#___________________________________________________ households and families S1101
years <- 2010:2019
all_hh_data <- list()
for (year in years) {
  data_path <- paste0("./Data/S1101/ACSST5Y", year, ".S1101-Data.csv")
  yearly_data <- read.csv(data_path)
  yearly_data <- yearly_data %>%
    mutate(
      GEOID = sub("1400000US", "", GEO_ID),                 # 提取GEOID
      across(c(S1101_C01_002E, S1101_C03_001E, S1101_C04_001E), as.numeric), # 转换为数值
      year = year,                                          # 添加年份列
      avg_hhSize = S1101_C01_002E,
      female_hher = S1101_C04_001E,
      male_hher = S1101_C03_001E
    ) %>%
    dplyr::select(GEOID, NAME, year, avg_hhSize, female_hher, male_hher)
  all_hh_data[[as.character(year)]] <- yearly_data
}
combined_hh_data <- bind_rows(all_hh_data)
la_hh_data <- combined_hh_data %>%
  filter(GEOID %in% la_city_tracts$GEOID)
hh_summary <- la_hh_data %>%
  group_by(year) %>%
  summarise(
    mean_hhsize = mean(avg_hhSize, na.rm = TRUE),
    sd_hhszie = sd(avg_hhSize, na.rm = TRUE),
    mean_fh = mean(female_hher, na.rm = TRUE),
    sd_fh = sd(female_hher, na.rm = TRUE),
    mean_mh = mean(male_hher, na.rm = TRUE),
    sd_mh = sd(male_hher, na.rm = TRUE)
  )
hh_summary
write.csv(hh_summary, file = "./Data/hh_summary.csv", row.names = FALSE)
#___________________________________________________ end





