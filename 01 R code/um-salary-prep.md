University of Michigan Salary Analysis
================
Sean R. Meyer, MBA, PhD
12/15/2023

- [Parallelize and combine salary
  spreadsheets](#parallelize-and-combine-salary-spreadsheets)
- [consumer price index prep](#consumer-price-index-prep)
- [gender data prep](#gender-data-prep)
- [Generate title rankings](#generate-title-rankings)
- [Generate department rankings](#generate-department-rankings)
- [generate historical title data](#generate-historical-title-data)
- [join prepped data sources](#join-prepped-data-sources)

``` r
library(tidyverse, quietly = TRUE)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.0     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.1     ✔ tibble    3.1.8
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the ]8;;http://conflicted.r-lib.org/conflicted package]8;; to force all conflicts to become errors

``` r
library(readxl)
library(furrr)
```

    ## Loading required package: future

``` r
library(fs)
library(gender)
parallel::detectCores()
```

    ## [1] 8

``` r
plan(multisession)
```

# Parallelize and combine salary spreadsheets

[University of Michigan
Library](https://quod.lib.umich.edu/e/errwpc/public/3/3/1/3314612.html)

``` r
csv_file_list = dir_ls('../00 data sources/um salaries/')

salary_data = csv_file_list %>%
  purrr::map_df(~ read_excel(.x, col_names = c("campus", "name", "title", "dept", "ftr", "basis", "fraction", "gen_fund"), skip = 1), .id = "filename") %>% 
  janitor::clean_names() %>% 
  mutate(year = str_sub(filename, 
                        start = 40L, 
                        end = 43L) %>% 
           as.numeric()) %>% 
  select(-filename) %>% 
  rename(full_name = name) %>% 
  separate(full_name, 
           into = c("last_name", "first_name"), 
           remove = FALSE, 
           sep = ",", 
           extra = "merge") %>% 
  separate(first_name, 
           into = c("first_name", "middle_name"), 
           remove = FALSE, 
           sep = " ", 
           extra = "merge") %>% 
  arrange(full_name, year, title, year, desc(ftr))
```

    ## Warning: Expecting numeric in E9342 / R9342C5: got ',001,858.00'

    ## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 368741 rows [4, 8, 13,
    ## 26, 34, 35, 36, 39, 44, 50, 51, 60, 61, 66, 81, 82, 84, 85, 95, 99, ...].

``` r
salary_data %>% 
  str()
```

    ## tibble [949,353 × 12] (S3: tbl_df/tbl/data.frame)
    ##  $ campus     : chr [1:949353] "UM_ANN-ARBOR" "UM_ANN-ARBOR" "UM_ANN-ARBOR" "UM_ANN-ARBOR" ...
    ##  $ full_name  : chr [1:949353] "(fnu),Agustini Lai" "-,Thippeswamy Gulappa" "-,Thippeswamy Gulappa" "A Perez,Jaime" ...
    ##  $ last_name  : chr [1:949353] "(fnu)" "-" "-" "A Perez" ...
    ##  $ first_name : chr [1:949353] "Agustini" "Thippeswamy" "Thippeswamy" "Jaime" ...
    ##  $ middle_name: chr [1:949353] "Lai" "Gulappa" "Gulappa" NA ...
    ##  $ title      : chr [1:949353] "LEO Lecturer I" "RESEARCH FELLOW" "RESEARCH FELLOW" "CUSTODIAN I" ...
    ##  $ dept       : chr [1:949353] "LSA Asian Languages & Cultures" "Obstetrics and Gynecology Dept" "Pathology Department" "Building Services" ...
    ##  $ ftr        : num [1:949353] 32200 30000 39000 37627 38750 ...
    ##  $ basis      : chr [1:949353] "8-Month" "12-Month" "12-Month" "12-Month" ...
    ##  $ fraction   : num [1:949353] 1 1 1 1 1 0.2 1 1 1 0.8 ...
    ##  $ gen_fund   : num [1:949353] 32200 0 0 37627 38750 ...
    ##  $ year       : num [1:949353] 2007 2008 2011 2022 2023 ...

# consumer price index prep

[U.S. Bureau of Labor
Statistics](https://data.bls.gov/timeseries/CUUR0000SA0?years_option=all_years)

``` r
# src: https://data.bls.gov/timeseries/CUUR0000SA0?years_option=all_years
# select 2002 - current year and check box to include annual averages
# Click GO and download the .xlsx file

cpi_data = read_excel("../00 data sources/cpi_data_2023.xlsx", skip = 10) %>% 
  janitor::clean_names() %>% 
  select(year, sep, annual, half1, half2) %>% 
  mutate(cpi = coalesce(sep, half1)) %>%  
  mutate(base_cpi = max(cpi))

cpi_data %>% str()
```

    ## tibble [22 × 7] (S3: tbl_df/tbl/data.frame)
    ##  $ year    : num [1:22] 2002 2003 2004 2005 2006 ...
    ##  $ sep     : num [1:22] 181 185 190 199 203 ...
    ##  $ annual  : num [1:22] 180 184 189 195 202 ...
    ##  $ half1   : num [1:22] 179 183 188 193 201 ...
    ##  $ half2   : num [1:22] 181 185 190 197 203 ...
    ##  $ cpi     : num [1:22] 181 185 190 199 203 ...
    ##  $ base_cpi: num [1:22] 308 308 308 308 308 ...

# gender data prep

[gender R package](https://github.com/lmullen/gender)

``` r
gender_data = salary_data %>% 
  distinct(first_name) %>% 
  pull(first_name) %>% 
  gender(years = c("1957", "2005"), method = "ssa")

gender_data %>% str()
```

    ## tibble [11,162 × 6] (S3: tbl_df/tbl/data.frame)
    ##  $ name             : chr [1:11162] "Aadil" "Aalap" "Aaleah" "Aaliyah" ...
    ##  $ proportion_male  : num [1:11162] 1 1 0 0.0018 1 1 0 0 0 0 ...
    ##  $ proportion_female: num [1:11162] 0 0 1 0.998 0 ...
    ##  $ gender           : chr [1:11162] "male" "male" "female" "female" ...
    ##  $ year_min         : chr [1:11162] "1957" "1957" "1957" "1957" ...
    ##  $ year_max         : chr [1:11162] "2005" "2005" "2005" "2005" ...

# Generate title rankings

``` r
title_rankings = salary_data %>% 
  group_by(year, title) %>% 
  reframe(full_name, title_rank = rank(desc(ftr), ties.method = "min"), title_count = n())
```

# Generate department rankings

``` r
dept_rankings = salary_data %>% 
  group_by(year, dept) %>% 
  reframe(full_name, year, dept_rank = rank(desc(ftr), ties.method = "min"), dept_count = n())
```

# generate historical title data

collapses title to csv grouped by name and year

``` r
historical_titles_by_year = salary_data %>% 
  select(full_name, year, title) %>% 
  distinct(across(everything())) %>% 
  group_by(full_name, year) %>% 
  dplyr::reframe(full_name, year, historical_titles_by_year = paste(c(title, year, ";"), collapse = " ")) %>% 
  ungroup() %>% 
  distinct(across(everything()))

historical_titles = salary_data %>% 
  ungroup() %>% 
  select(full_name, title) %>% 
  distinct(across(everything())) %>% 
  group_by(full_name) %>% 
  dplyr::reframe(full_name, historical_titles = paste(c(title, ";"), collapse = " ")) %>% 
  ungroup() %>% 
  distinct(across(everything()))
```

# join prepped data sources

``` r
salary_data %>% 
  left_join(historical_titles_by_year) %>% 
  left_join(historical_titles) %>% 
  left_join(gender_data %>% select(first_name = name, proportion_male:gender)) %>% 
  left_join(cpi_data) %>% 
  left_join(title_rankings) %>% 
  left_join(dept_rankings) %>% 
  # rename(full_name = name) %>% 
  # select(-filename) %>% 
  mutate(ftr_cpi_adjusted = (base_cpi/cpi * ftr) %>% round(2)) %>% 
  write_csv("../02 data output/salary_data.csv", append = FALSE)
```

    ## Joining with `by = join_by(full_name, year)`
    ## Joining with `by = join_by(full_name)`
    ## Joining with `by = join_by(first_name)`
    ## Joining with `by = join_by(year)`
    ## Joining with `by = join_by(full_name, title, year)`

    ## Warning in left_join(., title_rankings): Each row in `x` is expected to match at most 1 row in `y`.
    ## ℹ Row 447 of `x` matches multiple rows.
    ## ℹ If multiple matches are expected, set `multiple = "all"` to silence this
    ##   warning.

    ## Joining with `by = join_by(full_name, dept, year)`

    ## Warning in left_join(., dept_rankings): Each row in `x` is expected to match at most 1 row in `y`.
    ## ℹ Row 27 of `x` matches multiple rows.
    ## ℹ If multiple matches are expected, set `multiple = "all"` to silence this
    ##   warning.
