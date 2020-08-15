# Library -----------------------------------------------------------------
library(magrittr)


# Read --------------------------------------------------------------------
# https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data
df <- readr::read_csv("./input/train.csv",
                      col_types = readr::cols_only(
                        Id = readr::col_factor(), 
                        MSSubClass = readr::col_integer(), 
                        MSZoning = readr::col_factor(), 
                        LotFrontage = readr::col_integer(), 
                        LotArea = readr::col_integer(), 
                        Street = readr::col_factor(), 
                        Alley = readr::col_factor(), 
                        SalePrice = readr::col_integer()
                      ))



# diagnose ----------------------------------------------------------------
# 基本の関数。欠損具体とユニーク具合を確認する。
dlookr::diagnose(df)
# A tibble: 8 x 6
#   variables   types   missing_count missing_percent unique_count unique_rate
#   <chr>       <chr>           <int>           <dbl>        <int>       <dbl>
# 1 Id          factor              0             0           1460     1      
# 2 MSSubClass  integer             0             0             15     0.0103 
# 3 MSZoning    factor              0             0              5     0.00342
# 4 LotFrontage integer           259            17.7          111     0.0760 
# 5 LotArea     integer             0             0           1073     0.735  
# 6 Street      factor              0             0              2     0.00137
# 7 Alley       factor           1369            93.8            3     0.00205
# 8 SalePrice   integer             0             0            663     0.454 



# diagnose_category
# 質的変数のみの概観。
dlookr::diagnose_category(df %>% dplyr::select(!Id))
# A tibble: 10 x 6
#    variables levels      N  freq  ratio  rank
#    <chr>     <fct>   <int> <int>  <dbl> <int>
#  1 MSZoning  RL       1460  1151 78.8       1
#  2 MSZoning  RM       1460   218 14.9       2
#  3 MSZoning  FV       1460    65  4.45      3
#  4 MSZoning  RH       1460    16  1.10      4
#  5 MSZoning  C (all)  1460    10  0.685     5
#  6 Street    Pave     1460  1454 99.6       1
#  7 Street    Grvl     1460     6  0.411     2
#  8 Alley     NA       1460  1369 93.8       1
#  9 Alley     Grvl     1460    50  3.42      2
# 10 Alley     Pave     1460    41  2.81      3



# 全要素を表示してしまうのでIdのような変数は除外しておいたほうが良い。
dlookr::diagnose_category(df %>% dplyr::select(Id))
# A tibble: 1,460 x 6
#    variables levels     N  freq  ratio  rank
#    <chr>     <fct>  <int> <int>  <dbl> <int>
#  1 Id        1       1460     1 0.0685     1
#  2 Id        2       1460     1 0.0685     2
#  3 Id        3       1460     1 0.0685     3
#  4 Id        4       1460     1 0.0685     4
#  5 Id        5       1460     1 0.0685     5
#  6 Id        6       1460     1 0.0685     6
#  7 Id        7       1460     1 0.0685     7
#  8 Id        8       1460     1 0.0685     8
#  9 Id        9       1460     1 0.0685     9
# 10 Id        10      1460     1 0.0685    10
# … with 1,450 more rows



# diagnose_numeric
# 量的変数の概観。summaryとあまり変わらないがzero, minus, outlierなどが追加sれている。
dlookr::diagnose_numeric(df)
# A tibble: 4 x 10
#   variables     min      Q1     mean  median      Q3    max  zero minus outlier
#   <chr>       <int>   <dbl>    <dbl>   <dbl>   <dbl>  <int> <int> <int>   <int>
# 1 MSSubClass     20     20      56.9     50      70     190     0     0     103
# 2 LotFrontage    21     59      70.0     69      80     313     0     0      88
# 3 LotArea      1300   7554.  10517.    9478.  11602. 215245     0     0      68
# 4 SalePrice   34900 129975  180921.  163000  214000  755000     0     0      61



# outlier -----------------------------------------------------------------
# diagnose_numericで表示されていたoutlierについて見ていくdiagnose_ourlier。
dlookr::diagnose_outlier(df)
# A tibble: 4 x 6
#   variables   outliers_cnt outliers_ratio outliers_mean with_mean without_mean
#   <chr>              <int>          <dbl>         <dbl>     <dbl>        <dbl>
# 1 MSSubClass           103           7.05         171.       56.9         48.3
# 2 LotFrontage           88           6.03          83.9      70.0         69.0
# 3 LotArea               68           4.66       36103.    10517.        9267. 
# 4 SalePrice             61           4.18      425954.   180921.      170237. 



# 外れ値有無時の可視化。
dlookr::plot_outlier(df)




# imputate ------------------------------------------------------------------
# 外れ値や欠損値の補完を行うimputate系の関数。
# 欠損値の補完をするimputate_na。
imputate_na_LotFrontage <- dlookr::imputate_na(df, xvar = LotFrontage, yvar = SalePrice, method = "mice", seed = 2020)

# summaryで補完状況の概要。
summary(imputate_na_LotFrontage)
# * Impute missing values based on Multivariate Imputation by Chained Equations
#  - method : mice
#  - random seed : 2020
# 
# * Information of Imputation (before vs after)
#              Original   Imputation
# n        1201.0000000 1460.0000000
# na        259.0000000    0.0000000
# mean       70.0499584   70.7067123
# sd         24.2847518   23.1749607
# se_mean     0.7007485    0.6065164
# IQR        21.0000000   20.7000000
# skewness    2.1635691    1.9852213
# kurtosis   17.4528673   17.2866156
# p00        21.0000000   21.0000000
# p01        21.0000000   21.0000000
# p05        34.0000000   35.0000000
# p10        44.0000000   44.0000000
# p20        53.0000000   55.0000000
# p25        59.0000000   60.0000000
# p30        60.0000000   60.0000000
# p40        63.0000000   65.0000000
# p50        69.0000000   70.0000000
# p60        74.0000000   75.0000000
# p70        79.0000000   80.0000000
# p75        80.0000000   80.7000000
# p80        85.0000000   85.0000000
# p90        96.0000000   94.0600000
# p95       107.0000000  105.0000000
# p99       141.0000000  138.8200000
# p100      313.0000000  313.0000000

# 補完状況の可視化。
plot(imputate_na_LotFrontage)


# 外れ値の処理はimputate_outlier。
imputate_outlier_LotFrontage <- dlookr::imputate_outlier(df, xvar = LotFrontage, method = "capping")

# 処理状況概要。
summary(imputate_outlier_LotFrontage)
# Impute outliers with capping
# 
# * Information of Imputation (before vs after)
#              Original   Imputation
# n        1201.0000000 1201.0000000
# na        259.0000000  259.0000000
# mean       70.0499584   69.1898418
# sd         24.2847518   18.8490653
# se_mean     0.7007485    0.5438991
# IQR        21.0000000   21.0000000
# skewness    2.1635691    0.1198065
# kurtosis   17.4528673   -0.3981954
# p00        21.0000000   30.0000000
# p01        21.0000000   34.0000000
# p05        34.0000000   34.0000000
# p10        44.0000000   44.0000000
# p20        53.0000000   53.0000000
# p25        59.0000000   59.0000000
# p30        60.0000000   60.0000000
# p40        63.0000000   63.0000000
# p50        69.0000000   69.0000000
# p60        74.0000000   74.0000000
# p70        79.0000000   79.0000000
# p75        80.0000000   80.0000000
# p80        85.0000000   85.0000000
# p90        96.0000000   96.0000000
# p95       107.0000000  107.0000000
# p99       141.0000000  107.0000000
# p100      313.0000000  111.0000000

# 可視化。上下5%点の値で代替しているのでその部分にピークが生まれている。
plot(imputate_outlier_LotFrontage)




# relate ------------------------------------------------------------------
# 目的変数との関係を確認する。
# 量的変数（目的変数）×量的変数（説明変数）
rel_num_num <- df %>% 
  dlookr::target_by(target = SalePrice) %>% # 目的変数の指定。
  dlookr::relate(predictor = LotFrontage) # 説明変数の指定。

# 単変数線形回帰が行われている。
rel_num_num
# Call:
# lm(formula = formula_str, data = data)
# 
# Coefficients:
# (Intercept)  LotFrontage  
#       96149         1208 

# 説明変数×目的変数の散布図。
# 回帰残差×実測値の散布図。
plot(rel_num_num)



# 質的 × 質的
rel_cat_cat <- df %>% 
  dlookr::target_by(target = Street) %>% # 目的変数の指定。
  dlookr::relate(predictor = MSZoning) # 説明変数の指定。

rel_cat_cat
#       MSZoning
# Street   RL   RM C (all)   FV   RH
#   Pave 1148  217       8   65   16
#   Grvl    3    1       2    0    0

plot(rel_cat_cat)



# 質的 × 量的
rel_cat_num <- df %>% 
  dlookr::target_by(target = Street) %>% # 目的変数の指定。
  dlookr::relate(predictor = LotFrontage) # 説明変数の指定。

rel_cat_num
# A tibble: 3 x 27
#   variable Street     n    na  mean    sd se_mean   IQR skewness kurtosis   p00   p01   p05   p10   p20   p25   p30   p40   p50
#   <chr>    <fct>  <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl>    <dbl>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1 LotFron… Pave    1196   258  70.0  24.3   0.701    21   2.18      17.6     21  21    34    43.5  53      59    60    63    69
# 2 LotFron… Grvl       5     1  85.4  29.4  13.1      44   0.0750    -2.10    50  50.6  53.2  56.4  62.8    66    69    75    81
# 3 LotFron… total   1201   259  70.0  24.3   0.701    21   2.16      17.5     21  21    34    44    53      59    60    63    69
# … with 8 more variables: p60 <dbl>, p70 <dbl>, p75 <dbl>, p80 <dbl>, p90 <dbl>, p95 <dbl>, p99 <dbl>, p100 <dbl>

plot(rel_cat_num)



# 量的 × 質的
rel_num_cat <- df %>%
  dlookr::target_by(target = SalePrice) %>% # 目的変数の指定。
  dlookr::relate(predictor = MSZoning) # 説明変数の指定。

rel_num_cat
# Analysis of Variance Table
# 
# Response: SalePrice
#             Df     Sum Sq    Mean Sq F value    Pr(>F)
# MSZoning     4 9.9040e+11 2.4760e+11   43.84 < 2.2e-16 ***
# Residuals 1455 8.2175e+12 5.6478e+09
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

plot(rel_num_cat)



# normality -------------------------------------------------------------------
# 正規性
dlookr::normality(df)
# A tibble: 4 x 4
#   vars        statistic  p_value sample
#   <chr>           <dbl>    <dbl>  <dbl>
# 1 MSSubClass      0.805 9.11e-39   1460
# 2 LotFrontage     0.880 2.00e-29   1460
# 3 LotArea         0.351 7.93e-58   1460
# 4 SalePrice       0.870 3.21e-33   1460

dlookr::plot_normality(df, SalePrice)



# correlate ---------------------------------------------------------------
# 相関係数
dlookr::correlate(df)
# A tibble: 12 x 3
#    var1        var2        coef_corr
#    <fct>       <fct>           <dbl>
#  1 LotFrontage MSSubClass    -0.386 
#  2 LotArea     MSSubClass    -0.140 
#  3 SalePrice   MSSubClass    -0.0843
#  4 MSSubClass  LotFrontage   -0.386 
#  5 LotArea     LotFrontage    0.426 
#  6 SalePrice   LotFrontage    0.352 
#  7 MSSubClass  LotArea       -0.140 
#  8 LotFrontage LotArea        0.426 
#  9 SalePrice   LotArea        0.264 
# 10 MSSubClass  SalePrice     -0.0843
# 11 LotFrontage SalePrice      0.352 
# 12 LotArea     SalePrice      0.264 

dlookr::plot_correlate(df)




# transform ---------------------------------------------------------------
# 変換（標準化や対数化など）
transform_log <- dlookr::transform(df$SalePrice, method = "log")

summary(transform_log)
# * Resolving Skewness with log
# 
# * Information of Transformation (before vs after)
#              Original Transformation
# n        1.460000e+03   1.460000e+03
# na       0.000000e+00   0.000000e+00
# mean     1.809212e+05   1.202405e+01
# sd       7.944250e+04   3.994519e-01
# se_mean  2.079105e+03   1.045413e-02
# IQR      8.402500e+04   4.986339e-01
# skewness 1.882876e+00   1.213351e-01
# kurtosis 6.536282e+00   8.095320e-01
# p00      3.490000e+04   1.046024e+01
# p01      6.181597e+04   1.103186e+01
# p05      8.800000e+04   1.138509e+01
# p10      1.064750e+05   1.157567e+01
# p20      1.240000e+05   1.172804e+01
# p25      1.299750e+05   1.177510e+01
# p30      1.355000e+05   1.181673e+01
# p40      1.470000e+05   1.189819e+01
# p50      1.630000e+05   1.200151e+01
# p60      1.792800e+05   1.209670e+01
# p70      1.986200e+05   1.219915e+01
# p75      2.140000e+05   1.227373e+01
# p80      2.300000e+05   1.234583e+01
# p90      2.780000e+05   1.253538e+01
# p95      3.261000e+05   1.269496e+01
# p99      4.425670e+05   1.300032e+01
# p100     7.550000e+05   1.353447e+01

plot(transform_log)



# report ------------------------------------------------------------------
dlookr::eda_report(df %>% 
                     dplyr::select(!Id), 
                   output_format = "html", 
                   target = SalePrice)
# 1 Introduction
# 1.1 Information of Dataset
# 1.2 Information of Variables
# 1.3 About EDA Report
# 2 Univariate Analysis
# 2.1 Descriptive Statistics
# 2.2 Normality Test of Numerical Variables
# 2.2.1 Statistics and Visualization of (Sample) Data
# 3 Relationship Between Variables
# 3.1 Correlation Coefficient
# 3.1.1 Correlation Coefficient by Variable Combination
# 3.1.2 Correlation Plot of Numerical Variables
# 4 Target based Analysis
# 4.1 Grouped Descriptive Statistics
# 4.1.1 Grouped Numerical Variables
# 4.1.2 Grouped Categorical Variables
# 4.2 Grouped Relationship Between Variables
# 4.2.1 Grouped Correlation Coefficient
# 4.2.2 Grouped Correlation Plot of Numerical Variables



dlookr::diagnose_report(df %>% 
                     dplyr::select(!Id), 
                   output_format = "html")
# 1 Diagnose Data
# 1.1 Overview of Diagnosis
# 1.1.1 List of all variables quality
# 1.1.2 Diagnosis of missing data
# 1.1.3 Diagnosis of unique data(Text and Category)
# 1.1.4 Diagnosis of unique data(Numerical)
# 1.2 Detailed data diagnosis
# 1.2.1 Diagnosis of categorical variables
# 1.2.2 Diagnosis of numerical variables
# 1.2.3 List of numerical diagnosis (zero)
# 1.2.4 List of numerical diagnosis (minus)
# 2 Diagnose Outliers
# 2.1 Overview of Diagnosis
# 2.1.1 Diagnosis of numerical variable outliers
# 2.2 Detailed outliers diagnosis



dlookr::transformation_report(df %>% 
                                  dplyr::select_if(is.numeric),
                                output_format = "html",
                                target = SalePrice)
# 1 Imputation
# 1.1 Missing Values
# 1.1.1 Missing values imputation information
# 1.1.2 LotFrontage
# 1.2 Outliers
# 1.2.1 Outliers imputation information
# 2 Resolving Skewness
# 2.1 Skewed variables information
# 3 Binning
# 3.1 Numerical Variables for Binning
# 3.2 Binning
# 3.2.1 MSSubClass
# 3.2.2 LotFrontage
# 3.2.3 LotArea
# 3.3 Optimal Binning

