Credit card Market Segmentation by Clustering
================
Kar Ng
2022-05

-   [R PACKAGES](#r-packages)
-   [INTRODUCTION](#introduction)
-   [DATA PREPARATION](#data-preparation)
    -   [Data Import](#data-import)
    -   [Data Description](#data-description)
    -   [Data Exploration](#data-exploration)
-   [DATA CLEANING](#data-cleaning)
    -   [Rename all variables](#rename-all-variables)
    -   [NA Removal](#na-removal)
-   [EDA](#eda)
-   [Unsupervised Clustering](#unsupervised-clustering)

## R PACKAGES

``` r
library(tidyverse)
library(kableExtra)
library(skimr)
```

## INTRODUCTION

This project will use clustering technique to develop a customer
segmentation to define marketing strategy.

Dataset used in this project is called “Credit Card Dataset for
Clustering” by Arjun Bhasin. It is a public dataset acquired from
[Kaggle.com](https://www.kaggle.com/datasets/arjunbhasin2013/ccdata).

The dataset has 17 behavioral information of about 9000 active credit
card holders.

## DATA PREPARATION

### Data Import

Following codes import the dataset and specify the first column as row’s
name, as it is required to perform clustering.

``` r
cc <- read.csv("cc_dataset.csv",
               header = T,
               row.names = 1)   # specificy column 1 as row name 
```

randomly sample the first 10 rows of the dataset.

``` r
sample_n(cc, 10)
```

    ##            BALANCE BALANCE_FREQUENCY PURCHASES ONEOFF_PURCHASES
    ## C16859   76.096025          0.909091     71.22                0
    ## C10393  566.147105          1.000000      0.00                0
    ## C19094  539.162731          0.625000      0.00                0
    ## C12480  611.561026          1.000000    765.00              765
    ## C10271 2224.590946          0.818182   3183.00             3183
    ## C10103    4.708041          0.363636     79.00                0
    ## C18873 1040.030221          1.000000      0.00                0
    ## C16096  116.418751          1.000000   1176.00                0
    ## C19160   14.524779          0.333333    152.00              152
    ## C15449   97.168390          1.000000   1639.92                0
    ##        INSTALLMENTS_PURCHASES CASH_ADVANCE PURCHASES_FREQUENCY
    ## C16859                  71.22       0.0000            0.250000
    ## C10393                   0.00     187.5724            0.000000
    ## C19094                   0.00    1103.3038            0.000000
    ## C12480                   0.00       0.0000            0.125000
    ## C10271                   0.00       0.0000            0.166667
    ## C10103                  79.00       0.0000            0.250000
    ## C18873                   0.00    1390.6483            0.000000
    ## C16096                1176.00       0.0000            1.000000
    ## C19160                   0.00       0.0000            0.333333
    ## C15449                1639.92       0.0000            1.000000
    ##        ONEOFF_PURCHASES_FREQUENCY PURCHASES_INSTALLMENTS_FREQUENCY
    ## C16859                   0.000000                         0.250000
    ## C10393                   0.000000                         0.000000
    ## C19094                   0.000000                         0.000000
    ## C12480                   0.125000                         0.000000
    ## C10271                   0.166667                         0.000000
    ## C10103                   0.000000                         0.166667
    ## C18873                   0.000000                         0.000000
    ## C16096                   0.000000                         1.000000
    ## C19160                   0.333333                         0.000000
    ## C15449                   0.000000                         1.000000
    ##        CASH_ADVANCE_FREQUENCY CASH_ADVANCE_TRX PURCHASES_TRX CREDIT_LIMIT
    ## C16859               0.000000                0             3         1500
    ## C10393               0.083333                1             0         2000
    ## C19094               0.125000                1             0         1200
    ## C12480               0.000000                0             1         2000
    ## C10271               0.000000                0             2         9000
    ## C10103               0.000000                0             3         7000
    ## C18873               0.545455               18             0         1400
    ## C16096               0.000000                0            12         2150
    ## C19160               0.000000                0             2         1500
    ## C15449               0.000000                0            12         2150
    ##         PAYMENTS MINIMUM_PAYMENTS PRC_FULL_PAYMENT TENURE
    ## C16859  216.0591         159.4789         0.000000     12
    ## C10393 1177.6616         190.3652         0.000000     12
    ## C19094  360.9863         200.1012         0.000000      8
    ## C12480  204.4642         146.3523         0.000000      8
    ## C10271 3823.3462         629.0805         0.000000     12
    ## C10103  217.8202         121.7812         1.000000     12
    ## C18873  290.1296         287.9942         0.000000     11
    ## C16096 1083.7840        1364.8167         0.666667     12
    ## C19160    0.0000               NA         0.000000      6
    ## C15449 1620.2857        1555.7931         0.083333     12

The name of all variables are:

``` r
names(cc)
```

    ##  [1] "BALANCE"                          "BALANCE_FREQUENCY"               
    ##  [3] "PURCHASES"                        "ONEOFF_PURCHASES"                
    ##  [5] "INSTALLMENTS_PURCHASES"           "CASH_ADVANCE"                    
    ##  [7] "PURCHASES_FREQUENCY"              "ONEOFF_PURCHASES_FREQUENCY"      
    ##  [9] "PURCHASES_INSTALLMENTS_FREQUENCY" "CASH_ADVANCE_FREQUENCY"          
    ## [11] "CASH_ADVANCE_TRX"                 "PURCHASES_TRX"                   
    ## [13] "CREDIT_LIMIT"                     "PAYMENTS"                        
    ## [15] "MINIMUM_PAYMENTS"                 "PRC_FULL_PAYMENT"                
    ## [17] "TENURE"

### Data Description

Following is the data description extracted from the kaggle website.

``` r
Variables <- c("CUSTID", "BALANCE", "BALANCEFREQUENCY", "PURCHASES", "ONEOFFPURCHASES",
               "INSTALLMENTSPURCHASES", "CASHADVANCE", "PURCHASESFREQUENCY", "ONEOFFPURCHASESFREQUENCY", "PURCHASESINSTALLMENTSFREQUENCY", "CASHADVANCEFREQUENCY", "CASHADVANCETRX", "PURCHASESTRX", "CREDITLIMIT", "PAYMENTS", "MINIMUM_PAYMENTS", "PRCFULLPAYMENT", "TENURE")

Description <- c("Identification of Credit Card holder (Categorical)",
                 "Balance amount left in their account to make purchases",
                 "How frequently the Balance is updated, score between 0 and 1 (1 = frequently updated, 0 = not frequently updated)",
                 "Amount of purchases made from account",
                 "Maximum purchase amount done in one-go",
                 "Amount of purchase done in installment",
                 "Cash in advance given by the user",
                 "How frequently the Purchases are being made, score between 0 and 1 (1 = frequently purchased, 0 = not frequently purchased)",
                 "How frequently Purchases are happening in one-go (1 = frequently purchased, 0 = not frequently purchased)",
                 "How frequently purchases in installments are being done (1 = frequently done, 0 = not frequently done)",
                 "How frequently the cash in advance being paid",
                 "Number of Transactions made with Cash in Advanced",
                 "Number of purchase transactions made",
                 "Limit of Credit Card for user",
                 "Amount of Payment done by user",
                 "Minimum amount of payments made by user",
                 "Percent of full payment paid by user",
                 "Tenure of credit card service for user")


data.frame(Variables, Description) %>% 
  kbl() %>% 
  kable_styling(bootstrap_options = c("bordered", "stripped"))
```

<table class="table table-bordered" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Variables
</th>
<th style="text-align:left;">
Description
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
CUSTID
</td>
<td style="text-align:left;">
Identification of Credit Card holder (Categorical)
</td>
</tr>
<tr>
<td style="text-align:left;">
BALANCE
</td>
<td style="text-align:left;">
Balance amount left in their account to make purchases
</td>
</tr>
<tr>
<td style="text-align:left;">
BALANCEFREQUENCY
</td>
<td style="text-align:left;">
How frequently the Balance is updated, score between 0 and 1 (1 =
frequently updated, 0 = not frequently updated)
</td>
</tr>
<tr>
<td style="text-align:left;">
PURCHASES
</td>
<td style="text-align:left;">
Amount of purchases made from account
</td>
</tr>
<tr>
<td style="text-align:left;">
ONEOFFPURCHASES
</td>
<td style="text-align:left;">
Maximum purchase amount done in one-go
</td>
</tr>
<tr>
<td style="text-align:left;">
INSTALLMENTSPURCHASES
</td>
<td style="text-align:left;">
Amount of purchase done in installment
</td>
</tr>
<tr>
<td style="text-align:left;">
CASHADVANCE
</td>
<td style="text-align:left;">
Cash in advance given by the user
</td>
</tr>
<tr>
<td style="text-align:left;">
PURCHASESFREQUENCY
</td>
<td style="text-align:left;">
How frequently the Purchases are being made, score between 0 and 1 (1 =
frequently purchased, 0 = not frequently purchased)
</td>
</tr>
<tr>
<td style="text-align:left;">
ONEOFFPURCHASESFREQUENCY
</td>
<td style="text-align:left;">
How frequently Purchases are happening in one-go (1 = frequently
purchased, 0 = not frequently purchased)
</td>
</tr>
<tr>
<td style="text-align:left;">
PURCHASESINSTALLMENTSFREQUENCY
</td>
<td style="text-align:left;">
How frequently purchases in installments are being done (1 = frequently
done, 0 = not frequently done)
</td>
</tr>
<tr>
<td style="text-align:left;">
CASHADVANCEFREQUENCY
</td>
<td style="text-align:left;">
How frequently the cash in advance being paid
</td>
</tr>
<tr>
<td style="text-align:left;">
CASHADVANCETRX
</td>
<td style="text-align:left;">
Number of Transactions made with Cash in Advanced
</td>
</tr>
<tr>
<td style="text-align:left;">
PURCHASESTRX
</td>
<td style="text-align:left;">
Number of purchase transactions made
</td>
</tr>
<tr>
<td style="text-align:left;">
CREDITLIMIT
</td>
<td style="text-align:left;">
Limit of Credit Card for user
</td>
</tr>
<tr>
<td style="text-align:left;">
PAYMENTS
</td>
<td style="text-align:left;">
Amount of Payment done by user
</td>
</tr>
<tr>
<td style="text-align:left;">
MINIMUM_PAYMENTS
</td>
<td style="text-align:left;">
Minimum amount of payments made by user
</td>
</tr>
<tr>
<td style="text-align:left;">
PRCFULLPAYMENT
</td>
<td style="text-align:left;">
Percent of full payment paid by user
</td>
</tr>
<tr>
<td style="text-align:left;">
TENURE
</td>
<td style="text-align:left;">
Tenure of credit card service for user
</td>
</tr>
</tbody>
</table>

### Data Exploration

**Data Size and type**

The dataset contain 8950 rows and 17 variables. The “dbl” and “ind” are
data type allocated by R to a particular column. The “dbl” stands for
“double”, it is used for numerical variables that have decimal places.
The “int” stands for integer, it is used for numerical variables that
have integer values. All variables are numeric with either “dbl” and
“int”, they will be treated the same type during analysis.

``` r
glimpse(cc)
```

    ## Rows: 8,950
    ## Columns: 17
    ## $ BALANCE                          <dbl> 40.90075, 3202.46742, 2495.14886, 166~
    ## $ BALANCE_FREQUENCY                <dbl> 0.818182, 0.909091, 1.000000, 0.63636~
    ## $ PURCHASES                        <dbl> 95.40, 0.00, 773.17, 1499.00, 16.00, ~
    ## $ ONEOFF_PURCHASES                 <dbl> 0.00, 0.00, 773.17, 1499.00, 16.00, 0~
    ## $ INSTALLMENTS_PURCHASES           <dbl> 95.40, 0.00, 0.00, 0.00, 0.00, 1333.2~
    ## $ CASH_ADVANCE                     <dbl> 0.0000, 6442.9455, 0.0000, 205.7880, ~
    ## $ PURCHASES_FREQUENCY              <dbl> 0.166667, 0.000000, 1.000000, 0.08333~
    ## $ ONEOFF_PURCHASES_FREQUENCY       <dbl> 0.000000, 0.000000, 1.000000, 0.08333~
    ## $ PURCHASES_INSTALLMENTS_FREQUENCY <dbl> 0.083333, 0.000000, 0.000000, 0.00000~
    ## $ CASH_ADVANCE_FREQUENCY           <dbl> 0.000000, 0.250000, 0.000000, 0.08333~
    ## $ CASH_ADVANCE_TRX                 <int> 0, 4, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ PURCHASES_TRX                    <int> 2, 0, 12, 1, 1, 8, 64, 12, 5, 3, 12, ~
    ## $ CREDIT_LIMIT                     <dbl> 1000, 7000, 7500, 7500, 1200, 1800, 1~
    ## $ PAYMENTS                         <dbl> 201.8021, 4103.0326, 622.0667, 0.0000~
    ## $ MINIMUM_PAYMENTS                 <dbl> 139.50979, 1072.34022, 627.28479, NA,~
    ## $ PRC_FULL_PAYMENT                 <dbl> 0.000000, 0.222222, 0.000000, 0.00000~
    ## $ TENURE                           <int> 12, 12, 12, 12, 12, 12, 12, 12, 12, 1~

**Purchases**

During exploration, I found that “PURCHASES” is the sum of
“ONEOFF_PURCHASES” and “INSTALLMENTS_PURCHASES”. It may not be important
in this analysis.

Following select 10 rows among the dataset, and the new variable
“MY_PURCHASES” proves my finding, which is the same as the “PURCHASES”,
and is the sum of “ONEOFF_PURCHASES” and “INSTALLMENTS_PURCHASES”.

``` r
cc %>% 
  top_n(10, BALANCE) %>% 
  dplyr::select(ONEOFF_PURCHASES, INSTALLMENTS_PURCHASES, PURCHASES) %>% 
  mutate(MY_PURCHASES = ONEOFF_PURCHASES + INSTALLMENTS_PURCHASES)
```

    ##        ONEOFF_PURCHASES INSTALLMENTS_PURCHASES PURCHASES MY_PURCHASES
    ## C10144          9449.07               12560.85  22009.92     22009.92
    ## C10544           529.30                   0.00    529.30       529.30
    ## C10609          7564.81                 258.93   7823.74      7823.74
    ## C10914             0.00                   0.00      0.00         0.00
    ## C12434             0.00                1168.75   1168.75      1168.75
    ## C14256          3657.30                1630.98   5288.28      5288.28
    ## C14836           717.24                   0.00    717.24       717.24
    ## C15429           105.30                 579.44    684.74       684.74
    ## C15642             0.00                1770.57   1770.57      1770.57
    ## C16812          3582.45                1442.23   5024.68      5024.68

**Missing values check**

Again, proven by other function that the tables have 8950 rows of data
and 17 variables. All variables are numerical, and, by examining the
variables “n_missing” and “complete_rate” in following tables, there is
1 missing value in the variable “CREDIT_LIMIT” and 313 in the
“MINIMUM_PAYMENTS”. These missing values need to be handled.

``` r
skim_without_charts(cc)
```

<table style="width: auto;" class="table table-condensed">
<caption>
Data summary
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:left;">
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Name
</td>
<td style="text-align:left;">
cc
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of rows
</td>
<td style="text-align:left;">
8950
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of columns
</td>
<td style="text-align:left;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Column type frequency:
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Group variables
</td>
<td style="text-align:left;">
None
</td>
</tr>
</tbody>
</table>

**Variable type: numeric**

<table>
<thead>
<tr>
<th style="text-align:left;">
skim_variable
</th>
<th style="text-align:right;">
n_missing
</th>
<th style="text-align:right;">
complete_rate
</th>
<th style="text-align:right;">
mean
</th>
<th style="text-align:right;">
sd
</th>
<th style="text-align:right;">
p0
</th>
<th style="text-align:right;">
p25
</th>
<th style="text-align:right;">
p50
</th>
<th style="text-align:right;">
p75
</th>
<th style="text-align:right;">
p100
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
BALANCE
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1564.47
</td>
<td style="text-align:right;">
2081.53
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
128.28
</td>
<td style="text-align:right;">
873.39
</td>
<td style="text-align:right;">
2054.14
</td>
<td style="text-align:right;">
19043.14
</td>
</tr>
<tr>
<td style="text-align:left;">
BALANCE_FREQUENCY
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
0.24
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.00
</td>
</tr>
<tr>
<td style="text-align:left;">
PURCHASES
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1003.20
</td>
<td style="text-align:right;">
2136.63
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
39.63
</td>
<td style="text-align:right;">
361.28
</td>
<td style="text-align:right;">
1110.13
</td>
<td style="text-align:right;">
49039.57
</td>
</tr>
<tr>
<td style="text-align:left;">
ONEOFF_PURCHASES
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
592.44
</td>
<td style="text-align:right;">
1659.89
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
38.00
</td>
<td style="text-align:right;">
577.41
</td>
<td style="text-align:right;">
40761.25
</td>
</tr>
<tr>
<td style="text-align:left;">
INSTALLMENTS_PURCHASES
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
411.07
</td>
<td style="text-align:right;">
904.34
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
89.00
</td>
<td style="text-align:right;">
468.64
</td>
<td style="text-align:right;">
22500.00
</td>
</tr>
<tr>
<td style="text-align:left;">
CASH_ADVANCE
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
978.87
</td>
<td style="text-align:right;">
2097.16
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1113.82
</td>
<td style="text-align:right;">
47137.21
</td>
</tr>
<tr>
<td style="text-align:left;">
PURCHASES_FREQUENCY
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.49
</td>
<td style="text-align:right;">
0.40
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
1.00
</td>
</tr>
<tr>
<td style="text-align:left;">
ONEOFF_PURCHASES_FREQUENCY
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
1.00
</td>
</tr>
<tr>
<td style="text-align:left;">
PURCHASES_INSTALLMENTS_FREQUENCY
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.36
</td>
<td style="text-align:right;">
0.40
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
1.00
</td>
</tr>
<tr>
<td style="text-align:left;">
CASH_ADVANCE_FREQUENCY
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.22
</td>
<td style="text-align:right;">
1.50
</td>
</tr>
<tr>
<td style="text-align:left;">
CASH_ADVANCE_TRX
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.25
</td>
<td style="text-align:right;">
6.82
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
4.00
</td>
<td style="text-align:right;">
123.00
</td>
</tr>
<tr>
<td style="text-align:left;">
PURCHASES_TRX
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
14.71
</td>
<td style="text-align:right;">
24.86
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
7.00
</td>
<td style="text-align:right;">
17.00
</td>
<td style="text-align:right;">
358.00
</td>
</tr>
<tr>
<td style="text-align:left;">
CREDIT_LIMIT
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
4494.45
</td>
<td style="text-align:right;">
3638.82
</td>
<td style="text-align:right;">
50.00
</td>
<td style="text-align:right;">
1600.00
</td>
<td style="text-align:right;">
3000.00
</td>
<td style="text-align:right;">
6500.00
</td>
<td style="text-align:right;">
30000.00
</td>
</tr>
<tr>
<td style="text-align:left;">
PAYMENTS
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1733.14
</td>
<td style="text-align:right;">
2895.06
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
383.28
</td>
<td style="text-align:right;">
856.90
</td>
<td style="text-align:right;">
1901.13
</td>
<td style="text-align:right;">
50721.48
</td>
</tr>
<tr>
<td style="text-align:left;">
MINIMUM_PAYMENTS
</td>
<td style="text-align:right;">
313
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
864.21
</td>
<td style="text-align:right;">
2372.45
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
169.12
</td>
<td style="text-align:right;">
312.34
</td>
<td style="text-align:right;">
825.49
</td>
<td style="text-align:right;">
76406.21
</td>
</tr>
<tr>
<td style="text-align:left;">
PRC_FULL_PAYMENT
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.29
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
1.00
</td>
</tr>
<tr>
<td style="text-align:left;">
TENURE
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
11.52
</td>
<td style="text-align:right;">
1.34
</td>
<td style="text-align:right;">
6.00
</td>
<td style="text-align:right;">
12.00
</td>
<td style="text-align:right;">
12.00
</td>
<td style="text-align:right;">
12.00
</td>
<td style="text-align:right;">
12.00
</td>
</tr>
</tbody>
</table>

Alternatively, following code performs the missing-value check.

``` r
colSums(is.na(cc))
```

    ##                          BALANCE                BALANCE_FREQUENCY 
    ##                                0                                0 
    ##                        PURCHASES                 ONEOFF_PURCHASES 
    ##                                0                                0 
    ##           INSTALLMENTS_PURCHASES                     CASH_ADVANCE 
    ##                                0                                0 
    ##              PURCHASES_FREQUENCY       ONEOFF_PURCHASES_FREQUENCY 
    ##                                0                                0 
    ## PURCHASES_INSTALLMENTS_FREQUENCY           CASH_ADVANCE_FREQUENCY 
    ##                                0                                0 
    ##                 CASH_ADVANCE_TRX                    PURCHASES_TRX 
    ##                                0                                0 
    ##                     CREDIT_LIMIT                         PAYMENTS 
    ##                                1                                0 
    ##                 MINIMUM_PAYMENTS                 PRC_FULL_PAYMENT 
    ##                              313                                0 
    ##                           TENURE 
    ##                                0

There are 8950 rows of data and I will still have 96.5% of data left
after removal of these missing values and therefore I will simply remove
these missing values for the simplicity of this project.

Typically, missing value can be handled by either removal, replaced with
mean, median, or using imputation algorithm such as KNN or bagging
algorithm. These techniques are usually performed when there are too
many missing values in important variables. For example, when missing
values is higher than 5% and less than 60%.

## DATA CLEANING

### Rename all variables

The name of all variables are in capital form and which would be
difficult to read for readers and also myself.

``` r
colnames(cc)
```

    ##  [1] "BALANCE"                          "BALANCE_FREQUENCY"               
    ##  [3] "PURCHASES"                        "ONEOFF_PURCHASES"                
    ##  [5] "INSTALLMENTS_PURCHASES"           "CASH_ADVANCE"                    
    ##  [7] "PURCHASES_FREQUENCY"              "ONEOFF_PURCHASES_FREQUENCY"      
    ##  [9] "PURCHASES_INSTALLMENTS_FREQUENCY" "CASH_ADVANCE_FREQUENCY"          
    ## [11] "CASH_ADVANCE_TRX"                 "PURCHASES_TRX"                   
    ## [13] "CREDIT_LIMIT"                     "PAYMENTS"                        
    ## [15] "MINIMUM_PAYMENTS"                 "PRC_FULL_PAYMENT"                
    ## [17] "TENURE"

Following code transforms all the name into reading-friendly form.

``` r
cc <- cc %>% 
  rename_all(str_to_sentence)
```

Checking again the name of each variable.

``` r
colnames(cc)
```

    ##  [1] "Balance"                          "Balance_frequency"               
    ##  [3] "Purchases"                        "Oneoff_purchases"                
    ##  [5] "Installments_purchases"           "Cash_advance"                    
    ##  [7] "Purchases_frequency"              "Oneoff_purchases_frequency"      
    ##  [9] "Purchases_installments_frequency" "Cash_advance_frequency"          
    ## [11] "Cash_advance_trx"                 "Purchases_trx"                   
    ## [13] "Credit_limit"                     "Payments"                        
    ## [15] "Minimum_payments"                 "Prc_full_payment"                
    ## [17] "Tenure"

### NA Removal

Following code remove all the missing values in the dataset (314 rows
among 8950 rows)

``` r
cc <- cc %>% 
  na.omit()
```

Now, the number of rows have been reduced to 8636 from 8950.

``` r
str(cc)
```

    ## 'data.frame':    8636 obs. of  17 variables:
    ##  $ Balance                         : num  40.9 3202.5 2495.1 817.7 1809.8 ...
    ##  $ Balance_frequency               : num  0.818 0.909 1 1 1 ...
    ##  $ Purchases                       : num  95.4 0 773.2 16 1333.3 ...
    ##  $ Oneoff_purchases                : num  0 0 773 16 0 ...
    ##  $ Installments_purchases          : num  95.4 0 0 0 1333.3 ...
    ##  $ Cash_advance                    : num  0 6443 0 0 0 ...
    ##  $ Purchases_frequency             : num  0.1667 0 1 0.0833 0.6667 ...
    ##  $ Oneoff_purchases_frequency      : num  0 0 1 0.0833 0 ...
    ##  $ Purchases_installments_frequency: num  0.0833 0 0 0 0.5833 ...
    ##  $ Cash_advance_frequency          : num  0 0.25 0 0 0 0 0 0 0 0 ...
    ##  $ Cash_advance_trx                : int  0 4 0 0 0 0 0 0 0 0 ...
    ##  $ Purchases_trx                   : int  2 0 12 1 8 64 12 5 3 12 ...
    ##  $ Credit_limit                    : num  1000 7000 7500 1200 1800 13500 2300 7000 11000 1200 ...
    ##  $ Payments                        : num  202 4103 622 678 1400 ...
    ##  $ Minimum_payments                : num  140 1072 627 245 2407 ...
    ##  $ Prc_full_payment                : num  0 0.222 0 0 0 ...
    ##  $ Tenure                          : int  12 12 12 12 12 12 12 12 12 12 ...
    ##  - attr(*, "na.action")= 'omit' Named int [1:314] 4 46 48 55 56 57 64 94 95 98 ...
    ##   ..- attr(*, "names")= chr [1:314] "C10004" "C10047" "C10049" "C10056" ...

## EDA

A primary exploratory data analysis is suggested to quickly understand
the general distribution of the data.

``` r
# data frame

df <- cc %>% 
  pivot_longer(c(1:17), 
               names_to = "my.variable", 
               values_to = "my.value")

# graphs

ggplot(df, aes(x = my.value, fill = my.variable)) +
  geom_histogram() +
  facet_wrap(~my.variable, scale = "free") +
  theme(legend.position = "none")
```

![](cc_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Most of the variables follow Pareto trend (80:20 rule) with majority of
the data belong to one side of the value. it can be seem quite hard to
group the data and categories the data into several distinct group for
marketing purposes.

## Unsupervised Clustering