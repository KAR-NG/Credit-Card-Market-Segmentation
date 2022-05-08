cc_cluster
================
Kar Ng
2022-05

-   [R PACKAGES](#r-packages)
-   [INTRODUCTION](#introduction)
-   [DATA PREPARATION](#data-preparation)
    -   [Data Import](#data-import)
    -   [Data Description](#data-description)
    -   [Data Exploration](#data-exploration)
-   [Unsupervised Clustering](#unsupervised-clustering)

## R PACKAGES

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.4     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
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

    ##           BALANCE BALANCE_FREQUENCY PURCHASES ONEOFF_PURCHASES
    ## C19127  435.69685          1.000000      0.00             0.00
    ## C11624  103.39369          1.000000      0.00             0.00
    ## C15563 2933.38744          0.727273      0.00             0.00
    ## C15359 1915.67572          1.000000    855.27           855.27
    ## C18622 1060.01025          1.000000      0.00             0.00
    ## C16960   97.61714          0.222222    736.00           736.00
    ## C13065 1436.78380          1.000000    258.86            25.52
    ## C18349   38.91587          1.000000    564.00             0.00
    ## C12844  300.14036          1.000000   2366.36           555.00
    ## C13382  423.62927          1.000000   2485.54          2349.30
    ##        INSTALLMENTS_PURCHASES CASH_ADVANCE PURCHASES_FREQUENCY
    ## C19127                   0.00    460.28482            0.000000
    ## C11624                   0.00   1820.80072            0.000000
    ## C15563                   0.00   3317.25084            0.000000
    ## C15359                   0.00      0.00000            0.545455
    ## C18622                   0.00     78.97146            0.000000
    ## C16960                   0.00      0.00000            0.222222
    ## C13065                 233.34    143.22865            0.666667
    ## C18349                 564.00      0.00000            1.000000
    ## C12844                1811.36      0.00000            1.000000
    ## C13382                 136.24      0.00000            0.750000
    ##        ONEOFF_PURCHASES_FREQUENCY PURCHASES_INSTALLMENTS_FREQUENCY
    ## C19127                   0.000000                         0.000000
    ## C11624                   0.000000                         0.000000
    ## C15563                   0.000000                         0.000000
    ## C15359                   0.545455                         0.000000
    ## C18622                   0.000000                         0.000000
    ## C16960                   0.222222                         0.000000
    ## C13065                   0.083333                         0.666667
    ## C18349                   0.000000                         1.000000
    ## C12844                   0.166667                         1.000000
    ## C13382                   0.166667                         0.583333
    ##        CASH_ADVANCE_FREQUENCY CASH_ADVANCE_TRX PURCHASES_TRX CREDIT_LIMIT
    ## C19127               0.285714                4             0          500
    ## C11624               0.083333                1             0         3000
    ## C15563               0.250000                5             0         6500
    ## C15359               0.000000                0             8         6200
    ## C18622               0.100000                1             0         1200
    ## C16960               0.000000                0             2         1500
    ## C13065               0.083333                1            13         1500
    ## C18349               0.000000                0            12         2000
    ## C12844               0.000000                0            23         4000
    ## C13382               0.000000                0            10         4500
    ##          PAYMENTS MINIMUM_PAYMENTS PRC_FULL_PAYMENT TENURE
    ## C19127   70.41902         153.9935         0.000000      7
    ## C11624    0.00000               NA         0.000000     12
    ## C15563 9317.53393         929.3340         0.125000     12
    ## C15359  519.06450         438.1906         0.000000     11
    ## C18622  233.54579         257.0230         0.000000     10
    ## C16960    0.00000               NA         0.000000      9
    ## C13065  662.39639         404.7682         0.000000     12
    ## C18349  551.49780         179.3117         1.000000     12
    ## C12844 2324.52717         184.8808         0.666667     12
    ## C13382 2590.14100         180.4890         0.375000     12

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

The dataset contain 8950 rows and 17 variables. The “dbl” and “ind” are
data type allocated by R to a particular column. The “dbl” stands for
“double”, it is used for numerical variables that have decimal places.
The “int” stands for integer, it is used for numerical variables that
have integer values.

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

``` r
summary(cc)
```

    ##     BALANCE        BALANCE_FREQUENCY   PURCHASES        ONEOFF_PURCHASES 
    ##  Min.   :    0.0   Min.   :0.0000    Min.   :    0.00   Min.   :    0.0  
    ##  1st Qu.:  128.3   1st Qu.:0.8889    1st Qu.:   39.63   1st Qu.:    0.0  
    ##  Median :  873.4   Median :1.0000    Median :  361.28   Median :   38.0  
    ##  Mean   : 1564.5   Mean   :0.8773    Mean   : 1003.20   Mean   :  592.4  
    ##  3rd Qu.: 2054.1   3rd Qu.:1.0000    3rd Qu.: 1110.13   3rd Qu.:  577.4  
    ##  Max.   :19043.1   Max.   :1.0000    Max.   :49039.57   Max.   :40761.2  
    ##                                                                          
    ##  INSTALLMENTS_PURCHASES  CASH_ADVANCE     PURCHASES_FREQUENCY
    ##  Min.   :    0.0        Min.   :    0.0   Min.   :0.00000    
    ##  1st Qu.:    0.0        1st Qu.:    0.0   1st Qu.:0.08333    
    ##  Median :   89.0        Median :    0.0   Median :0.50000    
    ##  Mean   :  411.1        Mean   :  978.9   Mean   :0.49035    
    ##  3rd Qu.:  468.6        3rd Qu.: 1113.8   3rd Qu.:0.91667    
    ##  Max.   :22500.0        Max.   :47137.2   Max.   :1.00000    
    ##                                                              
    ##  ONEOFF_PURCHASES_FREQUENCY PURCHASES_INSTALLMENTS_FREQUENCY
    ##  Min.   :0.00000            Min.   :0.0000                  
    ##  1st Qu.:0.00000            1st Qu.:0.0000                  
    ##  Median :0.08333            Median :0.1667                  
    ##  Mean   :0.20246            Mean   :0.3644                  
    ##  3rd Qu.:0.30000            3rd Qu.:0.7500                  
    ##  Max.   :1.00000            Max.   :1.0000                  
    ##                                                             
    ##  CASH_ADVANCE_FREQUENCY CASH_ADVANCE_TRX  PURCHASES_TRX     CREDIT_LIMIT  
    ##  Min.   :0.0000         Min.   :  0.000   Min.   :  0.00   Min.   :   50  
    ##  1st Qu.:0.0000         1st Qu.:  0.000   1st Qu.:  1.00   1st Qu.: 1600  
    ##  Median :0.0000         Median :  0.000   Median :  7.00   Median : 3000  
    ##  Mean   :0.1351         Mean   :  3.249   Mean   : 14.71   Mean   : 4494  
    ##  3rd Qu.:0.2222         3rd Qu.:  4.000   3rd Qu.: 17.00   3rd Qu.: 6500  
    ##  Max.   :1.5000         Max.   :123.000   Max.   :358.00   Max.   :30000  
    ##                                                            NA's   :1      
    ##     PAYMENTS       MINIMUM_PAYMENTS   PRC_FULL_PAYMENT     TENURE     
    ##  Min.   :    0.0   Min.   :    0.02   Min.   :0.0000   Min.   : 6.00  
    ##  1st Qu.:  383.3   1st Qu.:  169.12   1st Qu.:0.0000   1st Qu.:12.00  
    ##  Median :  856.9   Median :  312.34   Median :0.0000   Median :12.00  
    ##  Mean   : 1733.1   Mean   :  864.21   Mean   :0.1537   Mean   :11.52  
    ##  3rd Qu.: 1901.1   3rd Qu.:  825.49   3rd Qu.:0.1429   3rd Qu.:12.00  
    ##  Max.   :50721.5   Max.   :76406.21   Max.   :1.0000   Max.   :12.00  
    ##                    NA's   :313

## Unsupervised Clustering
