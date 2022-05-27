# Credit Card Clustering for Market Segmentation

![pic2_thumbnail](https://user-images.githubusercontent.com/81752452/170691826-92280d91-4877-4caa-a7c2-755830b98ca9.png)

This project is trying to cluster a big credit card dataset that has 17 variables and 9000 rows of data. Prior to clustering, the dataset was cleaned, featured-selected using R-squared for multicollinearity assessment, and transformed via standardisation. 

Principal component analysis (PCA) was used for exploratory data analysis (EDA) and found that the amount of transaction for cash in advance, credit limit, magnitude of payments paid back to the credit card company, magnitude of purchases made, and the percentage of full payment made by credit card users are important variables (or known as features) to look at in this analysis. Other variables can be important supportive information such as preference of making minimum payments and purchase frequency in oneoff-nature or installment-nature. 

5 different machine learning clustering algorithms are performed, which are Clustering for Large Application (CLARA), Hierarchical K-means Clustering Hybrid (HKmeans), Fuzzy clustering, Model-based Clustering, Density-Based Spatial Clustering for Application with Noise (DBSCAN). Analysis outcome shows that VEV model suggested by model-based clustering is the best clustering model that is able to detect 8 distinct group of credit card users, which is also the largest amount suggested compared to other algorithms. These groups are:

Cluster 1: Less active user that prefer cash-in-advance.   
Cluster 2: Less active user that prefer to use credit card to make purchases, especially installment purchases.   
Cluster 3: Revolvers who prefer to make expensive purchases.  
Cluster 4: Less active users who prefer to make full repayment back to credit card company.      	  
Cluster 5: Active card users who make expensive purchases and make repayment in big amount sometime they prefer full payment, sometime they pay minimum payments.    
Cluster 6: Max payers who prefer to pay money owe in full with zero tenure.    
Cluster 7: Less active revolvers, they spend small amount of money to purchase cheaper products.   
Cluster 8: Max Payer but this users group is more active in making purchases than cluster 6.

*Highlight*

![pic1_highlight](https://user-images.githubusercontent.com/81752452/170691850-3225f5c6-b8b6-43ec-9148-ff8024f31c8a.png)


*Reference*

Arjun Bhasin 2018, *Credit card Dataset for Clustering*, viewed 10 May 2022, <https://www.kaggle.com/datasets/arjunbhasin2013/ccdata>

Alboukadel Kassambara 2017, *Practical Guide to Cluster Analysis in R*, Multivariate Analysis 1, Edition 1, sthda.com

Clustering and dimensionality reduction techniques on the Berlin Airbnb data and the problem of mixed data (n.d.),viewed 15 May 2022 <https://rstudio-pubs-static.s3.amazonaws.com/579984_6b9efbf84ee24f00985c29e24265d2ba.html>

Jenny Listman 2019, *Customer Segmentation / K Means Clustering*, viewed 14 May 2022, <https://www.kaggle.com/code/jennylistman/customer-segmentation-k-means-clustering>

Matt.0 2019, *10 Tips for Choosing the Optimal Number of Clusters*, viewed 12 May 2022, <https://towardsdatascience.com/10-tips-for-choosing-the-optimal-number-of-clusters-277e93d72d92>
