# crispy-disco
Team: <TBD>

Team Members:
David Langer, Pedro Pereira, Pier Lorenzo Paracchini, ...

#Recommender Systems

## Repo structure

* __data__ contains
    * __raw__ folder, it contains the original data/ files. This content is not managed by git.
    * __processed__ folder, it contains processed data/ files created during the process.
* __scripts__ folder, it contains all of the scripts created during the process. Focus on reusing
* __exploration__ folder, initial folder where to store exploration reports

## Supporting Material

* [Kaggle Competitions: How and Where to begin?](https://www.analyticsvidhya.com/blog/2015/06/start-journey-kaggle/)
* [Distributed File Systems](https://www.youtube.com/watch?v=xoA5v9AO7S0&list=PLLssT5z_DsK9JDLcT8T62VtzwyW9LNepV), Stanford University
  * Videos #41 thru #45 cover the basics of Recommenders
  * Videos #46 thru #50 cover Singular Value Decomposition (SVD) which is often used in Recommender and Information Retrieval (IR) solutions.
  * Videos #54 thru #56 cover using SVD to build better Recommenders.
* Other interesting readings
    * [Association Rules](http://www.rdatamining.com/examples/association-rules)
    * [Market Basket Analysis with R](http://www.salemmarafi.com/code/market-basket-analysis-with-r/)
    * [Market Basket Analysis more details](http://snowplowanalytics.com/guides/recipes/catalog-analytics/market-basket-analysis-identifying-products-that-sell-well-together.html)
    * [recommenderlab: Lab for Developing and Testing Recommender Algorithms](https://cran.r-project.org/web/packages/recommenderlab/index.html)

## Data Consideration

It appears to be too big to be loaded all at once in either Python (via the Pandas library) or in R (standard data frame) on machine with 4GB RAM. In general, there are a few options in these cases:

1 - Randomly Sample the Data to get a smaller representative subset (this has it own considerations I'll mention below).
2 - Get a bigger computer (usually not an option) 
3 - Rent a bigger machine from the cloud (Kagglers often use Amazon EC2 spot instances as they are quite cheap to rent).
4 - Use an R library (e.g., Revolution Analytics) specifically engineered to stream data from disk for use with Machine Learning models in R.

__1__ is by far the most common solution, and the one that has been well-studied in academic literature. However, the key is that you want a representative random sample and that usually makes things more complicated. For example, if your big data set is a binary classification problem and the split between classes is 70/30, you want your random sample to have the same 70/30 split. This is called stratification and the caret package in R provides a great function called createDataPartition() for this.

To make things more interesting, the data in this competition is time-based (e.g., there are repeated observations for the same customers over time). In these types of situations, __your sampling strategy may not be random, but leverage the top-down hypothesis of "More recent data is a better predictor" and instead use only the most recent data__.

A variation on this theme is also the idea of splitting the data down by some sort of segmentation. This leverages the top-down hypothesis of "I will train my models using different segments of the data as that leads to better predictions". For this competition, this top-down hypothesis my the take the form of splitting the data by income buckets and creating recommendations by income.

Lastly, read.csv() won't work in these situations as it loads everything into memory. Instead, you would need to use R functions that allow for reading files one line at a time, what to do with each line (e.g., ignore old data), and then write out the data you care about to the appropriate outbound file.

