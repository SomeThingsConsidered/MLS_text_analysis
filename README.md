# MLS_text_analysis
R code for Insights blog posts related to realtor comments contained in Multiple Listing Service (MLS) data

check_corpus_entries.R is code for finding a specific word in the set of listing agent remarks, and printing out the full listing containing that word

hedonic_regression_1.R creates a simple document term matrix based on the most common words contained in listing agent remarks, and uses this information as well as number bedrooms, number bathrooms and living square feet to run a hedonic regression using LASSO.  The words having the most positive and negative coefficients are plotting in a word cloud.
