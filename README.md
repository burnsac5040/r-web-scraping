Web Scraping with R
===================

### [Hacker News](hackernews)

The goal of this project was to scrape [Hacker News](https://news.ycombinator.com/front) and get the title of the post, external website the post links to, user who posted it, points/upvotes, time it was posted, and number of comments in an attempt to see if there are any keywords or external website that makes a post get more upvotes.  This was more so a learning experience on how to scrape data with R and was terminated in favor of a website with more information to gather.

-----------
### [Investors Hub](investorhub)

There is not much to this project.  The goal was to scrape [InvestorsHub](https://investorshub.advfn.com/) stock headlines and perform sentiment analysis on the data.  It was started at the same time as the [Hacker News](hackernews/rhacker_news.R) project where I was first learning more about the R programming language. This project was also terminated in favor of a better stock website to scrape data from.

------
### [Finviz](finviz)

- [Scraping](finviz/notebooks/fv-scrape.R): `rvest` and `httr` were used to scrape [Finviz](https://finviz.com) for news headlines about the stocks found in the S&P500.

- [Cleaning / Sentiment Analysis](finviz/notebooks/fv-plot.R): The data was cleaned and the polarity and emotion of each headline was classified.  Lastly in this file, various graphs comparing polarity and emotional scores of the headlines across dates, sectors, etc. were created.  I did find that the emotional polarity of the headlines changed a little more to the negative side after the January 6th, 2021 riots at the capitol.  There could definitely be a better place to get the data from where the stock headlines come from more places, as well as having more than the S&P500 stocks being the only headlines collected.

- [Plotting Continued](finviz/notebooks/fv-wc.R): A few more plots about the sentiment of the headlines before and after January 6th were created, as well as a wordcloud.  The project was left (for now) with stemming of words starting to be done.
