# stockcharts

Package that scrapes data from Yahoo Finance and turns it into charts, right there in RStudio.

Can also be used for getting data so you can manipulate it yourself.

Key function is stockchart() It takes 4 inputs: 

- two stock tickers (mandatory), 

- a number indicatig the number of years data you want (optional, defaults to 1), 

- either "price" or "index" (optional, defaults to price). 

You need to use the same syntax as Yahoo Finance, which means using .AX suffixes for Australian listed companies, and specific tickers for commodities, e.g. GC=F is gold.

stockcharts("BTC-USD", "GC=F", 1, "index") would return something like this:

![image](https://user-images.githubusercontent.com/87112118/176106668-9b455513-8293-4f18-875a-f22d93c47ff1.png)

The following four functions do similar things but only for one year. They offer price and index charts for 2 or 4 stocks.

pricechart2("TSLA", "BHP.AX")

indexchart2("FB", "KC=F")

pricechart4("BTC-USD", "SNAP", "CL=F", "KC=F")

indexchart4("CBA.AX", "GOOG","GME","BYND")

The next 3 functions chart the daily price changes in an asset

fallchart("^IXIC",1) #[biggest falls in Nasdaq Composite in last 1 year.]

risechart("^N225",20) #[biggest rises in Nikkei 225 in last 20 years]

changechart("^AXJO") #[all changes in S&PASX 200 in last 30 years]

Finally, the package includes a function that gets rid of the chart-making and just imports data to your machine. Takes a single stock ticker and a number of years.

getdata("FB",1)


This package requires packages tidyverse, ggtext and lubridate.
