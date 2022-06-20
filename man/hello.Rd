\name{stockcharts}
\alias{stockharts}
\title{stockcharts}
\usage{
pricechart2(x,y)
indexchart2(x,y)
pricechart4(a,b,c,d)
indexchart4(a,b,c,d)
stockchart(x,y,years , type = c("Price", "index"))

fallchart(x,years)
risechart(x,years)
changechart(x)

getdata(x,years)

}
\description{
The package includes nine functions that access data from Yahoo Finance. Five of the functions produces line charts of daily prices of either 2 or 4 assets. Two of these functions produce only price charts; another two  produce charts where price is indexed to 100 at a date a year ago. Those four simple functions give you a year of data. The stockchart function accepts two stock tickers as inputs and allows you to input how many years of data you want and to specify whether you want  a price chart or an index chart.

The first inputs to each of the function need to be stock tickers, matching those used by Yahoo Finance, e.g. "AMZN". In case of non-US stock markets you need suffixes, e.g. "BHP.AX" "SHEL.L" "SAP.DE". You can also get commodities and cryptocurrencies, e.g. "BZ=F" is crude oil futures. "KC=F" is coffee futures, "BTC-AUD" is Bitcoin in Australian dollars.

The function fallchart shows the 20 biggest one-day falls for a single index or asset in a specified period. risechart does the same for the 20 biggest one-day rises. changechart shows all daily rises and falls in the last 30 years from biggest fall to highest rise.

getdata is a function that retrieves stock price and volume data from Yahoo without plotting it.
}
\examples{
pricechart2("TSLA", "BHP.AX")
indexchart2("FB", "KC=F")
pricechart4("BTC-USD", "SNAP", "CL=F", "KC=F")
indexchart4("CBA.AX", "GOOG","GME","BYND")
stockchart("AZN","MRNA", 3, "index")

fallchart("^IXIC",1) #[biggest falls in Nasdaq Composite in last 1 year.]
risechart("^N225",20) #[biggest rises in Nikkei 225 in last 20 years]
changechart("^AXJO") #[all changes in S&PASX 200 in last 30 years]

getdata("FB",1)

}
