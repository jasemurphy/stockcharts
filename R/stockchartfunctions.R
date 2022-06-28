pricechart2 <- function(x,y){

  timenow0 <- as.POSIXct(Sys.time())

  timethen0 <- as.POSIXct(Sys.time()-years(1))

  timenow <- round(as.numeric(timenow0))

  timethen <- round(as.numeric(timethen0))


  name1 <- read_csv(paste0("https://query1.finance.yahoo.com/v7/finance/download/",x, "?period1=", timethen, "&period2=", timenow, "&interval=1d&events=history&includeAdjustedClose=true"))

  name2 <- read_csv(paste0("https://query1.finance.yahoo.com/v7/finance/download/",y, "?period1=", timethen, "&period2=", timenow, "&interval=1d&events=history&includeAdjustedClose=true"))


  name1 <- name1 %>%
    mutate(name = x) %>%
    filter(Close != "null")

  name2 <- name2 %>%
    mutate(name = y)%>%
    filter(Close != "null")


  titlename1 <- str_remove(x, ".AX")

  titlename2 <- str_remove(y, ".AX")



  name1 %>%
    rbind(name2) %>%
    filter(!is.na(Close)) %>%
    mutate(Close = as.numeric(Close)) %>%
    select(Date, Close, name) %>%
    group_by(name) %>%
    mutate(change = (last(Close) - first(Close))/first(Close)) %>%
    mutate(plus = if_else(change>0, "+", "")) %>%
    mutate(label = if_else(Date == max(Date), paste0(plus, round(change*100), "%"), NA_character_)) %>%
    ungroup() %>%
    ggplot()+
    aes(x= Date, y= Close, colour = name)+
    geom_line(show.legend = FALSE, size = 1)+
    geom_line(show.legend = FALSE, size = 4, alpha = .2)+
    geom_text(show.legend = FALSE, aes(label = label), size  = 5.5, hjust = -.1)+
    labs(title = paste("<span style = 'color:#FF0000'>", titlename1, "</span>and<span style = 'color:#0000FF'>",titlename2,"</span>over the last year."),
         subtitle = "Stock prices daily, data: Yahoo Finance")+
    scale_color_manual(breaks = c(x,y), values = c("#FF0000", "#0000FF"))+
    theme(plot.title = element_markdown(size = 15),
          axis.title = element_blank(),
          axis.text = element_text(colour = "black", size = 10),
          panel.background = element_rect("white"),
          panel.grid.major = element_line(size = .05, colour = "grey80"))+
    scale_x_date(limits = (c(as.Date(timethen0), as.Date(timenow0)+months(1))),
                 breaks = as.Date(c(timethen0, timenow0-months(6), timenow0)),
                 labels = format(as.Date(c(timethen0, timenow0-months(6), timenow0)), "%d-%b-%y"))+
    scale_y_continuous(labels = scales::dollar)
}


indexchart2 <- function(x,y){


  timenow0 <- as.POSIXct(Sys.time())

  timethen0 <- as.POSIXct(Sys.time()-lubridate::years(1))

  timenow <- round(as.numeric(timenow0))

  timethen <- round(as.numeric(timethen0))


  name1 <- read_csv(paste0("https://query1.finance.yahoo.com/v7/finance/download/",x, "?period1=", timethen, "&period2=", timenow, "&interval=1d&events=history&includeAdjustedClose=true"))

  name2 <- read_csv(paste0("https://query1.finance.yahoo.com/v7/finance/download/",y, "?period1=", timethen, "&period2=", timenow, "&interval=1d&events=history&includeAdjustedClose=true"))


  name1 <- name1 %>%
    mutate(name = x) %>%
    filter(Close != "null")

  name2 <- name2 %>%
    mutate(name = y)%>%
    filter(Close != "null")

  titlename1 <- str_remove(x, ".AX")

  titlename2 <- str_remove(y, ".AX")


  name1 %>%
    rbind(name2) %>%
    filter(!is.na(Close)) %>%
    mutate(Close = as.numeric(Close)) %>%
    select(Date, Close, name) %>%
    group_by(name) %>%
    mutate(change = (last(Close) - first(Close))/first(Close)) %>%
    mutate(plus = if_else(change>0, "+", "")) %>%
    mutate(indexedvalue = 100 * Close / first(Close)) %>%
    mutate(label = if_else(Date == max(Date), paste0(plus, round(change*100), "%"), NA_character_)) %>%
    ungroup() %>%
    ggplot()+
    aes(x= Date, y= indexedvalue, colour = name)+
    geom_line(show.legend = FALSE, size = 1)+
    geom_line(show.legend = FALSE, size = 4, alpha = .2)+
    geom_text(show.legend = FALSE, aes(label = label), size  = 5.5, hjust = -.1)+
    labs(title = paste("Change in<span style = 'color:#FF0000'>", titlename1, "</span>and<span style = 'color:#0000FF'>",titlename2,"</span>over the last year."),
         subtitle = paste("Change in prices daily.\nIndexed such that on", format(as.Date(timethen0), "%d %b '%y"), "both prices are set equal to the arbitrary value of 100. \nIndexing permits easy comparison of changes since. Data: Yahoo Finance"))+
    scale_color_manual(breaks = c(x,y), values = c("#FF0000", "#0000FF"))+
    theme(plot.title = element_markdown(size = 15),
          axis.title = element_blank(),
          axis.text = element_text(colour = "black", size = 10),
          panel.background = element_rect("white"),
          panel.grid.major = element_line(size = .05, colour = "grey80"))+
    scale_x_date(limits = (c(as.Date(timethen0), as.Date(timenow0)+months(1))),
                 breaks = as.Date(c(timethen0, timenow0-months(6), timenow0)),
                 labels = format(as.Date(c(timethen0, timenow0-months(6), timenow0)), "%d %b '%y"))
}


indexchart4 <- function(a,b,c,d){

  timenow0 <- as.POSIXct(Sys.time())

  timethen0 <- as.POSIXct(Sys.time()-years(1))

  timenow <- round(as.numeric(timenow0))

  timethen <- round(as.numeric(timethen0))


  name1 <- read_csv(paste0("https://query1.finance.yahoo.com/v7/finance/download/",a, "?period1=", timethen, "&period2=", timenow, "&interval=1d&events=history&includeAdjustedClose=true"))

  name2 <- read_csv(paste0("https://query1.finance.yahoo.com/v7/finance/download/",b, "?period1=", timethen, "&period2=", timenow, "&interval=1d&events=history&includeAdjustedClose=true"))

  name3 <- read_csv(paste0("https://query1.finance.yahoo.com/v7/finance/download/",c, "?period1=", timethen, "&period2=", timenow, "&interval=1d&events=history&includeAdjustedClose=true"))

  name4 <- read_csv(paste0("https://query1.finance.yahoo.com/v7/finance/download/",d, "?period1=", timethen, "&period2=", timenow, "&interval=1d&events=history&includeAdjustedClose=true"))


  name1 <- name1 %>%
    mutate(name = a) %>%
    filter(Close != "null")

  name2 <- name2 %>%
    mutate(name = b)%>%
    filter(Close != "null")

  name3 <- name3 %>%
    mutate(name = c) %>%
    filter(Close != "null")

  name4 <- name4 %>%
    mutate(name = d)%>%
    filter(Close != "null")

  titlename1 <- str_remove(a, ".AX")

  titlename2 <- str_remove(b, ".AX")

  titlename3 <- str_remove(c, ".AX")

  titlename4 <- str_remove(d, ".AX")


  name1 %>%
    rbind(name2) %>%
    rbind(name3) %>%
    rbind(name4) %>%
    filter(!is.na(Close)) %>%
    mutate(Close = as.numeric(Close)) %>%
    select(Date, Close, name) %>%
    group_by(name) %>%
    mutate(change = (last(Close) - first(Close))/first(Close)) %>%
    mutate(plus = if_else(change>0, "+", "")) %>%
    mutate(indexedvalue = 100 * Close / first(Close)) %>%
    mutate(label = if_else(Date == max(Date), paste0(plus, round(change*100), "%"), NA_character_)) %>%
    ungroup() %>%
    mutate(order = fct_reorder2(name, Date, Close)) %>%
    ggplot()+
    aes(x= Date, y= indexedvalue, colour = order)+
    geom_line(show.legend = FALSE, size = 1)+
    geom_line(show.legend = FALSE, size = 4, alpha = .2)+
    geom_text(show.legend = FALSE, aes(label = label), size  = 4, hjust = -.1)+
    labs(title = paste("Change in<span style = 'color:#FF0000'>",
                       titlename1,
                       "</span>,<span style = 'color:#0000FF'>",
                       titlename2,
                       "</span>,<span style = 'color:#FF985E'>",
                       titlename3,
                       "</span>and<span style = 'color:#998F85'>",
                       titlename4,
                       "</span>over the last year."),
         subtitle = paste("Change in prices daily, Indexed such that", format(as.Date(timethen0), "%d %b '%y"), "=100. Data: Yahoo Finance"))+
    scale_color_manual(breaks = c(a,b,c,d), values = c("#FF0000", "#0000FF", "#FF985E", "#998F85" ))+
    theme(plot.title = element_markdown(size = 15),
          axis.title = element_blank(),
          axis.text = element_text(colour = "black", size = 10),
          panel.background = element_rect("white"),
          panel.grid = element_line(size =.1, colour = "grey60"))+
    scale_x_date(limits = (c(as.Date(timethen0), as.Date(timenow0)+months(1))),
                 breaks = as.Date(c(timethen0, timenow0-months(6), timenow0)),
                 labels = format(as.Date(c(timethen0, timenow0-months(6), timenow0)), "%d %b '%y"))+
    facet_wrap(~order, ncol=2)
}

pricechart4 <- function(a,b,c,d){

  timenow0 <- as.POSIXct(Sys.time())

  timethen0 <- as.POSIXct(Sys.time()-years(1))

  timenow <- round(as.numeric(timenow0))

  timethen <- round(as.numeric(timethen0))


  name1 <- read_csv(paste0("https://query1.finance.yahoo.com/v7/finance/download/",a, "?period1=", timethen, "&period2=", timenow, "&interval=1d&events=history&includeAdjustedClose=true"))

  name2 <- read_csv(paste0("https://query1.finance.yahoo.com/v7/finance/download/",b, "?period1=", timethen, "&period2=", timenow, "&interval=1d&events=history&includeAdjustedClose=true"))

  name3 <- read_csv(paste0("https://query1.finance.yahoo.com/v7/finance/download/",c, "?period1=", timethen, "&period2=", timenow, "&interval=1d&events=history&includeAdjustedClose=true"))

  name4 <- read_csv(paste0("https://query1.finance.yahoo.com/v7/finance/download/",d, "?period1=", timethen, "&period2=", timenow, "&interval=1d&events=history&includeAdjustedClose=true"))


  name1 <- name1 %>%
    mutate(name = a) %>%
    filter(Close != "null")

  name2 <- name2 %>%
    mutate(name = b)%>%
    filter(Close != "null")

  name3 <- name3 %>%
    mutate(name = c) %>%
    filter(Close != "null")

  name4 <- name4 %>%
    mutate(name = d)%>%
    filter(Close != "null")

  titlename1 <- str_remove(a, ".AX")

  titlename2 <- str_remove(b, ".AX")

  titlename3 <- str_remove(c, ".AX")

  titlename4 <- str_remove(d, ".AX")


  name1 %>%
    rbind(name2) %>%
    rbind(name3) %>%
    rbind(name4) %>%
    filter(!is.na(Close)) %>%
    mutate(Close = as.numeric(Close)) %>%
    select(Date, Close, name) %>%
    group_by(name) %>%
    mutate(change = (last(Close) - first(Close))/first(Close)) %>%
    mutate(plus = if_else(change>0, "+", "")) %>%
    mutate(label = if_else(Date == max(Date), paste0(plus, round(change*100), "%"), NA_character_)) %>%
    ungroup() %>%
    mutate(order = fct_reorder2(name, Date, change)) %>%
    ggplot()+
    aes(x= Date, y= Close, colour = order)+
    geom_line(show.legend = FALSE, size = 1)+
    geom_line(show.legend = FALSE, size = 4, alpha = .2)+
    geom_text(show.legend = FALSE, aes(label = label), size  = 4, hjust = -.1)+
    labs(title = paste("Prices of<span style = 'color:#FF0000'>",
                       titlename1,
                       "</span>,<span style = 'color:#0000FF'>",
                       titlename2,
                       "</span>,<span style = 'color:#FF985E'>",
                       titlename3,
                       "</span>and<span style = 'color:#998F85'>",
                       titlename4,
                       "</span>over the last year."),
         subtitle = paste("Prices daily. Data: Yahoo Finance"))+
    scale_color_manual(breaks = c(a,b,c,d), values = c("#FF0000", "#0000FF", "#FF985E", "#998F85" ))+
    theme(plot.title = element_markdown(size = 15),
          axis.title = element_blank(),
          axis.text = element_text(colour = "black", size = 10),
          panel.background = element_rect("white"),
          panel.grid = element_line(size =.1, colour = "grey60"))+
    scale_y_continuous(labels = scales::dollar)+
    scale_x_date(limits = (c(as.Date(timethen0), as.Date(timenow0)+months(1))),
                 breaks = as.Date(c(timethen0, timenow0-months(6), timenow0)),
                 labels = format(as.Date(c(timethen0, timenow0-months(6), timenow0)), "%d %b '%y"))+
    facet_wrap(~order, ncol=2, scales = "free_y")
}

fallchart <- function(x,years=30){

  timenow0 <- as.POSIXct(Sys.time())
  timethen0 <- as.POSIXct(Sys.time()-lubridate::years(years))

  timenow <- round(as.numeric(timenow0))
  timethen <- round(as.numeric(timethen0))

  name1 <- read_csv(paste0("https://query1.finance.yahoo.com/v7/finance/download/",x, "?period1=", timethen, "&period2=", timenow, "&interval=1d&events=history&includeAdjustedClose=true"))

  name1 <- name1 %>%
    mutate(name = x) %>%
    filter(Close != "null")

  titlename1 <- str_remove(x, ".AX")

  name1 %>%
    filter(!is.na(Close)) %>%
    mutate(Close = as.numeric(Close)) %>%
    select(Date, Close, name) %>%
    mutate(change = 1-(Close / lag(Close))) %>%
    arrange(change) %>%
    mutate(id = row_number()) %>%
    filter(id<20) %>%
    mutate(date  = as.factor(Date)) %>%
    mutate(order = fct_reorder(date, change)) %>%
    ggplot()+
    aes(x= order, y= change, fill = Date)+
    geom_col(show.legend = FALSE)+

    geom_text(aes(label = date), angle = 90, colour = "snow1", hjust = -.1, size = 7, show.legend = FALSE)+
    labs(title = paste("Twenty biggest one day falls in", x, "in the last",years, "years" ),
         subtitle = "Calculated from daily prices, data: Yahoo Finance")+
    theme(plot.title = element_markdown(size = 20),
          axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(colour = "black", size = 12),
          panel.background = element_rect("white"),
          panel.grid.major.y = element_line(size = .1, colour = "black"))+
    scale_fill_gradientn(colours = c("black", "dark green"))+
    scale_y_continuous(labels = scales::percent)
}


risechart <- function(x,years=30){

  timenow0 <- as.POSIXct(Sys.time())
  timethen0 <- as.POSIXct(Sys.time()-lubridate::years(years))

  timenow <- round(as.numeric(timenow0))
  timethen <- round(as.numeric(timethen0))

  name1 <- read_csv(paste0("https://query1.finance.yahoo.com/v7/finance/download/",x, "?period1=", timethen, "&period2=", timenow, "&interval=1d&events=history&includeAdjustedClose=true"))

  name1 <- name1 %>%
    mutate(name = x) %>%
    filter(Close != "null")

  titlename1 <- str_remove(x, ".AX")

  name1 %>%
    filter(!is.na(Close)) %>%
    mutate(Close = as.numeric(Close)) %>%
    select(Date, Close, name) %>%
    mutate(change = 1-(Close / lag(Close))) %>%
    arrange(desc(change)) %>%
    mutate(id = row_number()) %>%
    filter(id<20) %>%
    mutate(date  = as.factor(Date)) %>%
    mutate(order = fct_reorder(date, change)) %>%
    ggplot()+
    aes(x= order, y= change, fill = Date)+
    geom_col(show.legend = FALSE)+
    geom_text(aes(label = date), angle = 90, colour = "snow1", hjust = 1.1, size = 5, show.legend = FALSE)+
    labs(title = paste("The twenty biggest one day rises in", titlename1),
         subtitle = paste("Calculated from", titlename1, "value daily from the last", years, "years, data: Yahoo Finance"))+
    theme(plot.title = element_markdown(size = 20),
          axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(colour = "black", size = 12),
          panel.background = element_rect("white"),
          panel.grid.major.y = element_line(size = .1, colour = "grey70"))+
    scale_fill_gradientn(colours = c("black","brown", "firebrick", "red", "orange", "gold"))+
    scale_y_continuous(labels = scales::percent)

}




changechart <- function(x){

  timenow0 <- as.POSIXct(Sys.time())
  timethen0 <- as.POSIXct(Sys.time()-lubridate::years(30))

  timenow <- round(as.numeric(timenow0))
  timethen <- round(as.numeric(timethen0))

  name1 <- read_csv(paste0("https://query1.finance.yahoo.com/v7/finance/download/",x, "?period1=", timethen, "&period2=", timenow, "&interval=1d&events=history&includeAdjustedClose=true"))

  name1 <- name1 %>%
    mutate(name = x) %>%
    filter(Close != "null")

  titlename1 <- str_remove(x, ".AX")

  name1 %>%
    filter(!is.na(Close)) %>%
    mutate(Close = as.numeric(Close)) %>%
    select(Date, Close, name) %>%
    mutate(change = 1-(Close / lag(Close))) %>%     #today close 101. yesterday close 100. close /lag close = 1.01. 1- 1.01 = .01, 1%. this is right.
    mutate(date  = as.factor(Date)) %>%
    mutate(order = fct_reorder(date, change)) %>%
    ggplot()+
    aes(x= order, y= change, fill = Date)+
    geom_col(show.legend = FALSE)+
    labs(title = paste("Daily changes in", titlename1, "from biggest fall to biggest rise "),
         subtitle = "Index value daily from the last 30 years, data: Yahoo Finance")+
    theme(plot.title = element_markdown(size = 20),
          axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(colour = "black", size = 15),
          panel.background = element_rect("white"),
          panel.grid.major.y = element_line(size = .1, colour = "black"))+
    scale_fill_gradientn(colours = c("black","brown", "firebrick", "red", "orange", "gold"))+
    scale_y_continuous(labels = scales::percent)
}


getdata <- function(x,years=1){

  timenow0 <- as.POSIXct(Sys.time())
  timethen0 <- as.POSIXct(Sys.time()-lubridate::years(years))

  timenow <- round(as.numeric(timenow0))
  timethen <- round(as.numeric(timethen0))

  name1 <- read_csv(paste0("https://query1.finance.yahoo.com/v7/finance/download/",x, "?period1=", timethen, "&period2=", timenow, "&interval=1d&events=history&includeAdjustedClose=true"))

  name1 <- name1 %>%
    mutate(Name = str_remove(x, ".AX")) %>%
    filter(Close != "null")

  name1 %>%
    filter(!is.na(Close)) %>%
    mutate(Close = as.numeric(Close)) %>%
    rename(Adj_Close = `Adj Close`)
}



stockchart <- function(x,y, years=1, type=c("price", "index")){

  timenow0 <- as.POSIXct(Sys.time())
  timethen0 <- as.POSIXct(Sys.time()-lubridate::years(years))

  timenow <- round(as.numeric(timenow0))
  timethen <- round(as.numeric(timethen0))

  name1 <- read_csv(paste0("https://query1.finance.yahoo.com/v7/finance/download/",x, "?period1=", timethen, "&period2=", timenow, "&interval=1d&events=history&includeAdjustedClose=true"))
  name2 <- read_csv(paste0("https://query1.finance.yahoo.com/v7/finance/download/",y, "?period1=", timethen, "&period2=", timenow, "&interval=1d&events=history&includeAdjustedClose=true"))

  name1 <- name1 %>%
    mutate(name = x) %>%
    filter(Close != "null")

  name2 <- name2 %>%
    mutate(name = y)%>%
    filter(Close != "null")

  titlename1 <- str_remove(x, ".AX")
  titlename2 <- str_remove(y, ".AX")

  if(type == "price"){
    return(

      name1 %>%
        rbind(name2) %>%
        filter(!is.na(Close)) %>%
        mutate(Close = as.numeric(Close)) %>%
        select(Date, Close, name) %>%
        group_by(name) %>%
        mutate(change = (last(Close) - first(Close))/first(Close)) %>%
        mutate(plus = if_else(change>0, "+", "")) %>%
        mutate(label = if_else(Date == max(Date), paste0(plus, round(change*100), "%"), NA_character_)) %>%
        ungroup() %>%
        ggplot()+
        aes(x= Date, y= Close, colour = name)+
        geom_line(show.legend = FALSE, size = 1)+
        geom_line(show.legend = FALSE, size = 4, alpha = .2)+
        geom_text(show.legend = FALSE, aes(label = label), size  = 5, hjust = -.1)+
        labs(title = paste("Change in<span style = 'color:#FF0000'>", titlename1, "</span>and<span style = 'color:#0000FF'>",titlename2,"</span>since",format(as.Date(timethen0), "%d %B %Y")),
             subtitle = paste("Prices daily.Data: Yahoo Finance"))+
        scale_color_manual(breaks = c(x,y), values = c("#FF0000", "#0000FF"))+
        theme(plot.title = element_markdown(size = 15),
              axis.title = element_blank(),
              axis.text = element_text(colour = "black", size = 10),
              panel.background = element_rect("white"),
              panel.grid.major = element_line(size = .08, colour = "grey70"))+
        scale_y_continuous(labels = scales::dollar)+
        scale_x_date(limits = (c(as.Date(timethen0), as.Date(timenow0)+months(years*1))),
                     breaks = as.Date(c(timethen0, timenow0-months(years*6), timenow0)),
                     labels = format(as.Date(c(timethen0, timenow0-months(years*6), timenow0)), "%d %b '%y"))

    )
  }
  else{

    return(
      name1 %>%
        rbind(name2) %>%
        filter(!is.na(Close)) %>%
        mutate(Close = as.numeric(Close)) %>%
        select(Date, Close, name) %>%
        group_by(name) %>%
        mutate(change = (last(Close) - first(Close))/first(Close)) %>%
        mutate(plus = if_else(change>0, "+", "")) %>%
        mutate(indexedvalue = 100 * Close / first(Close)) %>%
        mutate(label = if_else(Date == max(Date), paste0(plus, round(change*100), "%"), NA_character_)) %>%
        ungroup() %>%
        ggplot()+
        aes(x= Date, y= indexedvalue, colour = name)+
        geom_line(show.legend = FALSE, size = 1)+
        geom_line(show.legend = FALSE, size = 4, alpha = .2)+
        geom_text(show.legend = FALSE, aes(label = label), size  = 6, hjust = -.1)+
        labs(title = paste("Change in<span style = 'color:#FF0000'>", titlename1, "</span>and<span style = 'color:#0000FF'>",titlename2,"</span>since",format(as.Date(timethen0), "%d %B %Y")),
             subtitle = paste("Change in prices daily.\nIndexed such that on", format(as.Date(timethen0), "%d %b '%y"), "both prices are set equal to the arbitrary value of 100. \nIndexing permits easy comparison of changes since. Data: Yahoo Finance"))+
        scale_color_manual(breaks = c(x,y), values = c("#FF0000", "#0000FF"))+
        theme(plot.title = element_markdown(size = 15),
              axis.title = element_blank(),
              axis.text = element_text(colour = "black", size = 10),
              panel.background = element_rect("white"),
              panel.grid.major = element_line(size = .08, colour = "grey70"))+
        scale_x_date(limits = (c(as.Date(timethen0), as.Date(timenow0)+months(years*1))),
                     breaks = as.Date(c(timethen0, timenow0-months(years*6), timenow0)),
                     labels = format(as.Date(c(timethen0, timenow0-months(years*6), timenow0)), "%d %b '%y"))
    )
  }


}

