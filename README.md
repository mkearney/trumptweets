## Download all of Donald Trump's tweets


```{r}
## install rtweet package
install.package("rtweet")

## load rtweet
library(rtweet)

## function to download status ids
.trumptweets <- function(year) {
    url <- paste0(
        "http://trumptwitterarchive.com/data/realdonaldtrump/",
        year, ".json")
    r <- jsonlite::fromJSON(url)
    r[["id_str"]]
}
## function to download twitter data
trumptweets <- function() {
    years <- c(2009:2017)
    ids <- years %>%
        lapply(.trumptweets) %>%
        unlist(use.names = FALSE)
    message("downloading ", length(ids), " tweets...")
    rt1 <- lookup_statuses(ids[1:16000])
    message("halfway there...")
    rt2 <- lookup_statuses(ids[16001:(length(ids))])
    message("huzzah!!!")
    rbind(rt1, rt2)
}

## check out 100 most popular hashtags
djt$hashtags %>%
    strsplit(" ") %>%
    unlist(use.names = FALSE) %>%
    tolower %>%
    table() %>%
    sort(decreasing = TRUE) %>%
    head(100)

## save as excel file
install.packages("openxlsx")
openxlsx::write.xlsx(djt, "realdonaltrump-fullarchive.xlsx")

## save as csv file
write.csv(djt, "realdonaltrump-fullarchive.csv")

## run functions to download Trump's twitter archive
djt <- trumptweets()

## plot four groups of hashtags
p <- ts_filter(djt, "2 days", txt = "hashtags",
               filter = c("makeamericagreatagain|maga",
                          "trump",
                          "debate",
                          "draintheswamp|americafirst"),
               key = c("MakeAmericaGreatAgain",
                       "Trump",
                       "Debates",
                       "DrainTheSwamp/AmericaFirst"))
##library(ggplot2)
png("trumptweets.png", 7, 5, "in", res = 127.5)
p %>%
    ggplot(aes(x = time, y = freq, color = filter)) +
    theme_bw() +
    geom_line() +
    facet_wrap( ~ filter, ncol = 2) +
    labs(x = "", y = "",
         title = "Hashtags used by Donald Trump",
         subtitle = "Used entire archive of @realDonaldTrumpTweets") +
    theme(legend.position = "none",
          text = element_text(size = 12,
                              family = "Avenir Next Condensed"),
          plot.title = element_text(
              family = "Avenir Next Condensed Medium", size = 20))
dev.off()
```

<p align="center">
<img src="trumptweets.png.png" alt="creating">
</p>
