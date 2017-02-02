Download all of Donald Trump's tweets using R
---------------------------------------------

1.  Install and then load rtweet. \`

``` r
## install rtweet package
install.packages("rtweet")

## load rtweet
library(rtweet)
```

1.  Read in the following 3 functions. You'll use the last function, `trumptweets()` to download the data

``` r
## function to scrape IDs
.trumpids <- function(year) {
    ## build url
    url <- paste0("http://trumptwitterarchive.com/",
                  "data/realdonaldtrump/", year, ".json")
    ## return ids
    jsonlite::fromJSON(url)[["id_str"]]
}
## function to download status ids
trumpids <- function(trumptwitterarchive = TRUE) {
    ## scrape from trumptwitterarchive.com
    if (trumptwitterarchive) {
        ids <- c(2009:2017) %>%
            lapply(.trumpids) %>%
            unlist(use.names = FALSE)
    } else {
        ## or from my github page (note: this one is unlikely to
        ## be updated as frequently as trumptwitterarchive)
        ids <- paste0(
            "https://github.com/mkearney/trumptweets/blob/",
            "master/data/realdonaldtrump-ids-2009-2017.csv") %>%
            read.csv(stringsAsFactors = FALSE) %>%
            unlist(use.names = FALSE)
    }
    ## return ids
    ids
}
## function to download twitter data
trumptweets <- function() {
    ## get archive of status ids
    ids <- trumpids()
    ## get newest trump tweets (set to 1000 to be safe)
    rt1 <- get_timeline(
        "realdonaldtrump", n = 1000,
        since_id = ids[length(ids)])
    ## download archive
    message("    Downloading ", length(ids), " tweets...")
    rt2 <- lookup_statuses(ids[1:16000])
    message("    You're halfway there...")
    rt3 <- lookup_statuses(ids[16001:(length(ids))])
    message("    Huzzah!!!")
    ## combine data into list
    rt <- list(rt1, rt2, rt3)
    ## collapse into data frame (or salvage list if error)
    tryCatch(do.call("rbind", rt),
             error = function(e) return(rt))
}
```

1.  Download all of Trump's tweets.

``` r
## run function to download Trump's twitter archive
djt <- trumptweets()
```

1.  Save the data file.

``` r
## To save as an excel file:
install.packages("openxlsx")
openxlsx::write.xlsx(djt, "realdonaltrump-fullarchive.xlsx")

## To save as csv file
write.csv(djt, "realdonaltrump-fullarchive.csv",
          row.names = FALSE)

## To preserve meta information and save as csv file
install.packages("readr")
readr::write_csv(djt, "realdonaltrump-fullarchive.csv")
```

Inspecting the data
-------------------

``` r
## preview data
head(djt)

## check 100 most popular hashtags
djt$hashtags %>%
    strsplit(" ") %>%
    unlist(use.names = FALSE) %>%
    tolower %>%
    table() %>%
    sort(decreasing = TRUE) %>%
    head(100)

## check 100 most popular mentions
djt$mentions_screen_name %>%
    strsplit(" ") %>%
    unlist(use.names = FALSE) %>%
    tolower %>%
    table() %>%
    sort(decreasing = TRUE) %>%
    head(100)

## check text of 50 most recent tweets
djt$text[1:50]
```

Plotting the data
-----------------

``` r
## use the built in rtweet function
ts_plot(p, theme = "nerdy")

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

## you can continue plotting with rtweet functions but
## the current version (0.4.0) prints incorrect labels for
## the x-axis for multi-year plots.
ts_plot(p, theme = "spacegray")

## ggplot2 doesn't have that problem and is more robust and
## flexible anyway
## install and load ggplot2
install.packages("ggplot2")
library(ggplot2)

## uncomment following line and final line to save image
## png("trumptweets.png", 7, 5, "in", res = 127.5)
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
## dev.off()

## image I created using this code displayed below

## note: if Avenir Next Condensed will only work if currently
## installed on your machine. If that's the case, then either
## delete the family arguments or replace Avenir with the font
## of your choosing
```

<p align="center">
<img src="trumptweets.png" alt="trumphashtags">
</p>
