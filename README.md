## Download all of Donald Trump's tweets


```{r}
## install rtweet package
install.package("rtweet")

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

## run functions to download Trump's twitter archive
djt <- trumptweets()

## save as excel file
install.packages("openxlsx")
openxlsx::write.xlsx(trump, "realdonaltrump-fullarchive.xlsx")

## save as csv file
write.csv(djt, "realdonaltrump-fullarchive.csv")
```
