## r

## read in functions
source("R/funs.R")

## packages
pkgs <- c("ggplot2", "dplyr", "rtweet")
## install if necessary
if (any(!pkgs %in% installed.packages())) {
  install.packages(pkgs[!pkgs %in% installed.packages()])
}
## load packages
library(ggplot2)
library(dplyr)
library(rtweet)

##---------------------------------------------------------------
## GET DATA FROM TRUMPTWITTERARCHIVE.COM
##---------------------------------------------------------------

## get data from trumptwitterarchive.com
tta <- get_trumptwitterarchive()

## extract data and tidy into data frame
tta_data <- trumptwitterarchive_data(tta)

## view time series of tweets
rtweet::ts_plot(tta_data, "weeks")

##---------------------------------------------------------------
## LOOKUP STATUSES
##---------------------------------------------------------------

## create vector of status IDs recovered from trumptwitterarchive.com
statusids <- tta$status_id

## lookup data for status IDs
tw_rt_lookup <- rtweet::lookup_statuses(statusids)

##---------------------------------------------------------------
## GET TIMELINE DATA
##---------------------------------------------------------------

## get 3200 most recent tweets from Trump's timeline
tw_rt_tmline <- rtweet::get_timeline("realdonaldtrump", n = 3200)

## combine rows and return unique
tw_rt <- rbind(tw_rt_lookup, tw_rt_tmline)
usrs <- rbind(users_data(tw_rt_lookup), users_data(tw_rt_tmline))

## remove duplicates
kp <- which(!duplicated(tw_rt$status_id))
tw_rt <- tw_rt[kp, ]
usrs <- usrs[kp, ]
attr(tw_rt, "users") <- usrs

## save withi timestamp
timestamp <- round(as.numeric(Sys.time()), 0)
rds <- paste0("data/trumptweets-", timestamp, ".rds")
saveRDS(tw_rt, rds)

save_as_csv(tw_rt, "data/trumptweets-1515775693.csv")


##---------------------------------------------------------------
## MERGE ALL UNIQUE DATA
##---------------------------------------------------------------

if (any(!tta_data$id_str %in% tw_rt$status_id)) {
  uq_tta <- matrix(
    NA,
    sum(!tta_data$id_str %in% tw_rt$status_id, na.rm = TRUE),
    ncol(tw_rt)
  )
  uq_tta <- structure(
    as.data.frame(uq_tta),
    names = names(tw_rt),
    class = c("tbl", "tbl_df", "data.frame")
  )
  ## subset unique tta rows
  uqtta <- tta_data[!tta_data$id_str %in% tw_rt$status_id, ]
  uq_tta[["status_id"]] <- uqtta$id_str
  uq_tta[["text"]] <- uqtta$text
  uq_tta[["created_at"]] <- uqtta$created_at
  uq_tta[["source"]] <- uqtta$source
  uq_tta[["retweet_count"]] <- uqtta$retweet_count
  uq_tta[["favorite_count"]] <- uqtta$favorite_count
  uq_tta[["reply_to_user_id"]] <- uqtta$in_reply_to_user_id_str
  uq_tta[["is_retweet"]]   <- uqtta$is_retweet
  tw_rt <- rbind(tw_rt, uq_tta)
}

##---------------------------------------------------------------
## GET @realDonaldTrump's USER DATA
##---------------------------------------------------------------

## check how many statuses Trump has posted
rdt <- rtweet::lookup_users("realdonaldtrump")

## print number of statuses vs number of observations
message(paste("Trump has tweeted", rdt$statuses_count, "times."))

## number of tweets data collected so far...
ttws <- length(unique(tw_rt$status_id))
message(paste("Number of Trump tweets collected so far:", ttws))
rdt$statuses_count - ttws

##---------------------------------------------------------------
## HACK(Y) METHOD FOR RECOVERING MORE TWEETS
##---------------------------------------------------------------

## h.rtweet is a package i wrote to access a backdoor API
## for obvious reasons, it's not stored on a public repository
if ("h.rtweet" %in% installed.packages()) {

  ## backdoor API pkg
  library(h.rtweet)

  ## get all tweets
  hrt <- h.search_tweets(
    "from:realdonaldtrump",
    n = rdt$statuses_count
  )

  ## convert date string to posixct
  hrt$created_at <- as.POSIXct(
    as.numeric(hrt$created_at), origin = "1970-01-01",
    tz = "UTC"
  )

  if (any(!hrt$status_id %in% tw_rt$status_id)) {
    uq_tta <- matrix(
      NA,
      sum(!hrt$status_id %in% tw_rt$status_id, na.rm = TRUE),
      ncol(tw_rt)
    )
    uq_tta <- structure(
      as.data.frame(uq_tta),
      names = names(tw_rt),
      class = c("tbl", "tbl_df", "data.frame")
    )
    ## subset unique tta rows
    uqtta <- hrt[!hrt$status_id %in% tw_rt$status_id, ]
    uq_tta[["status_id"]] <- uqtta$status_id
    uq_tta[["text"]] <- uqtta$text
    uq_tta[["created_at"]] <- uqtta$created_at
    uq_tta[["screen_name"]] <- uqtta$screen_name
    uq_tta[["mentions_screen_name"]] <- strsplit(uqtta$mentions, " ")
    tw_rt <- rbind(tw_rt, uq_tta)
  }

}

##---------------------------------------------------------------
## SAVE DATA
##---------------------------------------------------------------

## save as R data file
saveRDS(tw_rt, "data/trumptweets-08-10-2017.rds")

## function to flatten (make csv friendly) data
flatten_data <- function(x) {
  recs <- vapply(x, is.recursive, logical(1))
  x[recs] <- lapply(x[recs], vapply, paste, collapse = " ", character(1))
  x
}

## flatten and then save as CSV
tw_rt_csv <- flatten_data(tw_rt)
readr::write_csv(tw_rt_csv, "data/trumptweets-08-10-2017.csv")

##---------------------------------------------------------------
## PLOT DAILY TIME SERIES
##---------------------------------------------------------------

## daily time series data
d <- rtweet::ts_data(tw_rt, "days")

## build plot
p <- d %>%
  ggplot(aes(time, n)) +
  geom_line() +
  theme_minimal() +
  labs(
    x = NULL, y = NULL,
    title = "Daily frequency of @realDonaldTrump tweets",
    subtitle = paste0("Data (N = ",
                      nrow(tw_rt),
                      ") collected using rtweet (an R package)")
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(family = "Roboto"),
    axis.text = element_text(colour = "black")
  ) +
  scale_x_datetime(
    date_breaks = "years",
    date_labels = "%Y"
  ) +
  coord_cartesian(
    xlim = c(as.POSIXct("2009-07-03"), as.POSIXct("2017-07-09"))
  )


## save plot as PNG
png("../trumptweets.png",
    width = 7, height = 4.5, units = "in", res = 127.5)
p
dev.off()
