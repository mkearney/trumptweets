## 3.075 million followers
djt <- get_followers("realDonaldTrump", n = 3e5 * 10, retryonratelimit = TRUE)

nrow(unique(djt))
saveRDS(djt, "~/r/trumptweets/data/djt-followers-1-3million.rds")

