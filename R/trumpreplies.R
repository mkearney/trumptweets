## load rtweet and tidyverse
library(rtweet)
library(tidyverse)

## read data
replies <- readRDS("trump-replies.rds")
rsample <- readRDS("random-sample.rds")

## get tokens / initalize output vector
tokens <- get_tokens()
replies_new <- vector("list", 3L)
rsample_new <- vector("list", 3L)

## read in functions
source("timezones.R")

## initalize counter
j <- 1L

##----------------------------------------------------------------------------##
##                                replies_data                                ##
##----------------------------------------------------------------------------##

## for loop
for (i in seq_along(replies_new)) {
  if (i == 1L) {
    replies_new[[i]] <- search_tweets(
      "to:realDonaldTrump",
      max_id = max_id(replies),
      n = 18000,
      verbose = FALSE,
      token = tokens[[j]])
  } else {
    replies_new[[i]] <- search_tweets(
      "to:realDonaldTrump",
      max_id = max_id(replies_new[[i - 1L]]),
      n = 18000,
      verbose = FALSE,
      token = tokens[[j]])
  }
  print(i)
  if (i == length(replies_new)) break
  j <- update_rl("search_tweets", token = j, n = 18000 / 100)
}

## collapse into single df
replies_new <- do_call_rbind(replies_new)
replies <- rbind(replies, join_rtweet(replies_new))

## initalize counter
j <- 3L

##----------------------------------------------------------------------------##
##                                rsample_data                                ##
##----------------------------------------------------------------------------##

## for loop
for (i in seq_along(rsample_new)) {
  if (i == 1L) {
    rsample_new[[i]] <- search_tweets(
      "to:realDonaldTrump",
      max_id = max_id(rsample),
      n = 18000,
      verbose = FALSE,
      token = tokens[[j]])
  } else {
    rsample_new[[i]] <- search_tweets(
      "to:realDonaldTrump",
      max_id = max_id(rsample_new[[i - 1L]]),
      n = 18000,
      verbose = FALSE,
      token = tokens[[j]])
  }
  print(i)
  if (i == length(rsample_new)) break
  #j <- update_rl("search_tweets", token = j, n = 18000 / 100)
  j <- j + 1L
}

## collapse into single df
rsample_new <- do_call_rbind(rsample_new)
rsample_new$sample <- "Random (lang:en)"
rsample <- rbind(rsample, join_rtweet(rsample_new))

## save data sets
saveRDS(replies, "replies.rds")
saveRDS(rsample, "rsample.rds")

## merge into final data
rsample$sample <- "Random (lang:en)"
replies$sample <- "Reply to Trump"

## create data frame of users data (one obs per user)
rsample_users <- rsample[!duplicated(rsample$user_id), ]

## create data frame of users data (one obs per user)
replies_users <- replies[!duplicated(replies$user_id), ]


## final data set -- all data
rt <- rbind(rsample, replies)

saveRDS(rt, "rt.rds")
rt_users <- rbind(rsample_users, replies_users)

## summarise users data
rt_users_ <- rt_users %>%
  filter(
    !is_retweet & !is_quote &
      account_created_at > "2006-12-31") %>%
  mutate(
    date = substr(account_created_at, 1, 8) + "01",
    date = as.Date(date),
    days = as.numeric(difftime(cst_time(), date), "days"),
    statuses = (statuses_count + .1) / (days + 1.1)) %>%
  group_by(date, sample)

kp <- sample(
  rt_users_$user_id[rt_users_$sample == "Random (lang:en)"],
  sum(rt_users_$sample == "Reply to Trump"))

rt_users_ <- rt_users_ %>%
  filter((sample == "Random (lang:en)" & user_id %in% kp)
         | sample == "Reply to Trump")

rt_users_summary <- rt_users_ %>%
  summarise(
    n = n(),
    statuses = mean(statuses, na.rm = TRUE)) %>%
  ungroup()

tabsort(rt_users_$sample)

## subtitle text
subtitle <- "Number of accounts created by month among users identified via " +
  "'to:realDonaldTrump' (n = 75k) and " + "'lang:en' (n = 75k)"

## plot
rt_users_summary %>%
  filter(date > "2007-12-31") %>%
  ggplot(aes(x = date, y = n)) +
  geom_ribbon(
    aes(fill = factor(sample, levels = c("Reply to Trump", "Random (lang:en)")),
                      ymin = 0, ymax = n),
    colour =  "transparent", alpha = .5) +
  scale_x_date(
    expand = c(.015, .0), date_breaks = "years", date_label = "%Y") +
  scale_y_continuous(expand = c(0,0)) +
  theme_mwk(
    base_size = 14,
    base_family = "Roboto Condensed") +
  theme(
    axis.text = element_text(size = rel(.6)),
    legend.position = c("bottom"),
    plot.caption = element_text(
      colour = "gray20", hjust = -.05, vjust = 9.75, size = rel(.55)),
    legend.justification = c(1, 1),
    plot.margin = margin(.35, .65, -1, .35, unit = "line"),
    plot.subtitle = element_text(size = rel(.7), vjust = 1.5)) +
  labs(y = NULL, x = NULL,
       caption = "Data collected from Twitter's search (REST) API using rtweet",
       title = "Account creation dates of users replying to Trump versus random users",
       subtitle = subtitle) +
  scale_fill_manual(values = c("dodgerblue4", "red3")) +
  ggsave("comparing.png")

sum(replies_users$user_id %in% rsample_users$user_id) / length(replies_users$user_id)

x1 <- rt_users_summary$n[rt_users_summary$sample == unique(rt_users_summary$sample)[1]]
x2 <- rt_users_summary$n[rt_users_summary$sample == unique(rt_users_summary$sample)[2]]

mean(x1)
mean(x2)

options(digits = 7)
print(rt_users_summary, n = 200)
arrange(rt_users_summary, date)

xx <- rbind(
  data.frame(
  date = seq.Date(as.Date("2007-01-01"), as.Date("2017-12-31"), "days"),
  sample = "Random (lang:en)",
  n = 0,
  statuses = 0,
  stringsAsFactors = FALSE),
  data.frame(
  date = seq.Date(as.Date("2007-01-01"), as.Date("2017-12-31"), "days"),
  sample = "Reply to Trump",
  n = 0,
  statuses = 0,
  stringsAsFactors = FALSE)
)

rt_users_summary <- rbind(xx, rt_users_summary) %>%
  group_by(date, sample) %>%
  summarise(n = sum(n),
            statuses = sum(statuses))


x <- cbind(
  filter(rt_users_summary, sample == "Random (lang:en)") %>% select(date, n_random = n),
  filter(rt_users_summary, sample == "Reply to Trump") %>% select(n_reply = n)
)

x$diff <- `-`(x$n_random, x$n_reply)
x
qplot(n_random, n_reply, data = x)
summary(glm(n ~ sample + date, data = rt_users_summary, family = poisson))


##  ggsave("account_created_at.png")
