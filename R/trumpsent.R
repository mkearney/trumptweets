
library(warcraft)
warcraft_mode()

library(rtweet)


d <- get_timeline("realdonaldtrump", n = 3200)

library(tidyverse)

tt <- readr::read_rds(
  "/Users/mwk/R/trumptweets/data/trumptweets-08-10-2017.rds"
)

d <- rbind(d[yin(names(d), names(tt))], tt[yin(names(tt), names(d))])
d <- d[!duplicated(d$status_id), ]

ja <- read.table(
  "http://elections.huffingtonpost.com/pollster/api/v2/questions/00c%20-Pres-45-Trump%20-%20Job%20Approval%20-%20National/poll-responses-clean.tsv",
  header = TRUE, sep = "\t"
  )
ja <- as_tbl(ja)
ja$end_date <- as.POSIXct(ja$end_date)
ja$start_date <- as.POSIXct(ja$start_date)

mid_datetime <- function(start_date, end_date) {
  diff <- difftime(end_date, start_date)
  (as.numeric(diff, "secs") %/% 2) + start_date
}

foo <- function(int) {
  j <- ja %>%
    mutate(
      date = mid_datetime(start_date, end_date),
      date = round_time(date, int)) %>%
    group_by(date) %>%
    summarise(
      approve = mean(Approve, na.rm = TRUE),
      disapprove = mean(Disapprove, na.rm = TRUE),
      net = approve - disapprove) %>%
    select(date, net)

  j <- gather(j, var, val, -date)

  d %>%
    filter(created_at > "2017-01-18") %>%
    mutate(
      fake_news = grepl("fake news|fakenews", text, ignore.case = TRUE),
      date = round_time(created_at, int)) %>%
    group_by(date) %>%
    summarise(fake_news = sum(fake_news)) -> df

  df <- gather(df, var, val, -date)

  scale_standard <- function(x) {
    xmin <- min(x, na.rm = TRUE)
    (x - xmin) / (max(x, na.rm = TRUE) - xmin)
  }

  #j$net <- scale_standard(j$net)
  #df$fake_news <- scale_standard(df$fake_news)
  #dfj <- full_join(df, j)

  #j$val <- scale_standard(j$val)
  #df$val <- scale_standard(df$val)


  dfj <- rbind(df, j)

  dd <- spread(dfj, var, val)

  return(list(dd, dfj))
}


ja %>%
  mutate(
    date = mid_datetime(start_date, end_date),
    date = round_time(date, "5 days")) %>%
  group_by(date) %>%
  summarise(net = mean(Approve - Disapprove)) -> ja2

nrep <- function(pat, text, ignore.case = TRUE) {
  x <- gregexpr(pat, text, ignore.case = ignore.case)
  m <- vapply(x, function(i) identical(i[1], -1L), logical(1))
  x <- lengths(x)
  x[m] <- 0L
  x
}

substr(Sys.time(), 12, 13)

early_tweet <- function(x) {
  tod <- as.integer(substr(x, 12, 13))
  tod < 7
}

time_of_day <- function(x) {
  tod <- as.integer(substr(x, 12, 13))
  ifelse(tod < 4 | tod > 20, "late night", "day")
}



d %>%
  filter(created_at > "2017-01-21" & !is_quote & !is_retweet) %>%
  mutate(
    nchar = nchar(text),
    tod = time_of_day(created_at),
    mention = nrep("@", text),
    hashtag = nrep("#", text),
    link = nrep("http", text),
    fake_news = nrep("fake|media|cnn|washington post|new york times|nbc|abc|msm", text),
    date = round_time(created_at, "days")) %>%
  group_by(date) %>%
  mutate(n_tweets = n()) %>%
  ungroup() %>%
  select(fake_news, n_tweets, nchar, mention, link, tod, hashtag, source, date) %>%
  left_join(ja2) %>%
  select(date, net, fake_news:source) -> xx

xx <- arrange(xx, date)
xx$net[1] <- na_omit(xx$net)[1L]

for (i in seq_len(nrow(xx))) {
  if (is.na(xx$net[i])) {
    xx$net[i] <- xx$net[i - 1L]
  }
}

xx %>%
  mutate(
    fake_news = fake_news > 0,
    link = link > 0,
    hashtag = hashtag > 0,
    mention = mention > 0,
    android = source == "Twitter for Android") %>%
  select(-source) -> xx


xxx <- mutate(xx, android = source == "Twitter for Android") %>% select(-source)


m1 <- xxx %>%
  glm(fake_news > 0L ~ date + net + nchar + mention + link + hashtag, ., family = binomial(link = "logit"))

summary(m1)
summary(lm(fake_news > 0L ~ date + net + nchar + mention + link + hashtag, xxx))

m1 <- glm(fake_news ~ date + tod + links + hashtags + mentions + net + n_tweets + (source=="Twitter for Android"), xx, family = quasipoisson)
prd <- c("predict_no", "predict_yes")[as.integer(predict(m1, type = "response") > .2) + 1L]
act <- c("actual_no", "actual_yes")[as.logical(xx$fake_news > 0) + 1L]
table(act, prd)
summary(m1)
summary(lm(fake_news > 0L ~ date + tod + links + hashtags + mentions + net + n_tweets + (source=="Twitter for Android"), xx))

sum(predict(m1, type = "response") > .146)

table(xx$fake_news > 0L, predict(m1, type = "response") > .50)

summary(glm(fake_news > 0L ~ date + net + n_tweets + (source=="Twitter for Android"), xx, family = binomial))



summary(lm(fake_news > 0L ~ date + net + n_tweets + (source=="Twitter for Android"), xx))

x <- foo("days")

dd <- x[[1]]
barplot(dd$fake_news)

summary(glm(fake_news ~ net, dd, family = poisson))

dd %>%
  ggplot(aes(x = net, y = fake_news)) +
  geom_point(size = 3) +
  stat_smooth(method = "glm", method.args = list(family = poisson)) +
  theme_mwk(base_family = "Avenir Next") +
  scale_colour_manual(
    values = c("royalblue2", "red3")) +
  scale_fill_manual(
    values = c("dodgerblue3", "red3")) +
  labs(
    title = "Trump tweets more about \"fake news\" when his net-approval is lower") +
  ggsave("~/Desktop/trumpfake.png")




cor(dd$fake_news, dd$net)


?geom_col


d %>%
  dplyr::filter(!is_retweet & created_at > "2014-01-01") %>%
  dplyr::mutate(week = aggfun(30)(created_at)) %>%
  dplyr::group_by(week) %>%
  dplyr::summarise(
    snt = mean(sentiment - .5)) %>%
  dplyr::select(week, snt) %>%
  dplyr::mutate(valence = ifelse(snt > 0, "Positive", "Negative")) %>%
  ggplot(aes(x = week, y = snt, fill = valence, color = valence)) +
  geom_col(position = "dodge", width = 2350000, size = .25) +
  theme_mwk(base_family = "Helvetica Neue") +
  theme(legend.position = "bottom",
        plot.caption = element_text(size = rel(.7))) +
  labs(
    title = "Positive/negative sentiment of @realDonaldTrump's tweets",
    subtitle = "Mean sentiment of Trump's [non-retweet] Twitter statuses aggregated by month",
    caption = "Source: Data collected from Twitter's REST API using rtweet",
    x = NULL, y = NULL
  ) +
  scale_fill_manual(values = c(Positive = "#2244ff", Negative = "#ee4444")) +
  scale_color_manual(values = c(Positive = "#2244ff", Negative = "#ee4444")) +
  geom_vline(xintercept = as.POSIXct("2015-06-16"), size = .75, colour = "#550055") +
  annotate("text", as.POSIXct("2015-06-18"), y = -.35, angle = 90,
           label = "\nCandidate Trump", size = 3) +
  ggsave("~/Desktop/trumpsentiment.png")

?  geom_histogram

  ylim(-1, 1)

  geom_line(aes(y = 0), color = "black", size=  1)

?stat_bin
  ggsave("~/Desktop/rtfvs.png", width = 7.5, height = 5.7, units = "in")
