time2utc <- function(x) UseMethod("time2utc")
time2utc.POSIXt <- function(x) {
  utc_time(x)
}
time2utc.data.frame <- function(x) {
  if (any(map_lgl_(inherits, x, "POSIXt"))) {
    psx <- map_lgl_(inherits, x, "POSIXt")
    x[psx] <- lapply(x[psx], time2utc)
  }
  x
}


get_rl <- function(tokens = get_tokens(), new = FALSE) {
  if (!new && exists(".rate_limit")) {
    rl <- get("rl", envir = .rate_limit)
  } else {
    rl <- rate_limit(tokens)
    rl$timestamp <- utc_time()
    rl$reset_at <- utc_time(rl$reset_at)
    .rate_limit <- new.env()
    assign(".rate_limit", .rate_limit, envir = .GlobalEnv)
    assign("rl", rl, envir = .rate_limit)
  }
  rl
}

utc_time <- function(x = Sys.time()) {
  if (is.numeric(x)) x <- try(as.POSIXct(x, origin = "1970-01-01"))
  stopifnot(inherits(x, "POSIXt"))
  as.POSIXct(
    format(x, tz = "UTC"),
    tz = "UTC")
}
cst_time <- function(x = Sys.time()) {
  if (is.numeric(x)) x <- try(as.POSIXct(x, origin = "1970-01-01"))
  stopifnot(inherits(x, "POSIXt"))
  as.POSIXct(
    format(x, tz = "America/Chicago"),
    tz = "America/Chicago")
}
est_time <- function(x = Sys.time()) {
  if (is.numeric(x)) x <- try(as.POSIXct(x, origin = "1970-01-01"))
  stopifnot(inherits(x, "POSIXt"))
  as.POSIXct(
    format(x, tz = "America/New_York"),
    tz = "America/New_York")
}
mst_time <- function(x = Sys.time()) {
  if (is.numeric(x)) x <- try(as.POSIXct(x, origin = "1970-01-01"))
  stopifnot(inherits(x, "POSIXt"))
  as.POSIXct(
    format(x, tz = "America/Denver"),
    tz = "America/Denver")
}
pst_time <- function(x = Sys.time()) {
  if (is.numeric(x)) x <- try(as.POSIXct(x, origin = "1970-01-01"))
  stopifnot(inherits(x, "POSIXt"))
  as.POSIXct(
    format(x, tz = "America/Los_Angeles"),
    tz = "America/Los_Angeles")
}


update_rl <- function(fun = NULL, token = 1L, n = 1L, refresh = FALSE) {
  rl <- get_rl(new = refresh)
  if (!is.null(fun)) {
    twapis <- unlist(rtweet:::funs_and_apis())
    if (any(grepl(fun, twapis))) {
      kp <- names(twapis)[grep(fun, twapis)]
    } else {
      kp <- grep(fun, names(twapis), value = TRUE)
    }
    if (length(kp) == 0L) stop("couldn't match API path")
    rl$remaining[
      rl$app == unique(rl$app)[token] & rl$query %in% kp] <- rl$remaining[
      rl$app == unique(rl$app)[token] & rl$query %in% kp] - n
  }
  timestamp <- utc_time()
  rl$remaining <- ifelse(
    rl$reset_at <= timestamp, rl$limit, rl$remaining)
  rl$reset_at <- utc_time(
    ifelse(rl$reset_at <= timestamp,
           rl$reset_at + 60 * 15,
           rl$reset_at)
  )
  rl$reset <- difftime(rl$reset_at, utc_time(), units = "mins")
  assign("rl", rl, envir = get(".rate_limit", envir = .GlobalEnv))
  if (sum(rl$remaining))
  rl <- subset(rl, grepl(paste(kp, collapse = "|"), query),
               select = c(limit, remaining, reset, app))
  if (sum(rl$remaining > 0L) == 0L) {
    slp <- as.numeric(min(rl$reset), "secs")
    app <- rl$app[rl$reset == min(rl$reset)][1]
    message("Sleeping for " + slp + " seconds")
    Sys.sleep(slp)
  } else {
    app <- rl$app[order(rl$remaining, decreasing = TRUE)][1]
  }
  which(unique(rl$app) == app)
}

## function to join tweets and users data
join_rtweet <- function(x) {
  if ("description" %in% names(x)) {
    users <- x
    tweets <- tweets_data(x)
  } else {
    tweets <- x
    users <- users_data(x)
  }
  users <- users[, names(users) != "screen_name"]
  users <- users[!duplicated(users$user_id), ]
  dplyr::left_join(tweets, users, by = "user_id")
}


scale_standard <- function(x) {
  xmin <- min(x, na.rm = TRUE)
  (x - xmin) / (max(x, na.rm = TRUE) - xmin)
}
