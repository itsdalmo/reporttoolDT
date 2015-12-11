library(devtools)
library(testthat)
load_all()

rm(list = ls(all = TRUE))
x <- read_data("test.sav")
y <- survey(x)

z <- set_association(y, mainentity = "q1", epsi = c("q3", "q6", "q16"))

get_attributes(y, "associations")
get_attributes(z, "associations")

identical(get_attributes(y, "associations"), get_attributes(z, "associations"))

# Association
x <- setNames(rep(NA, 3), c("q1", "q3", "q10"))
r <- list(epsi = c("q3", "q10"))
replace(x, r, by = names(x), invert = TRUE)

# x[match_all(unlist(r), names(x))] <- names(r)


# Labels
x <- setNames(rep(NA, 3), c("q1", "q3", "q10"))
r <- list(q1 = "Hvem er din leverandør?", q3 = "Alt i alt", q3 = "Ville du valgt xx pånytt?")
replace(x, r, by = names(x), invert = FALSE)

# Marketshares
x <- setNames(rep(NA, 3), c("e1", "e2", "e3"))
r <- list(e1 = .5, e2 = .25, e3 = .25)
replace(x, r, by = names(x), invert = FALSE)


lol <- function(...) substitute(list(...))[-1]
lol(a, b, c)

lol <- function(...) substitute(list(...))
lol(a, b, c)

