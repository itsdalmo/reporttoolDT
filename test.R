rm(list = ls(all = TRUE))
x <- read_data("test.sav")
x <- survey(x)

x[, "test" := "lol"]
x$test2 <- "lol2"
x[["test3"]] <- "lol3"
x[, "test4"] <- "lol4"

attr(x, which = "labels")
attr(x, which = "associations")


names(x)[1] <- "testdate"


