library(devtools)
library(testthat)
library(stringi)

rm(list = ls(all = TRUE))
devtools::load_all()
x <- reporttool::read_data("./tests/testthat/test.sav")

# TODO: Add tidyr methods for Survey.
x <- survey_dt(x)
x <- dplyr::select(x, q1, Image:EPSI)
x <- tidyr::gather(x, latent, score, -q1)
x <- dplyr::summarise(dplyr::group_by(x, q1, latent), score = mean(score))

tidyr::spread(x, latent, score)
