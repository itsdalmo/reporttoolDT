context("Rmarkdown/Generate report")

sav <- seamless::read_data(system.file("extdata", "raw_data.sav", package = "reporttoolDT"))
srv <- survey_tbl(sav)
srv <- set_config(srv, cutoff = .3)
srv <- set_translation(srv, language = "english")
srv <- set_association(srv, common = TRUE)
srv <- latents_mean(srv)

# Add a contrast
srv <- add_contrast(filter(srv, q1 == "CompanyA"), mutate(srv, q1 = "Average"))
