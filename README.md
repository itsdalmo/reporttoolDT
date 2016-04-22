<!-- README.md is generated from README.Rmd. Please edit that file -->
reporttool
----------

[![Build Status](https://travis-ci.org/itsdalmo/reporttoolDT.svg?branch=master)](https://travis-ci.org/itsdalmo/reporttoolDT) [![codecov.io](http://codecov.io/github/itsdalmo/reporttoolDT/coverage.svg?branch=master)](http://codecov.io/github/itsdalmo/reporttoolDT?branch=master)

A tool (work in progress) for generating reports from Survey data.

Installation
------------

#### 1. Install R

1.  Install the latest version of R (&gt;= 3.2.5) from [CRAN](https://cran.r-project.org/).
2.  Next, install Rtools (required for .xlsx files). You can find it on [CRAN](https://cran.r-project.org/bin/windows/Rtools/).
3.  Optional: Get the Rstudio (desktop) IDE from their [webpage](https://www.rstudio.com/products/rstudio/download/).

#### 2. Install LaTeX

LaTeX is required to create PDF files from rmarkdown and pandoc.

See Pandoc's [instructions](http://pandoc.org/installing.html) for installing LaTeX. Bulletpoints below:

-   Windows: [MikTex](http://miktex.org/). Use the "MikTex {version} Net Installer 64-bit" under "Other Downloads" on this [page](http://miktex.org/download). Do a full install.
-   OS X: Use [MacTex](https://tug.org/mactex/) (large download, but you'll have everything you need after.)
-   Linux: Install [Tex Live](http://www.tug.org/texlive/) using the package manager for the OS.

After installing, you might also want to change Rstudio's global options (Sweave) to "typeset LaTex into PDF using:" XeLatex. This allows you to generate PDF's with custom fonts.

#### 3. Install dependencies ahead of CRAN.

``` r
if (!require(devtools)) {
    install.packages("devtools")
}
devtools::install_github("hadley/haven")
```

Dev. dependencies:

-   [Haven](https://github.com/hadley/haven) 0.2.0.9000: Fixes crashes when writing strings longer than 256 characters.

#### 4. Install reporttolDT.

Development version:

``` r
devtools::install_github("itsdalmo/reporttoolDT", build_vignettes = TRUE)
```

CRAN:

``` r
# Not on CRAN yet.
```

#### 5. Optional install

Powerpoint (`generate_ppt`):

-   The latest JRE from [Java](http://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html).
-   [ReporteRs](https://github.com/davidgohel/ReporteRs) from CRAN:

``` r
install.packages("ReporteRs")
```

Whats next
----------

Head over to the [vignettes](https://github.com/itsdalmo/reporttoolDT/tree/master/vignettes) for the introduction, or run `vignette("introduction", package = "reporttoolDT")` in R.
