# Seeing regression — Shiny app

This app accompanies the simple linear regression exercise in `ME19549Task14Oct2026.qmd`.

## Run it

Install Shiny once if needed:

```r
install.packages("shiny")
```

Then open the repository folder as your working directory in R or RStudio and run:

```r
shiny::runApp(".")
```

The app uses only base R graphics plus the `shiny` package. The first tab generates and fits one dataset. The second repeats the same data-generating process and displays the p-value and slope sampling distributions, with the rejection rate interpreted as either Type I error (when `beta1 = 0`) or power (when `beta1 != 0`).

## Update and publish

The GitHub repository is the source of truth. Make and test changes locally, commit and push them, and only then deploy that committed version:

```r
rsconnect::deployApp(
  appDir = ".",
  appName = "IllustratingSimpleRegression",
  appFiles = c("app.R")
)
```

The first deployment requires a shinyapps.io account connection. In the shinyapps.io dashboard, choose **Tokens → Add Token → Show → Show Secret**, copy the generated `rsconnect::setAccountInfo(...)` command, and run it privately in the R console. Never save that token in this repository.
