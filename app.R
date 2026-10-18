library(shiny)

# Simulate and fit a simple linear regression without extra package dependencies.
simulate_once <- function(n, beta0, beta1, sigma) {
  x <- runif(n, 5, 20)
  y <- beta0 + beta1 * x + rnorm(n, sd = sigma)
  fit <- lm(y ~ x)
  list(data = data.frame(x = x, y = y), fit = fit,
       truth = c(beta0 = beta0, beta1 = beta1, sigma = sigma))
}

simulate_many <- function(B, n, beta0, beta1, sigma) {
  slope <- se <- p_value <- numeric(B)
  for (i in seq_len(B)) {
    x <- runif(n, 5, 20)
    y <- beta0 + beta1 * x + rnorm(n, sd = sigma)
    x_centred <- x - mean(x)
    sxx <- sum(x_centred^2)
    slope[i] <- sum(x_centred * (y - mean(y))) / sxx
    intercept <- mean(y) - slope[i] * mean(x)
    residuals <- y - intercept - slope[i] * x
    se[i] <- sqrt(sum(residuals^2) / (n - 2) / sxx)
    p_value[i] <- 2 * pt(-abs(slope[i] / se[i]), df = n - 2)
  }
  data.frame(slope = slope, se = se, p_value = p_value)
}

ui <- fluidPage(
  tags$head(
    tags$title("Seeing regression"),
    tags$style(HTML("
      :root { --ink:#17324d; --blue:#1479a8; --coral:#dd5b45; --paper:#f6f8fa; }
      body { background:var(--paper); color:var(--ink); font-family:system-ui,-apple-system,'Segoe UI',sans-serif; }
      .container-fluid { max-width:1400px; padding:0 28px 28px; }
      .hero { margin:0 -28px 24px; padding:24px 32px 20px; color:white;
              background:linear-gradient(120deg,#12314b,#1479a8); }
      .hero h1 { margin:0 0 4px; font-weight:750; letter-spacing:-.025em; }
      .hero p { margin:0; opacity:.9; font-size:16px; }
      .control-card,.content-card,.metric { background:white; border:1px solid #dfe7ed;
        border-radius:14px; box-shadow:0 3px 14px rgba(23,50,77,.07); }
      .control-card { padding:20px; position:sticky; top:12px; }
      .content-card { padding:18px 22px; margin-bottom:18px; }
      .control-card h3 { margin-top:0; } .form-group { margin-bottom:14px; }
      .nav-tabs { border:0; margin-bottom:14px; }
      .nav-tabs>li>a { border:0!important; border-radius:10px; color:var(--ink); font-weight:650; }
      .nav-tabs>li.active>a { background:#dceff7!important; color:#075a80!important; }
      .btn-primary { background:var(--blue); border-color:var(--blue); border-radius:9px; width:100%; font-weight:650; }
      .metrics { display:grid; grid-template-columns:repeat(3,1fr); gap:12px; margin-bottom:18px; }
      .metric { padding:13px 15px; } .metric .label { color:#62788a; font-size:12px; text-transform:uppercase; letter-spacing:.06em; }
      .metric .value { font-size:24px; font-weight:750; margin-top:2px; }
      .note { border-left:4px solid var(--blue); background:#eef7fb; padding:12px 14px; border-radius:0 8px 8px 0; }
      .warning-note { border-left-color:var(--coral); background:#fff2ef; }
      @media(max-width:767px){ .control-card{position:static}.metrics{grid-template-columns:1fr}.container-fluid{padding:0 14px}.hero{margin:0 -14px 18px} }
    "))
  ),
  div(class = "hero",
      h1("Seeing regression"),
      p("Change the data-generating process, then watch estimation and hypothesis testing respond.")),
  fluidRow(
    column(3, div(class = "control-card",
      h3("True model"),
      helpText("Y = beta0 + beta1 X + error, with X sampled uniformly from 5 to 20."),
      numericInput("beta0", "Intercept (beta0)", value = 10, step = 1),
      numericInput("beta1", "Slope (beta1)", value = 1.2, step = 0.1),
      numericInput("sigma", "Error SD (sigma)", value = 3, min = 0.1, step = 0.5),
      sliderInput("n", "Sample size (n)", min = 5, max = 500, value = 50, step = 1),
      numericInput("seed", "Random seed", value = 123, min = 1, step = 1),
      conditionalPanel("input.tabs == 'single'",
        checkboxInput("show_true", "Show true line", TRUE),
        checkboxInput("show_residuals", "Show residuals", FALSE),
        actionButton("draw", "Generate a new dataset", class = "btn-primary")
      ),
      conditionalPanel("input.tabs == 'repeat'",
        sliderInput("B", "Number of experiments (B)", min = 100, max = 10000, value = 1000, step = 100),
        sliderInput("alpha", "Significance level (alpha)", min = 0.01, max = 0.10, value = 0.05, step = 0.01),
        actionButton("run", "Run repeated experiments", class = "btn-primary")
      )
    )),
    column(9,
      tabsetPanel(id = "tabs",
        tabPanel("One dataset", value = "single",
          uiOutput("single_metrics"),
          div(class = "content-card", plotOutput("single_plot", height = "500px")),
          div(class = "content-card", h3("What this sample says"), uiOutput("single_text"),
              tags$details(tags$summary("Model summary"), verbatimTextOutput("model_summary")))
        ),
        tabPanel("Repeated experiments", value = "repeat",
          uiOutput("repeat_metrics"),
          div(class = "content-card", plotOutput("p_plot", height = "390px")),
          fluidRow(
            column(7, div(class = "content-card", plotOutput("slope_plot", height = "330px"))),
            column(5, div(class = "content-card", h3("How to read this"), uiOutput("repeat_text")))
          )
        ),
        tabPanel("Learning guide", value = "guide",
          div(class = "content-card",
            h2("Questions to try"),
            tags$ol(
              tags$li("Set beta1 to 0. Are the p-values roughly uniform? Is the rejection rate close to alpha?"),
              tags$li("Set beta1 away from 0. How do sample size, noise, and effect size change power?"),
              tags$li("Compare the true slope with the distribution of estimated slopes. Is it centred correctly?"),
              tags$li("Turn on residuals in the first tab. Which points contribute most to the residual sum of squares?")
            ),
            div(class="note", strong("Key idea: "),
                "A p-value is random because the sample is random. Power is the long-run probability that the test rejects a false null hypothesis."),
            h3("About the simulation"),
            p("Each experiment samples X uniformly between 5 and 20, generates independent Normal errors with standard deviation sigma, fits Y ~ X, and performs the usual two-sided t-test of H0: beta1 = 0."),
            p("The supplied lagartos data and class exercise motivated the default length–weight setting; this app focuses on the known-truth simulation stage of that exercise.")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  single <- eventReactive(input$draw, {
    req(input$n >= 3, input$sigma > 0)
    set.seed(as.integer(input$seed) + input$draw)
    simulate_once(input$n, input$beta0, input$beta1, input$sigma)
  }, ignoreNULL = FALSE)

  repeated <- eventReactive(input$run, {
    req(input$n >= 3, input$sigma > 0, input$B >= 1)
    set.seed(as.integer(input$seed) + 100000L + input$run)
    list(data = simulate_many(input$B, input$n, input$beta0, input$beta1, input$sigma),
         truth = c(beta0 = input$beta0, beta1 = input$beta1, sigma = input$sigma),
         n = input$n, B = input$B)
  }, ignoreNULL = FALSE)

  output$single_plot <- renderPlot({
    obj <- single(); d <- obj$data; fit <- obj$fit
    pad <- max(diff(range(d$y)) * .08, 0.5)
    plot(d$x, d$y, pch = 19, col = adjustcolor("#1479a8", .72), cex = 1.15,
         xlab = "Predictor, X", ylab = "Response, Y", main = "One sample from a known model",
         ylim = range(d$y) + c(-pad, pad), bty = "l")
    grid(col = "#dfe7ed"); points(d$x, d$y, pch = 19, col = adjustcolor("#1479a8", .72), cex = 1.15)
    if (isTRUE(input$show_residuals)) {
      segments(d$x, fitted(fit), d$x, d$y, col = adjustcolor("#dd5b45", .65), lwd = 1.4)
    }
    if (isTRUE(input$show_true)) abline(a = obj$truth[["beta0"]], b = obj$truth[["beta1"]], col = "#263746", lwd = 2, lty = 2)
    abline(fit, col = "#dd5b45", lwd = 3)
    legend("topleft", inset = .01, bty = "n", lwd = c(3, 2), lty = c(1, 2),
           col = c("#dd5b45", "#263746"), legend = c("Fitted line", "True line"))
  }, res = 110)

  output$single_metrics <- renderUI({
    cf <- summary(single()$fit)$coefficients
    div(class="metrics",
      div(class="metric", div(class="label", "Estimated slope"), div(class="value", sprintf("%.3f", cf[2,1]))),
      div(class="metric", div(class="label", "Slope standard error"), div(class="value", sprintf("%.3f", cf[2,2]))),
      div(class="metric", div(class="label", "p-value for slope"), div(class="value", format.pval(cf[2,4], digits=3, eps=.001)))
    )
  })

  output$single_text <- renderUI({
    cf <- summary(single()$fit)$coefficients; p <- cf[2,4]
    div(class = paste("note", if (p >= .05) "warning-note" else ""),
      HTML(sprintf("The fitted slope is <strong>%.3f</strong>, compared with the true slope of <strong>%.3f</strong>. For H<sub>0</sub>: beta1 = 0, p = <strong>%s</strong>; this sample %s at the 5%% level.",
                   cf[2,1], single()$truth[["beta1"]], format.pval(p, digits=3, eps=.001),
                   if (p < .05) "rejects the null" else "does not reject the null")))
  })
  output$model_summary <- renderPrint(summary(single()$fit))

  output$p_plot <- renderPlot({
    d <- repeated()$data
    breaks <- sort(unique(c(seq(0, 1, by = .05), input$alpha)))
    h <- hist(d$p_value, breaks=breaks, plot=FALSE)
    plot(h, freq=FALSE, col="#b9ddeb", border="white", main="Sampling distribution of p-values",
         xlab="p-value for the slope", ylab="Density", bty="l", ylim=c(0,max(h$density, 1)*1.14))
    idx <- h$breaks[-1] <= input$alpha
    if (any(idx)) rect(h$breaks[which(idx)], 0, h$breaks[which(idx)+1], h$density[idx], col="#dd5b45", border="white")
    abline(h=1, col="#62788a", lwd=1.5, lty=3)
    abline(v=input$alpha, col="#a63423", lwd=2, lty=2)
    legend("topright", bty="n", fill=c("#dd5b45", "#b9ddeb"),
           legend=c(sprintf("p < %.2f", input$alpha), sprintf("p >= %.2f", input$alpha)))
  }, res=110)

  output$slope_plot <- renderPlot({
    obj <- repeated(); d <- obj$data; den <- density(d$slope)
    plot(den, lwd=3, col="#1479a8", main="Sampling distribution of estimated slopes",
         xlab="Estimated slope", ylab="Density", bty="l")
    polygon(den, col=adjustcolor("#1479a8",.18), border=NA)
    lines(den, lwd=3, col="#1479a8"); abline(v=obj$truth[["beta1"]], lwd=2, lty=2, col="#263746")
    abline(v=0, lwd=2, lty=3, col="#dd5b45")
    legend("topright", bty="n", lty=c(2,3), lwd=2, col=c("#263746","#dd5b45"), legend=c("True slope","Null value"))
  }, res=110)

  output$repeat_metrics <- renderUI({
    obj <- repeated(); d <- obj$data; rate <- mean(d$p_value < input$alpha)
    label <- if (abs(obj$truth[["beta1"]]) < .Machine$double.eps^.5) "Type I error rate" else "Estimated power"
    div(class="metrics",
      div(class="metric", div(class="label", label), div(class="value", sprintf("%.1f%%", 100*rate))),
      div(class="metric", div(class="label", "Mean estimated slope"), div(class="value", sprintf("%.3f", mean(d$slope)))),
      div(class="metric", div(class="label", "Experiments"), div(class="value", format(nrow(d), big.mark=",")))
    )
  })

  output$repeat_text <- renderUI({
    obj <- repeated(); d <- obj$data; beta1 <- obj$truth[["beta1"]]
    rate <- mean(d$p_value < input$alpha); null <- abs(beta1) < .Machine$double.eps^.5
    if (null) tagList(
      p(HTML(sprintf("Because the true slope is zero, the null hypothesis is true. <strong>%.1f%%</strong> of experiments rejected it; with many repetitions this should be near alpha = %.2f.", 100*rate, input$alpha))),
      p("Under a true null, p-values are Uniform(0, 1): equal-width regions should contain roughly equal numbers of results."),
      div(class="note", "These rejections are false positives (Type I errors), not power.")
    ) else tagList(
      p(HTML(sprintf("The true slope is %.3f, so the null is false. <strong>%.1f%%</strong> of experiments detected the effect at alpha = %.2f. This is the estimated power.", beta1, 100*rate, input$alpha))),
      p("Power generally rises with a larger absolute slope, a larger sample, less noise, or a less stringent alpha."),
      div(class="note", HTML(sprintf("Monte Carlo SE for this power estimate: about <strong>%.3f</strong>.", sqrt(rate*(1-rate)/nrow(d)))))
    )
  })
}

shinyApp(ui, server)
