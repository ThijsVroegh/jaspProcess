test_that("comparing jasp and spss mediation analysis - spss as baseline", {

  # jasp results --------------------------------------------------------------

  # compute mediation analysis in JASP
  options <- jaspTools::analysisOptions("ClassicProcess")
  options$dependent  <- "reaction"
  options$covariates <- list("pmi")
  options$factors    <- list("cond")
  options$statisticalPathPlotsCovariances       <- TRUE
  options$statisticalPathPlotsResidualVariances <- TRUE
  options$errorCalculationMethod <- "standard"
  options$naAction   <- "fiml"
  options$emulation  <- "lavaan"
  options$estimator  <- "default"
  options$moderationProbes <- list(list(probePercentile = 16, value = "16"),
                                   list(probePercentile = 50, value = "50"),
                                   list(probePercentile = 84, value = "84"))
  options$pathPlotsLegend <- TRUE
  options$pathPlotsColorPalette <- "colorblind"

  options$processModels <- list(list(conceptualPathPlot = TRUE,
                                     independentCovariances = TRUE,
                                     inputType = "inputVariables",
                                     mediationEffects = TRUE,
                                     mediatorCovariances = TRUE,
                                     modelNumber = 1,
                                     modelNumberCovariates = list(),
                                     modelNumberIndependent = "",
                                     modelNumberMediators = list(),
                                     modelNumberModeratorW = "",
                                     modelNumberModeratorZ = "",
                                     name = "Model 1",
                                     pathCoefficients = TRUE,
                                     processRelationships = list(list(processDependent   = "reaction",
                                                                      processIndependent = "cond",
                                                                      processType        = "mediators",
                                                                      processVariable    = "pmi")),
                                     residualCovariances = TRUE,
                                     statisticalPathPlot = FALSE,
                                     totalEffects = TRUE,
                                     localTests = FALSE,
                                     localTestType = "cis",
                                     localTestBootstrap = FALSE,
                                     localTestBootstrapSamples = 1000))

  set.seed(1)
  data <- read.csv("C:/Projects/ProcessModelsJasp/Data/hayes2022data/pmi/pmi.csv")

  results <- jaspTools::runAnalysis("ClassicProcess", data, options)

  # pathCoefficientsTable
  table_paths <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]

  # Function to round numeric values in a list
  round_numeric <- function(x) {
    if (is.numeric(x)) {
      round(x, digits = 4)
    } else if (is.list(x)) {
      lapply(x, round_numeric)
    } else {
      x
    }
  }

  # Apply the rounding function to the original list
  table_paths <- lapply(table_paths, round_numeric)

  # spss results (Hayes, p.94) -------------------------------------------------
  # path coefficients OLS regression output eq. 3.1 and  eq. 3.2 combined
  spss <- list()
  spss[[1]] <- list(
    ci.lower = -.2522,
    ci.upper = .7609,
    est = .2544,
    lhs = "cond",
    op = "→",
    pvalue = .3221,
    rhs = "reaction",
    se = .2558,
    z =  .9943
  )
  spss[[2]] <- list(
    ci.lower = .3143,
    ci.upper = .6986,
    est = .5064,
    lhs = "pmi",
    op = "→",
    pvalue = .0000,
    rhs = "reaction",
    se = .0970,
    z = 5.2185
  )

  spss[[3]] <- list(
    ci.lower = .0099,
    ci.upper = .9431,
    est = .4765,
    lhs = "cond",
    op = "→",
    pvalue = .0454,
    rhs = "pmi",
    se = .2357,
    z = 2.0218
  )

  spss <- lapply(spss, round_numeric)

  # Check the length of the lists
  length_jasp <- length(table_paths)
  length_spss <- length(spss)

  # Check the structure of each sublist
  structure_jasp <- sapply(table_paths, class)
  structure_spss <- sapply(spss, class)

  # Compare lengths
  if (length_jasp != length_spss) {
    stop("Lists have different lengths.")
  }

  # Compare structures
  if (!identical(structure_jasp, structure_spss)) {
    stop("Structures of sublists are not identical.")
  }

  # compare jasp and spss parameter estimates --------------------------------
  testthat::expect_equal(table_paths, spss, tolerance = 1e-2)

  # totalEffectsTable JASP
  table_total <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_totalEffectsTable"]][["data"]]
  table_total <- lapply(table_total, round_numeric)

  # TotalEffects table from SPSS
  spss_effects <- list()

  spss_effects[[1]] <- list(
    ci.lower = -.0538,
    ci.upper = 1.0452,
    est = .4957,
    lhs = "cond",
    op = "→",
    pvalue = .0766,
    rhs = "reaction",
    se = .2775,
    z =  1.7860
  )
  spss_effects[[2]] <- list(
    ci.lower = .0067,
    ci.upper = .5258,
    est = .2413,
    lhs = "pmi",
    op = "→",
    pvalue = .0635,
    rhs = "reaction",
    se = .1300,
    z = 1.8559
  )

  # compare jasp and spss total effects --------------------------------
  testthat::expect_equal(table_total, spss_effects,tolerance = 1e-2)

})

# II
test_that("comparing jasp and spss moderation analysis - spss as baseline", {

  # jasp results --------------------------------------------------------------

  # compute moderation analysis in JASP
  options <- jaspTools::analysisOptions("ClassicProcess")
  options$dependent  <- "justify"
  options$covariates <- list("skeptic")
  options$factors    <- list("frame")
  options$statisticalPathPlotsCovariances       <- TRUE
  options$statisticalPathPlotsResidualVariances <- TRUE
  options$errorCalculationMethod <- "standard"
  options$naAction   <- "fiml"
  options$emulation  <- "lavaan"
  options$estimator  <- "default"
  options$moderationProbes <- list(list(probePercentile = 16, value = "16"),
                                   list(probePercentile = 50, value = "50"),
                                   list(probePercentile = 84, value = "84"))
  options$pathPlotsLegend <- TRUE
  options$pathPlotsColorPalette <- "colorblind"

  options$processModels <- list(list(conceptualPathPlot = TRUE,
                                     independentCovariances = TRUE,
                                     inputType = "inputVariables",
                                     mediationEffects = TRUE,
                                     mediatorCovariances = TRUE,
                                     modelNumber = 1,
                                     modelNumberCovariates = list(),
                                     modelNumberIndependent = "",
                                     modelNumberMediators = list(),
                                     modelNumberModeratorW = "",
                                     modelNumberModeratorZ = "",
                                     name = "Model 1",
                                     pathCoefficients = TRUE,
                                     processRelationships = list(list(processDependent   = "justify",
                                                                      processIndependent = "frame",
                                                                      processType        = "moderators",
                                                                      processVariable    = "skeptic")),
                                     residualCovariances = TRUE,
                                     statisticalPathPlot = FALSE,
                                     totalEffects = TRUE,
                                     localTests = FALSE,
                                     localTestType = "cis",
                                     localTestBootstrap = FALSE,
                                     localTestBootstrapSamples = 1000))

  set.seed(1)
  data <- read.csv("C:/Projects/ProcessModelsJasp/Data/hayes2022data/disaster/disaster.csv")
  results <- jaspTools::runAnalysis("ClassicProcess", data, options)

  # Function to round numeric values in a list
  round_numeric <- function(x) {
    if (is.numeric(x)) {
      round(x, digits = 4)
    } else if (is.list(x)) {
      lapply(x, round_numeric)
    } else {
      x
    }
  }

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]
  table <- lapply(table, round_numeric)

  # spss results (Hayes, p.253) -----------------------------------------------
  # path coefficients OLS regression output eq. 3.1 and  eq. 3.2 combined

  spss <- list()

  spss[[1]] <- list(
    ci.lower = -.9921,
    ci.upper = -.1328,
    est = -.5625,
    lhs = "frame",
    op = "→",
    pvalue = .0105,
    rhs = "justify",
    se = .2179,
    z =  -2.5811
  )
  spss[[2]] <- list(
    ci.lower = .0299,
    ci.upper = .1803,
    est = .1051,
    lhs = "skeptic",
    op = "→",
    pvalue = .0064,
    rhs = "justify",
    se = .0381,
    z = 2.7559
  )

  spss[[3]] <- list(
    ci.lower = .0922,
    ci.upper = .3101,
    est = .2012,
    lhs = "frame:skeptic",
    op = "→",
    pvalue = .0003,
    rhs = "justify",
    se = .0553,
    z = 3.6401
  )

  # compare jasp and spss parameter estimates --------------------------------
  testthat::expect_equal(table,spss,tolerance = 1e-2)
})

# III
test_that("comparing jasp and spss conditional process analysis - spss as baseline", {

  # jasp results --------------------------------------------------------------

  # compute conditional process analysis in JASP
  options <- jaspTools::analysisOptions("ClassicProcess")
  options$dependent  <- "perform"
  options$covariates <- list("negtone","dysfunc","negexp")
  #options$factors    <- list("frame")
  options$statisticalPathPlotsCovariances       <- TRUE
  options$statisticalPathPlotsResidualVariances <- TRUE
  options$errorCalculationMethod <- "standard"
  options$naAction   <- "fiml"
  options$emulation  <- "lavaan"
  options$estimator  <- "default"
  options$moderationProbes <- list(list(probePercentile = 16, value = "16"),
                                   list(probePercentile = 50, value = "50"),
                                   list(probePercentile = 84, value = "84"))
  options$pathPlotsLegend <- TRUE
  options$pathPlotsColorPalette <- "colorblind"

  options$processModels <- list(list(conceptualPathPlot = TRUE,
                                     independentCovariances = TRUE,
                                     inputType = "inputVariables",
                                     mediationEffects = TRUE,
                                     mediatorCovariances = TRUE,
                                     modelNumber = 1,
                                     modelNumberCovariates = list(),
                                     modelNumberIndependent = "",
                                     modelNumberMediators = list(),
                                     modelNumberModeratorW = "",
                                     modelNumberModeratorZ = "",
                                     name = "Model 1",
                                     pathCoefficients = TRUE,
                                     processRelationships = list(list(processDependent   = "perform",
                                                                      processIndependent = "dysfunc",
                                                                      processType        = "mediators",
                                                                      processVariable    = "negtone"),

                                                                 list(processDependent   = "perform",
                                                                      processIndependent = "negtone",
                                                                      processType        = "moderators",
                                                                      processVariable    = "negexp")
                                                                 ),
                                     residualCovariances = TRUE,
                                     statisticalPathPlot = FALSE,
                                     totalEffects = TRUE,
                                     localTests = FALSE,
                                     localTestType = "cis",
                                     localTestBootstrap = FALSE,
                                     localTestBootstrapSamples = 1000))

  set.seed(1)
  data    <- read.csv("C:/Projects/ProcessModelsJasp/Data/hayes2022data/teams/teams.csv")
  results <- jaspTools::runAnalysis("ClassicProcess", data, options)

  # Function to round numeric values in a list
  round_numeric <- function(x) {
    if (is.numeric(x)) {
      round(x, digits = 4)
    } else if (is.list(x)) {
      lapply(x, round_numeric)
    } else {
      x
    }
  }

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]
  table <- lapply(table, round_numeric)

  # spss results (Hayes, p.432) -----------------------------------------------
  # path coefficients OLS regression output

  spss <- list()

  spss[[1]] <- list(
    ci.lower = .0097,
    ci.upper = .7224,
    est = .3661,
    lhs = "dysfunc",
    op = "→",
    pvalue = .0443,
    rhs = "perform",
    se = .1778,
    z =  2.0585
  )
  spss[[2]] <- list(
    ci.lower = -.6974,
    ci.upper = -.1741,
    est = -.4357,
    lhs = "negtone",
    op = "→",
    pvalue = .0015,
    rhs = "perform",
    se = .1306,
    z = -3.3377
  )

  spss[[3]] <- list(
    ci.lower = -.2545,
    ci.upper = .2161,
    est = -.0192,
    lhs = "negexp",
    op = "→",
    pvalue = .8708,
    rhs = "perform",
    se = .1174,
    z = -.1634
  )

  spss[[4]] <- list(
      ci.lower = -.9998,
      ci.upper = -.0341,
      est = -.5170,
      lhs = "negtone:negexp",
      op = "→",
      pvalue = .0363,
      rhs = "perform",
      se = .2409,
      z = -2.1458
  )

  spss[[5]] <- list(
    ci.lower = .2858,
    ci.upper = .9537,
    est = .6198,
    lhs = "dysfunc",
    op = "→",
    pvalue = .0005,
    rhs = "negtone",
    se = .1668,
    z = 3.7148
  )

  # compare jasp and spss parameter estimates --------------------------------
  testthat::expect_equal(table, spss, tolerance = 1e-2)
})

