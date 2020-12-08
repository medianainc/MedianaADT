# Test the input of the ModuleC function

# Normal case parameters
normalCase = list(

  # Number of enrolled patients in each trial arm
  sample_size = c(120, 120),

  # Prevalence
  prevalence = 0.4,

  # Primary endpoint's type
  endpoint_type = "Normal",

  # Mean and SD in the control arm 
  control_mean = c(0, 0),
  control_sd = c(1, 1),
  
  # Mean and SD in the treatment arm 
  treatment_mean = c(0.2, 0.4),
  treatment_sd = c(1, 1),
  
  # Information fractions at IA1, IA2, FA
  info_frac = c(0.4, 0.6, 1.0),

  # Futility threshold
  futility_threshold = 0.1,

  # Influence threshold for population selection
  influence = 0.1,

  # Interaction threshold for population selection
  interaction = 1.3,

  # Dropout rate at the end of the treatment period
  dropout_rate = 0.05,

  # One-sided Type I error rate
  alpha = 0.025,

  # Number of simulations
  nsims = 1000
)

# Binary case parameters
binaryCase = list(

  # Endpoint type
  endpoint_type = "Binary",

  # Number of enrolled patients (control, treatment) 
  sample_size = c(120, 120),

  # Prevalence of biomarker-positive patients in the overall population
  prevalence = 0.4,

  # Patient dropout rate
  dropout_rate = 0.15,

  # Response rate in the control arm (biomarker-negative, biomarker-positive)
  control_rate = c(0.1, 0.1),

  # Response rate in the treatment arm (biomarker-negative, biomarker-positive)
  treatment_rate = c(0.25, 0.4),

  # Information fractions at IA1, IA2, FA
  info_frac = c(0.4, 0.6, 1),

  # Futility threshold for conditional power at IA1
  futility_threshold = 0.2,

  # Influence threshold at IA2
  influence = 0.1,

  # Interaction threshold at IA2
  interaction = 1.3,

  # One-sided alpha level
  alpha = 0.025,

  # Number of simulations
  nsims = 1000
)

# Time-to-event case parameters
timeToEventCase = list(

  # Endpoint type
  endpoint_type = "Time-to-event",

  # Number of enrolled patients (control, treatment) 
  sample_size = c(230, 230),

  # Prevalence of biomarker-positive patients in the overall population
  prevalence = 0.4,

  # Annual patient dropout rate
  dropout_rate = 0.05,

  # Median times in the control arm (biomarker-negative, biomarker-positive)
  control_time = c(7.5, 7.5),

  # Median times in the treatment arm (biomarker-negative, biomarker-positive)
  treatment_time = c(9.5, 11),

  # Information fractions at IA1, IA2, FA
  info_frac = c(0.4, 0.6, 1),

  # Target event count at FA (overall, biomarker-positive)
  event_count = c(300, 150),

  # Futility threshold for conditional power at IA1
  futility_threshold = 0.1,

  # Influence threshold at IA2
  influence = 0.1,

  # Interaction threshold at IA2
  interaction = 1.3,

  # Enrollment period
  enrollment_period = 12,

  # Median enrollment time
  enrollment_parameter = 8,

  # One-sided alpha level
  alpha = 0.025,

  # Number of simulations
  nsims = 1000
)

context("ModuleC - Success runs")

test_that("Success run ModuleC with Normal case", {

  # Success run
  results = ModuleC(
    list(
        sample_size = normalCase$sample_size,
        prevalence = normalCase$prevalence,
        endpoint_type = normalCase$endpoint_type,
        control_mean = normalCase$control_mean,
        control_sd = normalCase$control_sd,
        treatment_mean = normalCase$treatment_mean,
        treatment_sd = normalCase$treatment_sd,
        info_frac = normalCase$info_frac,
        futility_threshold = normalCase$futility_threshold,
        influence = normalCase$influence,
        interaction = normalCase$interaction,
        dropout_rate = normalCase$dropout_rate#,
        # missing, use default 0.025
        #alpha = normalCase$alpha,
        # missing, use default 1000
        #nsims = 1000 # normalCase$nsims
    )
  )
  expect_is(results, "ModuleCResults")

  expect_type(  results$sim_results, "double")
  expect_length(results$sim_results, 19000)
  
  expect_type(    results$sim_summary, "list")
  expect_true(abs(results$sim_summary$futility - 0.2) < 0.2)
  
  expect_is(      results$sim_summary$trad_power, "numeric")
  expect_length(  results$sim_summary$trad_power, 2)
  expect_true(abs(results$sim_summary$trad_power[1] - 0.5) < 0.1)
  expect_true(abs(results$sim_summary$trad_power[2] - 0.4) < 0.1)

  expect_is(      results$sim_summary$ad_power, "numeric")
  expect_length(  results$sim_summary$ad_power, 3)
  expect_true(abs(results$sim_summary$ad_power[1] - 0.4) < 0.1)
  expect_true(abs(results$sim_summary$ad_power[2] - 0.3) < 0.1)
  expect_true(abs(results$sim_summary$ad_power[3] - 0.5) < 0.1)

  expect_is(      results$sim_summary$hypothesis_selection, "numeric")
  expect_length(  results$sim_summary$hypothesis_selection, 3)
  expect_true(abs(results$sim_summary$hypothesis_selection[1] - 0.3) < 0.1)
  expect_true(abs(results$sim_summary$hypothesis_selection[2] - 0.3) < 0.1)
  expect_true(abs(results$sim_summary$hypothesis_selection[3] - 0.3) < 0.1)

  expect_is(      results$sim_summary$look_time, "numeric")
  expect_length(  results$sim_summary$look_time, 4)
  expect_true(abs(results$sim_summary$look_time[1] - 0) < 0.1)
  expect_true(abs(results$sim_summary$look_time[2] - 0) < 0.1)
  expect_true(is.nan(results$sim_summary$look_time[3]))
  expect_true(is.nan(results$sim_summary$look_time[4]))

  # Check for report generation
  ModuleCReport(results, tempfile(fileext = ".docx"))
})

test_that("Success run ModuleC with Binary case", {

  # Success run
  results = ModuleC(binaryCase)
  expect_is(results, "ModuleCResults")

  expect_type(  results$sim_results, "double")
  expect_length(results$sim_results, 19000)
  
  expect_type(    results$sim_summary, "list")
  expect_true(abs(results$sim_summary$futility - 0.2) < 0.2)
  
  expect_is(      results$sim_summary$trad_power, "numeric")
  expect_length(  results$sim_summary$trad_power, 2)
  expect_true(
    abs(results$sim_summary$trad_power[1] - 0.9) < 0.1,
    info = paste0("trad_power[1] is out of range (",results$sim_summary$trad_power[1],")"))
  expect_true(
    abs(results$sim_summary$trad_power[2] - 0.8) < 0.1,
    info = paste0("trad_power[2] is out of range (",results$sim_summary$trad_power[2],")"))

  expect_is(      results$sim_summary$ad_power, "numeric")
  expect_length(  results$sim_summary$ad_power, 3)
  expect_true(
    abs(results$sim_summary$ad_power[1] - 0.8) < 0.1,
    info = paste0("ad_power[1] is out of range (",results$sim_summary$ad_power[1],")"))
  expect_true(
    abs(results$sim_summary$ad_power[2] - 0.7) < 0.2,
    info = paste0("ad_power[2] is out of range (",results$sim_summary$ad_power[2],")"))
  expect_true(
    abs(results$sim_summary$ad_power[3] - 0.8) < 0.1,
    info = paste0("ad_power[3] is out of range (",results$sim_summary$ad_power[3],")"))

  expect_is(      results$sim_summary$hypothesis_selection, "numeric")
  expect_length(  results$sim_summary$hypothesis_selection, 3)
  expect_true(
    abs(results$sim_summary$hypothesis_selection[1] - 0.3) < 0.1,
    info = paste0("hypothesis_selection[1] is out of range (",results$sim_summary$hypothesis_selection[1],")"))
  expect_true(
    abs(results$sim_summary$hypothesis_selection[2] - 0.1) < 0.1,
    info = paste0("hypothesis_selection[2] is out of range (",results$sim_summary$hypothesis_selection[2],")"))
  expect_true(
    abs(results$sim_summary$hypothesis_selection[3] - 0.6) < 0.1,
    info = paste0("hypothesis_selection[3] is out of range (",results$sim_summary$hypothesis_selection[3],")"))

  expect_is(      results$sim_summary$look_time, "numeric")
  expect_length(  results$sim_summary$look_time, 4)
  expect_true(abs(results$sim_summary$look_time[1] - 0) < 0.1)
  expect_true(abs(results$sim_summary$look_time[2] - 0) < 0.1)
  expect_true(is.nan(results$sim_summary$look_time[3]))
  expect_true(is.nan(results$sim_summary$look_time[4]))

  # Check for report generation
  ModuleCReport(results, tempfile(fileext = ".docx"))
})

test_that("Success run ModuleC with Time-to-event case", {

  # Success run
  results = ModuleC(timeToEventCase)
  expect_is(results, "ModuleCResults")

  expect_type(  results$sim_results, "double")
  expect_length(results$sim_results, 19000)
  
  expect_type(    results$sim_summary, "list")
  expect_true(abs(results$sim_summary$futility - 0.2) < 0.2)
  
  expect_is(      results$sim_summary$trad_power, "numeric")
  expect_length(  results$sim_summary$trad_power, 2)
  expect_true(
    abs(results$sim_summary$trad_power[1] - 0.65) < 0.1,
    info = paste0("trad_power[1] is out of range (",results$sim_summary$trad_power[1],")"))
  expect_true(
    abs(results$sim_summary$trad_power[2] - 0.6) < 0.1,
    info = paste0("trad_power[2] is out of range (",results$sim_summary$trad_power[2],")"))

  expect_is(      results$sim_summary$ad_power, "numeric")
  expect_length(  results$sim_summary$ad_power, 3)
  expect_true(
    abs(results$sim_summary$ad_power[1] - 0.5) < 0.1,
    info = paste0("ad_power[1] is out of range (",results$sim_summary$ad_power[1],")"))
  expect_true(
    abs(results$sim_summary$ad_power[2] - 0.4) < 0.1,
    info = paste0("ad_power[2] is out of range (",results$sim_summary$ad_power[2],")"))
  expect_true(
    abs(results$sim_summary$ad_power[3] - 0.65) < 0.1,
    info = paste0("ad_power[3] is out of range (",results$sim_summary$ad_power[3],")"))

  expect_is(      results$sim_summary$hypothesis_selection, "numeric")
  expect_length(  results$sim_summary$hypothesis_selection, 3)
  expect_true(
    abs(results$sim_summary$hypothesis_selection[1] - 0.3) < 0.2,
    info = paste0("hypothesis_selection[1] is out of range (",results$sim_summary$hypothesis_selection[1],")"))
  expect_true(
    abs(results$sim_summary$hypothesis_selection[2] - 0.25) < 0.1,
    info = paste0("hypothesis_selection[2] is out of range (",results$sim_summary$hypothesis_selection[2],")"))
  expect_true(
    abs(results$sim_summary$hypothesis_selection[3] - 0.4) < 0.1,
    info = paste0("hypothesis_selection[3] is out of range (",results$sim_summary$hypothesis_selection[3],")"))

  expect_is(      results$sim_summary$look_time, "numeric")
  expect_length(  results$sim_summary$look_time, 4)

  # Check for report generation
  ModuleCReport(results, tempfile(fileext = ".docx"))
})

test_that("Default values for dropout rate check ModuleC", {

  # Success run
  results = ModuleC(
      list(
        sample_size = normalCase$sample_size,
        prevalence = normalCase$prevalence,
        endpoint_type = normalCase$endpoint_type,
        control_mean = normalCase$control_mean,
        control_sd = normalCase$control_sd,
        treatment_mean = normalCase$treatment_mean,
        treatment_sd = normalCase$treatment_sd,
        info_frac = normalCase$info_frac,
        futility_threshold = normalCase$futility_threshold,
        influence = normalCase$influence,
        interaction = normalCase$interaction,
        # missing, use default 0
        #dropout_rate = 0, #normalCase$dropout_rate,
        alpha = normalCase$alpha,
        nsims = 100
      )
  )
  expect_is(results, "ModuleCResults")

  expect_type(  results$sim_results, "double")
  expect_length(results$sim_results, 1900)
  
  expect_type(    results$sim_summary, "list")

  expect_is(      results$sim_summary$trad_power, "numeric")
  expect_length(  results$sim_summary$trad_power, 2)
  expect_true(abs(results$sim_summary$trad_power[1] - 0.5) < 0.3)
  expect_true(abs(results$sim_summary$trad_power[2] - 0.4) < 0.3)

  expect_is(      results$sim_summary$ad_power, "numeric")
  expect_length(  results$sim_summary$ad_power, 3)

  expect_is(      results$sim_summary$hypothesis_selection, "numeric")
  expect_length(  results$sim_summary$hypothesis_selection, 3)

  expect_is(      results$sim_summary$look_time, "numeric")
  expect_length(  results$sim_summary$look_time, 4)
})

context("ModuleC - Error checks")

test_that("Input parameters errors check ModuleC", {

  # Errors check
  expect_error(
    ModuleC(
      c("Not a list")
    ),
    info = "Checking for wrong parameters collection type"
  )

  expect_error(
    ModuleC(
      list(
        sample_size = normalCase$sample_size,
        prevalence = normalCase$prevalence,
        # missing
        #endpoint_type = normalCase$endpoint_type,
        control_mean = normalCase$control_mean,
        control_sd = normalCase$control_sd,
        treatment_mean = normalCase$treatment_mean,
        treatment_sd = normalCase$treatment_sd,
        info_frac = normalCase$info_frac,
        futility_threshold = normalCase$futility_threshold,
        influence = normalCase$influence,
        interaction = normalCase$interaction,
        dropout_rate = normalCase$dropout_rate,
        alpha = normalCase$alpha,
        nsims = normalCase$nsims
      )
    ),
    info = "Checking for missing endpoint type"
  )

  expect_error(
    ModuleC(
      list(
        sample_size = normalCase$sample_size,
        prevalence = normalCase$prevalence,
        # wrong
        endpoint_type = "WrongType", # normalCase$endpoint_type,
        control_mean = normalCase$control_mean,
        control_sd = normalCase$control_sd,
        treatment_mean = normalCase$treatment_mean,
        treatment_sd = normalCase$treatment_sd,
        info_frac = normalCase$info_frac,
        futility_threshold = normalCase$futility_threshold,
        influence = normalCase$influence,
        interaction = normalCase$interaction,
        dropout_rate = normalCase$dropout_rate,
        alpha = normalCase$alpha,
        nsims = normalCase$nsims
      )
    ),
    info = "Checking for wrong endpoint type"
  )

  expect_error(
    ModuleC(
      list(
        sample_size = normalCase$sample_size,
        prevalence = normalCase$prevalence,
        endpoint_type = normalCase$endpoint_type,
        control_mean = normalCase$control_mean,
        control_sd = normalCase$control_sd,
        treatment_mean = normalCase$treatment_mean,
        treatment_sd = normalCase$treatment_sd,
        # missing
        #info_frac = normalCase$info_frac,
        futility_threshold = normalCase$futility_threshold,
        influence = normalCase$influence,
        interaction = normalCase$interaction,
        dropout_rate = normalCase$dropout_rate,
        alpha = normalCase$alpha,
        nsims = normalCase$nsims
      )
    ),
    info = "Checking for missing information fractions at IA1, IA2, FA"
  )

  expect_error(
    ModuleC(
      list(
        sample_size = normalCase$sample_size,
        prevalence = normalCase$prevalence,
        endpoint_type = normalCase$endpoint_type,
        control_mean = normalCase$control_mean,
        control_sd = normalCase$control_sd,
        treatment_mean = normalCase$treatment_mean,
        treatment_sd = normalCase$treatment_sd,
        # wrong
        info_frac = c(0.8, 0.6, 1.0), # normalCase$info_frac = c(0.4, 0.6, 1.0),
        futility_threshold = normalCase$futility_threshold,
        influence = normalCase$influence,
        interaction = normalCase$interaction,
        dropout_rate = normalCase$dropout_rate,
        alpha = normalCase$alpha,
        nsims = normalCase$nsims
      )
    ),
    info = "Checking for error information fractions (IA1 must be < IA2)"
  )

  expect_error(
    ModuleC(
      list(
        sample_size = normalCase$sample_size,
        prevalence = normalCase$prevalence,
        endpoint_type = normalCase$endpoint_type,
        control_mean = normalCase$control_mean,
        control_sd = normalCase$control_sd,
        treatment_mean = normalCase$treatment_mean,
        treatment_sd = normalCase$treatment_sd,
        # wrong
        info_frac = c(0.4, 1.0, 0.9), # normalCase$info_frac = c(0.4, 0.6, 1.0),
        futility_threshold = normalCase$futility_threshold,
        influence = normalCase$influence,
        interaction = normalCase$interaction,
        dropout_rate = normalCase$dropout_rate,
        alpha = normalCase$alpha,
        nsims = normalCase$nsims
      )
    ),
    info = "Checking for error information fractions (IA2 must be < FA)"
  )

  expect_error(
    ModuleC(
      list(
        sample_size = normalCase$sample_size,
        prevalence = normalCase$prevalence,
        endpoint_type = normalCase$endpoint_type,
        control_mean = normalCase$control_mean,
        control_sd = normalCase$control_sd,
        treatment_mean = normalCase$treatment_mean,
        treatment_sd = normalCase$treatment_sd,
        # wrong
        info_frac = c(0.4, 0.6, 0.9), # normalCase$info_frac = c(0.4, 0.6, 1.0),
        futility_threshold = normalCase$futility_threshold,
        influence = normalCase$influence,
        interaction = normalCase$interaction,
        dropout_rate = normalCase$dropout_rate,
        alpha = normalCase$alpha,
        nsims = normalCase$nsims
      )
    ),
    info = "Checking for error information fractions (FA must be 1)"
  )

  expect_error(
    ModuleC(
      list(
        sample_size = normalCase$sample_size,
        prevalence = normalCase$prevalence,
        endpoint_type = normalCase$endpoint_type,
        control_mean = normalCase$control_mean,
        control_sd = normalCase$control_sd,
        treatment_mean = normalCase$treatment_mean,
        treatment_sd = normalCase$treatment_sd,
        # wrong
        info_frac = c(0.1, 0.4, 0.6, 1.0), # c(0.4, 0.6, 1.0) = normalCase$info_frac,
        futility_threshold = normalCase$futility_threshold,
        influence = normalCase$influence,
        interaction = normalCase$interaction,
        dropout_rate = normalCase$dropout_rate,
        alpha = normalCase$alpha,
        nsims = normalCase$nsims
      )
    ),
    info = "Checking for wrong information fractions at IA1, IA2, FA (incorrect value size)"
  )

  expect_error(
    ModuleC(
      list(
        sample_size = normalCase$sample_size,
        prevalence = normalCase$prevalence,
        endpoint_type = normalCase$endpoint_type,
        control_mean = normalCase$control_mean,
        control_sd = normalCase$control_sd,
        treatment_mean = normalCase$treatment_mean,
        treatment_sd = normalCase$treatment_sd,
        # wrong
        info_frac = c(0, 0.6, 1.0), # c(0.4, 0.6, 1.0) = normalCase$info_frac,
        futility_threshold = normalCase$futility_threshold,
        influence = normalCase$influence,
        interaction = normalCase$interaction,
        dropout_rate = normalCase$dropout_rate,
        alpha = normalCase$alpha,
        nsims = normalCase$nsims
      )
    ),
    info = "Checking for wrong information fractions at IA1, IA2, FA (incorrect value <= 0)"
  )

  expect_error(
    ModuleC(
      list(
        sample_size = normalCase$sample_size,
        prevalence = normalCase$prevalence,
        endpoint_type = normalCase$endpoint_type,
        control_mean = normalCase$control_mean,
        control_sd = normalCase$control_sd,
        treatment_mean = normalCase$treatment_mean,
        treatment_sd = normalCase$treatment_sd,
        # wrong
        info_frac = c(0.4, 0.6, 1.1), # c(0.4, 0.6, 1.0) = normalCase$info_frac,
        futility_threshold = normalCase$futility_threshold,
        influence = normalCase$influence,
        interaction = normalCase$interaction,
        dropout_rate = normalCase$dropout_rate,
        alpha = normalCase$alpha,
        nsims = normalCase$nsims
      )
    ),
    info = "Checking for wrong information fractions at IA1, IA2, FA (incorrect value > 1)"
  )

  testParameterErrors = function(params, paramName, paramDesc, 
    checkMissing = TRUE, checkSize = TRUE, checkMin = NA, checkMax = NA) {

    func = ModuleC

    paramDesc = paste0(paramDesc, " (", paramName, ")")
    if (!is.null(params$endpoint_type)) 
      paramDesc = paste0(params$endpoint_type, ": ", paramDesc)
    # Missing
    if (checkMissing) {
      testParams = params
      testParams[paramName] <- NULL
      expect_error(func(testParams), 
        info = paste0("Checking for missing ", paramDesc))
    }
    # Check size
    if (checkSize) {
      testParams = params
      testParams[[paramName]] <- append(testParams[[paramName]], testParams[[paramName]][1])
      expect_error(func(testParams), 
        info = paste0("Checking for wrong ", paramDesc, " (incorrect value size)"))
    }
    # Check below min value
    if (!is.null(checkMin) && !is.na(checkMin)) {
      testParams = params
      testParams[[paramName]][1] <- checkMin
      expect_error(func(testParams), 
        info = paste0("Checking for wrong ", paramDesc, " (incorrect value < min)"))
    }
    # Check under max value
    if (!is.null(checkMax) && !is.na(checkMax)) {
      testParams = params
      testParams[[paramName]][length(testParams[[paramName]])] <- checkMax
      expect_error(func(testParams), 
        info = paste0("Checking for wrong ", paramDesc, " (incorrect value > max)"))
    }
  }

  testParameterErrors(normalCase, 
    'sample_size', 
    'Number of enrolled patients',
    checkMissing = TRUE,
    checkSize = TRUE,
    checkMin = 0,
    checkMax = 1001)

  testParameterErrors(normalCase, 
    'prevalence', 
    'Prevalence of biomarker-positive patients',
    checkMissing = TRUE,
    checkSize = TRUE,
    checkMin = 0,
    checkMax = 0.999)

  testParameterErrors(normalCase, 
    'futility_threshold', 
    'Futility threshold at IA1',
    checkMissing = TRUE,
    checkSize = TRUE,
    checkMin = -0.001,
    checkMax = 0.999)

  testParameterErrors(normalCase, 
    'influence', 
    'Influence threshold at IA2',
    checkMissing = TRUE,
    checkSize = TRUE,
    checkMin = 0,
    checkMax = 0.999)

  testParameterErrors(normalCase, 
    'interaction', 
    'Interaction threshold at IA2',
    checkMissing = TRUE,
    checkSize = TRUE,
    checkMin = 1,
    checkMax = 2)

  testParameterErrors(normalCase, 
    'dropout_rate', 
    'Patient dropout rate',
    checkMissing = FALSE,
    checkSize = TRUE,
    checkMin = -0.001,
    checkMax = 1)

  testParameterErrors(normalCase, 
    'nsims', 
    'Number of simulations',
    checkMissing = FALSE,
    checkSize = TRUE,
    checkMin = 0,
    checkMax = 10001)

  testParameterErrors(normalCase, 
    'alpha', 
    'One-sided Type I error rate',
    checkMissing = FALSE,
    checkSize = TRUE,
    checkMin = 0.001,
    checkMax = 0.5)

  testParameterErrors(normalCase, 
    'control_mean', 
    'Mean effects in the control arm',
    checkMissing = TRUE,
    checkSize = TRUE,
    checkMin = NA,
    checkMax = NA)

  testParameterErrors(normalCase, 
    'treatment_mean', 
    'Mean effects in the treatment arm',
    checkMissing = TRUE,
    checkSize = TRUE,
    checkMin = NA,
    checkMax = NA)

  testParameterErrors(normalCase, 
    'control_sd', 
    'Standard deviations in the control arm',
    checkMissing = TRUE,
    checkSize = TRUE,
    checkMin = 0,
    checkMax = NA)

  testParameterErrors(normalCase, 
    'treatment_sd', 
    'Standard deviations in the treatment arm',
    checkMissing = TRUE,
    checkSize = TRUE,
    checkMin = 0,
    checkMax = NA)

  testParameterErrors(binaryCase, 
    'control_rate', 
    'Response rates in the control arm',
    checkMissing = TRUE,
    checkSize = TRUE,
    checkMin = 0,
    checkMax = 1)

  testParameterErrors(binaryCase, 
    'treatment_rate', 
    'Responses rate in the treatment arm',
    checkMissing = TRUE,
    checkSize = TRUE,
    checkMin = 0,
    checkMax = 1)

  testParameterErrors(timeToEventCase, 
    'control_time', 
    'Median times in the control arm',
    checkMissing = TRUE,
    checkSize = TRUE,
    checkMin = 0,
    checkMax = NA)

  testParameterErrors(timeToEventCase, 
    'treatment_time', 
    'Median times in the treatment arm',
    checkMissing = TRUE,
    checkSize = TRUE,
    checkMin = 0,
    checkMax = NA)

  testParameterErrors(timeToEventCase, 
    'enrollment_period', 
    'Patient enrollment period',
    checkMissing = TRUE,
    checkSize = TRUE,
    checkMin = 0,
    checkMax = NA)

  testParameterErrors(timeToEventCase, 
    'enrollment_parameter', 
    'Median enrollment time',
    checkMissing = TRUE,
    checkSize = TRUE,
    checkMin = 0,
    checkMax = timeToEventCase$enrollment_period)

  testParameterErrors(timeToEventCase, 
    'event_count', 
    'Target number of events at FA',
    checkMissing = TRUE,
    checkSize = TRUE,
    checkMin = 0,
    checkMax = sum(timeToEventCase$sample_size))

})

test_that("Input parameters errors check ModuleCReport", {

  expect_error(
    ModuleCReport(""),
    info = "Checking for wrong parameter type for report generator"
  )

})
