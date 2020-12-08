ModuleB = function(parameters) {

  # Error checks

  if (typeof(parameters) != "list") stop("Function parameters must be a list of named values.", call. = FALSE)

  if (is.null(parameters$endpoint_type)) stop("Endpoint type (endpoint_type): Value must be specified.", call. = FALSE)

  if (!tolower(parameters$endpoint_type) %in% tolower(endpoint_list)) stop("Endpoint type (endpoint_type): Value must be Normal, Binary or Time-to-event.", call. = FALSE)

  parameters$endpoint_index = 1  

  for (i in 1:length(endpoint_list)) {
      if (tolower(endpoint_list[i]) == tolower(parameters$endpoint_type)) parameters$endpoint_index = i
  }   

  endpoint_index = parameters$endpoint_index

  if (is.null(parameters$sample_size)) stop("Number of enrolled patients (sample_size): Value must be specified.", call. = FALSE)

  sample_size = ContinuousErrorCheck(parameters$sample_size, 
                                     NA, 
                                     lower_values = 0,
                                     lower_values_sign = ">",
                                     upper_values = 1000,
                                     upper_values_sign = "<=",
                                     "Number of enrolled patients (sample_size)",
                                     NA,
                                     "int",
                                     NA) 

  narms = length(sample_size)

  if (is.null(parameters$info_frac)) stop("Information fractions at IA1, IA2, FA (info_frac): Value must be specified.", call. = FALSE)

  info_frac = ContinuousErrorCheck(parameters$info_frac, 
                                     3, 
                                     lower_values = 0,
                                     lower_values_sign = ">",
                                     upper_values = 1,
                                     upper_values_sign = "<=",
                                     "Information fractions at IA1, IA2, FA (info_frac)",
                                     NA,
                                     "double",
                                     NA) 

  if (info_frac[1] >= info_frac[2]) stop("Information fractions at IA1, IA2, FA (info_frac): Information fraction at IA1 must be less than Information fraction at IA2.", call. = FALSE)

  if (info_frac[2] >= info_frac[3]) stop("Information fractions at IA1, IA2, FA (info_frac): Information fraction at IA2 must be less than Information fraction at FA", call. = FALSE)

  if (info_frac[3] != 1) stop("Information fractions at IA1, IA2, FA (info_frac): Information fraction at FA must be equal to 1.", call. = FALSE)
  
  if (is.null(parameters$futility_threshold)) stop("Futility threshold at IA1 (futility_threshold): Value must be specified.", call. = FALSE)

  futility_threshold = 
        ContinuousErrorCheck(parameters$futility_threshold, 
                             1, 
                             lower_values = c(0),
                             lower_values_sign = c(">="),
                             upper_values = c(0.999),
                             upper_values_sign = c("<"),
                             "Futility threshold at IA1 (futility_threshold)",
                             c("Value"),
                             "double",
                             NA) 

  if (!is.null(parameters$dropout_rate)) {

    dropout_rate = 
      ContinuousErrorCheck(parameters$dropout_rate, 
                           1, 
                           lower_values = c(0),
                           lower_values_sign = c(">="),
                           upper_values = c(1),
                           upper_values_sign = c("<"),
                           "Patient dropout rate (dropout_rate)",
                           c("Value"),
                           "double",
                           NA) 

  } else {
    parameters$dropout_rate = 0
  }

  if (!is.null(parameters$nsims)) {

    nsims = 
      ContinuousErrorCheck(parameters$nsims, 
                           1, 
                           lower_values = c(1),
                           lower_values_sign = c(">="),
                           upper_values = c(10000),
                           upper_values_sign = c("<="),
                           "Number of simulations (nsims)",
                           c("Value"),
                           "int",
                           NA) 

  } else {
    parameters$nsims = 1000
  }

  if (!is.null(parameters$alpha)) {

    alpha = 
          ContinuousErrorCheck(parameters$alpha, 
                               1, 
                               lower_values = c(0.001),
                               lower_values_sign = c(">"),
                               upper_values = c(0.5),
                               upper_values_sign = c("<"),
                               "One-sided Type I error rate (alpha)",
                               c("Value"),
                               "double",
                               NA) 
  } else {
    parameters$alpha = 0.025
  }

  # Treatment effect assumptions 

  if (endpoint_index == 1) {

    if (is.null(parameters$control_mean)) stop("Mean effect in the control arm (control_mean): Value must be specified.", call. = FALSE)

    control_mean = 
          ContinuousErrorCheck(parameters$control_mean, 
                               1, 
                               lower_values = c(NA),
                               lower_values_sign = c(NA),
                               upper_values = c(NA),
                               upper_values_sign = c(NA),
                               "Mean effect in the control arm (control_mean)",
                               c("Value"),
                               "double",
                               NA) 

    if (is.null(parameters$treatment_mean)) stop("Mean effects in the treatment arms (treatment_mean): Value must be specified.", call. = FALSE)

    treatment_mean = 
          ContinuousErrorCheck(parameters$treatment_mean, 
                               narms - 1, 
                               lower_values = c(NA),
                               lower_values_sign = c(NA),
                               upper_values = c(NA),
                               upper_values_sign = c(NA),
                               "Mean effects in the treatment arms (treatment_mean)",
                               c("Value"),
                               "double",
                               NA) 

    if (is.null(parameters$control_sd)) stop("Standard deviation in the control arm (control_sd): Value must be specified.", call. = FALSE)

    control_sd = 
          ContinuousErrorCheck(parameters$control_sd, 
                               1, 
                               lower_values = c(0),
                               lower_values_sign = c(">"),
                               upper_values = c(NA),
                               upper_values_sign = c(NA),
                               "Standard deviation in the control arm (control_sd)",
                               c("Value"),
                               "double",
                               NA) 


    if (is.null(parameters$treatment_sd)) stop("Standard deviations in the treatment arms (treatment_sd): Value must be specified.", call. = FALSE)

    treatment_sd = 
          ContinuousErrorCheck(parameters$treatment_sd, 
                               narms - 1, 
                               lower_values = c(0),
                               lower_values_sign = c(">"),
                               upper_values = c(NA),
                               upper_values_sign = c(NA),
                               "Standard deviations in the treatment arms (treatment_sd)",
                               c("Value"),
                               "double",
                               NA) 

  }

  if (endpoint_index == 2) {

    if (is.null(parameters$control_rate)) stop("Response rate in the control arm (control_rate): Value must be specified.", call. = FALSE)

    control_rate = 
          ContinuousErrorCheck(parameters$control_rate, 
                               1, 
                               lower_values = c(0),
                               lower_values_sign = c(">"),
                               upper_values = c(1),
                               upper_values_sign = c("<"),
                               "Response rate in the control arm (control_rate)",
                               c("Value"),
                               "double",
                               NA) 

    if (is.null(parameters$treatment_rate)) stop("Response rates in the treatment arms (treatment_rate): Value must be specified.", call. = FALSE)

    treatment_rate = 
          ContinuousErrorCheck(parameters$treatment_rate, 
                               narms - 1, 
                               lower_values = c(0),
                               lower_values_sign = c(">"),
                               upper_values = c(1),
                               upper_values_sign = c("<"),
                               "Responses rate in the treatment arms (treatment_rate)",
                               c("Value"),
                               "double",
                               NA) 

  }

  if (endpoint_index == 3) {

    if (is.null(parameters$control_time)) stop("Median time in the control arm (control_time): Value must be specified.", call. = FALSE)

    control_time = 
          ContinuousErrorCheck(parameters$control_time, 
                               1, 
                               lower_values = c(0),
                               lower_values_sign = c(">"),
                               upper_values = c(NA),
                               upper_values_sign = c(NA),
                               "Median time in the control arm (control_time)",
                               c("Value"),
                               "double",
                               NA) 

    if (is.null(parameters$treatment_time)) stop("Median times in the treatment arms (treatment_time): Value must be specified.", call. = FALSE)

    treatment_time = 
          ContinuousErrorCheck(parameters$treatment_time, 
                               narms - 1, 
                               lower_values = c(0),
                               lower_values_sign = c(">"),
                               upper_values = c(NA),
                               upper_values_sign = c(NA),
                               "Median times in the treatment arms (treatment_time)",
                               c("Value"),
                               "double",
                               NA) 

    if (is.null(parameters$enrollment_period)) stop("Patient enrollment period (enrollment_period): Value must be specified.", call. = FALSE)

    enrollment_period = 
          ContinuousErrorCheck(parameters$enrollment_period, 
                               1, 
                               lower_values = c(0),
                               lower_values_sign = c(">"),
                               upper_values = c(NA),
                               upper_values_sign = c(NA),
                               "Patient enrollment period (enrollment_period)",
                               c("Value"),
                               "double",
                               NA)           

    if (is.null(parameters$enrollment_parameter)) stop("Median enrollment time (enrollment_parameter): Value must be specified.", call. = FALSE)

    enrollment_parameter = 
          ContinuousErrorCheck(parameters$enrollment_parameter, 
                               1, 
                               lower_values = c(0),
                               lower_values_sign = c(">"),
                               upper_values = c(parameters$enrollment_period),
                               upper_values_sign = c("<"),
                               "Median enrollment time (enrollment_parameter)",
                               c("Value"),
                               "double",
                               NA)   

    if (is.null(parameters$event_count)) stop("Target number of events at FA (event_count): Value must be specified.", call. = FALSE)

    event_count = 
          ContinuousErrorCheck(parameters$event_count, 
                               1, 
                               lower_values = c(0),
                               lower_values_sign = c(">"),
                               upper_values = c(sum(parameters$sample_size)),
                               upper_values_sign = c("<"),
                               "Target number of events at FA (event_count)",
                               c("Value"),
                               "int",
                               NA)   

  }

  #############################################

  parameters$means = 0
  parameters$sds = 0
  parameters$rates = 0
  parameters$hazard_rates = 0
  parameters$dropout_parameter = 0
  parameters$enrollment_distribution = 2
  parameters$sample_size_ia1 = 0
  parameters$sample_size_ia2 = 0
  parameters$sample_size_fa = 1
  parameters$event_count_ia1 = 0
  parameters$event_count_ia2 = 0
  parameters$event_count_fa = 0
  parameters$max_sample_size = max(parameters$sample_size)

  # All means and SDs
  if (endpoint_index == 1) {
    parameters$means = c(parameters$control_mean, parameters$treatment_mean)
    parameters$sds = c(parameters$control_sd, parameters$treatment_sd)
    parameters$enrollment_period = 0
    parameters$enrollment_parameter = 0

  }

  # All rates
  if (endpoint_index == 2) {
    parameters$rates = c(parameters$control_rate, parameters$treatment_rate)
    parameters$enrollment_period = 0
    parameters$enrollment_parameter = 0
  }

  # All hazard rates
  if (endpoint_index == 3) {
    parameters$times = c(parameters$control_time, parameters$treatment_time)
    parameters$hazard_rates = log(2) / c(parameters$control_time, parameters$treatment_time)
    parameters$dropout_parameter = c(parameters$dropout_rate, dropout_period = 12)
    parameters$event_count_ia1 = floor(parameters$event_count * info_frac[1])
    parameters$event_count_ia2 = floor(parameters$event_count * info_frac[2])
    parameters$event_count_fa = floor(parameters$event_count * info_frac[3])
  }

  # Total sample size after accounting for dropout rates
  if (endpoint_index != 3) {
    parameters$sample_size_ia1 = floor(parameters$sample_size * (1 - parameters$dropout_rate) * info_frac[1])
    parameters$sample_size_ia2 = floor(parameters$sample_size * (1 - parameters$dropout_rate) * info_frac[2])
    parameters$sample_size_fa = floor(parameters$sample_size * (1 - parameters$dropout_rate) * info_frac[3])
  }

  ###########################################################

  # Run simulations to compute key characteristics

  simulations = ModuleBC(parameters)

  sim_results = simulations$sim_results

  sim_summary = list()

  nsims = parameters$nsims

  ad_outcome = sim_results[, 1]
  select_flag = sim_results[, 2]
  trad_outcome = sim_results[, 2 + (1:(narms - 1))]
  futility = sim_results[, 2 + narms - 1 + (1:(narms - 1))]

  sim_summary$ad_power = mean(ad_outcome)
  select = rep(narms)
  for (i in 1:(narms - 1)) select[i] = mean(select_flag == i - 1)
  select[narms] = mean(select_flag == -1)  
  sim_summary$select = select
  if (is.matrix(futility)) {
    sim_summary$futility = colMeans(futility) 
    sim_summary$overall_futility = mean(rowSums(futility) == narms - 1)
  } else {
    sim_summary$futility = mean(futility)
    sim_summary$overall_futility = mean(futility)
  }
  if (is.matrix(trad_outcome)) sim_summary$trad_power = colMeans(trad_outcome) else sim_summary$trad_power = mean(trad_outcome)

  results = list(parameters = parameters,
                 sim_results = sim_results,
                 sim_summary = sim_summary)

  class(results) = "ModuleBResults"

  return(results)

}    

ModuleBReportDoc = function(results) {

   #############################################################################

   # Error checks

   if (class(results) != "ModuleBResults") stop("The object was not created by the ModuleB function", call. = FALSE)

  #############################################################################

  statistics = c("Lower quartile", "Median", "Mean", "Upper quartile")

  #############################################################################

  # Empty list of tables to be included in the report
  item_list = list()
  item_index = 1
  table_index = 1
  figure_index = 1

  width = 6.5
  height = 5

  parameters = results$parameters
  sim_results = results$sim_results
  sim_summary = results$sim_summary
  endpoint_index = parameters$endpoint_index
  narms = length(parameters$sample_size)

  # Trial arms  
  trial_arms = "Control"
  if (narms >= 3) {
    for (i in 2:narms) trial_arms = c(trial_arms, paste0("Treatment ", i - 1))
  } else {
    trial_arms = c(trial_arms, "Treatment")
  }

  # All means and SDs
  if (endpoint_index == 1) {
    means = c(parameters$control_mean, parameters$treatment_mean)
    sds = c(parameters$control_sd, parameters$treatment_sd)
  }

  # All rates
  if (endpoint_index == 2) {
    rates = c(parameters$control_rate, parameters$treatment_rate)
  }

  # All hazard rates
  if (endpoint_index == 3) {
    times = c(parameters$control_time, parameters$treatment_time)
  }

  #############################################################################

  report_title = "Module B: Adaptive design with data-driven treatment arm selection"

  item_list[[item_index]] = list(type = "paragraph", label = "Description", value = "The simulation report presents key operating characteristics of an adaptive design for a multi-arm Phase III clinical trial with two interim analyses. The first interim analysis supports early stopping for futility and the second interim analysis enables adaptive treatment selection to identify the best performing treatment.")

  item_index = item_index + 1

  #############################################################################

  column_names = c("Trial arm", "Sample size")

  col1 = trial_arms
  col2 = parameters$sample_size

  data_frame = data.frame(col1, col2)
  title = paste0("Table ", table_index, ". Number of enrolled patients")

  column_width = c(5, 1.5)
  item_list[[item_index]] = CreateTable(data_frame, column_names, column_width, title, FALSE)
  item_index = item_index + 1
  table_index = table_index + 1

  #############################################################################

  column_names = c("Parameter", "Value")

  if (endpoint_index %in% c(1, 2)) col1 = c("Dropout rate at the end of the treatment period (%)")
  if (endpoint_index %in% c(3)) col1 = c("Annual dropout rate (%)")
  col2 = c(100 * parameters$dropout_rate)

  data_frame = data.frame(col1, col2)
  title = paste0("Table ", table_index, ". Other design parameters")

  column_width = c(5, 1.5)
  item_list[[item_index]] = CreateTable(data_frame, column_names, column_width, title, FALSE)
  item_index = item_index + 1
  table_index = table_index + 1

  #############################################################################

  column_names = c("Trial arm", "Parameter", "Value")

  col1 = NULL
  col2 = NULL
  col3 = NULL

  for (i in 1:narms) {

    if (endpoint_index == 1) {
      col1 = c(col1, trial_arms[i], "")
      col2 = c(col2, "Mean", "SD")
      col3 = c(col3, means[i], sds[i])
    }

    if (endpoint_index == 2) {
      col1 = c(col1, trial_arms[i])
      col2 = c(col2, "Rate (%)")
      col3 = c(col3, 100 * rates[i])
    }

    if (endpoint_index == 3) {
      col1 = c(col1, trial_arms[i])
      col2 = c(col2, "Median time")
      col3 = c(col3, times[i])
    }

  }

  data_frame = data.frame(col1, col2, col3)
  title = paste0("Table ", table_index, ". Treatment effect assumptions")

  column_width = c(2, 2, 2.5)
  item_list[[item_index]] = CreateTable(data_frame, column_names, column_width, title, FALSE)
  item_index = item_index + 1
  table_index = table_index + 1

  #############################################################################

  if (endpoint_index %in% c(1, 2)) column_names = c("Decision point", "Total number of patients", "Information fraction (%)")

  if (endpoint_index %in% c(3)) column_names = c("Decision point", "Total number of events", "Information fraction (%)")

  if (endpoint_index %in% c(1, 2)) {
    col1 = c("Interim analysis 1", 
           "Interim analysis 2",
           "Final analysis")
    col2 = c(sum(parameters$sample_size_ia1), sum(parameters$sample_size_ia2), sum(parameters$sample_size_fa))
    col3 = round(100 * parameters$info_frac[1:3], 1)    
  }
  if (endpoint_index %in% c(3)) {
    col1 = c("Interim analysis 1", 
           "Interim analysis 2",
           "Final analysis")
    col2 = c(parameters$event_count_ia1, parameters$event_count_ia2, parameters$event_count_fa)
    col3 = round(100 * parameters$info_frac[1:3], 1)    
  }

  data_frame = data.frame(col1, col2, col3)
  if (endpoint_index %in% c(1, 2)) title = paste0("Table ", table_index, ". Number of patients at the interim and final analyses")
  if (endpoint_index %in% c(3)) title = paste0("Table ", table_index, ". Number of events at the interim and final analyses")

  if (endpoint_index %in% c(1, 2)) footnote = "The number of patients at the interim and final analyses may be reduced due to patient dropout." else footnote = NULL 

  column_width = c(2.5, 2, 2)
  item_list[[item_index]] = CreateTable(data_frame, column_names, column_width, title, FALSE, footnote)
  item_index = item_index + 1
  table_index = table_index + 1

  #############################################################################

  column_names = c("Parameter", "PPS (%)")

  col1 = c("Futility threshold")
  col2 = 100 * c(parameters$futility_threshold)
  
  data_frame = data.frame(col1, col2)
  title = paste0("Table ", table_index, ". Decision rule at Interim analysis 1")

  footnote = "PPS: Predicted probability of success at Interim analysis 1. The trial will be stopped for futility at Interim analysis 1 if the predicted probability of success is less than the futility threshold."

  column_width = c(5, 1.5)
  item_list[[item_index]] = CreateTable(data_frame, column_names, column_width, title, FALSE, footnote)
  item_index = item_index + 1
  table_index = table_index + 1

  #############################################################################

  column_names = c("Parameter", "Value")

  col1 = c("One-sided Type I error rate", "Number of simulations")
  col2 = c(sprintf("%0.3f", sum(parameters$alpha)),
           sprintf("%d", parameters$nsims))

  data_frame = data.frame(col1, col2)
  title = paste0("Table ", table_index, ". Simulation parameters")

  column_width = c(5, 1.5)
  item_list[[item_index]] = CreateTable(data_frame, column_names, column_width, title, TRUE)
  item_index = item_index + 1
  table_index = table_index + 1

  #############################################################################

  column_names = c("Treatment arm", "Probability of futility stopping (%)")

  col1 = c(trial_arms[2:narms], "All treatments")
  col2 = c(round(100 * sim_summary$futility, 1), round(100 * sim_summary$overall_futility, 1))

  data_frame = data.frame(col1, col2)
  title = paste0("Table ", table_index, ". Simulation results: Futility stopping at Interim analysis 1")

  footnote = "Probability of dropping each treatment due to futility. The trial is terminated at Interim analysis 1 if all treatments are dropped."  

  column_width = c(2.5, 4)

  item_list[[item_index]] = CreateTable(data_frame, column_names, column_width, title, FALSE, footnote)
  item_index = item_index + 1
  table_index = table_index + 1

  #############################################################################

  column_names = c("Treatment arm", "Selection probability (%)")

  col1 = c(trial_arms[2:narms], "No treatment")
  col2 = round(100 * sim_summary$select, 1)

  data_frame = data.frame(col1, col2)
  title = paste0("Table ", table_index, ". Simulation results: Treatment selection at Interim analysis 2")

  footnote = "Probability that each treatment is selected at the best performing treatment for the final analysis. No treatment is selected if the trial is terminated at Interim analysis 1."  

  column_width = c(2.5, 4)

  item_list[[item_index]] = CreateTable(data_frame, column_names, column_width, title, FALSE, footnote)
  item_index = item_index + 1
  table_index = table_index + 1

  #############################################################################

  column_names = c("Treatment arm", "Power (%)")

  col1 = trial_arms[2:narms]
  col2 = round(100 * sim_summary$trad_power, 1)

  data_frame = data.frame(col1, col2)
  title = paste0("Table ", table_index, ". Simulation results: Traditional designs")

  footnote = "Power for two-arm traditional designs that compare each treatment to control."

  column_width = c(2.5, 4)
  item_list[[item_index]] = CreateTable(data_frame, column_names, column_width, title, FALSE, footnote)
  item_index = item_index + 1
  table_index = table_index + 1

  #############################################################################

  column_names = c("Design", "Power (%)")

  col1 = "Adaptive design"
  col2 = round(100 * sim_summary$ad_power, 1)

  data_frame = data.frame(col1, col2)
  title = paste0("Table ", table_index, ". Simulation results: Adaptive design")

  column_width = c(2.5, 4)

  item_list[[item_index]] = CreateTable(data_frame, column_names, column_width, title, FALSE)
  item_index = item_index + 1
  table_index = table_index + 1

  #############################################################################

  report = item_list

  doc = SaveReport(report, report_title)

  return(doc)

}
# End of ModuleBReportDoc

ModuleBReport = function(results, report_filename) {

  doc = ModuleBReportDoc(results)

  # Print the report
  print(doc, target = report_filename)          

}
