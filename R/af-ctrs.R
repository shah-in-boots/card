# This page is for the AF Composite Treatment Response Score

#' Atrial Fibrillation Composite Treatment Response Score
#'
#' This function calculates a composite score based on symptom burden and
#' electrical burden.
#'
#' @param symptom_type A `character` string indicating the type of symptom
#'   burden ("AFEQT" or "NYHA").
#'
#' @param afeqt_total A `numeric` value representing the total AFEQT score
#'   (0-100), if applicable.
#'
#' @param afeqt_symptoms A `numeric` value representing the AFEQT symptoms
#'   domain score (0-100), if applicable.
#'
#' @param afeqt_activities A `numeric` value representing the AFEQT daily
#'   activities domain score (0-100), if applicable.
#'
#' @param afeqt_treatment A `numeric` value representing the AFEQT treatment
#'   concerns domain score (0-100), if applicable.
#'
#' @param nyha_class A `character` string representing the NYHA class ("I",
#'   "II", "III", "IV"), if applicable.
#'
#' @param electrical_type A `character` string indicating the type of data used
#'   for electrical burden ("ECG" or "MCT").
#'
#' @param mct_burden A `numeric` value representing the percentage of time in AF
#'   from MCT data, if applicable.
#'
#' @param mct_duration An `integer` value representing the number of days of
#'   monitoring that occurred. For very short duration monitoring, please round
#'   to the nearest whole day (> 0).
#'
#' @param ecg_total A `integer` value representing the total number of ECGs, if
#'   applicable.
#'
#' @param ecg_sinus A `integer` value representing the number of ECGs in sinus
#'   rhythm, if applicable.
#'
#' @param ecg_time_range A `character` string representing the time range for
#'   ECG data (e.g., "6 months", "12 months").
#'
#' @return A `numeric` value representing the composite AF score.
#'
#' @examples
#' afib_score(
#'   symptom_type = "AFEQT", afeqt_total = 80, afeqt_symptoms = 70,
#'   afeqt_activities = 85, afeqt_treatment = 75,
#'   electrical_type = "MCT", mct_burden = 0.2
#' )
#'
#' afib_score(
#'   symptom_type = "NYHA", nyha_class = "III", electrical_type =
#'     "ECG", ecg_total = 10, ecg_sinus = 7, ecg_time_range = "12 months"
#' )
#'
#' @export
afib_score <- function(symptom_type = c("AFEQT", "NYHA"),
                       afeqt_total = NULL,
                       afeqt_symptoms = NULL,
                       afeqt_activities = NULL,
                       afeqt_treatment = NULL,
                       nyha_class = NULL,
                       electrical_type = c("MCT", "ECG"),
                       mct_burden = NULL,
                       mct_duration = NULL,
                       ecg_total = NULL,
                       ecg_sinus = NULL,
                       ecg_time_range = NULL) {
  symptom_type <- match.arg(symptom_type)
  electrical_type <- match.arg(electrical_type)

  # Retrieve current parameters from global options
  parameters <- getOption("afib_parameters")

  # Calculate symptom burden based on symptom_type
  symptomBurden <- 0
  if (symptom_type == "NYHA") {
    nyhaClass <- toupper(nyha_class)
    nyhaScale <- c("I" = 25, "II" = 50, "III" = 75, "IV" = 100)
    if (!nyhaClass %in% names(nyhaScale)) {
      stop("Invalid NYHA class. Please provide one of I, II, III, or IV.")
    }
    symptomBurden <- nyhaScale[nyhaClass] * parameters$weight_nyha_class
  } else if (symptom_type == "AFEQT") {
    if (!is.null(afeqt_total)) {
      symptomBurden <- symptomBurden + afeqt_total * parameters$weight_afeqt_total
    }
    if (!is.null(afeqt_symptoms)) {
      symptomBurden <- symptomBurden + afeqt_symptoms * parameters$weight_afeqt_symptoms
    }
    if (!is.null(afeqt_activities)) {
      symptomBurden <- symptomBurden + afeqt_activities * parameters$weight_afeqt_activities
    }
    if (!is.null(afeqt_treatment)) {
      symptomBurden <- symptomBurden + afeqt_treatment * parameters$weight_afeqt_treatment
    }
  } else {
    stop("Invalid symptom type. Please specify either 'AFEQT' or 'NYHA'.")
  }

  # Calculate electrical burden based on electrical_type
  # The data probably should be weighed against the time duration of monitoring
  # The "density" of monitoring also probably matters
  electricalBurden <- 0
  if (electrical_type == "MCT") {
    if (!is.null(mct_burden)) {
      if (mct_burden < parameters$mct_cutoff_low) {
        electricalBurden <- 1
      } else if (mct_burden < parameters$mct_cutoff_medium) {
        electricalBurden <- 2
      } else {
        electricalBurden <- 3
      }
    } else {
      stop("MCT burden data is required for MCT electrical_type.")
    }
  } else if (electrical_type == "ECG") {
    if (!is.null(ecg_total) && !is.null(ecg_sinus) && !is.null(ecg_time_range)) {
      ecg_burden <- ecg_sinus / ecg_total
      if (ecg_burden < parameters$ecg_cutoff_low) {
        electricalBurden <- 1
      } else if (ecg_burden < parameters$ecg_cutoff_medium) {
        electricalBurden <- 2
      } else {
        electricalBurden <- 3
      }
    } else {
      stop("ECG data (ecg_total, ecg_sinus, ecg_time_range) is required for ECG `electrical_type`.")
    }
  } else {
    stop("Invalid electrical burden data type. Please specify either 'ECG' or 'MCT'.")
  }

  # TODO
  # Structural burden
  # Can consider LA size, volume, strain, etc
  # May also incorporate actual most recent ECG to evaluate P wave morphology

  # Calculate the composite score
  compositeScore <-
    (symptomBurden * parameters$weight_symptoms) +
    (electricalBurden * parameters$weight_electrical)

  return(compositeScore)
}


#' Set the weights for the `afib_score()` function
#'
#' This function sets the global parameters for the [afib_score()] function.
#'
#' @param weight_afeqt_total A `numeric` value representing the weight of the
#'   total AFEQT score in the composite score.
#'
#' @param weight_afeqt_symptoms A `numeric` value representing the weight of the
#'   AFEQT symptoms domain in the composite score.
#'
#' @param weight_afeqt_activities A `numeric` value representing the weight of
#'   the AFEQT daily activities domain in the composite score.
#'
#' @param weight_afeqt_treatment A `numeric` value representing the weight of
#'   the AFEQT treatment concerns domain in the composite score.
#'
#' @param weight_nyha_class A `numeric` value representing the weight of the
#'   NYHA class in the composite score.
#'
#' @param weight_electrical A `numeric` value representing the weight of the
#'   electrical burden in the composite score.
#'
#' @param mct_cutoff_low A `numeric` value representing the cutoff for low MCT
#'   burden.
#'
#' @param mct_cutoff_medium A `numeric` value representing the cutoff for medium
#'   MCT burden.
#'
#' @param ecg_cutoff_low A `numeric` value representing the cutoff for low ECG
#'   burden.
#'
#' @param ecg_cutoff_medium A `numeric` value representing the cutoff for medium
#'   ECG burden.
#'
#' @export
#'
#' @examples
#' set_afib_score_parameters(
#'   weight_symptoms = 0.5,
#'   weight_electrical = 0.5,
#'   weight_afeqt_total = 0.5, weight_afeqt_symptoms = 0.2,
#'   weight_afeqt_activities = 0.2, weight_afeqt_treatment = 0.1, weight_nyha_class = 0.7,
#'   weight_electrical = 0.5, mct_cutoff_low = 0.33, mct_cutoff_medium = 0.66,
#'   ecg_cutoff_low = 0.33, ecg_cutoff_medium = 0.66
#' )
#'
#' @export
set_afib_score_parameters <- function(weight_symptoms = NULL,
                                      weight_afeqt_total = NULL,
                                      weight_afeqt_symptoms = NULL,
                                      weight_afeqt_activities = NULL,
                                      weight_afeqt_treatment = NULL,
                                      weight_nyha_class = NULL,
                                      weight_electrical = NULL,
                                      mct_cutoff_low = NULL,
                                      mct_cutoff_medium = NULL,
                                      ecg_cutoff_low = NULL,
                                      ecg_cutoff_medium = NULL) {
  currentParameters <- getOption("afib_parameters")

  # Overall weights for composite score
  if (!is.null(weight_symptoms)) {
    currentParameters$weight_symptoms <- weight_symptoms
  }
  if (!is.null(weight_electrical)) {
    currentParameters$weight_electrical <- weight_electrical
  }

  # AFEQT and NYHA symptom scores
  if (!is.null(weight_afeqt_total)) {
    currentParameters$weight_afeqt_total <- weight_afeqt_total
  }
  if (!is.null(weight_afeqt_symptoms)) {
    currentParameters$weight_afeqt_symptoms <- weight_afeqt_symptoms
  }
  if (!is.null(weight_afeqt_activities)) {
    currentParameters$weight_afeqt_activities <- weight_afeqt_activities
  }
  if (!is.null(weight_afeqt_treatment)) {
    currentParameters$weight_afeqt_treatment <- weight_afeqt_treatment
  }
  if (!is.null(weight_nyha_class)) {
    currentParameters$weight_nyha_class <- weight_nyha_class
  }

  # ECG and MCT cut-offs for low/medium/high
  if (!is.null(mct_cutoff_low)) {
    currentParameters$mct_cutoff_low <- mct_cutoff_low
  }
  if (!is.null(mct_cutoff_medium)) {
    currentParameters$mct_cutoff_medium <- mct_cutoff_medium
  }
  if (!is.null(ecg_cutoff_low)) {
    currentParameters$ecg_cutoff_low <- ecg_cutoff_low
  }
  if (!is.null(ecg_cutoff_medium)) {
    currentParameters$ecg_cutoff_medium <- ecg_cutoff_medium
  }

  options(afib_parameters = currentParameters)
}
