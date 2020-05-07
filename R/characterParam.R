#'@title characterParam
#'@description function generates list of character settings from sparrow_control
#'@return character vector of all character settings in sparrow_control


characterParam<-function(){
  charParms<-c("if_verify_demtarea",
               "calculate_reach_attribute_list",
               "if_reverse_hydseq",
               "if_data1_type",
               "if_mean_adjust_delivery_vars",
               "if_estimate",
               "ifHess",
               "NLLS_weights",
               "if_auto_scaling",
               "if_diagnostics",
               "select_corr",
               "if_validate",
               "if_boot_estimate",
               "if_predict",
               "if_boot_predict")
  return(charParms)
}