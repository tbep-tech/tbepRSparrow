getOptionSett<-function(){
optionSettings<-c("calculate_reach_attribute_list = c('hydseq','headflag','termflag','demtarea')",
                  "NLLS_weights = c('default','area','lnload','user')",
                  "if_predict_scenarios = c('all reaches','none','selected reaches')",
                  "convertShapeToBinary.list = c('lineShapeName','polyShapeName','LineShapeGeo')",
                  "output_map_type = c('stream','catchment','both')",
                  "outputERSImaps = c('yes','no')")
return(optionSettings)
}