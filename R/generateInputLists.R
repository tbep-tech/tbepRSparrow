#generate inputLists
if (exists("runScript")){
  if (runScript=="yes"){
   estimate.input.list <- named.list(ifHess, s_offset, NLLS_weights,if_auto_scaling,if_mean_adjust_delivery_vars,
                                     yieldFactor,ConcFactor,confInterval,
                                     loadUnits,yieldUnits,ConcUnits,MoranDistanceWeightFunc) 
   mapping.input.list <- named.list(lat_limit,lon_limit,master_map_list,
                                 lineShapeName,lineWaterid,polyShapeName,polyWaterid,LineShapeGeo,
                                 LineShapeGeo,CRStext,convertShapeToBinary.list,
                                 map_siteAttributes.list,
                                 #site_mapPointScale,
                                 #residual_map_breakpoints,
                                 if_verify_demtarea_maps,output_map_type,  
                                 outputERSImaps,
                                 
                                 #diagnosticPlots
                                 loadUnits,
                                 yieldUnits,
                                 diagnosticPlotPointSize,
                                 diagnosticPlotPointStyle,
                                 
                                 #prediction map settings
                                 predictionTitleSize,
                                 predictionLegendSize,
                                 predictionLegendBackground,
                                 predictionMapColors,
                                 predictionClassRounding,
                                 predictionMapBackground,
                                 lineWidth,
                                 
                                 #residual maps settings
                                 residual_map_breakpoints,
                                 ratio_map_breakpoints,
                                 residualTitleSize,
                                 residualLegendSize,
                                 residualColors,
                                 residualPointStyle,
                                 residualPointSize_breakpoints,
                                 residualPointSize_factor,
                                 residualMapBackground,
                                 
                                 #siteAttribute maps settings
                                 siteAttrTitleSize,
                                 siteAttrLegendSize,
                                 siteAttrColors,
                                 siteAttrClassRounding,
                                 siteAttr_mapPointStyle,
                                 siteAttr_mapPointSize,
                                 siteAttrMapBackground,
                                 
                                 #scenarios
                                 scenarioMapColors) 
  }
}
