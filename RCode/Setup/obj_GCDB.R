#######################################################################
####################### ODDobj CLASS DEFINITION #######################
########### (Oxford-University Disaster Displacement Object) ##########
#######################################################################
# FIELDS:
#   - Spatial
#   - Hazard
#   - 
#   - 
#   - 
#   - 
#   - 
# METHODS:
#   - Sample hazard intensity (mean,sd) truncated normal distribution
#   - plotGDP, plotPop, plotHaz
#   - PosteriorTotal, PosteriorSample, PosteriorTime
#   - ODDfill: AddGDP, FindISOs, ISO indicators, hazard(haz_type)
#   - NOTE: hazard(haz_type) does kriging for irregular grid or non-gridded data 
#           and cubic spline interpolation otherwise
#   - AffectedPop(magnitude)
#   - SampleBuildHeight(object@Population,object@buildheightpars)
#   - SampleBuildArea(object@Population,object@buildareapars)
#   - Extract hazard intensity values (I0)
#######################################################################