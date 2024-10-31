
set C = 1 .. 71021; # Set of grid cells (71021)
set Z = 1 .. 100; # Set of Zones/Clusters (100)
set N = {1,2,3};

##############################################################
# Parameters
##############################################################
param COV{Z,Z}; # Covariance between Zones

param CF_Zones{Z}; # Capactiy factor for zones

param WPSS{C}; # Wind power suitability score for cells
param WPSS_min; # minimum WPSS score for selected cell

param ZID{C}; # zone ID for cells
param DistNW{C}; # distance to Norway for cells

param dist_min; # minimum difference between two selected cells (wind parks)
param min_w; # minimum weight on a selected cell
param max_w; # maximum weight on a selected cell
param maxLoc default 0; # maximum number of selected cells (non-zero weights)

param DistNW_min; # minimum distance to Norway for selected cells

param pMVP; # utopia point for MVP objective
param pMCF; # utopia point for MCF objective
param pMVP_max; # Nadir point for MVP objective
param pMCF_max; # Nadir point for MCF objective

param MaxLocs{N}; # for different values of maxLoc
param wMVP; # for different weights on blended objectives



##############################################################
# Decision variables
##############################################################
var w{Z} >= 0; # weight on zones
var u{Z} binary; # binary variable if zone is selected
var c{C} binary; # binary variable if cell is selected



##############################################################
# Objectives
##############################################################

# Objective 1: minimize mean portfolio variance
minimize MVP:
	sum{z in Z, y in Z} COV[z, y] * w[z] * w[y];

# Objective 2: minimize mean capacity factor
minimize MCF:
	sum{z in Z}w[z] * CF_Zones[z];

# Blended objective	
minimize BiObj:
	wMVP * (((sum{z in Z, y in Z} COV[z, y] * w[z] * w[y]) - pMVP) / (pMVP_max - pMVP))
	+ (1 - wMVP) * (((sum{z in Z} w[z] * CF_Zones[z]) - pMCF) / (pMCF_max - pMCF));



##############################################################
# Constraints
##############################################################
subject to

# mapping cells to zones
cellzone{z in Z}:
	u[z] = sum{i in C: ZID[i] = z}c[i];

# sum of weights must equal 1
sumOfWeights:
	sum{z in Z} w[z] = 1;

# if weight on z > 0, then u_z = 1
# if weight on z = 0 then u_z = 0
linkuw{z in Z}:
	(w[z] > 0) ==> (u[z] = 1);
linkuw2{z in Z}:
	(w[z] = 0) ==> (u[z] = 0);
	
# maximum number of wind farms
maxfarms:
	sum{z in Z}u[z] <= maxLoc;
	
# minimum weight on one location
minweight{z in Z}:
	w[z] >= min_w * u[z];
	
# maximum weight on one location
maxweight{z in Z}:
	w[z] <= max_w * u[z];
	
# minimum WPSS
minwpss{i in C}:
	c[i] * WPSS[i] >= c[i] * WPSS_min;
	
# minimum distance to Norway
mindistnw{i in C}:
	c[i] * DistNW[i] >= c[i] * DistNW_min;
	