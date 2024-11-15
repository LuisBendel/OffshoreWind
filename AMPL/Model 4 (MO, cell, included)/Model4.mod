
set C = 1 .. 71021; # Set of grid cells (71021)
set Z = 1 .. 150; # Set of Zones/Clusters (150)
set N = {1,2,3,4}; # for different values of maxLoc

##############################################################
# Parameters
##############################################################
param COV{Z,Z}; # Covariance between Zones

param CF{C}; # Capacity factor for cells

param WPSS{C}; # Wind power suitability score for cells
param WPSS_min; # minimum WPSS score for selected cell

param ZID{C}; # zone ID for cells
param DistNW{C}; # distance to Norway for cells

param min_w; # minimum weight on a selected cell
param max_w; # maximum weight on a selected cell
param cell_weight; # in MW, MW that can be placed in one cell
param MW_multiple; # MW that every wind farm must be a multiple of
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
var v{C} >= 0;# Auxiliary variable to linearize the MCF objective
var k{Z} integer, >= 0; # auxiliary variable to enforce multiples of zone weights



##############################################################
# Objectives
##############################################################

# Objective 1: minimize mean portfolio variance
minimize MVP:
	sum{z in Z, y in Z} COV[z, y] * w[z] * w[y];

# Objective 2: minimize mean capacity factor
minimize MCF:
	sum{i in C} v[i] * CF[i];

# Blended objective	
minimize BiObj:
	wMVP * (((sum{z in Z, y in Z} COV[z, y] * w[z] * w[y]) - pMVP) / (pMVP_max - pMVP))
	+ (1 - wMVP) * (((sum{i in C} v[i] * CF[i]) - pMCF) / (pMCF_max - pMCF));



##############################################################
# Constraints
##############################################################
subject to

# sum of weights must equal 1
sumOfWeights:
	sum{z in Z}w[z] = 1;

# if weight on z > 0, then u_z = 1
# if weight on z = 0 then u_z = 0
linkuw{z in Z}:
	(w[z] > 0) ==> (u[z] = 1);
linkuw2{z in Z}:
	(w[z] = 0) ==> (u[z] = 0);

# minimum WPSS
minwpss{i in C}:
	c[i] * WPSS[i] >= c[i] * WPSS_min;
	
# minimum distance to Norway
mindistnw{i in C}:
	c[i] * DistNW[i] >= c[i] * DistNW_min;



# maximum number of wind farms
maxfarms:
	sum{z in Z}u[z] <= maxLoc;
	
# minimum weight on one location
minweight{z in Z}:
	w[z] >= min_w * u[z];
	
# maximum weight on one location
maxweight{z in Z}:
	w[z] <= max_w * u[z];
	



# Constraints to set v[i]
cellweight1{i in C}:
	(c[i] > 0) ==> (v[i] = cell_weight / 30000);
cellweight2{i in C}:
	(c[i] = 0) ==> (v[i] = 0);
	
cellweight3{z in Z}:
	sum{i in C: ZID[i] = z} v[i] = w[z];

# set the number of cells in each zone
number_of_cells{z in Z}:
	sum{i in C: ZID[i] = z} c[i] = k[z] * (MW_multiple / cell_weight);
	
	
	
# w must correspond to a multiple of 1GW or 1.5GW (approx)
# the lower bound of 1GW is restricted by the contraint minweight
weight_multiple{z in Z}:
	w[z] = k[z] * (MW_multiple / 30000);
