
set C = 1 .. 71021; # Set of grid cells (71021)
set Z = 1 .. 100; # Set of Zones/Clusters (100)
set N = {1,2,3};
set NeighB = {1,2,3,4,5,6,7,8}; # Neighbor Index

##############################################################
# Parameters
##############################################################
param VAR{Z}; # Variance of Zones
param COV{Z,Z}; # Covariance between Zones

param CF{C}; # Capacity factor for cells
param CF_Zones{Z}; # Capactiy factor for zones

param WPSS{C}; # Wind power suitability score for cells
param WPSS_min; # minimum WPSS score for selected cell

param X{C}; # X coordinate for cells
param Y{C}; # Y coordinate for cells
param ZID{C}; # zone ID for cells
param DistNW{C}; # distance to Norway for cells
param NID{C, NeighB};

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
var v{C} >= 0;# Auxiliary variable to linearize the MCF objective
var f{C,C} binary;



##############################################################
# Objectives
##############################################################

# Objective 1: minimize mean portfolio variance
minimize MVP:
	sum{z in Z, y in Z}COV[z, y]*w[z]*w[y];

# Objective 2: minimize mean capacity factor
minimize MCF:
	sum{i in C}v[i] * CF[i];

# Blended objective	
minimize BiObj:
	wMVP * (((sum{z in Z, y in Z} COV[z, y] * w[z] * w[y]) - pMVP) / (pMVP_max - pMVP))
	+ (1 - wMVP) * (((sum{i in C}v[i] * CF[i]) - pMCF) / (pMCF_max - pMCF));



##############################################################
# Constraints
##############################################################
subject to

# Constraints to set v[i]
# could also be done with an indicator function:
# if c[i] > 0 ==> v[i] = 0.005
link_v1{z in Z}:
	sum{i in C: ZID[i] = z} v[i] = w[z];
	
link_v2{i in C}:
    v[i] <= c[i] * 0.0025;
    
link_v3{i in C}:
    v[i] >= (c[i] * 0.0025) - ((1 - c[i]) * 1);


# mapping cells to zones
number_of_cells{z in Z}:
	sum{i in C: ZID[i] = z} c[i] <= w[z] / 0.0025 + 1;
	
number_of_cells2{z in Z}:
	sum{i in C: ZID[i] = z} c[i] >= w[z] / 0.0025 - 1;
	
number_of_cells3{z in Z}:
	(u[z] = 0) ==> ((sum{i in C: ZID[i] = z} c[i]) = 0);
	
	
# Creating the flow
# setting the edges between selected cells
setedge{z in Z, i in C, j in NeighB: ZID[i] = z and NID[i,j] <> 0}:
	f[i,NID[i,j]] >= c[i] + c[NID[i,j]] - 1;
	
setedge2{z in Z, i in C, j in NeighB: ZID[i] = z and NID[i,j] <> 0}:
	f[i,NID[i,j]] <= c[i] + c[NID[i,j]] - (c[i] - 1);
	
# setting the flow: minimum number of edges per zone
setflow{z in Z}:
	sum{i in C, j in NeighB: ZID[i] = z} f[i,NID[i,j]] = sum{i in C: ZID[i] = z} c[i] * 2;


# sum of weights must equal 1
sumOfWeights:
	sum{z in Z}w[z] = 1;

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
	w[z] * u[z] >= min_w*u[z];
	
# maximum weight on one location
maxweight{z in Z}:
	w[z] * u[z] <= max_w*u[z];
	
# minimum WPSS
minwpss{i in C}:
	c[i] * WPSS[i] >= c[i] * WPSS_min;

# minimum CF for a selected cell
mincf{i in C}:
	c[i] * CF[i] <= c[i] * -0.45;
	
# minimum distance to Norway
mindistnw{i in C}:
	c[i] * DistNW[i] >= c[i] * DistNW_min;
	
