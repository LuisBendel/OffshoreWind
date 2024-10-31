
set C = 1 .. 71021; # Set of grid cells (71021)
set Z = 1 .. 100; # Set of Zones/Clusters (100)

param VAR{Z}; # Variance of Zones
param COV{Z,Z}; # Covariance between Zones
param CF{C}; # Capacity factor for cells
param WPSS{C}; # Wind power suitability score for cells
param WPSS_min; # minimum WPSS score for selected cell
param X{C}; # X coordinate for cells
param Y{C}; # Y coordinate for cells
param dist_min; # minimum difference between two selected cells (wind parks)
param min_w; # minimum weight on a selected cell
param maxLoc; # maximum number of selected cells (non-zero weights)
param ZID{C}; # zone ID for cells
param DistNW{C}; # distance to Norway for cells
param DistNW_min; # minimum distance to Norway for selected cells

var w{Z}; # weight on zones
var u{Z} binary; # binary variable if zone is selected
var c{C} binary; # binary variable if cell is selected

minimize MVP:
	sum{z in Z, y in Z}COV[z, y]*w[z]*w[y];
	
subject to

# mapping cells to zones
cellzone{z in Z}:
	u[z] = sum{i in C: ZID[i] = z}c[i];

# sum of weights must equal 1
sumOfWeights:
	sum{z in Z}w[z] = 1;

# if weight on i > 0, then u_i = 1
linkuw{z in Z}:
	(w[z] = 0) ==> (u[z] = 0);
linkuw2{z in Z}:
	(w[z] > 0) ==> (u[z] = 1);
	
# maximum number of wind farms
maxfarms:
	sum{z in Z}u[z] <= maxLoc;
	
# minimum weight on one location
minweight{z in Z}:
	w[z] >= min_w*u[z];
	
# minimum WPSS
minwpss{i in C}:
	c[i] * WPSS[i] >= c[i] * WPSS_min;
	
# minimum distance to Norway
mindistnw{i in C}:
	c[i] * DistNW[i] >= c[i] * DistNW_min;
	
