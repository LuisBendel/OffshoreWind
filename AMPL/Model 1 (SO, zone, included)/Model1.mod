
set C = 1 .. 71021; # Set of grid cells (71021)
set Z = 1 .. 150; # Set of Zones/Clusters (100)

param COV{Z,Z}; # Covariance between Zones
param CF{C}; # Capacity factor for cells
param WPSS{C}; # Wind power suitability score for cells
param WPSS_min; # minimum WPSS score for selected cell
param min_w; # minimum weight on a selected zone
param max_w; # maximum weight on a selected zone
param cell_weight; # in MW, MW that can be placed in one cell
param MW_multiple; # MW that every wind farm must be a multiple of
param maxLoc; # maximum number of selected locations (non-zero weights)
param ZID{C}; # zone ID for cells
param DistNW{C}; # distance to Norway for cells
param DistNW_min; # minimum distance to Norway for selected cells

var w{Z}; # weight on zones
var u{Z} binary; # binary variable if zone is selected
var c{C} binary; # binary variable if cell is selected
var k{Z} integer, >= 0; # auxiliary variable to enforce multiples of zone weights
var l{Z} integer, >= 0; # auxiliary variable to enforce multiples of cell weights

minimize MVP:
	sum{z in Z, y in Z} COV[z, y] * w[z] * w[y];
	
subject to


# sum of weights must equal 1
sumOfWeights:
	sum{z in Z}w[z] = 1;

# if weight on z > 0, then u_z = 1
linkuw{z in Z}:
	(w[z] = 0) ==> (u[z] = 0);
linkuw2{z in Z}:
	(w[z] > 0) ==> (u[z] = 1);

# mapping cells to zones
cellzone{z in Z}:
	u[z] = sum{i in C: ZID[i] = z}c[i];
	
# minimum WPSS
minwpss{i in C}:
	c[i] * WPSS[i] >= c[i] * WPSS_min;
	
# minimum distance to Norway
mindistnw{i in C}:
	c[i] * DistNW[i] >= c[i] * DistNW_min;


	
# maximum number of wind farms
maxfarms:
	sum{z in Z}u[z] <= maxLoc;
	
# minimum and maximum weight on one location
minweight{z in Z}:
	w[z] >= min_w * u[z];
maxweight{z in Z}:
	w[z] <= max_w * u[z];
	
# w must correspond to a multiple of 1GW or 1.5GW (approx)
# the lower bound of 1GW is restricted by the contraint minweight
weight_multiple{z in Z}:
	w[z] = k[z] * (MW_multiple / 30000);
	

	
