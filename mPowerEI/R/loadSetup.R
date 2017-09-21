

loadSetup <- function()
  {
 setup = list();
 
 setup$dims=c("x","y","z");
 
 setup$designpoint = 100; # number of milliseconds per design point
 setup$pareto = 0.8 # number of elements in design-point analysis to be considered a good point
 
 
 # https://en.wikipedia.org/wiki/Earth_radius
 setup$g = setup$g_n = 9.80665; # standard gravity m/s^2
 setup$g_0 = 9.78033 # normal equatorial value for gravity m/s^2
 setup$r_e = 6371008.8 # radius of earth in meters
 
 setup$jsonlist = synapseListJSON;
 
 setup;
 
}