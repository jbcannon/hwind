#' Create wind profile function for GALES
#'
#' This function generates a wind profile for modeling wind damage using the
#' modified Souza Model presented in de Santana et al. 2017.
#' @param ws wind speed (m/s) at canopy height
#' @param LAI Leaf area index (unitless) modifies wind speed
#' @param ch canopy height (m), canopy height at which ws measured
#' @param z height (m) at which to output wind speeds. Should be in 1 m increments
#' @param plot Logical; whether to print wind profile output to a plot
#' @keywords wind, profile, Souza, de Santana
#' @return a dataframe containing vector of windspeed `ws` (m/s) at each height `z`
#' @export
#' @examples
#' ws = wind_profile(ws=20, LAI=6, ch=30)
#' print(ws)
wind_profile <- function(ws, LAI, ch, z=1:40, plot=TRUE) {
  u_z=1.012
  beta=0.1
  a = (-1 + exp(u_z*z))/exp(z)
  b = tanh(beta+exp(-LAI*(1-z/ch)))
  wp = ws*a*b
  if(plot == TRUE) {
    graphics::par(mar=c(4,4,1,1))
    plot(z,wp,type='l')
  }
  return(data.frame(z=z,ws=wp))
}

#' Profile method for calculating wind drag
#'
#' This method uses the 'profile' method Peltola 1999 to calculate wind
#' drag (Newtons) on tree segments using a vector of vertical distribution and wind profiel
#' @param Az  vector representing cross sectional area (m^2) of each 1 m tree/canopy segment
#' @param Uz vector representing wind speed (m/s) at each 1 m tree/canopy segment
#' @param z vector of heights of tree, 1m segements assumed if NULL
#' @param Cd drag coeffiecent (numeric). `Cd` can also be a function that returns
#' a drag coefficient for each wind speed, in which case a dynamic drag coefficient is used
#' is a function of wind speed that returns a vector of coefficients
#' @param rho density of air (kg m-3), standar for 20C @ 1 atm
#' @export
#' @examples
#' #there will be an exmaple here
#' @note  There should be a note here
#' #test
drag = function(Az, Uz, z=NULL, Cd = 0.7, rho = 1.204) {
  if(is.null(z)) z = 1:length(Az)
  if(!all(length(Az) == length(Uz) & length(Uz) == length(z))) {
    stop('`Az`, `Uz`, and `z` must all have the same length')
  }
  if(is.function(Cd)) Cd = Cd(Az)
  D = 0.5 * rho * Cd * Az * Uz^2
  return(D)
}

#' Find air density based on Temp, Humidity, and Pressure
#'
#' This method calculates air density (kg m-3) based on Air temperature (C)
#' and Relative Humidity (%) using some crazy ass equations I found
#' on the internet.
#' https://www.calctool.org/atmospheric-thermodynamics/air-density
#' @param T_C  Air temperature (deg. C)
#' @param RH Relative Humidity (0-100%)
#' @param P_mbar Air pressure (mbar)
#' @export
#' @examples
#' #there will be an exmaple here
#' @note  There should be a note here
air_dens = function(T_C, RH, P_mbar){
  T_K = T_C + 273.15 #convert air temp K
  P_Pa = P_mbar*100 #conver pressure to Pascals
  R_dry = 287.05 # constant J/(kg K)
  R_vap = 461.495 # constant J/(kg K)
  Pv = VP_Pa(RH, T_K); #in Pa
  Pd = P_Pa - Pv;# in Pa
  # calculate air density
  rho = Pd/(R_dry*T_K) + Pv/(R_vap*T_K)
  return(rho)
}

# Helper function to calculate partial pressures
# of water vapor. see https://www.calctool.org/atmospheric-thermodynamics/air-density
VP_Pa = function(RH, T_K) {
  # Get water vapor partial pressure
  SVP_mb = 6.1078/(wobus_p(T_K)^8)
  VP = RH/100 * SVP_mb
  VP = VP *100
  return(VP)
}

# Helper function to calculate partial pressures
# of water vapor. see https://www.calctool.org/atmospheric-thermodynamics/air-density
wobus_p = function(T_K) {
  T_C = T_K - 273.15
  c = c(0.99999683,-0.90826951e-2,0.78736169e-4,-0.61117958e-6,
  0.43884187e-8,0.29883885e-10,0.21874425e-12,-0.17892321e-14,
  0.11112018e-16,-0.30994571e-19)
  out = c[10]
  for(i in 9:1) out = c[i] + T_C*out
  return(out)
}

#' Leaf area Index for Longleaf Pine
#'
#' This function calculates leaf area index for longleaf pine using equations from
#' Gonzalez-Benecke 2015
#' @param Dq  quadratic mean diameter (cm)
#' @param TPH tree density (trees per ha)
#' @param Hdom dominant tree height (m)
#' @param age stand age (years)
#' @export
#' @examples
#' #there will be an example here
#' @note  There should be a note here
pred_LAI_longleaf = function(Dq, TPH, Hdom, age) {
  TPA = TPH / 2.47105
  Trees_per_m2 = TPH/10000
  foliage_biomass = 1.1846*Dq^2.316*Hdom^-1.1735*age^-0.4295 #kg/ tree
  specific_needle_area = 2.8172+(1.3218*exp(-0.0366*age)) #m2/kg
  lai = specific_needle_area * foliage_biomass * Trees_per_m2
  return(lai)
}

#' Critical turning moment for Longleaf pine
#'
#' This function estimates estimates Critical turning moment based on tree size
#' (dbh) for longleaf pine. See Garms and Dean 2019 for details
#' Output is critical turning moment (Mmax) in kN-m
#' @param DBH_cm  tree diameter at breast height (cm)
#' @export
#' @examples
#' #there will be an example here
garmsAndDean = function(DBH_cm) {
  BA_cm2 = (DBH_cm/2)^2*pi
  Mmax = BA_cm2*0.132563 -4.658763
  return(Mmax)
}

# Just some code to extract Garms and Dean equations
#d = read_csv('data/garms_and_dean_2019.csv')
#fig = d %>% ggplot(aes(x=BA_cm2, y=Mmax_kNm)) +
#  geom_point() +
#  geom_smooth(method='lm', se=FALSE) +
#  theme_bw() +
#  labs(x='basal area (cm2)', y='Mmax (kN-m)')
#ggsave('data/garms_and_dean.png', fig, width=4, height=4, dpi=600)
#sink('data/garms_and_dean.txt')
#lm(Mmax_kNm ~ BA_cm2, data=d) %>% summary
#sink()

