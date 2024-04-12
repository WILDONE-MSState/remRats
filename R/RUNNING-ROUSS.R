#======================================================================
#  RUNNING-ROUSS.R:  calculates maximum likelihood and restricted
#  maximum likelihood estimates for the Ornstein-Uhlenbeck state space
#  model (stationary and non-stationary), using population abundance
#  data having possibly unequal observation time intervals.  The script
#  ROUSSE-1.0.R must be present in the working directory of R.
#  Be patient;  R is slow.
#======================================================================

#----------------------------------------------------------------------
#  Load all the OUSS model functions and needed packages
#----------------------------------------------------------------------
library("MASS")
source("ROUSSE-1.0.R")

#----------------------------------------------------------------------
#        USER INPUT SECTION
#----------------------------------------------------------------------
#  User supplies time series data here into the vector "Observed.t.
#  User can substitute R statements to read population abundance data
#  from a file into the vector "Observed.t". No zeros! Do not change
#  the object name "Observed.t". Times of observation are entered into
#  the vector "Time.t". Do not change the object name "Time.t".

#  First example data set is bobcat (Lynx rufus) in Idaho, data set
#  212 from the Global Population Dynamics Database. Various other data
#  sets are also included.  Pick any of these data sets and associated
#  sampling years by "commenting out" the Idaho bobcat data and
#  "uncommenting" the desired data.

# Lynx rufus, from Idaho, GPPD data set 212.
Observed.t=c(346,675,802,1478,1173,756,861,972,854,1161,1318,901,901,
1173,608,811,903,584,1179,1020,1129,966)  #  No zeros!
Time.t=c(1956,1957,1958,1959,1960,1961,1962,1963,1964,1965,1970,1971,
1972,1973,1974,1975,1976,1977,1978,1979,1980,1981)

#  Linx rufus, from Florida, GPPD data set 211.
# Time.t=c(1946,1947,1948,1949,1950,1954,1955,1956,1957,1958,
# 1959,1960,1961,1963,1964,1965,1966,1967,1968,1975,1976,1977,
# 1978,1979,1980,1981)
# Observed.t=c(672,1028,538,566,300,400,400,400,400,300,250,
# 450,450,13,23,23,2,400,20,389,537,983,1698,1132,1702,1031)

# Lynx rufus, California, GPPD data set 208.
# Time.t=c(1934,1935,1936,1938,1940,1941,1942,1943,1944,1945,
# 1946,1947,1948,1949,1950,1951,1952,1954,1955,1956,1957,1958,
# 1959,1960,1961,1962,1963,1964,1965,1966,1967,1968,1969,1970,
# 1971,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981)
# Observed.t=c(1994,1436,1290,2292,2776,3239,1923,2898,2063,1730,
# 1072,689,169,375,293,239,336,223,228,276,202,142,175,304,205,
# 295,361,221,221,241,244,381,588,319,588,686,1244,1393,2203,
# 3618,4445,6928,7809,9595,9337)

# Lynx rufus, Michigan, GPPD data set 218.
# Time.t=c(1936,1937,1939,1940,1941,1942,1943,1944,1945,1946,
# 1947,1948,1949,1950,1951,1952,1954,1955,1956,1957,1958,1962,
# 1963,1964,1966, 1969,1976,1977,1978,1979,1980,1981)
# Observed.t=c(1134,811,598,528,529,375,2538,2802,2910,2363,
# 2174,2063,1547,1753,1443,1836,696,847,880,762,200,588,494,265,
# 400,300,341,331,386,597,223,200)

# Lynx rufus, Maine, GPPD data set 216.
# Time.t=c(1934,1935,1936,1937,1942,1943,1944,1945,1946,1947,
# 1948,1949,1950,1951,1952,1953,1954,1956,1957,1958,1959,1961,
# 1962,1963,1964,1965,1966,1968,1970,1971,1972,1973,1974,1975,
# 1976,1977,1978,1979,1980,1981)
# Observed.t=c(644,911,687,400,133,105,184,1044,181,178,489,100,
# 263,83,106,795,667,695,263,198,221,278,231,588,269,152,233,153,
# 730,654,641,573,544,373,436,389,278,318,381,345)

# Lynx rufus, Wisconsin, GPPD data set 239.
# Time.t=c(1934,1935,1936,1937,1938,1940,1941,1942,1943,1944,
# 1945,1946,1947,1948,1949,1950,1951,1952,1954,1956,1959,1960,
# 1969,1970,1971,1974,1975,1976,1977,1978,1979,1980,1981)
# Observed.t=c(302,428,513,461,593,180,283,191,765,384,1048,577,
# 427,437,482,525,724,740,524,321,479,869,148,148,147,205,223,
# 275,163,223,131,81,168)

# Elk, central valley of Grand Teton National Park, cited in
# Dennis and Taper (1994 Ecological Monographs).
# Observed.t = c(1627,1527,824,891,1140,1322,1431,1733,1131,1611,
# 1644,1991,1762,1076,1442,1800,1667,1558,1396,1753,1453,1804)
# Time.t = c(1963,1964,1965,1966,1967,1968,1969,1970,1971,1972,
# 1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,1984,1985)

# Rangeland grasshoppers, MT western mountain region, from
# Kemp and Dennis 1993 Oecologia).
# Observed.t=c(5.7981,7.7194,4.8022,3.9397,11.8806,10.7568,8.9586,
# 10.6619,6.5895,4.4905,3.0684,6.9973,5.3986,4.2777,6.1166,7.2989,
# 5.0850,4.8298,5.3997,4.7679,4.5073,1.9714,4.1007,5.6403,3.0492,
# 2.8144,4.4071,2.4121,3.2233,1.4236,2.3404,10.5283,7.6872,2.7305,
# 3.4570,5.4336,3.1487,3.8315,4.4805)
# Time.t=c(1948,1951,1952,1953,1954,1955,1956,1957,1958,1959,1960,
# 1961,1962,1963,1964,1965,1966,1967,1968,1969,1970,1971,1972,1973,
# 1974,1975,1977,1978,1979,1980,1981,1983,1984,1985,1986,1987,1988,
# 1989,1990)

# American Redstart, record # 02014 3328 08636 from the North
# American Breeding Bird Survey 1966-95 (Table 1 in Dennis et
# al. 2006 Ecological Monographs).
# Observed.t = c(18,10,9,14,17,14,5,10,9,5,11,11,4,5,4,8,2,3,9,2,4,
# 7,4,1,2,4,11,11,9,6)
# Time.t = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
# 21,22,23,24,25,26,27,28,29)

# Log-transform the observations to carry out all the calculations
# in this program.
log.obs = log(Observed.t)
#--------------------------------------------------------------------


#--------------------------------------------------------------------
#        PARAMETER ESTIMATION, PARAMETRIC BOOTSTRAP AND PREDICTIONS
#--------------------------------------------------------------------
# Before doing the calculations, the user has to specify ONLY the
# following 4 options:

# 1. Do you want to compute the ML estimates or the REML estimates?

method = "REML" # alternatively, set method = "ML"

# 2. Do you want to plot the predictions?
pred.plot = "TRUE" # Set it to "FALSE" if you do not want to plot the
                   # predictions

# 3. Do you want to plot the parametric bootstrap distribution of the
# estimates?
pboot.plot = "TRUE" # Set it to "FALSE" if you do not want to plot the
                    # bootstrap distribution of the estimates

# 4. How many bootstrap replicates?
NBoot = 1000


#--------------------------------------------------------------------
#  5. THE FOLLOWING LINES OF CODE COMPUTE THE ESTIMATES, PREDICTIONS,
#  AND PARAMETRIC BOOTSTRAP CONFIDENCE INTERVALS. THE USER DOES NOT
#  NEED TO MODIFY THESE LINES OF CODE.
#
#  THE OUTPUT OF THE FUNCTION 'ROUSS.CALCS' IS A LIST AND THE USER
#  CAN RETRIEVE EACH OF THE LIST ELEMENTS PRINTED AND SAVED IN THE
#  OBJECT "all.results".
#
#  THE 95% PARAMETRIC BOOTSTRAP FOR BOTH, THE PARAMETERS AND THE
#  PREDICTIONS ARE COMPUTED BY THE FUNCTION "ROUSS.CALCS".
#
#--------------------------------------------------------------------

all.results = ROUSS.CALCS(Yobs=log.obs,Tvec=Time.t,pmethod=method,
               nboot=NBoot,plot.pred=pred.plot,
               plot.bootdists=pboot.plot)
