#The function below will generate a random observation from dens, a given density object.
#We will need to use this to generate values form all statistics distributions.
rdens <- function(dens){
  q = runif(1,0,1)
  c_prob = 0
  i = 1
  while(c_prob < q){
    c_prob = c_prob + dens$y[i] #Add marginal density to CDF up to this point
    i = i + 1 #Update i to move to next section of density
  }
  return (dens$x[i-1]) #We return last section such that CDF is less than q
}

data = read.csv("clean_data.csv") #Reads cleaned, untrimmed data

oilers_data = filter(data, Team == "Edmonton Oilers")
panthers_data = filter(data, Team  == "Florida Panthers")

#We now build density distributions for the Edmonton Oilers:
o_pdo = oilers_data$PDO %>% density()
o_pdoa = oilers_data$PDO_A %>% density()
o_sf = oilers_data$SF %>% density()
o_sa =  oilers_data$SA %>% density()
#Shot differential will be calculated from already generated data
o_shpct = oilers_data$SH_PCT %>% density()
o_shpcta = oilers_data$SH_PCT_A %>% density()
o_fow = oilers_data$FOW %>% density()
o_fol = oilers_data$FOL %>% density()
#Faceoff differential will be calculated from already generated data
#Faceoff percentage will be calculated from already generated data
o_svpct = oilers_data$SV_PCT %>% density()
o_svpcta = oilers_data$SV_PCT_A %>% density()
o_hit = oilers_data$HITS %>% density()
o_hita = oilers_data$HITS_A %>% density()
#Hit differential will be calculated from already generated data
o_bs = oilers_data$BS %>% density()
o_bsa = oilers_data$BS_A %>% density()
#Blocked shot differential will be calculated from already generated data.

#We now build density distributions for the Florida Panthers:
f_pdo = panthers_data$PDO %>% density()
f_pdoa = panthers_data$PDO_A %>% density()
f_sf = panthers_data$SF %>% density()
f_sa =  panthers_data$SA %>% density()
#Shot differential will be calculated from already generated data
f_shpct = panthers_data$SH_PCT %>% density()
f_shpcta = panthers_data$SH_PCT_A %>% density()
f_fow = panthers_data$FOW %>% density()
f_fol = panthers_data$FOL %>% density()
#Faceoff differential will be calculated from already generated data
#Faceoff percentage will be calculated from already generated data
f_svpct = panthers_data$SV_PCT %>% density()
f_svpcta = panthers_data$SV_PCT_A %>% density()
f_hit = panthers_data$HITS %>% density()
f_hita = panthers_data$HITS_A %>% density()
#Hit differential will be calculated from already generated data
f_bs = panthers_data$BS %>% density()
f_bsa = panthers_data$BS_A %>% density()
#Blocked shot differential will be calculated from already generated data.

#Generate 10000 random samples of each stat. For stats for the Oilers and Against stats for the Panthers are averaged (and vice versa)
#We produce our collection of observations from the POV of the Oilers.

#PDO related stats
pdo = (replicate(10000, rdens(o_pdo)) + replicate(10000, rdens(f_pdoa)))/2
pdoa = (replicate(10000, rdens(o_pdoa)) + replicate(10000, rdens(f_pdo)))/2

#Shot Stats
sf = (replicate(10000, rdens(o_sf)) + replicate(10000, rdens(f_sa)))/2
sa = (replicate(10000, rdens(o_sa)) + replicate(10000, rdens(f_sf)))/2
sd = sf - sa

#Shot Percentages
shpct = (replicate(10000, rdens(o_shpct)) + replicate(10000, rdens(f_shpcta)))/2
shpcta = (replicate(10000, rdens(o_shpcta)) + replicate(10000, rdens(f_shpct)))/2

#Faceoff Stats
fow = (replicate(10000, rdens(o_fow)) + replicate(10000, rdens(f_fol)))/2
fol = (replicate(10000, rdens(o_fol)) + replicate(10000, rdens(f_fow)))/2
fod = fow-fol
fopct = fow/(fow+fol)

#Save Percentages
svpct = (replicate(10000, rdens(o_svpct)) + replicate(10000, rdens(f_svpcta)))/2
svpcta = (replicate(10000, rdens(o_svpcta)) + replicate(10000, rdens(f_svpct)))/2

#Hit Statistics
hits = (replicate(10000, rdens(o_hit)) + replicate(10000, rdens(f_hita)))/2
hitsa = (replicate(10000, rdens(o_hita)) + replicate(10000, rdens(f_hit)))/2
hitd = hits - hitsa

#Blocked Shots
bs = (replicate(10000, rdens(o_bs)) + replicate(10000, rdens(f_bsa)))/2
bsa = (replicate(10000, rdens(o_bsa)) + replicate(10000, rdens(f_bs)))/2
bsd = bs-bsa

#Location - This is is a prediction for game 1 of the Stanley Cup Finals.
#The Oilers are at home, so we assign every random stats distribution a location code of 1

loc = replicate(10000, 1)

data_out = data.frame(loc, pdo, pdoa, sf, sa, sd, shpct, shpcta, fow, fol, fod,  fopct, svpct, svpcta, hits, hitsa, hitd, bs, bsa, bsd)

write.table(data_out, "oiler_panther_scf.csv", row.names = FALSE, col.names = FALSE, sep=",") #This is the clean data with only numerical values, intended for use in Python