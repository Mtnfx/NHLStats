#The function below will generate a random observation from dens, a given density object.
#We will need to use this to generate values form all statistics distributions.
rdens <- function(dens){
  q = runif(1,0,sum(dens$y))
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
o_pdo = oilers_data$PDO %>% density(from = min(oilers_data$PDO), to = max(oilers_data$PDO))
o_pdoa = oilers_data$PDO_A %>% density(from = min(oilers_data$PDO_A), to = max(oilers_data$PDO_A))
o_sf = oilers_data$SF %>% density(from = min(oilers_data$SF), to = max(oilers_data$SF))
o_sa =  oilers_data$SA %>% density(from = min(oilers_data$SA), to = max(oilers_data$SA))
#Shot differential will be calculated from already generated data
o_shpct = oilers_data$SH_PCT %>% density(from = min(oilers_data$SH_PCT), to = max(oilers_data$SH_PCT))
o_shpcta = oilers_data$SH_PCT_A %>% density(from = min(oilers_data$SH_PCT_A), to = max(oilers_data$SH_PCT_A))
o_fow = oilers_data$FOW %>% density(from = min(oilers_data$FOW), to = max(oilers_data$FOW))
o_fol = oilers_data$FOL %>% density(from = min(oilers_data$FOL), to = max(oilers_data$FOL))
#Faceoff differential will be calculated from already generated data
#Faceoff percentage will be calculated from already generated data
o_svpct = oilers_data$SV_PCT %>% density(from = min(oilers_data$SV_PCT), to = max(oilers_data$SV_PCT))
o_svpcta = oilers_data$SV_PCT_A %>% density(from = min(oilers_data$SV_PCT_A), to = max(oilers_data$SV_PCT_A))
o_hit = oilers_data$HITS %>% density(from = min(oilers_data$HITS), to = max(oilers_data$HITS))
o_hita = oilers_data$HITS_A %>% density(from = min(oilers_data$HITS_A), to = max(oilers_data$HITS_A))
#Hit differential will be calculated from already generated data
o_bs = oilers_data$BS %>% density(from = min(oilers_data$BS), to = max(oilers_data$BS))
o_bsa = oilers_data$BS_A %>% density(from = min(oilers_data$BS_A), to = max(oilers_data$BS_A))
#Blocked shot differential will be calculated from already generated data.

#We now build density distributions for the Florida Panthers:
f_pdo = panthers_data$PDO %>% density(from = min(panthers_data$PDO), to = max(panthers_data$PDO))
f_pdoa = panthers_data$PDO_A %>% density(from = min(panthers_data$PDO_A), to = max(panthers_data$PDO_A))
f_sf = panthers_data$SF %>% density(from = min(panthers_data$SF), to = max(panthers_data$SF))
f_sa =  panthers_data$SA %>% density(from = min(panthers_data$SA), to = max(panthers_data$SA))
#Shot differential will be calculated from already generated data
f_shpct = panthers_data$SH_PCT %>% density(from = min(panthers_data$SH_PCT), to = max(panthers_data$SH_PCT))
f_shpcta = panthers_data$SH_PCT_A %>% density(from = min(panthers_data$SH_PCT_A), to = max(panthers_data$SH_PCT_A))
f_fow = panthers_data$FOW %>% density(from = min(panthers_data$FOW), to = max(panthers_data$FOW))
f_fol = panthers_data$FOL %>% density(from = min(panthers_data$FOL), to = max(panthers_data$FOL))
#Faceoff differential will be calculated from already generated data
#Faceoff percentage will be calculated from already generated data
f_svpct = panthers_data$SV_PCT %>% density(from = min(panthers_data$SV_PCT), to = max(panthers_data$SV_PCT))
f_svpcta = panthers_data$SV_PCT_A %>% density(from = min(panthers_data$SV_PCT_A), to = max(panthers_data$SV_PCT_A))
f_hit = panthers_data$HITS %>% density(from = min(panthers_data$HITS), to = max(panthers_data$HITS))
f_hita = panthers_data$HITS_A %>% density(from = min(panthers_data$HITS_A), to = max(panthers_data$HITS_A))
#Hit differential will be calculated from already generated data
f_bs = panthers_data$BS %>% density(from = min(panthers_data$BS), to = max(panthers_data$BS))
f_bsa = panthers_data$BS_A %>% density(from = min(panthers_data$BS_A), to = max(panthers_data$BS_A))
#Blocked shot differential will be calculated from already generated data.

#Generate 10000 random samples of each stat.
#We produce 10000 observations from the POV of the Oilers and 10000 from the POV of the Panthers

#PDO related stats
pdo = c(replicate(10000, rdens(o_pdo)), replicate(10000, rdens(f_pdoa)))
pdoa = c(replicate(10000, rdens(o_pdoa)), replicate(10000, rdens(f_pdo)))

#Shot Stats
sf = c(replicate(10000, rdens(o_sf)), replicate(10000, rdens(f_sa)))
sa = c(replicate(10000, rdens(o_sa)), replicate(10000, rdens(f_sf)))
sd = sf - sa

#Shot Percentages
shpct = c(replicate(10000, rdens(o_shpct)), replicate(10000, rdens(f_shpcta)))
shpcta = c(replicate(10000, rdens(o_shpcta)), replicate(10000, rdens(f_shpct)))

#Faceoff Stats
fow = c(replicate(10000, rdens(o_fow)), replicate(10000, rdens(f_fol)))
fol = c(replicate(10000, rdens(o_fol)), replicate(10000, rdens(f_fow)))
fod = fow-fol
fopct = fow/(fow+fol)

#Save Percentages
svpct = c(replicate(10000, rdens(o_svpct)), replicate(10000, rdens(f_svpcta)))
svpcta = c(replicate(10000, rdens(o_svpcta)), replicate(10000, rdens(f_svpct)))

#Hit Statistics
hits = c(replicate(10000, rdens(o_hit)), replicate(10000, rdens(f_hita)))
hitsa = c(replicate(10000, rdens(o_hita)), replicate(10000, rdens(f_hit)))
hitd = hits - hitsa

#Blocked Shots
bs = c(replicate(10000, rdens(o_bs)), replicate(10000, rdens(f_bsa)))
bsa = c(replicate(10000, rdens(o_bsa)), replicate(10000, rdens(f_bs)))
bsd = bs-bsa

#Location - This is is a prediction for game 1 of the Stanley Cup Finals.
#The Oilers are at home, so we assign every random stats distribution a location code of 1

loc = replicate(20000, 1)

data_out = data.frame(loc, pdo, pdoa, sf, sa, sd, shpct, shpcta, fow, fol, fod,  fopct, svpct, svpcta, hits, hitsa, hitd, bs, bsa, bsd)

write.table(data_out, "oiler_panther_scf.csv", row.names = FALSE, col.names = FALSE, sep=",") #This is the clean data with only numerical values, intended for use in Python