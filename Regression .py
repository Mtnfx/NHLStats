"""
This is a very rough starting point, containing very little content.

Next step is using some 2025 NHL playoff games as test data to check for overfitting.
"""

import numpy as np
from sklearn.linear_model import LinearRegression

data = np.loadtxt('clean_data_trim.csv', delimiter = ',')
data = data.T
GF = data[0].T
GA = data[1].T
GD = data[2].T
X = data[3:].T

model_GF = LinearRegression().fit(X,GF) #Fit linear regression model to attempt to predict goals for
model_GA = LinearRegression().fit(X,GA) #Fit linear regression model to attempt to predict goals against
model_GD = LinearRegression().fit(X,GD) #Fit linear regression model to attempt to predict goals against

"""
Using the models that predict goals for and against, my first idea is to use the models to simply predict the number of goals for and against.

If GF > GA, we project a win. If GF < GA, we project a loss. If GF = GA, we will take out answer to be inconclusive (this should be a very rare situation).

We will do a rough check on the Anaheim Ducks' data to see if this approach might have merit.
"""
W = 0
L = 0
T = 0
for i in range(0,82):
    f = model_GF.predict(X[i].reshape(20,1).T)
    a = model_GA.predict(X[i].reshape(20,1).T)
    if (f>a):
        W += 1
    elif(f<a):
        L += 1
    else:
        T += 1
        
print("Results for Anaheim Ducks (Goals for and against model):")
print(f"Wins: {W}")
print(f"Losses: {L}")
print(f"Inconclusive Results: {T}")

"""
My second idea is simply to use a model to predict the goal differential. The disadvantage of this is the fact that it gives us less information; we don't predict how many goals the teams score.

We can verify that the predicted goals for less the predicted goals against will be exactly the goal differential that model_GD predicts.
"""

W = 0
L = 0
T = 0
for i in range(0,82):
    d = model_GD.predict(X[i].reshape(20,1).T)
    if (d>0):
        W += 1
    elif(d<0):
        L += 1
    else:
        T += 1
        
print("Results for Anaheim Ducks (Goal differential model):")
print(f"Wins: {W}")
print(f"Losses: {L}")
print(f"Inconclusive Results: {T}")

# My test yielded 34 wins, 48 losses, and no inconclusive results. This is only one win off of the actual Anaheim Ducks results for the 2024-2025 season.
# Nonetheless, we still have the very obvious flaw in this model that we haven't actually guaranteed that this model won't predict both teams winning or losing.

# Print the predicted goal differential from a game between the Anaheim Ducks and New York Rangers according to both teams (Anaheim won in OT).
print(model_GD.predict(X[10].reshape(20,1).T)) #Predicted GD (Anaheim): 0.9540868

print(model_GD.predict(X[1485].reshape(20,1).T)) #Predicted GD (New York): -0.9538964

#Here are the prediction for game 1 of the Stanley Cup Final (In Edmonton)
scf_data = np.loadtxt('oiler_panther_scf.csv', delimiter = ',')

#Loop through all 20000 sets of stats we generated)
W = 0
L = 0
T = 0
for i in range(0,20000):
    d = model_GD.predict(scf_data[i].reshape(20,1).T)
    if (d > 0):
        W += 1
    elif(d < 0):
        L += 1
    else:
        T += 1

print("-----------------------------------------------------------------------")
print("Based off all statistics:")    
print(f"Oilers have predicted {W*100/20000}% chance to win.")
print(f"Panthers have predicted {L*100/20000}% chance to win.")
print(f"{T*100/20000}% of simulations were inconclusive.")

#Running this simulation with the current simulated statistics gives the following projection:
#The Oilers have a 50.48% chance of winning game 1.

#First 10000 (Oilers Derived Stats)
W = 0
L = 0
T = 0
for i in range(0,10000):
    d = model_GD.predict(scf_data[i].reshape(20,1).T)
    if (d > 0):
        W += 1
    elif(d < 0):
        L += 1
    else:
        T += 1

print("-----------------------------------------------------------------------")
print("Based off Oilers' statistics:")  
print(f"Oilers have predicted {W*100/10000}% chance to win.")
print(f"Panthers have predicted {L*100/10000}% chance to win.")
print(f"{T*100/10000}% of simulations were inconclusive.")

#Oilers' statistics give Oilers the edge, with a 52.46% chance to win

#Second 10000 (Panthers Derived Stats)
W = 0
L = 0
T = 0
for i in range(10000,20000):
    d = model_GD.predict(scf_data[i].reshape(20,1).T)
    if (d > 0):
        W += 1
    elif(d < 0):
        L += 1
    else:
        T += 1

print("-----------------------------------------------------------------------")
print("Based off Panthers' statistics:")  
print(f"Oilers have predicted {W*100/10000}% chance to win.")
print(f"Panthers have predicted {L*100/10000}% chance to win.")
print(f"{T*100/10000}% of simulations were inconclusive.")

#Panthers' statistics give Panthers the edge with a 51.50% chance to win