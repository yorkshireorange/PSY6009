"""
Data manipulation based on the work of Stafford et al. (2014)
Guide retrieved from https://osf.io/akqt4
Code retrieved from https://osf.io/w7tds
Minor modifications introduced.
"""
print ("importing libraries")
import pandas as pd #for dealing with csv import
import numpy as np
df = pd.read_csv(r"C:\Users\user\Documents\R\uni\diss_experimental\data\CrowdstormingDataJuly1st.csv")

#add new vars
df['avrate']=(df['rater1']+df['rater2'])/2
df['allReds']=df['yellowReds']+df['redCards']
df['strictReds']=df['redCards']
df['refCount']=0

#add a column which tracks how many games each ref is involved in
refs=pd.unique(df['refNum'].values.ravel()) #list all unique ref IDs

#for each ref, count their dyads
for r in refs:
    df['refCount'][df['refNum']==r]=len(df[df['refNum']==r])    

for r in refs:
    if len(df[df['refNum']==refs[r]])>1:
        print (len(df[df['refNum']==refs[r]]))
        
colnames=list(df.columns)

j = 0
out = [0 for _ in range(sum(df['games']))]

for _, row in df.iterrows():
        n = row['games']
        c = row['allReds']
        d = row['strictReds']          
        
for _ in range(n):
    row['allReds'] = 1 if (c-_) > 0 else 0
    row['strictReds'] = 1 if (d-_) > 0 else 0
    rowlist=list(row)  #convert from pandas Series to prevent overwriting previous values of out[j]
    out[j] = rowlist
    j += 1
    if j%10==0:    
        print ("Number "+ str(j) + " of " + str(df.shape[0]))

#Identify referees with <22 dyads
refCont = df[df.refCount <22]
contList = refCont.refNum.unique().tolist()

# Save to .csv manipulation in R
contRefDict = {'refNum':contList}
contRefDf = pd.DataFrame(contRefDict)
contRefDf.to_csv(r"C:\Users\user\Documents\R\uni\diss_experimental\data_sup\ContRefs.csv")