# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
import pandas as pd
import matplotlib as plt
df = pd.read_csv("C:\\Users\\DSAMIT\\Desktop\\DATA SCIENCE\\KAGGLE\\TITANIC\\train.csv") 

###SELECTING A SERIES FROM DATAFRAME########
df['Name']
df.Name
df['New']= df.Name + " "+df.Sex
df=df.drop('New',axis = 1)

type(df)#fucntion
df.dtypes#attribute
df.describe()

######RENAMING##########
df.columns
df.rename(columns ={"Name":"name"},inplace=True)

########DELETING COLUMNS and rows FROM DATAFRAME###########
df.drop('name',axis=1,inplace = True)
df.drop([0],axis=0,inplace = True)

#########SORTING################
df.Pclass.sort_values()#gives sorted values of Pclass
df.sort_values("Name")##sorts df by name
df_ordered = df.sort_values(["Sex","Fare"],ascending = False)
df_ordered.head()
#%%
##########FILTERING BOTH ROWS AND COLUMNS###########
df.loc[df.Fare>500,"Age"].size
df.loc[df.Age > 30,:].size
df.head()
df.loc[(df.Sex == "female") and (df.Fare > 500)]
df[df.Genre.isin(["Action","Crime"])]

###########USING STRING METHODS#######
df.Name=df.Name.str.upper()
new_list=df.Sex.str.contains('female')
df1 = df[new_list==True]

####CHANGING DATA TYPES##########
a=str(1)
type(a)
df.dtypes
df.Fare.astype(int)

#########USING GROUPBY#########
grouped_df = df.groupby('Sex').Fare.mean()
grouped_df=df.groupby('Sex').Fare.agg(['count','min','max','mean'])

##############USING SERIES OPERATIONS#########
df.Sex.value_counts()
pd.crosstab(df.Sex,df.Cabin)###equavalent to table in R#####
df.Cabin.unique()
df.Fare.value_counts().plot(kind='hist')

######HANDLING MISSING VALUES##########
df.Sex.isnull().sum()
df.Sex.notnull().sum()
missing_columns = df.columns[df.isnull().any()].tolist()
df.dropna(how='all').shape

#######WORKING WITH INDICES#######3
df.index
df.set_index('Sex',inplace = True)
df.loc['male','Name']
df.reset_index(inplace = True)
df.describe().loc['min','Fare']


#####SLICING AND DICING BY SELECTING MULTIPLE ROWS AND COLUMNS#####
df.head(3)
df.loc[0:15,['Name','Sex']] #in loc both indices are inclusive
df.loc[0:15] ##includes 15 too
df[0:15] #excludes 15
df['Name']
df.iloc[0:2,0:3] ###i refers to integer indexing,first index is inclusive and last is exlusive
df.ix[0:2,'Name']##mix of both loc and iloc, but first index is inclusive and last is exlusive

########WHEN TO USE INPLACE PARAMETER#######
##inplace=True changes the underlying dataset without creating a new copy of a dataset####
df['New']= df.Name + ' '+df.Sex
df.drop('New',axis=1)##does not affect underlying dataset as inplace is False by default
df.head()
df_new = df.drop('New',axis=1)##creating a new dataset, keeping original dataset unchanged, but eats memory
df.drop('New',axis=1,inplace = True) #affects underlying datset as inplace = True


