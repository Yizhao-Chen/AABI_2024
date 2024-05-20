#Script to compare the tree growth at different age stage
#Yizhao 2020/8/27

import pandas as pd
import numpy as np
from scipy import signal
from matplotlib import pyplot as plt

root_age = ".\\Age_analysis\\us_age\\"
root_dbio = ".\\Age_analysis\\us_delta_bio\\"

#site-level comparison
site_age = pd.read_csv(root_age + "ga021_age.csv")
site_dbio = pd.read_csv(root_dbio + "ga021_delta_bio.csv")

#classify data into 4 quartile(75% 50% 25%)
#df for the last year
age_df = pd.DataFrame(site_age.iloc[(len(site_age['Unnamed: 0'])-1),:])
age_df.columns = ['age']
age_df.sort_values("age",ascending=False,inplace=True)
age_df.drop(age_df[age_df['age']<0].index,inplace=True)
age_df.drop(index = 'Unnamed: 0',inplace=True)
age_list = list(age_df.index)

#age distribution
plt.hist(age_df['age'],bins=int((len(age_df['age'])/10)))
plt.xlabel('Age')
plt.ylabel('Tree number')
plt.savefig(".\\Age_analysis\\"
                "site_corr\\ga021_age_hist.jpg")
plt.show()

#exclude sites with no value for 1982
age_df1 = pd.DataFrame(site_age.iloc[site_age[site_age['Unnamed: 0']==1982].index,:])
age_df1 = age_df1.T
age_df1.columns = ['age']
age_df1.sort_values("age",ascending=False,inplace=True)
age_df1.drop(age_df1[age_df1['age']<0].index,inplace=True)
age_df1.drop(index = 'Unnamed: 0',inplace=True)

age_list1 = list(age_df1.index)
list_intersec = list(set(age_list).intersection(set(age_list1)))
list_exclude = list(set(age_list).difference(set(list_intersec)))

for m in range(len(list_exclude)):
    age_df.drop(index = list_exclude[m], inplace=True)

#age distribution
#25
num_25 = int(len(age_df.age)*0.25)
age_sub_25 = age_df.head(num_25)
age_25_sub = list(age_sub_25.index)

#50
num_50 = int(len(age_df.age)*0.5)
age_sub_50 = age_df.iloc[(num_25+1):(num_50)]
age_50_sub = list(age_sub_50.index)

#75
num_75 = int(len(age_df.age)*0.75)
age_sub_75 = age_df.iloc[(num_50+1):(num_75)]
age_75_sub = list(age_sub_75.index)

#100
num_100 = int(len(age_df.age)*1)
age_sub_100 = age_df.iloc[(num_75+1):(num_100)]
age_100_sub = list(age_sub_100.index)


#dbio_df
dbio_df = site_dbio.drop(site_dbio[site_dbio['Unnamed: 0']<1982].index)
dbio_df.rename(columns = {'Unnamed: 0':'Year'},inplace=True)

#25
dbio_df_25 = dbio_df.loc[:,'Year']
for i in range(len(age_25_sub)):
    dbio_df_25 = pd.concat([dbio_df_25,dbio_df.loc[:,age_25_sub[i]]],axis=1)

#50
dbio_df_50 = dbio_df.loc[:,'Year']
for i in range(len(age_50_sub)):
    dbio_df_50 = pd.concat([dbio_df_50,dbio_df.loc[:,age_50_sub[i]]],axis=1)

#75
dbio_df_75 = dbio_df.loc[:,'Year']
for i in range(len(age_75_sub)):
    dbio_df_75 = pd.concat([dbio_df_75,dbio_df.loc[:,age_75_sub[i]]],axis=1)

#100
dbio_df_100 = dbio_df.loc[:,'Year']
for i in range(len(age_100_sub)):
    dbio_df_100 = pd.concat([dbio_df_100,dbio_df.loc[:,age_100_sub[i]]],axis=1)


dbio_df_25['mean'] = dbio_df_25.iloc[:,1:len(dbio_df_25.columns)].mean(1)
dbio_df_50['mean'] = dbio_df_50.iloc[:,1:len(dbio_df_50.columns)].mean(1)
dbio_df_75['mean'] = dbio_df_75.iloc[:,1:len(dbio_df_75.columns)].mean(1)
dbio_df_100['mean'] = dbio_df_100.iloc[:,1:len(dbio_df_100.columns)].mean(1)

#dbio_df_25['detrend'] = signal.detrend(dbio_df_25['mean'])
#dbio_df_50['detrend'] = signal.detrend(dbio_df_50['mean'])
#dbio_df_75['detrend'] = signal.detrend(dbio_df_75['mean'])
#dbio_df_100['detrend'] = signal.detrend(dbio_df_100['mean'])

#create df to analyze correlations between different ages
#df_corr = pd.DataFrame({"d25":dbio_df_25['detrend'],"d50":dbio_df_50['detrend'],
#                        "d75":dbio_df_75['detrend'],"d100":dbio_df_100['detrend']})

df_corr = pd.DataFrame({"d25":dbio_df_25['mean'],"d50":dbio_df_50['mean'],
                        "d75":dbio_df_75['mean'],"d100":dbio_df_100['mean']})

out_corr = df_corr.corr()
out_corr.to_csv(".\\Age_analysis\\"
                "site_corr\\ga021_corr.csv")

dbio_df_25.index = dbio_df_25['Year']
dbio_df_50.index = dbio_df_50['Year']
dbio_df_75.index = dbio_df_75['Year']
dbio_df_100.index = dbio_df_100['Year']

d25, = plt.plot(dbio_df_25['mean'])
d50, = plt.plot(dbio_df_50['mean'])
d75, = plt.plot(dbio_df_75['mean'])
d100, = plt.plot(dbio_df_100['mean'])
plt.legend([d25,d50,d75,d100],['d25','d50','d75','d100'])
my_x_ticks = np.arange(1982, dbio_df_25['Year'].iloc[(len(dbio_df_25['Year'])-1)], 5)
plt.xticks(my_x_ticks)
plt.savefig(".\\Age_analysis\\"
                "site_corr\\ga021.jpg")
plt.show()

plot_data = pd.read_csv(".\\Age_analysis\\"
                        "Corr_25_100_for_plot.csv")

#matplotlib
plot_data.boxplot(by="group",column=['value'],grid =False)

#seaborn
import seaborn as sns
sns.boxplot(x='group',y='Correlation coefficient(r)',data = plot_data,palette="RdBu_r")
plt.rcParams['figure.dpi'] = 300  
plt.savefig(".\\Age_analysis\\corr_summary.tif")
plt.show()


#test the significance between different age
import scipy.stats as stats

stat_25_50 = stats.kruskal(df_corr['d25'],df_corr['d50'])
stat_25_75 = stats.kruskal(df_corr['d25'],df_corr['d75'])
stat_25_100 = stats.kruskal(df_corr['d25'],df_corr['d100'])