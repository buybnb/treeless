# -*- coding: utf-8 -*-
"""
Created on Mon Oct 23 01:36:41 2023

@author: linzi
"""

# import packages
import pandas as pd
import numpy as np
from pyclustering.cluster.kmedoids import kmedoids
import random
#import scipy as sp
import matplotlib.pyplot as plt
from sklearn.ensemble import ExtraTreesClassifier
#from sklearn.preprocessing import OneHotEncoder, StandardScaler
#from sklearn.model_selection import cross_val_predict, StratifiedKFold
#from sklearn.metrics import roc_auc_score
#from sklearn import datasets
#from sklearn.datasets import make_classification
from sklearn.metrics import silhouette_score
from random import choices
#from tqdm import tqdm
#from sklearn.tree import DecisionTreeClassifier, ExtraTreeClassifier
#from sklearn.model_selection import train_test_split
from xlsxwriter.utility import xl_range
from sklearn.metrics import jaccard_score
from sklearn.metrics.cluster import adjusted_rand_score
from sklearn.metrics.cluster import normalized_mutual_info_score
from sklearn.metrics import accuracy_score


def findall(element, seq):
    result = []
    for i in range(len(seq)):
        if seq[i] == element:
            result.append(i)
    return result
def cb(idx,matrix):
    for i in range(len(idx)):
        for j in range(len(idx)):
            if i!=j:
                matrix[idx[i],idx[j]]+=1
            else:
                matrix[idx[i], idx[j]]=1
    return matrix
def px(lf,matrix):
    lf1=np.unique(lf)
    for i in range(len(lf1)):
        k = findall(lf1[i], lf)
        matrix=cb(k, matrix)
    return matrix

def pamcl(matrix,n):
    md=[]
    for i in range(n):
        md.append(i)
    kmedoids_instance = kmedoids(1 - matrix, md, data_type='distance_matrix')
    kmedoids_instance.process()
    clusters = kmedoids_instance.get_clusters()
    return clusters


ntree=100

# 没装seaborn的话记得安装一下，在terminal
# pip install seaborn

# 定义一个cov_matrix
cov_matrix = np.array([[1.10811287,-0.070019447,0.012603602,0.130594683],
                       [-0.07001945,  1.187710575, 0.004715465, 0.039490367],
                       [0.01260360,  0.004715465, 0.618353955, 0.007392267],
                       [0.13059468,  0.039490367, 0.007392267, 1.011761855]])

## 生成协方差矩阵的图
# sns.heatmap(cov_matrix, annot=True, cmap="hot")
# plt.show()

# 生成随机数dataframe

cls=2


rs=np.zeros(shape=(4, 9))


u=0
for i in [0,0.25,0.5,0.75,1,1.25,1.5,1.75,2]:
    for j in range(100):
        random_nums1 = pd.DataFrame(np.random.multivariate_normal(mean=[i, i, i, i], cov=cov_matrix, size=200))
        random_nums2 = pd.DataFrame( np.random.multivariate_normal(mean=[-i, -i, -i, -i], cov=cov_matrix, size=200))
        data = pd.concat([random_nums1, random_nums2])
        ct=np.array([])
        ct = np.concatenate((ct, np.zeros(200), np.ones(200)))
        uq=np.unique(ct)
        data1=pd.concat([data,data,data,data,data,data,data,data,data,data,
                         data,data,data,data,data,data,data,data,data,data])
        X=data1.to_numpy()
        plt.style.use('bmh')
        et = ExtraTreesClassifier(n_estimators=ntree,
                                bootstrap=True,  n_jobs=-1, random_state=1,max_features=1,min_samples_split=2000)
        y = choices(uq, k=20*len(ct))
        et.fit(X, y)
        leaves = et.apply(X)
        matrix = np.zeros(shape=(len(X), len(X)))
        for z in range(ntree):
            matrix = px(leaves[:, z], matrix)
        matrix = matrix / ntree
        np.fill_diagonal(matrix, 1)
        clusters = pamcl(matrix, cls)
        y = np.zeros(len(X))
        D = np.sqrt(1 - matrix)
        for k in range(len(clusters)):
            y[clusters[k]] = k + 1
            
        rs[0,u] =rs[0,u]+ silhouette_score(D, y, metric="precomputed")
        rs[1,u] = rs[1,u]+adjusted_rand_score(np.tile(ct,20), y)
        rs[2,u] = rs[2,u]+normalized_mutual_info_score(np.tile(ct,20), y)
        rs[3,u] = rs[3,u]+accuracy_score(np.tile(ct,20),y-1)
    u=u+1



writer = pd.ExcelWriter('label.xlsx', engine='xlsxwriter')
workbook = writer.book
df = pd.DataFrame(data=rs)
df.to_excel(
    writer, 'TEST',
    startcol=1,
    startrow=1
)
writer.close()