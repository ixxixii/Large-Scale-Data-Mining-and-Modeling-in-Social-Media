import numpy as np
import statsmodels.api as sm
#use data from h700 to h800
#since the feature starts at h340, here we use from 360 to 460

X = np.loadtxt('3_2/feature_#nfl', delimiter=',')
# add a column of 1s
X = sm.add_constant(X)
Y = np.loadtxt('3_2/next_#nfl')
# np.savetxt('tt', X, fmt='%d', delimiter=',')

model = sm.OLS(Y[360:460], X[360:460])
result = model.fit()

# print(result.summary())
# print(result.params)
# print(result.rsquared)

f = open('3_2/result_#nfl.txt', 'w')
f.write(str(result.summary()))
f.close()

