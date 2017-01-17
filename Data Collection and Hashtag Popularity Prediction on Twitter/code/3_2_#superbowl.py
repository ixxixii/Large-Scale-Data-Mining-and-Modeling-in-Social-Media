import numpy as np
import statsmodels.api as sm
#use data from h700 to h800
#since the feature starts at h376, we use feature(324:424)

X = np.loadtxt('3_2/feature_#superbowl', delimiter=',')
# add a column of 1s
X = sm.add_constant(X)
Y = np.loadtxt('3_2/next_#superbowl')

model = sm.OLS(Y[324:424], X[324:424])
result = model.fit()

f = open('3_2/result_#superbowl.txt', 'w')
f.write(str(result.summary()))
f.close()

