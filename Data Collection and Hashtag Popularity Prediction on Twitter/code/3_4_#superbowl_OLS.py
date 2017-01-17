import numpy as np
import statsmodels.api as sm


X  = np.loadtxt('3_4/newfeature1_#superbowl', delimiter=',')
X2 = np.loadtxt('3_4/newfeature2_#superbowl', delimiter=',')
X3 = np.loadtxt('3_4/newfeature3_#superbowl', delimiter=',')
#choose feature
mask=np.logical_not(np.ones(len(X.transpose()),dtype=bool))
mask[[1,4,5]]=True
X=X.transpose()[mask].transpose()
X2=X2.transpose()[mask].transpose()
X3=X3.transpose()[mask].transpose()
X  = sm.add_constant(X)
X2 = sm.add_constant(X2)
X3 = sm.add_constant(X3)
Y  = np.loadtxt('3_4/next1_#superbowl')
Y2 = np.loadtxt('3_4/next2_#superbowl')
Y3 = np.loadtxt('3_4/next3_#superbowl')

error  = np.zeros(12)
error2 = np.zeros(12)
error3 = np.zeros(12)

#seek for best parameters, which will be used in prob 5
minErr = 999999;
minErr2 = 999999;
minErr3 = 999999;

for i in range(12):
	mask=np.ones(len(X),dtype=bool)
	mask2=np.ones(len(X2),dtype=bool)
	mask[i*10:(i+1)*10]=False
	mask2[i*1:(i+1)*1]=False

	trainData =X [mask]
	trainData2=X2[mask2]
	trainData3=X3[mask]
	trainTar =Y [mask]
	trainTar2=Y2[mask2]
	trainTar3=Y3[mask]
	test_mask=np.logical_not(mask)
	test_mask2=np.logical_not(mask2)
	testData=X[test_mask]
	testData2=X2[test_mask2]
	testData3=X3[test_mask]
	testTar=Y[test_mask]
	testTar2=Y2[test_mask2]
	testTar3=Y3[test_mask]

	model = sm.OLS(trainTar, trainData)
	model2 = sm.OLS(trainTar2, trainData2)
	model3 = sm.OLS(trainTar3, trainData3)
	result = model.fit()
	result2 = model2.fit()
	result3 = model3.fit()
	params = result.params
	params2 = result2.params
	params3 = result3.params

	testRes=np.dot(testData,params)
	testRes2=np.dot(testData2,params2)
	testRes3=np.dot(testData3,params3)
	error[i] = np.mean(np.abs(testTar-testRes))
	error2[i] = np.mean(np.abs(testTar2-testRes2))
	error3[i] = np.mean(np.abs(testTar3-testRes3))

	if error[i]<minErr:
		minErr=error[i]
		bestparams=params
	if error2[i]<minErr2:
		minErr2=error2[i]
		bestparams2=params2
	if error3[i]<minErr3:
		minErr3=error3[i]
		bestparams3=params3

totalError=np.mean(error)
totalError2=np.mean(error2)
totalError3=np.mean(error3)

print totalError
print totalError2
print totalError3

f=open('./3_4/result_#superbowl.txt','w')
f.write(str(totalError)+'\n')
f.write(str(totalError2)+'\n')
f.write(str(totalError3)+'\n')
f.write(str(bestparams)+'\n')
f.write(str(bestparams2)+'\n')
f.write(str(bestparams3)+'\n')


