import pandas as pd
from pathlib import Path
from sklearn.ensemble import RandomForestClassifier
from sklearn.impute import SimpleImputer
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder
from sklearn.compose import ColumnTransformer
from sklearn.metrics import classification_report
from sklearn.model_selection import GridSearchCV
from sklearn.utils import shuffle
from imblearn.over_sampling import RandomOverSampler
import pickle

def dataPreprocess(dataPath, testDate):
    print('Data Preprocessing')
    dataPath = Path(dataPath)
    data = pd.read_csv(dataPath)
    data['DATE'] = pd.to_datetime(data['DATE'])
    trainData = data[data['DATE'] < testDate]
    testData = data[data['DATE'] >= testDate]
    trainDataY = trainData['WILDFIRE']
    trainDataX = trainData.drop(columns=['WILDFIRE', 'DATE', 'ID'], axis=1)
    roseSampling = RandomOverSampler(random_state=15)
    trainDataX, trainDataY = roseSampling.fit_resample(trainDataX, trainDataY)
    testDataY = testData['WILDFIRE']
    testDataX = testData.drop(columns=['WILDFIRE', 'DATE', 'ID'], axis=1)
    return (trainDataX, trainDataY), (testDataX, testDataY)

def randomForest(dataTrain, dataTest):
    print('Random Forest started')
    dataTrainX = dataTrain[0]
    dataTrainY = dataTrain[1]
    dataTestX = dataTest[0]
    dataTestY = dataTest[1]
    print(dataTrainY.sum())
    numericTransformer = Pipeline(steps=[
        ('imputer', SimpleImputer(strategy='median'))
    ])
    categoricTransformer = Pipeline(steps=[
        ('encoder', OneHotEncoder())
    ])
    numericFeatures = dataTrainX.select_dtypes(include=['int64', 'float64']).columns
    categoricFeatures = dataTrainX.select_dtypes(include=['object']).columns
    preprocessor = ColumnTransformer(
        transformers=[
            ('num', numericTransformer, numericFeatures),
            ('cat', categoricTransformer, categoricFeatures)])
    dataTrainX = preprocessor.fit_transform(dataTrainX)
    dataTestX = preprocessor.fit_transform(dataTestX)
    # rf = Pipeline(steps=[('preprocessor', preprocessor),
    #                     ('classifier', RandomForestClassifier(random_state=15, n_jobs=8))])
    rf = RandomForestClassifier(random_state=15, n_jobs=-3)
    
    paramGrid = {
        'n_estimators': [200, 800, 1200],
        'max_features': ['auto', 'log2', 0.25, 0.5, 0.75, 1.0]
    }
    cv = GridSearchCV(estimator=rf, param_grid=paramGrid, cv=5, scoring='f1_macro', verbose=2.1, n_jobs=1, error_score='raise')
    cv.fit(dataTrainX, dataTrainY)
    print(f'Best parameters: {cv.best_params_}')
    with open('wildfirearea/modelling/bestParams.pkl') as f:
        pickle.dump(cv.best_params_, f)
    
    print(f'Overall results: {cv.cv_results_}')
    with open('wildfirearea/modelling/results.pkl') as f:
        pickle.dump(cv.cv_results_, f)
    predClass = cv.predict(dataTestX)
    print(f'Sum of prediction:{sum(predClass)}' )
    print(f'Model score: {classification_report(dataTestY, predClass)}')

if __name__ =='__main__':
    trainData, testData = dataPreprocess('data/usecase/usecase1.csv', '2020-01-01')
    randomForest(trainData, testData)