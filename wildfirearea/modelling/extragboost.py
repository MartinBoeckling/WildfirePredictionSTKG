# import packages
import pandas as pd
from pathlib import Path
from sklearn.compose import ColumnTransformer
from sklearn.impute import SimpleImputer
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder
from sklearn.metrics import classification_report
# from sklearn.model_selection import GridSearchCV
from skopt import BayesSearchCV
from skopt.space.space import Real, Categorical, Integer
from imblearn.over_sampling import RandomOverSampler
import pickle
import xgboost as xgb

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
    numericTransformer = Pipeline(steps=[
        ('imputer', SimpleImputer(strategy='median'))
    ])
    categoricTransformer = Pipeline(steps=[
        ('encoder', OneHotEncoder())
    ])
    numericFeatures = trainDataX.select_dtypes(include=['int64', 'float64']).columns
    categoricFeatures = trainDataX.select_dtypes(include=['object']).columns
    preprocessor = ColumnTransformer(
        transformers=[
            ('num', numericTransformer, numericFeatures),
            ('cat', categoricTransformer, categoricFeatures)])
    trainDataX = preprocessor.fit_transform(trainDataX)
    testDataX = preprocessor.fit_transform(testDataX)
    return (trainDataX, trainDataY), (testDataX, testDataY)


def xgboostDef(dataTrain, dataTest):
    print('XGBoost started')
    dataTrainX = dataTrain[0]
    dataTrainY = dataTrain[1]
    dataTestX = dataTest[0]
    dataTestY = dataTest[1]
    print(dataTrainY.sum())
    

    xgbCl = xgb.XGBClassifier(objective="binary:logistic", seed=15, n_jobs=-1)
    cv = BayesSearchCV(estimator=xgbCl,
                        search_spaces={
                            'learning_rate': Real(0.01, 1.0, prior='log-uniform'),
                            'min_child_weight': Real(0, 10, prior='uniform'),
                            'max_depth': Integer(0, 50, prior='uniform'),
                            'max_delta_step': Real(0, 20, prior='uniform'),
                            'subsample': Real(0.01, 1.0, prior='uniform'),
                            'colsample_bytree': Real(0.01, 1.0, prior='uniform'),
                            'colsample_bylevel': Real(0.01, 1.0, prior='uniform'),
                            'reg_lambda': Real(1e-9, 1000, prior='log-uniform'),
                            'reg_alpha': Real(1e-9, 1.0, 'log-uniform'),
                            'gamma': Real(1e-9, 0.5, prior='log-uniform'),
                            'n_estimators': Integer(50, 200, prior='uniform'),
                            'scale_pos_weight': Real(1e-6, 500, prior='log-uniform')},
                        cv=5,
                        scoring='f1_macro',
                        verbose=3,
                        n_jobs=1,
                        error_score='raise',
                        random_state=14)
    cv.fit(dataTrainX, dataTrainY)
    predClass = cv.predict(dataTestX)
    print(f'Best parameters: {cv.best_params_}')
    with open('wildfirearea/modelling/xgboostBestParams.pkl') as f:
        pickle.dump(cv.best_params_, f)
    
    print(f'Overall results: {cv.cv_results_}')
    with open('wildfirearea/modelling/xgboostResults.pkl') as f:
        pickle.dump(cv.cv_results_, f)
    predClass = cv.predict(dataTestX)
    print(f'Sum of prediction:{sum(predClass)}' )
    print(f'Model score: {classification_report(dataTestY, predClass)}')

if __name__ == '__main__':
    trainData, testData = dataPreprocess('/home/martinboe/GitHub/wildfirearea/data/usecase/usecase1.csv', '2020-01-01')
    xgboostDef(trainData, testData)
