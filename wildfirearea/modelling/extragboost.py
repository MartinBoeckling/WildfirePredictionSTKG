''''
Title:
Extra Gradient Boosting Script

Description:
This script uses the extra gradient boosting algorithm to predict wildfires. The
hyperparameters are optimized using bayes search cross validation implemented by 
the scikit-learn optimization package. The optimization is performed on a f1-score.
The datasets are highly imbalanced, therefore a random oversampling for the minority
class is performed. The random over sampling is implemented by the imblearn package.

Input:
    - dataPath: Path of dataset for use case in format dir/.../file
    - testDate: Date where split is performed

Output:
    - Optimal parameter combination
    - Score development over time
    - Classification report implemented by scikit-learn

'''
# import packages
import pandas as pd
from pathlib import Path
from sklearn.compose import ColumnTransformer
from sklearn.impute import SimpleImputer
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder
from sklearn.metrics import classification_report
from skopt import BayesSearchCV
from skopt.space.space import Real, Categorical, Integer
from imblearn.over_sampling import RandomOverSampler
import pickle
import xgboost as xgb


def dataPreprocess(dataPath, testDate):
    print('Data Preprocessing')
    # transform datafile into pathlib object
    dataPath = Path(dataPath)
    # check if input is eligeble for data processing
    # check if dataPath input is a file
    assert dataPath.is_file(), f'{dataPath} does not link to a file'
    # check if testDate input can be interpreted as date
    assert not pd.isna(pd.to_datetime(testDate, errors='coerce')), f'{testDate} is not an eligible date'
    # read file into dataframe
    data = pd.read_csv(dataPath)
    # transform DATE column into datetime
    data['DATE'] = pd.to_datetime(data['DATE'])
    # split data into train and testset based on specified date
    # create train dataframe which is under specified date
    trainData = data[data['DATE'] < testDate]
    # create test dataframe which is over specified date
    testData = data[data['DATE'] >= testDate]
    # extract wildfire column as target
    trainDataY = trainData.pop('WILDFIRE')
    # Drop Date and ID column
    trainDataX = trainData.drop(columns=['DATE', 'ID'], axis=1)
    # specify random over sampling
    roseSampling = RandomOverSampler(random_state=15)
    # resample data with specified strategy
    trainDataX, trainDataY = roseSampling.fit_resample(trainDataX, trainDataY)
    # extract Wildfire column as testdata target
    testDataY = testData.pop('WILDFIRE')
    # Drop Date and ID column
    testDataX = testData.drop(columns=['DATE', 'ID'], axis=1)
    # create preprocessing pipeline for numerical and categorical data
    # create numerical transformer pipeline
    numericTransformer = Pipeline(steps=[
        ('imputer', SimpleImputer(strategy='median'))
    ])
    # create categorical transformer pipeline
    categoricTransformer = Pipeline(steps=[
        ('encoder', OneHotEncoder())
    ])
    # select columns with numerical dtypes
    numericFeatures = trainDataX.select_dtypes(include=['int64', 'float64']).columns
    # select columns with categorical dtype
    categoricFeatures = trainDataX.select_dtypes(include=['object']).columns
    # construct column transformer object to apply pipelines to each column
    preprocessor = ColumnTransformer(
        transformers=[
            ('num', numericTransformer, numericFeatures),
            ('cat', categoricTransformer, categoricFeatures)],
        n_jobs=-1, verbose=True)
    # apply column transformer to train and test data
    trainDataX = preprocessor.fit_transform(trainDataX)
    testDataX = preprocessor.fit_transform(testDataX)
    # return trainData and testdata X and Y dataframe in tuple format
    return (trainDataX, trainDataY), (testDataX, testDataY)


def xgboostDef(dataTrain, dataTest):
    print('XGBoost started')
    # extract dataframes from train and test data tuples
    dataTrainX = dataTrain[0]
    dataTrainY = dataTrain[1]
    dataTestX = dataTest[0]
    dataTestY = dataTest[1]
    
    # specify extra gradient boosting classifier
    xgbCl = xgb.XGBClassifier(objective="binary:logistic", seed=15, n_jobs=-2)
    '''
    specify bayesian search cross validation with the following specifications
        - estimator: specified extra gradient boosting classifier
        - search_spaces: defined optimization area for hyperparameter tuning
        - cv: Specifying split for cross validation with 5 splits
        - scoring: optimization function based on f1_marco-score optimization
        - verbose: Output while optimizing
        - n_jobs: Parallel jobs to be used for optimization using 2 jobs
        - n_iter: Iteration for optimization
        - return_train_score: Return training scores for each fold
    '''
    cv = BayesSearchCV(estimator=xgbCl,
                        # specifying search space for 
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
                        random_state=14,
                        n_iter=30,
                        return_train_score=True)
    # fit specified cross validation setup to 
    cv.fit(dataTrainX, dataTrainY)
    # predict test classes based on test data
    predClass = cv.predict(dataTestX)
    # print best parameter combination
    print(f'Best parameters: {cv.best_params_}')
    # store best parameter combination in pickle format
    with open('wildfirearea/modelling/xgboostBestParams.pkl') as f:
        pickle.dump(cv.best_params_, f)
    # print detailed results of cross validation
    print(f'Overall results: {cv.cv_results_}')
    # store results of cross validation
    with open('wildfirearea/modelling/xgboostResults.pkl') as f:
        pickle.dump(cv.cv_results_, f)
    # print sum of predicted classes to see number of predicted wildfires
    print(f'Sum of prediction:{sum(predClass)}' )
    # print classification report
    print(f'Model score: {classification_report(dataTestY, predClass)}')

if __name__ == '__main__':
    trainData, testData = dataPreprocess('/home/martinboe/GitHub/wildfirearea/data/usecase/usecase1.csv', '2020-01-01')
    xgboostDef(trainData, testData)
