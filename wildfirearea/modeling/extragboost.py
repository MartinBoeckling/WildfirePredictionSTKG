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
import argparse
import matplotlib.pyplot as plt
import pandas as pd
import pickle
from pathlib import Path
from sklearn.compose import ColumnTransformer
from sklearn.impute import SimpleImputer
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder
from sklearn.metrics import classification_report, confusion_matrix, roc_auc_score, roc_curve
from skopt import BayesSearchCV
from skopt.space.space import Real, Integer
from imblearn.over_sampling import RandomOverSampler
import xgboost as xgb
import shap

class modelPrediction:
    def __init__(self, validation, dataPath, testDate):
        # transform datafile path into pathlib object
        self.dataPath = Path(dataPath)
        # create directory for use case
        self.loggingPath = Path('wildfirearea/modelling').joinpath(self.dataPath.stem)
        self.loggingPath.mkdir(exist_ok=True, parents=True)
        # check if input is eligeble for data processing
        # check if dataPath input is a file
        assert self.dataPath.is_file(), f'{self.dataPath} does not link to a file'
        # check if testDate input can be interpreted as date
        assert not pd.isna(pd.to_datetime(testDate, errors='coerce')), f'{testDate} is not an eligible date'
        # assign test date as class variable
        self.testDate = testDate
        # perform data preprocessing for training and test data
        trainData, testData = self.dataPreprocess()
        # perform validation if set to true
        if validation:
            parameterSettings = self.parameterTuning(trainData, testData)
        # if validation set to false empty dict is used
        else:
            parameterSettings = {}
        # perform training based on train and test dataset and parametersettings
        self.modelTraining(trainData, testData, parameterSettings)

    
    def dataPreprocess(self):
        print('Data Preprocessing')
        # read file into dataframe
        data = pd.read_csv(self.dataPath)
        # transform DATE column into datetime
        data['DATE'] = pd.to_datetime(data['DATE'])
        # check if column osmCluster is present in dataframe
        if 'osmCluster' in data.columns:
            # change datatype of column osmCluster to categorical data type
            data = data.astype({'osmCluster':'object'})
        # split data into train and testset based on specified date
        # create train dataframe which is under specified date
        trainData = data[data['DATE'] < self.testDate]
        # create test dataframe which is over specified date
        testData = data[data['DATE'] >= self.testDate]
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


    def parameterTuning(self, dataTrain, dataTest):
        print('Parameter tuning')
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
            - refit: Set to false as only parameter settings need to be extracted
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
                            n_iter=25,
                            refit=False)
        # fit specified cross validation setup to 
        cv.fit(dataTrainX, dataTrainY)
        # predict class
        predClass = cv.predict(dataTestX)
        # print best parameter combination
        print(f'Best parameters: {cv.best_params_}')
        # store best parameter combination in pickle format
        with open(f'{self.loggingPath}/xgboostBestParams.pkl', 'wb') as f:
            pickle.dump(cv.best_params_, f)
        # print detailed results of cross validation
        print(f'Overall results: {cv.cv_results_}')
        # store results of cross validation
        with open(f'{self.loggingPath}/xgboostResults.pkl', 'wb') as f:
            pickle.dump(cv.cv_results_, f)
        # print sum of predicted classes to see number of predicted wildfires
        print(f'Sum of prediction:{sum(predClass)}' )
        # print classification report
        print(f'Model score:\n{classification_report(dataTestY, predClass)}')
        # print confusion matrix of classification
        print(f'Confusion matrix:\n{confusion_matrix(dataTestY, predClass)}')

    def modelTraining(self, trainData, testData, parameterSettings):
        print('Model training')
        # extract dataframes from train and test data tuples
        dataTrainX = trainData[0]
        dataTrainY = trainData[1]
        dataTestX = testData[0]
        dataTestY = testData[1]
        # specify extra gradient boosting classifier
        xgbCl = xgb.XGBClassifier(**parameterSettings, objective="binary:logistic", seed=15, n_jobs=-6)
        # fit specified model to training data
        xgbCl.fit(dataTrainX, dataTrainY)
        # store model
        xgbCl.save_model(f'{str(self.loggingPath)}/trainedModel.json')
        self.model = xgbCl
        # perform prediction on test dataset with trained model
        predClass = xgbCl.predict(dataTestX)
        # calculate probability for AUC calculation
        predProb = xgbCl.predict_proba(dataTestX)[:,1]
        # print feature importance
        print(f'Feature imporance:{xgbCl.feature_importances_}')
        # print confusion matrix of classification
        print(f'Confusion matrix:\n{confusion_matrix(dataTestY, predClass)}')
        # print AUC metric
        aucScore = roc_auc_score(dataTestY, predProb)
        print(f'AUC Score:\n{aucScore}')
        # print classification report
        print(f'Model score:\n{classification_report(dataTestY, predClass)}')
        # save roc_curve
        # extract false positive and true positive value series
        fp, tp, _ = roc_curve(dataTestY,  predProb)
        # construct roc curve plot
        # initialize matplotlib figure
        plt.figure()
        # add roc curve to matplotlib figure
        plt.plot(fp, tp, label=f"ROC Curve (AUC={aucScore})", color='dimgray', lw=2)
        # add line regarding random performance
        plt.plot([0, 1], [0, 1], color="darkgrey", lw=2, label=f"Random guess", linestyle="--")
        # add limitations to x- and y-axis
        plt.xlim([0.0, 1.0])
        plt.ylim([0.0, 1.05])
        # add labels to x-axis and y-axis
        plt.ylabel('True Positive Rate')
        plt.xlabel('False Positive Rate')
        plt.legend(loc="lower right")
        # save roc-curve plot
        plt.savefig(f'{str(self.loggingPath)}/rocCurve.png')

    def modelExplanation(self, testData, testDataColumns):
        explainer = shap.TreeExplainer(self.model)
        shapValues = explainer.shap_values(testData)
        shap.summary_plot(shapValues, testData, plot_type="violin", max_display=15, show=False)
        plt.savefig(f"summaryPlotModelPerformance{self.dataPath.stem}.png", dpi=250, bbox_inches='tight')


if __name__ == '__main__':
    # initialize the command line argparser
    parser = argparse.ArgumentParser(description='XGBoost argument parameters')
    # add validation argument parser
    parser.add_argument('-v', '--validation', default=False, action='store_true',
    help="use parameter if grid parameter search should be performed")
    # add path argument parser
    parser.add_argument('-p', '--path', type=str, required=True,
    help='string value to data path')
    # add date argument parser
    parser.add_argument('-d', '--date', type=str, required=True,
    help='date value for train test split', default='2020-01-01')
    # store parser arguments in args variable
    args = parser.parse_args()
    # Pass arguments to class function to perform xgboosting
    model = modelPrediction(validation=args.validation, dataPath=args.path, testDate=args.date)
