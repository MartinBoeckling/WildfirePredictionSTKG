'''
Title: Unification of weather data
Description: Unify single weather csv files and append them to a pandas dataframe. Output
the constructed dataframe as a csv file
Input:
    - inputPath: Path to folder where csv files are stored in format dir/.../dir
    - outputPath: Path to output folder where dataframe is stored in format dir/.../dir
Output:
    - CSV file containing with all files
'''
# import packages
import pandas as pd
from tqdm import tqdm
from pathlib import Path

# base settings and parameters
# define path input
inputPath = Path('data/weather/singlefiles')
outputPath = Path('data/weather')
# extract all csv files storing in a sorted list
weatherFilesList = sorted(list(inputPath.glob('*.csv')))
# create empty pandas dataframe
weatherDf = pd.DataFrame()

# iterate over file list
for filePath in tqdm(weatherFilesList):
    # read csv file from 
    data = pd.read_csv(filePath, low_memory=False)
    # append to pandas dataframe
    weatherDf = pd.concat([weatherDf, data], ignore_index=True)

# write df to output Path
weatherDf.to_csv(f'{outputPath}/weather.csv', index=False)