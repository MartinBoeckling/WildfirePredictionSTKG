import os
import pandas as pd

# base settings
input_path = 'data/weather/singlefiles'
output_path = 'data/weather'
weather_files_list = sorted([file for file in os.listdir(input_path) if not file.startswith('.')])
weather_df = pd.DataFrame()

# iterate over file list
for file in weather_files_list:
    data = pd.read_csv(f'{input_path}/{file}')
    weather_df = weather_df.append(data)

# write df out
weather_df.to_csv(f'{output_path}/weather.csv', index=False)
