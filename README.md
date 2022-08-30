# Master thesis from Martin Böckling
## Introduction
This repository is a code collection for all aspects regarding the master thesis of Martin Böckling. The structure of the repository is structured into the CRISP-DM phases Data Understanding, Data Preparation and Data Modelling. The thesis document is provided separately. 

The coding was run on an Ubuntu 22.04.1 system with 160GB of memory. Certain computations within scripts are memory intensive and need the amount of memory.
## Dependencies
The coding related to the thesis is implemented in Python and R. For the different programming languages the dependencies are outlined in the following sections.

### R
The R coding is based on the R version 4.1.2.
The dependencies of the project can be found in the [renv.lock](wildfirearea/renv.lock) file. The dependencies can directly be installed in R by using the [renv](https://cran.r-project.org/web/packages/renv/index.html) package. To install all necessary packages simply use the command `renv::restore(lockfile='*lockfile path*')` within your R environment.

For using the geospatial packages like sf within R GDAL, GEOS and Proj.4 needs to be installed. Instructions for downloading can be found under the following [link](https://r-spatial.github.io/sf/#installing)

### Python
The python coding is based on python version 3.9.12.
The dependencies can be found in the [requirements.txt](wildfirearea/requirements.txt) file. To install the packages used in this project please run `pip install -r requirements.txt` in your created virtual environement. 

>Example virtuelenv creation: `virtualenv wildfire -p` *path to python 3.9.12 environment*

For the igraph package you must assure that the required dependencies on a C environment are met. Therefore, please make sure to follow the [installation guidelines provided by igraph](https://igraph.org/python/#pyinstall).

## Data
To derive the data in total two different data compositions can be derived:
- All datasets created between each script under the following [OneDrive Link](https://1drv.ms/u/s!AijsqF7qjxxBiNhV5e9ar3pq7CKHKQ?e=qfHjIa) (Size 268 GB)
- All datasets used at the start of this thesis under the following [OneDrive Link](https://1drv.ms/u/s!AijsqF7qjxxBiOp9XJ7D7UMut5x2Vg?e=yj1IFN) (Size 24.7 GB)

The data can be found under the following OneDrive [Folder](https://1drv.ms/u/s!AijsqF7qjxxBiNhW7pu3QFB4LJxgPg?e=W37f3T). The structure is for both the same and the individual folders should be placed into the data folder. The provided coding uses relative paths, therefore make sure to make the necessary changes to your selected editor.

## Data Mining Pipeline
The Data Mining Pipeline shows the overall procedure and steps taken for this master thesis. It sketches a rough overview of the steps taken to classify wildfires. An outline can be found in the following image:

![Data Mining Pipeline](img/DMPipeline.png)

In the following each step within the Data Mining Pipeline is explained and associated to each script. 
- Data:
  - Weather data: (All single files for 3 month period, need to be unified to single dataset with [weather unification script](wildfirearea/datagathering/weatherdata.py))
  - Landscape data (Single data files are not prepared beforehand)
  - Elevation data (Unify Elevation datasets to one single spatial elevation grid with [this script](wildfirearea/datagathering/elevationdata.R))
  - OpenStreetMap data (Data is downloaded using [this script](wildfirearea/datagathering/openstreetmap.R))
  - Wildfire data (No additional preparation needed)
- Weather aggregation
  - Is performed in [Data Preparation script](https://github.com/MartinBoeckling/wildfirearea/blob/03a2fa85b8bef239a0cae6f707e66ce4d3a6f559/wildfirearea/datapreparation/datapreparation.R#L674-L771)
  - Weather aggregation uses mean, maximum, minimum and discretization aggregation
- Spatial Grid
  - Creation of spatial grid is performed in [Data Preparation script](https://github.com/MartinBoeckling/wildfirearea/blob/03a2fa85b8bef239a0cae6f707e66ce4d3a6f559/wildfirearea/datapreparation/datapreparation.R#L576-L596)
  - Spatial Grid consists of 20 km² big hexagonal grid cells
- Spatial Interpolation
  - The aggregated weather data needs to get values interpolated as currently each point wise weather measurement is expanded over each spatial grid cell
  - In total four different interpolation techniques are implemented
    - Inverse Distance Weighting
    - Indicator Kriging
    - Ordinary Kriging
    - Universal Kriging
  - The complete part for the interpolation can be found in the [following script](https://github.com/MartinBoeckling/wildfirearea/blob/03a2fa85b8bef239a0cae6f707e66ce4d3a6f559/wildfirearea/datapreparation/datapreparation.R#L788-L995)
- Spatial Aggregation
  - Higher resolution of elevation and land cover dataset are aggregated to created spatial grid
  - Spatial aggregation is present in the [Data Preparation script](https://github.com/MartinBoeckling/wildfirearea/blob/03a2fa85b8bef239a0cae6f707e66ce4d3a6f559/wildfirearea/datapreparation/datapreparation.R#L1002-L1076) 
- DE-9IM:
  - Used for the relation and creation of a Spatial-Temporal Knowledge Graph relating different geometries together
  - Can be found in the part of the [Data Preparation script](https://github.com/MartinBoeckling/wildfirearea/blob/03a2fa85b8bef239a0cae6f707e66ce4d3a6f559/wildfirearea/datapreparation/datapreparation.R#L1102-L1511)
- RDF2Vec
  - The RDF2Vec method is used to transform the Knowledge Graph into a vector representation using graph walks
  - The own implementation based on igraph is present in the following script
- XGBoost
  - The classification algorithm used for the wildfire detection
  - The script for XGBoost is present in the [following script](wildfirearea\modeling\extragboost.py)
## Code Structure
In the following list the structure of the repository with the associated files are highlighted. 
- Data Understanding
  - [Data visualization script](wildfirearea/dataunderstanding/datavisualization.R) (Contains plot visualization coding that are used within the thesis document)
  - [Data Understanding script](wildfirearea/dataunderstanding/SpatialDataUnderstanding.R) (Contains visualizations related to the Data Understanding chapter of the thesis document)
- Data Preparation
  - [Data Unification](wildfirearea/datapreparation/weather/dataunification.py) (Combines single weather files into one file using pandas library)
  - [Data Preparation](wildfirearea/datapreparation/datapreparation.R) (Data Preparation script which involves weather interpolation)
  - [RDF2Vec script](wildfirearea/modelling/rdf2vec.py) (Script which transforms Knowledge Graph into vector representation)
  - [Use case Data Preparation](wildfirearea/datapreparation/usecasecreation.R) (script which creates the use case datasets on which master thesis is conducted)
- Modeling
  - [XgBoost classification](wildfirearea/modelling/extragboost.py) (Script using the xgboost python package for wildfire classification)

