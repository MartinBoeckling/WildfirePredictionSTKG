'''
Title:
RDF2Vec transformation script

Description:
This script transforms an edge dataframe containing the following columns into a vector
representation using the RDF2Vec algorithm:
    - from: ID of grid cell or ID of geometric object as starting node
    - to: ID of grid cell or ID of geometric object as target node
    - description: Description of edge relation between starting and target node
    - YEAR: Year of geometric object
    - ID: Associated ID of grid cell.

The script contains three main methods. The first method is the dataPreparation method
which splits the edge dataframe into a training dataframe

Input:

Output:

'''
# import packages
import pandas as pd
import numpy as np
import pickle
from tqdm import tqdm
from pathlib import Path
from pyrdf2vec import RDF2VecTransformer
from pyrdf2vec.embedders import Word2Vec
from pyrdf2vec.graphs import KG, Vertex
from pyrdf2vec.walkers import RandomWalker


class kgEmbedding:

    def __init__(self, dataPath):
        # transform string to Path structure
        self.dataPath = Path(dataPath)
        # create logging directory Path name based on file name
        loggingDirectory = Path(f'wildfirearea/modelling/KnowledgeGraph/{self.dataPath.stem}')
        # create logging directory
        loggingDirectory.mkdir(exist_ok=True)
        # create training and transform dataframe for two differrent phases
        trainingDf, transformDf = self.dataPreparation()
        # extract file dictionary containing transformer path
        fileDict = self.kgTraining(trainingDf, loggingDirectory)
        # extract result dictionary with ID and corresponding year
        resultDict = self.kgTransformer(transformDf, fileDict)
        print('Write result')
        # store resultDict into pickle file
        with open(f'{str(loggingDirectory)}/result.pkl', 'wb') as f:
            pickle.dump(resultDict, f)


    def dataPreparation(self):
        print('Data Preparation')
        # Read a CSV file containing the entities we want to classify.
        graphData = pd.read_csv(self.dataPath)
        # change all columns to object type
        graphData = graphData.astype(str)
        # group graphData to a year stored in list
        trainingDf = graphData.groupby('YEAR')[['from', 'to', 'description', 'ID']].agg(list)
        # group graphData to year and ID stored in list
        transformDf = graphData.groupby(['YEAR', 'ID'])[['from', 'to', 'description']].agg(list)
        # return trainingDf and transformDf dataframe
        return trainingDf, transformDf


    def kgTraining(self, trainingDf, loggingPath):
        print('KG Embedding Training')
        #initalize rdf2vec transformer dictionary
        transformerDict = {}
        # build knowledge graph
        for index, row in tqdm(trainingDf.iterrows(), total=trainingDf.shape[0]):
            # transform row of type list into dictionary to store into a dataframe with unique rows
            rowValuesDf = pd.DataFrame(dict(row)).drop_duplicates(ignore_index=True)
            # extract entities from column from
            entities = pd.unique(rowValuesDf.pop('ID'))
            # transform values of row values dataframe into list
            rowValues = rowValuesDf.values.tolist()
            # initialize Knowledge Graph
            osmKG = KG()
            # create subject, predicate and object and append it to walk
            for rowValue in tqdm(rowValues, leave=False):
                subj = Vertex(rowValue[0])
                obj = Vertex(rowValue[1])
                pred = Vertex(rowValue[2], predicate=True, vprev=subj, vnext=obj)
                osmKG.add_walk(subj, pred, obj)
            # build transformer
            transformer = RDF2VecTransformer(
                Word2Vec(epochs=20),
                walkers=[RandomWalker(14, 36, with_reverse=False, n_jobs=6)],
                verbose=1
            )
            # extract walks
            extractedWalks = transformer.get_walks(osmKG, entities)
            # extract embeddings
            transformer.fit(extractedWalks)
            # save transformer model
            transformerPath = f'{str(loggingPath)}/transformer{index}.pkl'
            transformer.save(transformerPath)
            # append created path to dictionary
            transformerDict[index] = transformerPath
        return transformerDict

    def kgTransformer(self, transformDf, fileDict):
        print('Transformation started')
        resultDict = {}
        for index, row in tqdm(transformDf.iterrows(), total=transformDf.shape[0]):
            year = index[0]
            rowValuesDf = pd.DataFrame(dict(row)).drop_duplicates(ignore_index=True)
            entity = index[1]
            rowValues = rowValuesDf.values.tolist()
            osmKG = KG()
             # build transformer
            fileName = fileDict[year]
            transformer = RDF2VecTransformer.load(fileName)
            for rowValue in tqdm(rowValues, leave=False):
                subj = Vertex(rowValue[0])
                obj = Vertex(rowValue[1])
                pred = Vertex(rowValue[2], predicate=True, vprev=subj, vnext=obj)
                osmKG.add_walk(subj, pred, obj)
            entitiesVector, literals = transformer.fit_transform(osmKG, entity)
            resultDict[index] = {'vector': entitiesVector, 'literals': literals}
        return resultDict

if __name__ == '__main__':
    kgEmbedding('data/network/openstreetmapGraph.csv')