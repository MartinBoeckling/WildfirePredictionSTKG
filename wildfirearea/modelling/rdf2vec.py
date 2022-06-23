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
which splits the edge dataframe into a training datafram grouping the dataframe to the
column year and a transformation dataframe grouping to the columns year and id.
The second script is the kgTraining method in which the training dataframe is grouping
to the year 

Input:
    - Path to edge dataframe in format dir/.../file
    - Distance of node to other node
    - maximum number of walks

Output:
    - Transformer models in format of pickle file
    - Vector representation of grid cell IDs in format of pickle file

'''
# import packages
import argparse
import pandas as pd
import numpy as np
import pickle
import random
from tqdm import tqdm
from pathlib import Path
from igraph import Graph
from itertools import groupby
import multiprocessing as mp
import re
from gensim.models.word2vec import Word2Vec as W2V


class kgEmbedding:

    def __init__(self, dataPath, distance, maxWalks, train):
        # transform string to Path structure
        self.dataPath = Path(dataPath)
        # assign distance variable to class variable
        self.distance = distance
        # assign maximum walks to class variable
        self.maxWalks = maxWalks
        # assign train to class variable
        self.train = train
        # create logging directory Path name based on file name
        loggingDirectory = Path(f'wildfirearea/modelling/KnowledgeGraph/{self.dataPath.stem}')
        # create logging directory
        loggingDirectory.mkdir(exist_ok=True)
        # create training and transform dataframe for two differrent phases
        trainingDf, transformDf = self.dataPreparation()
        if self.train:
            # extract file dictionary containing transformer pathS
            fileDict = self.kgTraining(trainingDf, loggingDirectory)
        else:
            loggingFiles = list(loggingDirectory.glob('transformer*.model'))
            years = [re.findall(r'\d+', str(loggingFile))[0] for loggingFile in loggingFiles]
            fileDict = dict(zip(years, loggingFiles))
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
        transformDf = graphData.groupby(['YEAR'])[['ID']].agg(list)
        # return trainingDf and transformDf dataframe
        return trainingDf, transformDf

    def predicateGeneration(self, pathList):
        # assign class graph to graph variable
        graph = self.graph
        # extract description of edge given edge id stored in numpy
        predValues = np.array([e.attributes()['description'] for e in graph.es(pathList)])
        # extract node sequences that are part of the edge path and flatten numpy array
        nodeSequence = np.array([graph.vs().select(e.tuple).get_attribute_values('name') for e in graph.es(pathList)]).flatten()
        # delete consecutive character values in numpy array based from prior matrix
        nodeSequence = np.array([key for key, _group in groupby(nodeSequence)])
        # combine description values and node sequences to one single array
        pathSequence = np.insert(predValues, np.arange(len(nodeSequence)), nodeSequence)
        # convert numpy array to list 
        pathSequence = pathSequence.tolist()
        # return path sequence numpy array
        return pathSequence


    def walkIteration(self, idNumber):
        # assign class graph variable to local graph variable
        graph = self.graph
        # assign class maxWalks variable to local maxWalks variable
        maxWalks = self.maxWalks
        # extract index of graph node
        nodeIndex = graph.vs.find(idNumber).index
        # perform breadth-first search algorithm
        bfsList = graph.bfsiter(nodeIndex, 'out', advanced=True)
        # iterate over breadth-first search iterator object to filter those paths out
        # defined distance variable
        distanceList = [nodePath for nodePath in bfsList if nodePath[1] <= self.distance]
        # create vertex list from distance list extracting vertex element
        vertexList = [vertexElement[0] for vertexElement in distanceList]
        # limit maximum walks to maximum length of walkSequence length
        if len(vertexList) < maxWalks: maxWalks = len(vertexList)
        # random sample defined maximumWalk from vertexList list
        random.seed(15)
        vertexList = random.sample(vertexList, maxWalks)
        # compute shortest path from focused node index to extracted vertex list outputting edge ID
        shortestPathList = graph.get_shortest_paths(v=nodeIndex, to=vertexList, output='epath')
        # extract walk sequences with edge id to generate predicates
        walkSequence = list(map(self.predicateGeneration, shortestPathList))
        # return walkSequence list
        return walkSequence

    def kgTraining(self, trainingDf, loggingPath):
        print('KG Embedding Training')
        #initalize Word2Vec model path dictionary
        modelPathDict = {}
        # build knowledge graph
        for index, row in tqdm(trainingDf.iterrows(), total=trainingDf.shape[0], desc='Year iteration'):
            # transform row of type list into dictionary to store into a dataframe with unique rows
            rowValuesDf = pd.DataFrame(dict(row)).drop_duplicates(ignore_index=True)
            # extract entities from column from
            entities = pd.unique(rowValuesDf.pop('ID'))
            # transform values of row values dataframe into list
            rowValues = rowValuesDf.to_records(index=False)
            # initialize Knowledge Graph
            self.graph = Graph().TupleList(rowValues, directed=True, edge_attrs=['description'])
            # initialize multiprocessing pool with cpu number
            pool = mp.Pool(mp.cpu_count())
            # extract walk predicates using the walkIteration method 
            walkPredicateList = list(tqdm(pool.imap_unordered(self.walkIteration, entities, chunksize=8), desc='Walk Extraction', total=len(entities)))
            # close multiprocessing pool
            pool.close()
            # build up corpus on extracted walks
            corpus = [walk for entity_walks in walkPredicateList for walk in entity_walks]
            # initialize Word2Vec model
            model = W2V(min_count = 0, workers=1, seed=15)
            # pass corpus to build vocabolary for Word2Vec model
            model.build_vocab(corpus)
            # train Word2Vec model on corpus
            model.train(corpus, total_examples=model.corpus_count, epochs=10)
            # save trained model
            modelPath = f'{str(loggingPath)}/transformer{index}.model'
            model.save(modelPath)
            # delete variable with large memory consumption
            del corpus, walkPredicateList, model
            # append created path to dictionary
            modelPathDict[index] = modelPath
        return modelPathDict

    def kgTransformer(self, transformDf, fileDict):
        print('Transformation started')
        # initialize result dictionary
        resultDict = {}
        # iterate over given transform dataframe
        for index, row in tqdm(transformDf.iterrows(), total=transformDf.shape[0], desc='Year iteration'):
            rowValuesDf = pd.DataFrame(dict(row)).drop_duplicates(ignore_index=True)
            entityList = pd.unique(rowValuesDf['ID'])
            modelFilePath = fileDict[index]
            model = W2V.load(str(modelFilePath))
            entityVector = [model.wv.get_vector(entity) for entity in entityList]
            dictEntity = dict(zip(entityList, entityVector))
            resultDict[index] = dictEntity
        return resultDict

if __name__ == '__main__':
    # initialize the command line argparser
    parser = argparse.ArgumentParser(description='RDF2Vec argument parameters')
    # add train argument parser
    parser.add_argument('-t', '--train', default=False, action='store_true',
    help="use parameter if training should be performed")
    # add path argument parser
    parser.add_argument('-p', '--path', type=str, required=True,
    help='string value to data path')
    # add distance argument parser
    parser.add_argument('-d', '--distance', type=int, required=True,
    help='walk distance from selected node')
    # add walk number argument parser
    parser.add_argument('-w', '--walknumber', type=int, required=True,
    help='maximum walk number from selected node')
    # store parser arguments in args variable
    args = parser.parse_args()
    kgEmbedding(args.path, args.distance, args.walknumber, args.train)