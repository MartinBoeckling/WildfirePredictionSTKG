'''
Title:
RDF2Vec transformation script

Description:
This script transforms an edge dataframe containing the following columns into a vector
representation using the RDF2Vec algorithm:
    - from: ID of grid cell or ID of geometric object as starting node
    - to: ID of grid cell or ID of geometric object as target node
    - description: Description of edge relation between starting and target node
    - DATE: Date of geometric object (YEAR or monthly data)
    - ID: Associated ID of grid cell.

The script contains three main methods. The first method is the dataPreparation method
which splits the edge dataframe into a training datafram grouping the dataframe to the
column year and a transformation dataframe grouping to the columns year and id.
The second script is the kgTraining method in which the training dataframe is grouping
to the year 

Input:
    - dataPath: Path to folder with edge dataframes in format dir/.../dir
    - distance: Distance of node to other node
    - maxWalks: maximum number of walks per defined entity
    - train: Boolean value if RDF2Vec should be performed
    - clustering: Boolean value if vector representation should be clustered with KMeans
    - 



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
from gensim_word2vec_procrustes_align import smart_procrustes_align_gensim
from tqdm import tqdm
from pathlib import Path
from igraph import Graph
from itertools import groupby
import multiprocessing as mp
import re
from gensim.models.word2vec import Word2Vec as W2V
from sklearn.cluster import KMeans

class kgEmbedding:

    def __init__(self, dataPath, distance, maxWalks,
                train, clustering, chunksize, savePath,
                retrain, alignmentProjection):
        # transform string to Path structure
        self.dataPath = Path(dataPath)
        # assign distance variable to class variable
        self.distance = distance
        # assign maximum walks to class variable
        self.maxWalks = maxWalks
        # assign train to class variable
        self.train = train
        # assign clustering to class variable
        self.clustering = clustering
        # assign chunksize to class variable
        self.chunksize = chunksize
        # assign savepath to class variable
        self.savePath = savePath
        # assign retrain to class variable
        self.retrain = retrain
        # assign alignment to class variable
        self.alignment = alignmentProjection
        # create logging directory Path name based on file name
        loggingDirectory = Path(f'wildfirearea/modelling/KnowledgeGraph/{self.dataPath.stem}')
        # create logging directory
        loggingDirectory.mkdir(parents=True, exist_ok=True)
        # extract all file paths from directory
        directoryFiles = list(sorted(self.dataPath.glob('*')))
        # assign empty method to get accessed
        self.model = W2V(min_count = 0, workers=1, seed=15)
        # if training variable is true, extract vectors in RDF2Vec
        if self.train:
            for filePath in tqdm(directoryFiles, position=0, leave=True):
                date = filePath.stem
                graphData = self.dataPreparation(filePath)
                self.kgTraining(graphData, loggingDirectory, date)
        # extract stored models from loggingDirectory
        loggingFiles = list(sorted(loggingDirectory.glob('*.pkl')))
        # extract dates from stored files name
        loggingFilesDate = [re.findall(r'\d+-\d+-\d+', loggingFile.stem)[0] for loggingFile in loggingFiles]
        # construct file dictionary with loggingFilesDate as key and file name as value
        fileDict = dict(zip(loggingFilesDate, loggingFiles))
        # extract result dictionary with ID and corresponding year
        resultDict = self.kgTransformer(fileDict)
        # create vector representation as dataframe
        self.vectorDf(resultDict)
        if self.clustering:
            self.vectorClustering(resultDict)

    def dataPreparation(self, filePath: Path) -> pd.DataFrame:
        # check if variable dataPath is a directory or a file
        # Read a CSV file containing the entities we want to classify
        graphData = pd.read_csv(filePath)
        # change all columns to object type
        graphData = graphData.astype(str)
        # order columns in determined order
        graphData = graphData[['from', 'to', 'description', 'ID']]
        # return prepared edge dataframe
        return graphData

    def predicateGeneration(self, pathList) -> list:
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


    def walkIteration(self, idNumber) -> list:
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
        # check if all paths should be extracted
        if maxWalks == -1:
            pass
        else:
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

    def kgTraining(self, graphData, loggingPath, date) -> None:
        entities = pd.unique(graphData.pop('ID'))
        # transform values of row values dataframe into list
        graphValues = graphData.to_records(index=False)
        # initialize Knowledge Graph
        self.graph = Graph().TupleList(graphValues, directed=True, edge_attrs=['description'])
        print(self.graph.summary())
        # initialize multiprocessing pool with cpu number
        pool = mp.Pool(mp.cpu_count())
        # extract walk predicates using the walkIteration method 
        walkPredicateList = list(tqdm(pool.imap_unordered(self.walkIteration, entities, chunksize=self.chunksize),
                                    desc=f'Walk Extraction {date}', total=len(entities), position=0, leave=True))
        # close multiprocessing pool
        pool.close()
        # build up corpus on extracted walks
        corpus = [walk for entity_walks in walkPredicateList for walk in entity_walks]
        # retrain routine
        # check if retraining is true
        if self.retrain:
            # retrieve class model states
            model = self.model
            # check if already word vector exists
            if len(self.model.wv) == 0 or date == '2020-01-01':
                # pass corpus to build vocabolary for Word2Vec model
                model.build_vocab(corpus)
            else:
                # pass corpus to build vocabolary for Word2Vec model
                model.build_vocab(corpus, update=True)
            # train Word2Vec model on corpus
            model.train(corpus, total_examples=model.corpus_count, epochs=10)
            # assign model back to class variable to enable retraining
            self.model = model
        # check if alignment is used
        elif self.alignment:
            # retrieve class model states
            model = W2V(min_count = 0, workers=1, seed=15)
            # build up vocabulary of current iteration
            model.build_vocab(corpus)
            # train Word2Vec model on corpus
            model.train(corpus, total_examples=model.corpus_count, epochs=10)
            # check if previous model has been trained
            if len(self.model.wv) == 0 or date == '2020-01-01':
                # no action as then nothing needs to be done
                pass
            else:
                # assign previous model stored in class variable to variable called previousModel
                previousModel = self.model
                # perform procrustes based alignment of gensim models with previous model
                model = smart_procrustes_align_gensim(previousModel, model)
            self.model = model
        else:
            # initialize Word2Vec model
            model = W2V(min_count = 0, workers=1, seed=15)
            # pass corpus to build vocabolary for Word2Vec model
            model.build_vocab(corpus)
            # train Word2Vec model on corpus
            model.train(corpus, total_examples=model.corpus_count, epochs=10)
        # save trained model
        modelPath = f'{str(loggingPath)}/entityVector{date}.pkl'
        entityVector = [model.wv.get_vector(entity) for entity in entities]
        dictEntity = dict(zip(entities, entityVector))
        with open(modelPath, 'wb') as f:
            pickle.dump(dictEntity, f)
        # delete variables with large memory consumption
        del corpus, walkPredicateList, model

    def kgTransformer(self, fileDict) -> dict:
        print('Transformation started')
        # initialize result dictionary
        resultDict = {}
        # iterate over given transform dataframe
        for transformKey in tqdm(fileDict, desc='Transformation iteration'):
            modelFilePath = fileDict[transformKey]
            with open(modelFilePath, 'rb') as file:
                entityVector = pickle.load(file)
            resultDict[transformKey] = entityVector
        return resultDict

    def vectorDf(self, vectorDict) -> None:
        kgVectorDfList = []
        for dateKey in tqdm(vectorDict.keys(), desc='Pandas Df generation'):
            valueDict = vectorDict[dateKey]
            columnNameList = [f'osmVector{number}' for number in range(0, 100)]
            kgVectorDf = pd.DataFrame.from_dict(valueDict, orient='index', columns=columnNameList)
            kgVectorDf['ID'] = kgVectorDf.index
            kgVectorDf['DATE'] = dateKey
            kgVectorDfList.append(kgVectorDf)
        kgVectorCompleteDf = pd.concat(kgVectorDfList, ignore_index=True)
        kgVectorCompleteDf.to_csv(f'{self.savePath}/vectorDf.csv', index=False)
    
    def vectorClustering(self, vectorDict):
        kgDf = pd.DataFrame()
        for date in tqdm(vectorDict):
            valuesList = list(vectorDict[date].values())
            valuesArray = np.stack(valuesList)
            clusterValues = KMeans(n_clusters=10, random_state=15).fit_predict(valuesArray)
            resultKeys = list(vectorDict[date].keys())
            resultDf = pd.DataFrame(data={'ID': resultKeys, 'osmCluster': clusterValues.tolist()})
            resultDf['DATE'] = date
        kgDf = pd.concat([kgDf, resultDf], ignore_index=True)
        kgDf.to_csv(f'{self.savePath}/osmCluster.csv', index=False)

if __name__ == '__main__':
    # initialize the command line argparser
    parser = argparse.ArgumentParser(description='RDF2Vec argument parameters')
    # add train argument parser
    parser.add_argument('-t', '--train', default=False, action='store_true',
    help="use parameter if Word2Vec training should be performed")
    # add clustering argument
    parser.add_argument('-c', '--clustering', default=False, action='store_true',
    help="use parameter if clustering should be performed on extracted vectors")
    # add path argument parser
    parser.add_argument('-p', '--path', type=str, required=True,
    help='string value to data path')
    # add distance argument parser
    parser.add_argument('-d', '--distance', type=int, required=True,
    help='walk distance from selected node')
    # add walk number argument parser
    parser.add_argument('-w', '--walknumber', type=int, required=True,
    help='maximum walk number from selected node')
    # add chunksize argument
    parser.add_argument('-chunk', '--chunksize', type=int, required=True,
    help="use parameter to determine chunksize for parallel processing")
    parser.add_argument('-save', '--savepath', type=str, required=True,
    help="use parameter to save path for files")
    parser.add_argument('-r', '--retrain', default=False, action='store_true',
    help="use parameter if Word2Vec model should be retrained to align vector spaces")
    parser.add_argument('-a', '--alignmentprojection', default=False, action='store_true',
    help="use parameter if extracted vectors should be aligned")
    # store parser arguments in args variable
    args = parser.parse_args()
    kgEmbedding(args.path, args.distance, args.walknumber, args.train,
                args.clustering, args.chunksize, args.savepath, args.retrain,
                args.alignmentprojection)