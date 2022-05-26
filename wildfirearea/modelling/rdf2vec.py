# import packages
import pandas as pd
import pickle
from tqdm import tqdm
from pathlib import Path
from pyrdf2vec import RDF2VecTransformer
from pyrdf2vec.embedders import Word2Vec
from pyrdf2vec.graphs import KG, Vertex
from pyrdf2vec.walkers import RandomWalker


class kgEmbedding:

    def __init__(self, dataPath):
        self.dataPath = Path(dataPath)
        trainingDf, transformDf, entities = self.dataPreparation()
        transformer = self.kgTraining(trainingDf, entities)
        resultDict = self.kgTransformer(transformer, transformDf, entities)
        with open('wildfirearea/modelling/KnowledgeGraph', 'wb') as f:
            pickle.dump(resultDict, f)


    def dataPreparation(self):
        print('Data Preparation')
        # Read a CSV file containing the entities we want to classify.
        graphData = pd.read_csv(self.dataPath)
        trainingDf = graphData.groupby('YEAR')[['from', 'to', 'description']].agg(list)
        transformDf = graphData.groupby(['YEAR', 'ID'])[['from', 'to', 'description']].agg(list)
        entitiesFrom = list(set(graphData['from']))
        entitiesTo = list(set(graphData['to']))
        entities = list(set(entitiesFrom + entitiesTo))
        return trainingDf, transformDf, entities


    def kgTraining(self, trainingDf, inputEntities):
        #initalize rdf2vec transformer dictionary
        transformerDict = {}
        # build knowledge graph
        for index, row in tqdm(trainingDf.iterrows()):
            # initialize knowledge Graph
            osmKG = KG()
            rowValues = pd.DataFrame(dict(row)).values.tolist()
            for row in tqdm(rowValues.iterrows(), leave=False):
                subj = Vertex(row[0])
                obj = Vertex(row[1])
                pred = Vertex(row[2], predicate=True, vprev=subj, vnext=obj)
                osmKG.add_walk(subj, pred, obj)
            # build transformer
            transformer = RDF2VecTransformer(
                Word2Vec(epochs=20),
                walkers=[RandomWalker(8, 12, with_reverse=False, n_jobs=-2)],
                verbose=1
            )
            # extract embeddings
            transformer.fit(osmKG, inputEntities)
            transformerDict[index] = transformer
        return transformer

    def kgTransformer(self, transformer, transformDf, inputEntities):
        resultDict = {}
        for index, row in tqdm(transformDf.iterrows()):
            rowValues = pd.DataFrame(dict(row)).values.tolist()
            osmKG = KG()
            for row in tqdm(rowValues, leave=False):
                subj = Vertex(row[0])
                obj = Vertex(row[1])
                pred = Vertex(row[2], predicate=True, vprev=subj, vnext=obj)
                osmKG.add_walk(subj, pred, obj)
            entities, literals = transformer.transform(osmKG, inputEntities)
            resultDict[index] = {'entities': entities, 'literals': literals}
        return resultDict

if __name__ == '__main__':
    kgEmbedding('data/network/openstreetmapGraph.csv')