# import packages
import pandas as pd
from tqdm import tqdm
from pyrdf2vec import RDF2VecTransformer
from pyrdf2vec.embedders import Word2Vec
from pyrdf2vec.graphs import KG, Vertex
from pyrdf2vec.walkers import RandomWalker

# Read a CSV file containing the entities we want to classify.
graphData = pd.read_csv("data/network/openstreetmapGraph.csv")
print(graphData.head())
graphDataList = graphData.values.tolist()
entities = list(set(graphData['from'].tolist()))
# initialize knowledge Graph
osmKG = KG()
# build knowledge graph
for row in tqdm(graphDataList):
    subj = Vertex(row[0])
    obj = Vertex(row[1])
    pred = Vertex(row[2], predicate=True, vprev=subj, vnext=obj)
    osmKG.add_walk(subj, pred, obj)
# build transformer
transformer = RDF2VecTransformer(
    Word2Vec(epochs=10),
    walkers=[RandomWalker(4, 10, with_reverse=False, n_jobs=2)],
    verbose=1
)
# extract embeddings
embeddings, literals = transformer.fit_transform(osmKG, entities)
print(literals[:8])