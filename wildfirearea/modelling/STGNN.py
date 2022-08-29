# load packages
import torch
from torch_geometric_temporal.signal import DynamicGraphTemporalSignal
from torch_geometric_temporal.nn.attention.stgcn import STConv,TemporalConv
import pandas as pd
import numpy as np
import torch.nn as nn
from pathlib import Path
from itertools import product
from sklearn.preprocessing import OneHotEncoder
from tqdm.notebook import tqdm
import multiprocessing as mp

class STGNNClass:
    def __init__(self):
        print('yes')
        self.nodeIndex = 0
        self.nodeDict = {}
        self.previousWildfires = []
        
    def dataLoader(self, filePath):
        # read in csv
        data = pd.read_csv(filePath)
        date = filePath.stem
        # transform from to column into numpy array
        nodes = data[['from', 'to']].values.flatten()
        # construct edge dataframe
        edgeDf = data[['from', 'to', 'description']]
        # construct dataframe with each wildfire
        wildfireDf = data[['from', 'to', 'WILDFIRE']]
        # assign nodes to node dictionary
        for node in nodes:
            node = str(node)
            if node in self.nodeDict:
                continue
            else:
                self.nodeDict[node] = self.nodeIndex
                self.nodeIndex += 1
        # transform edge description into one hot encoded feature set
        oneHotEncodeDescription = pd.get_dummies(edgeDf.description)
        edgeDf = edgeDf.drop(labels=['description'], axis=1)
        edgeFeature = pd.concat([edgeDf, oneHotEncodeDescription], axis=1)
        columnAgg = dict(product(oneHotEncodeDescription.columns.values, ['max']))
        edgeFeature = edgeFeature.groupby(by=['from', 'to']).agg(columnAgg)
        # extract labels
        labelFrom = wildfireDf.groupby(by=['from']).agg({'WILDFIRE': max})
        labelTo = wildfireDf.groupby(by=['to']).agg({'WILDFIRE': max})
        nodeLabel = labelFrom.append(labelTo)
        nodeLabel['node'] = nodeLabel.index
        # create edge index from edge dataframe
        edgeIndexList = list(edgeFeature.index)
        # construct edge feature Dictionary
        edgeFeatureDict = dict(zip(edgeFeature.index, edgeFeature.values))
        # transform dictionary to edge features
        edgeFeatures = np.array([edgeFeatureDict[edgeIndex] for edgeIndex in edgeFeature.index])
        # construct edge indices numpy array
        edgeIndicesFrom = np.array([self.nodeDict[edgeIndex[0]] for edgeIndex in edgeFeature.index])
        edgeIndicesTo = np.array([self.nodeDict[edgeIndex[1]] for edgeIndex in edgeFeature.index])
        edgeIndices = np.stack((edgeIndicesFrom, edgeIndicesTo), axis=0)
        # labels to numpy array
        nodeLabel = nodeLabel.groupby(by=['node']).agg({'WILDFIRE': max})
        labels = nodeLabel['WILDFIRE'].values
        # previous wildfire occurence
        global previousWildfires
        if len(previousWildfires) == 0:
            nodeFeatures = np.zeros(nodes.shape)
        else:
            nodeFeatures = np.array([previousWildfires[nodeName] if nodeName in previousWildfires else 0 for nodeName in nodeLabel.index])
        # assign wildfire labels to previous wildfires
        previousWildfires = labels
        # return numpy arrays
        return edgeIndices, edgeFeatures, nodeFeatures, labels

class STGNN(nn.Module):
  def __init__(self, nodeNumber, numberFeatures, hiddenChannels, outChannels, kernelSize, filter):
    super(STGNN, self).__init__()
    self.STC1 = STConv(num_nodes = nodeNumber, in_channels = numberFeatures, hidden_channels=hiddenChannels, out_channels = outChannels, kernel_size = kernelSize, K = filter, normalization=None)
    self.STC2 = STConv(num_nodes = nodeNumber, in_channels = numberFeatures*2, hidden_channels=hiddenChannels, out_channels = outChannels*2, kernel_size = kernelSize, K = filter, normalization=None)
    self.output = nn.linear(outChannels*2, 2)
  def forward(self, x, edgeIndex, edgeWeight):
    outputSTC1 = self.STC1(x, edgeIndex, edgeWeight)
    outputSTC2 = self.STC2(outputSTC1)
    output = self.output(outputSTC2)
    return nn.log_softmax(dim=1)(output)

if __name__ == '__main__':

    edgeIndicesList = [element[0] for element in result]
    edgeFeaturesList = [element[1] for element in result]
    nodeFeaturesList = [element[2] for element in result]
    labelsList = [element[3] for element in result]
    dataLoader = DynamicGraphTemporalSignal(edgeIndicesList, edgeFeaturesList, nodeFeaturesList, labelsList)
    stgnn = STGNN(nodeNumber = len(nodeDict), numberFeatures = 3, hiddenChannels=1, outChannels=32, kernelSize=1, filter=1)
    optimizer = torch.optim.Adam(stgnn.parameters(), lr=0.01)
    for epoch in tqdm(range(200)):
        for time, snapshot in enumerate(dataLoader):
            print(snapshot)
            y_hat = stgnn(snapshot.x, snapshot.edge_index, snapshot.edge_attr)
            print(y_hat)
            break