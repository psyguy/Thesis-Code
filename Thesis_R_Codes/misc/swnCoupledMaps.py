import numpy as np
from scipy import linalg
import matplotlib.pyplot as plt
import swnBinaryMetrics as swnb
import swnWeightedMetrics as swnw

# ESTIMATENODENEXTSTATE outputs a vector with the t+1 states of the nodes using coupled quadratic logistic maps (Gong, Leeuwen 2004 Europhysics)
# INPUT
# A: adjacency matrix, can be either binary or weighted
# stateNodeVec: the state of nodes at time t
# couplingStrength, alpha: parameters in the equation
# OUTPUT
# nextStateNodeVec: the vector with the t+1 states of the nodes


def estimateNodeNextState(A, stateNodeVec, couplingStrength, alpha):

    deg = np.sum(A, axis=1)
    fstateNodeVec = 1 - alpha * (stateNodeVec**2)
    weightedSumF = np.sum(A * fstateNodeVec, axis=1)  # becomes a row vector and tiled along the rows
    nextStateNodeVec = ((1 - couplingStrength) * fstateNodeVec) + (couplingStrength / deg) * weightedSumF

    return nextStateNodeVec


# REWIRECOUPLEDMAP rewires the adjacency matrix using coupled logistic maps according to (Gong, Leeuwen 2004 Europhysics)
# INPUT
# Arand: random symmetric adjacency matrix
# transientTime: the time(iterations) for which you let the states evolve but you do not start the rewiring
# iterations: number of iterations the wiring take place according to the similarity rule
# couplingStrength: the epsilon parameter in the equation
# alpha: the alpha parameter in the logistic map equation
# OUTPUT
# A: returns a rewired symmetric matrix
def rewireCoupledMap(Arand, transientTime, iterations, couplingStrength, alpha):

    A = Arand.copy()

    vertices = A.shape[0]

    stateNodeVec = np.random.uniform(low=-1.0, high=1.0, size=vertices)  # initialize the state of the nodes
    # you iterate for some time (transient time) the states
    for r in range(transientTime):
        stateNodeVec = estimateNodeNextState(A, stateNodeVec, couplingStrength, alpha)

    indAll = np.arange(vertices)
    # You will have in a separate loop here the iterations of the map
    for k in range(iterations):
        deg = np.sum(A, axis=1)  # deg[i] = the number of connections of i+1 node
        if any(deg == 0):
            print('For %d iterations, coupling strength = %f, and bifurcation parameter = %f one of the nodes has no connections. Redoing the rewiring' % (iterations, couplingStrength, alpha))
            return rewireCoupledMap(Arand, transientTime, iterations, couplingStrength, alpha)

        stateNodeVec = estimateNodeNextState(A, stateNodeVec, couplingStrength, alpha)

        nodeRandInd = np.random.choice(vertices)  # pick a random node's ind
        distAllNodes = np.abs(stateNodeVec - stateNodeVec[nodeRandInd])  # measure the absolute distance between node state values

        indMinusSelf = np.delete(indAll, nodeRandInd)  # get the indices of the nodes it is connected to
        minTemp = np.argmin(distAllNodes[indMinusSelf])
        minInd = indMinusSelf[minTemp]

        # We want to find the maximum difference among connected nodes
        AOnesInd = np.argwhere(A[:, nodeRandInd] > 0)
        maxTemp = np.argmax(distAllNodes[AOnesInd])
        maxInd = AOnesInd[maxTemp][0]

        if A[nodeRandInd, minInd] == 0:
            #print('rewiring in %d iteration' % (k))
            A[nodeRandInd, minInd] = A[nodeRandInd, maxInd]
            A[minInd, nodeRandInd] = A[nodeRandInd, maxInd]
            A[nodeRandInd, maxInd] = 0
            A[maxInd, nodeRandInd] = 0

    return A


def getMetricsPlotAdjCMIter(iterations, vertices, edges, transientTime, couplingStrength, alpha, weight):

    total = len(iterations)

    clusterCoefAll = np.zeros(total)
    pathLengthAll = np.zeros(total)
    smallWorldnessAll = np.zeros(total)

    if weight == 'False':
        Arand = swnb.generateBinaryRandSymAdj(vertices, edges)
        (smallWorldnessAll[0], clusterCoefAll[0], pathLengthAll[0]) = swnb.compSmallWorldness(Arand)
        cmap = 'Greys'
    elif weight == 'True':
        Arand = swnw.generateWeightedRandSymAdj(vertices, edges, weightDistribution='normal')
        (smallWorldnessAll[0], clusterCoefAll[0], pathLengthAll[0]) = swnw.compWeightSmallWorldness(Arand)
        cmap = 'hot'

    plt.rcParams['figure.figsize'] = [20, 7]
    plt.subplot(2, int(total / 2), 1)
    plt.title('random')
    ArandReord = swnb.reorderA2Visualize(Arand)
    plt.imshow(ArandReord, cmap=cmap)
    for counter, iteration in enumerate(iterations[1:]):
        A = rewireCoupledMap(Arand, transientTime, iteration, couplingStrength, alpha)
        AReord = swnb.reorderA2Visualize(A)
        ttl = '%d iterations' % (iteration)
        plt.subplot(2, int(total / 2), counter + 2)
        plt.title(ttl)
        plt.imshow(AReord, cmap=cmap)
        print(ttl)
        if weight == 'False':
            (smallWorldnessAll[counter + 1], clusterCoefAll[counter + 1], pathLengthAll[counter + 1]) = swnb.compSmallWorldness(A)
        elif weight == 'True':
            (smallWorldnessAll[counter + 1], clusterCoefAll[counter + 1], pathLengthAll[counter + 1]) = swnw.compWeightSmallWorldness(A)

    plt.show()

    return (clusterCoefAll, pathLengthAll, smallWorldnessAll)
