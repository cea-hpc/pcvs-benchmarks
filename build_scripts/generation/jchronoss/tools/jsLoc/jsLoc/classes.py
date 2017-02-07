#!/usr/bin/python
############################################################################
#                                                                          #
#                         Copyright or (C) or Copr.                        #
#       Commissariat a l'Energie Atomique et aux Energies Alternatives     #
#                                                                          #
# Version : 1.2                                                            #
# Date    : Tue Jul 22 13:28:10 CEST 2014                                  #
# Ref ID  : IDDN.FR.001.160040.000.S.P.2015.000.10800                      #
# Author  : Julien Adam <julien.adam@cea.fr>                               #
#           Marc Perache <marc.perache@cea.fr>                             #
#                                                                          #
# This file is part of JCHRONOSS software.                                 #
#                                                                          #
# This software is governed by the CeCILL-C license under French law and   #
# abiding by the rules of distribution of free software.  You can  use,    #
# modify and/or redistribute the software under the terms of the CeCILL-C  #
# license as circulated by CEA, CNRS and INRIA at the following URL        #
# "http://www.cecill.info".                                                #
#                                                                          #
# As a counterpart to the access to the source code and  rights to copy,   #
# modify and redistribute granted by the license, users are provided only  #
# with a limited warranty  and the software's author,  the holder of the   #
# economic rights,  and the successive licensors  have only  limited       #
# liability.                                                               #
#                                                                          #
# In this respect, the user's attention is drawn to the risks associated   #
# with loading,  using,  modifying and/or developing or reproducing the    #
# software by the user in light of its specific status of free software,   #
# that may mean  that it is complicated to manipulate,  and  that  also    #
# therefore means  that it is reserved for developers  and  experienced    #
# professionals having in-depth computer knowledge. Users are therefore    #
# encouraged to load and test the software's suitability as regards their  #
# requirements in conditions enabling the security of their systems and/or #
# data to be ensured and,  more generally, to use and operate it in the    #
# same conditions as regards security.                                     #
#                                                                          #
# The fact that you are presently reading this means that you have had     #
# knowledge of the CeCILL-C license and that you accept its terms.         #
#                                                                          #
############################################################################

#define the timestamp manipulation
class testTimeStamp:
      def __init__(self):
            self.time = 0

      def initTimeStamp(self, chain):
            try:
                  self.time = int(float(chain)*100)
            except:
                  print("ERROR:", chain, " is not convertible to number\n")
                  return 0

            return 1
      
      def getTimeStamp(self):
            return self.time
      
      def setTimeStamp(self, time):
            selt.time = time

      def isBefore(self, ts):
            return(self.time < ts.time)

      def getDifferenceTimeStamp(self, ts2):
            return(self.time - ts2.time)

#define a test
class test:
      def __init__(self, nbNodes):
            self.nom =""
            self.t1 = testTimeStamp()
            self.t0 = testTimeStamp()
            self.nbNodes = 0
            self.refNodes = list()
            self.initRefNodes(nbNodes)

      def initRefNodes(self, nbNodes):
            i=0
            while i < nbNodes:
                  self.refNodes.append(1)
                  i+=1

      def getTimeStampInterval(self):
            return(self.t1.getDifferenceTimeStamp(self.t0))

      def display(self):
            print("=============================\nAFFICHAGE D'UN TEST:")
            print("\t--> nom   = ", self.nom)
            print("\t--> t0    = ", self.t0.getTimeStamp())
            print("\t--> t1    = ", self.t1.getTimeStamp())
            print("\t--> Nodes = ", self.nbNodes)
            for i in self.refNodes:
                  print(i, " ")
            print("=============================")

      #CHECK if self test begans while a another test still running
      def isCrossingAnotherTest(self, current):
            return(self.t0.getTimeStamp() >= current.t0.getTimeStamp() and self.t0.getTimeStamp() < current.t1.getTimeStamp())

      #find nodes where aren't used by the current test
      def findNotCommonNode(self, current, nbNodes):
            i=0
            while i < nbNodes:
                  if self.refNodes[i] == 1 and current.refNodes[i] == 1:
                        self.refNodes[i] = 0
                  i+=1

      #finalize the nodes generation: delete "1" mark if there are too much than required
      def finalize(self, nbNodes):
            node =0
            recorded=0
            first_step=True
            while node < nbNodes:
                  if first_step:
                        if self.refNodes[node] == 1:
                              recorded+=1
                        if recorded == self.nbNodes:
                              first_step = False
                  else:
                        self.refNodes[node] = 0
                  node+=1
            assert recorded == self.nbNodes

      def writingData(self, file_out, sizeNode, y_timePerPx,TotalNbNodes, x_oh, y_oh, begin_time,color) :
            node=0
            while node < TotalNbNodes:
                  if self.refNodes[node] == 1:
                        line = " <rect onclick=\"display('"+str(self.nom)+"',"+str(float(self.getTimeStampInterval())/float(100))+","+str(self.nbNodes)+")\"" +\
                               "  x=\"" + str((sizeNode*node)+x_oh)+ \
                               "\" y=\"" + str(((self.t0.getTimeStamp()-begin_time)/y_timePerPx)+y_oh) + \
                               "\" width=\"" + str(sizeNode) + \
                               "\" height=\"" + str(self.getTimeStampInterval()/y_timePerPx)+ \
                               "\" style=\"fill:"+color + \
                               "\"/>\n"
                        file_out.write(line)
                  node+=1

#define the main object representing the graph
class testsList:
      def __init__(self, nbNodes):
            self.testList = list()
            self.treatedTestList = list()
            self.nbNodes = nbNodes
            
      def displayTests(self):
            print("AFFICHAGE DE LA LISTE:")
            for elt in self.testList:
                  elt.display()
                  
      #find the next smallest test
      def findNextTest(self,ind):
            minInd = 0
            minVal = self.testList[0].t0
            
            for i,elt in enumerate(self.testList):
                  if elt.t0.isBefore(minVal):
                        minInd = i
                        minVal = elt.t0
            return minInd
      
      def displayGraph(self):
            print("GRAPH:")
      def fixPosition(self, oneTest):
            for current_test in self.treatedTestList:
                  if oneTest.isCrossingAnotherTest(current_test):
                        oneTest.findNotCommonNode(current_test, self.nbNodes)
      
      #find the test which finish last
      def findLastTest(self):
            maxInd = 0
            maxVal = self.treatedTestList[0].t1

            for i,elt in enumerate(self.treatedTestList):
                  if not elt.t1.isBefore(maxVal):
                        maxInd = i
                        maxVal = elt.t1
            return self.treatedTestList[maxInd]
                        
#draw the graph lines
def tabTrace(file, sizeNode, y_timeUnit,y_timePerPx, totalTime, totalNbNodes,nbTimes, x_oh, y_oh, surfaceAverage):
      text_x_overhead = 4
      text_y_overhead = 6
      timeUnit = int(y_timeUnit)
      #draw columns
      for i in range(1, totalNbNodes):
            file.write("<line style=\"stroke:rgb(0,0,0);stroke-width:0.8\" y1=\""+str(y_oh/2)+"\""+\
                       " x1=\""+str(sizeNode*i)+"\""+\
                       " x2=\""+str(sizeNode*i)+"\""+\
                       " y2=\""+str((totalTime/y_timePerPx)+y_oh)+"\"/>\n")

      #draw number column
      for i in range(0, totalNbNodes):
	    if (y_oh/2) < sizeNode:
		    font=int(y_oh/2)
	    else:
		    font=sizeNode*0.5
            file.write("<text x=\""+str((sizeNode*i)+(sizeNode/5))+"\" y=\""+str((y_oh/2)*1.6)+"\" font-size=\""+str(font)+"\">"+str(i+1)+"</text>")

      #draw lines
      for j in range(0, int(nbTimes+1)):
            if j*timeUnit > totalTime:
		    break
            file.write("<line style=\"stroke:rgb(0,0,0);stroke-dasharray:5,5;stroke-width:0.6\" x1=\"0\" y1=\""+str((j*timeUnit/y_timePerPx)+y_oh)+"\" x2=\""+str(sizeNode*totalNbNodes)+"\" y2=\""+str((j*timeUnit/y_timePerPx)+y_oh)+"\" />\n")      
            file.write("<text x=\""+str(sizeNode*totalNbNodes + text_x_overhead)+"\" y=\""+str(j*timeUnit/y_timePerPx+text_y_overhead+y_oh)+"\">"+str(j*float(timeUnit)/100)+"</text>\n")
      
      #vertical graph edge
      file.write("<line style=\"stroke:rgb(0,0,0);stroke-width:2\" y1=\"0\" x1=\""+str(sizeNode*totalNbNodes)+"\" x2=\""+str(sizeNode*totalNbNodes)+"\" y2=\""+str((totalTime/y_timePerPx)+y_oh)+"\"/>\n")
      file.write("<line style=\"stroke:rgb(0,0,0);stroke-width:2\" y1=\"0\" x1=\"0\" y2=\""+str((totalTime/y_timePerPx)+y_oh)+"\" x2=\"0\" />\n")
      
      #horizonal graph edges
      file.write("<line style=\"stroke:rgb(0,0,0);stroke-width:2\" x1=\"0\" y1=\"0\" x2=\""+str(sizeNode*totalNbNodes)+"\" y2=\"0\"/>\n")
      file.write("<text x=\""+str((sizeNode*totalNbNodes)/5)+"\" y=\""+str((y_oh/2)*0.7)+"\">Job Scheduling Locality (JsLoc): Node x "+str(totalNbNodes)+" / Surface filling Rate : "+str(round(surfaceAverage, 2))+"%</text>\n")
      file.write("<line style=\"stroke:rgb(0,0,0);stroke-width:2\" x1=\"0\" y1=\""+str(y_oh/2)+"\" x2=\""+str(sizeNode*totalNbNodes)+"\" y2=\""+str(y_oh/2)+"\"/>\n")
      file.write("<line style=\"stroke:rgb(0,0,0);stroke-width:2\" x1=\"0\" y1=\""+str(y_oh)+"\" x2=\""+str(sizeNode*totalNbNodes)+"\" y2=\""+str(y_oh)+"\"/>\n")
      file.write("<line style=\"stroke:rgb(0,0,0);stroke-width:2\" x1=\"0\" y1=\""+str((totalTime/y_timePerPx)+y_oh)+"\" x2=\""+str(sizeNode*totalNbNodes)+"\" y2=\""+str((totalTime/y_timePerPx)+y_oh)+"\"/>\n")
     
def settingOptions(parser):
    parser.add_option("-d", "--dest", dest="dest", metavar="STRING", help="The file where SVG instructions will be stored.")
