#!/usr/bin/python
# -*-coding:utf-8 -*
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
#imports 
import os
from classes import *
from optparse import OptionParser

######## CONST VARS ###########################################################################
NB_COLORS=10
colorTab = ["red", "green", "blue", "yellow", "orange", "purple", "aquamarine", "chocolate", "grey", "gold"]
x_oh = 0
x_sizeMax = 1024
y_oh = 60
y_sizeMax = 800
y_nbUnits= 30

######### X VARS ##############################################################################
x_nbUnits = 16
x_sizeUnit = int(x_sizeMax/x_nbUnits)
x_nbUnits = 16

######### Y VARS ##############################################################################
y_sizeUnit = int(y_sizeMax/y_nbUnits)
y_timeMax = 0
y_timeUnit = 0

######### OTHERS VARS #########################################################################
TotalNbTests = 0
header_svg = ""
footer_svg = ""
filename_in = ""
filename_out = "output.svg"
totalFilledSurface=0.0
#header generation 
#header_svg  = "<html><head></head><body>\n"
header_svg = ""
header_svg +="<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\""+str(x_sizeMax+64)+"\" height=\""+str(y_sizeMax+y_oh)+"\">\n"
header_svg += "<script language='javascript'>function display(name, time, number){ alert('Test name : '+name+'\\nTime duration : '+time+'\\nNb Nodes : '+number+'\\n');}</script>"
#footer
footer_svg = "</svg>\n"
#footer_svg += "</body>\n</html>"

#parser
using="Usage: %prog [options] INPUT_FILE"
parser=OptionParser(usage=using)
settingOptions(parser)

options,args=parser.parse_args()
if options.dest != "":
    filename_out=options.dest
filename_in = args[0]

#file reading
file_in = open(filename_in, "r")
buff = file_in.read()
file_in.close()
contenu = buff.split("\n")

temp = contenu[0].split(":")
x_nbUnits=int(temp[1])
begin_time=int(float(temp[2])*100)
x_sizeUnit = int(x_sizeMax/x_nbUnits)
mainList = testsList(x_nbUnits)

#each line loop
for elt in contenu:
      #if elt[0] == "#":
	#      continue
      obj = test(x_nbUnits)
      tabArgs = elt.split(":")
      obj.nom = tabArgs[0]

      #throw exception( cast str -> int and assertion)
      try:
            obj.t0.initTimeStamp(tabArgs[1])
            obj.t1.initTimeStamp(tabArgs[2])
            assert obj.t0.getTimeStamp() <= obj.t1.getTimeStamp()
            obj.nbNodes = int(tabArgs[3])
      except:
      #      print("ERROR: Unable to parse line: ",elt);
            continue
      #adding
      mainList.testList.append(obj)
      TotalNbTests+=1

#scheduling loop
for i in range(0,TotalNbTests):
      #find test with time the smallest
      nextObj = mainList.testList[mainList.findNextTest(i)]
      mainList.testList.remove(nextObj)
      #compare with treated tests
      mainList.fixPosition(nextObj)
      #update refNodes (adjust to the good size)
      nextObj.finalize(x_nbUnits)
      #fix treated test
      mainList.treatedTestList.append(nextObj)
      totalFilledSurface+=(nextObj.nbNodes*(nextObj.t1.getTimeStamp()-nextObj.t0.getTimeStamp()))

#header writing in file
file_out = open(filename_out, "w")
nb = file_out.write(header_svg)

#setting Y dimensions
y_timeMax = mainList.findLastTest().t1.getTimeStamp()-begin_time
if y_timeMax <= 1:
	y_timeMax = 1

y_timeUnit = float(float(y_timeMax)/float(y_nbUnits))
y_timePerPx = float(float(y_timeUnit)/float(y_sizeUnit))
#color cpt
j=0

#tests writing
for current_test in mainList.treatedTestList:
      current_test.writingData(file_out, x_sizeUnit, y_timePerPx, x_nbUnits, x_oh, y_oh, begin_time,colorTab[j%NB_COLORS])
      #color id
      j+=1

#line trace to delimit the graph
totalAvailableSurface=int(x_nbUnits*y_timeMax)
surfacefilledAverage=float((totalFilledSurface/totalAvailableSurface)*100)
tabTrace(file_out, x_sizeUnit, y_timeUnit,y_timePerPx, y_timeMax , x_nbUnits, y_nbUnits, x_oh, y_oh, surfacefilledAverage)

#footer writing in file
nb = file_out.write(footer_svg)
file_out.close()
