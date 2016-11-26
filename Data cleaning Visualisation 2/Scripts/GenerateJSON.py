import sys
import json

class JNode:
    def __init__(self, index, links, label):
        self.index = index
        self.links = links
        self.label = label
        self.score = 7 #same bubble size for all
        self.id = index

    def toJSON(self):
        return json.dumps({'index':self.index,'links':self.links,'label':self.label,'score':self.score,'id':self.index},separators=(',', ':'),indent=4)

class JLink:
    def __init__(self, source, target, weight):
        self.source = source
        self.target = target
        self.weight = weight

    def toJSON(self):
        return json.dumps({'source':self.source,'target':self.target,'weight':self.weight},separators=(',', ':'),indent=4)


sepChar = ';'

countriesList = [];
nodesList = [];
linksList = [];

FileLink = sys.argv[1]
FileCountry = sys.argv[2]


linesLink = [line.rstrip('\n') for line in open(FileLink)]
linesCountry = [line.rstrip('\n') for line in open(FileCountry)]

print len(linesCountry)
for i in range(1, len(linesCountry)):
	lineList = linesCountry[i].split(sepChar)
	countriesList.append(lineList[1])
	nodesList.append(JNode(len(countriesList)-1,[],lineList[1]))


#for i in range(1, len(linesLink)):
#    lineList = linesLink[i].split(sepChar)
#    if(lineList[2] != lineList[3]):
#        #node
#        currentCountryIndex = countriesList.index(lineList[2])
#        cNode = nodesList[currentCountryIndex]
#        cNode.links.append(countriesList.index(lineList[3]))
#        #link
#        linksList.append(JLink(cNode.index, countriesList.index(lineList[3]), float(lineList[5])))

#write JSON output file
outputName = "output"
outputFile = open(outputName, 'w')

outputFile.write('{\n')
outputFile.write('"nodes": [\n');
for i in range(len(nodesList)-1):
    outputFile.write(nodesList[i].toJSON())
    outputFile.write(',\n')
#outputFile.write(nodesList[len(nodesList)-1].toJSON())
#outputFile.write('\n')
#outputFile.write('],\n')
#outputFile.write('"links":[\n')
#for i in range(len(linksList)-1):
#    outputFile.write(linksList[i].toJSON())
#    outputFile.write(',\n')
#outputFile.write(linksList[len(linksList)-1].toJSON())
#outputFile.write(']\n')
#outputFile.write('}')
