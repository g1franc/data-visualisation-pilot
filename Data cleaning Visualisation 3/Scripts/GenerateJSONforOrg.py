#Scripts/GenerateJSONfororg.py Output/NbLinkOrg.csv Output/OrgInformation.csv


import sys
import json

class JNode:
	def __init__(self, index, links, line):
		self.index = index
		self.links = links
		self.label = line[1]
		self.NumberProjectParticipation = line[2]
		self.score = 7 #same bubble size for all
		self.id = index
	def toJSON(self):
		return json.dumps({'index':self.index,'links':self.links,'label':self.label,'NumberProjectParticipation':self.NumberProjectParticipation,'score':self.score,'id':self.index},separators=(',', ':'),indent=4)

class JLink:
    def __init__(self, source, target, weight):
        self.source = source
        self.target = target
        self.weight = weight

    def toJSON(self):
        return json.dumps({'source':self.source,'target':self.target,'weight':self.weight},separators=(',', ':'),indent=4)


sepChar = ';'

orgList = [];
nodesList = [];
linksList = [];

FileLink = sys.argv[1]
FileOrg = sys.argv[2]


linesLink = [line.rstrip('\n') for line in open(FileLink)]
linesOrg = [line.rstrip('\n') for line in open(FileOrg)]


for i in range(1, len(linesOrg)):
	lineList = linesOrg[i].split(sepChar)
	orgList.append(lineList[1])
	nodesList.append(JNode(len(orgList)-1,[],lineList))


for i in range(1, len(linesLink)):
	lineList = linesLink[i].split(sepChar)
	if(lineList[1] != lineList[2]):
		#node
		framework = lineList[4]
		currentOrgIndex = orgList.index(lineList[1])
		cNode = nodesList[currentOrgIndex]
		cNode.links.append(orgList.index(lineList[2]))
		linksList.append(JLink(cNode.index, orgList.index(lineList[2]), float(lineList[4])))

#write JSON output file
def WriteJSON(nameOutputFile, nodelist, linksList):
	outputName = nameOutputFile
	outputFile = open(outputName, 'w')
	outputFile.write('{\n')
	outputFile.write('"nodes": [\n');
	for i in range(len(nodelist)-1):
		outputFile.write(nodelist[i].toJSON())
		outputFile.write(',\n')
	outputFile.write(nodelist[len(nodelist)-1].toJSON())
	outputFile.write('\n')
	outputFile.write('],\n')
	outputFile.write('"links":[\n')
	for i in range(len(linksList)-1):
		outputFile.write(linksList[i].toJSON())
		outputFile.write(',\n')
	outputFile.write(linksList[len(linksList)-1].toJSON())
	outputFile.write(']\n')
	outputFile.write('}')



WriteJSON("outputJS/org.json",nodesList, linksList)


