#Scripts/GenerateJSONfororg.py Output/NbLinkOrgLevel1.csv Output/OrgInformation.csv


import sys
import json
import operator

projMin = 1;
projMax = 1;
maxBubblesize = 20;

class JNode:
    def __init__(self, index, links, line, label):
        self.index = index
        self.links = links
        self.label = label
        self.score = float(line[3])
        self.id = index
        if self.label == line[4]:
            self.level = 1
        else:
            self.level = 2

    def toJSON(self):
        return json.dumps({'index':self.index,'links':self.links,'label':self.label,'score':self.score,'id':self.index, 'level':self.level},separators=(',', ':'),indent=4)

    def setScore(self, projectNbr):
        self.score = (((float(projectNbr) - projMin)*(maxBubblesize - 1)) / (projMax - projMin)) + 1
        #NewValue = (((OldValue - OldMin) * (NewMax - NewMin)) / (OldMax - OldMin)) + NewMin

    def getID(self):
        return self.id;

class JLink:
    def __init__(self, source, target, weight):
        self.source = source
        self.target = target
        self.weight = weight
    def toJSON(self):
        return json.dumps({'source':self.source,'target':self.target,'weight':self.weight},separators=(',', ':'),indent=4)


#write JSON output file
def WriteJSON(nameOutputFile, orig_nodelist, linksList):
    try:
        nodelist = sorted(orig_nodelist, key=operator.attrgetter("id"))
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
    except Exception as e:
        print(e)
        print(nameOutputFile)



sepChar = ';'

orgDictionary = {};


FileLink = sys.argv[1]
FileOrg = sys.argv[2]

linesLink = [line.rstrip('\n') for line in open(FileLink, encoding="utf-8")]
orgsLink = [line.rstrip('\n') for line in open(FileOrg, encoding="utf-8")]

count = 0
for i in range(1, len(linesLink)):
    lineList = linesLink[i].split(sepChar)
    try:
        value = orgDictionary[lineList[1]];
    except KeyError:
        orgDictionary[lineList[1]] = JNode(count,[],lineList,lineList[1])
        count += 1

for i in range(1, len(linesLink)):
    lineList = linesLink[i].split(sepChar)
    try:
        value = orgDictionary[lineList[2]];
    except KeyError:
        orgDictionary[lineList[2]] = JNode(count,[],lineList,lineList[2])
        count += 1

projMax = float(orgsLink[1].split(sepChar)[2])
projMin = float(orgsLink[1].split(sepChar)[2])

orgInfoDict = {}

for i in range(1, len(orgsLink)):
    orgList = orgsLink[i].split(sepChar)
    try:
        value = orgDictionary[orgList[1]];
        orgInfoDict[orgList[1]] = orgList;
        if(float(orgList[2]) > projMax):
            projMax = float(orgList[2])
        elif(float(orgList[2]) < projMin):
            projMin = float(orgList[2])
    except KeyError:
        pass

for key, value in orgInfoDict.items():
    orgDictionary[key].setScore(value[2])

name = "";
NodeDic = {};
linksList = [];
for i in range(1, len(linesLink)):
    lineList = linesLink[i].split(sepChar)
    curName = lineList[4]
    if(name != curName and len(NodeDic) != 0):
        NodeList = [];
        for key, value in NodeDic.items():
            NodeList.append(value)
        WriteJSON("outputJS/" + name.replace(' ','_') + ".json", NodeList, linksList)
        NodeDic = {};
        linksList = [];
    name = curName

    org1Name = lineList[1]
    org2Name = lineList[2]
    if (not org1Name in NodeDic)  :
        NodeDic[org1Name] = orgDictionary[org1Name]
    if (not org2Name in NodeDic):
        NodeDic[org2Name] = orgDictionary[org2Name]

    org1Index = NodeDic[org1Name].id
    org2Index = NodeDic[org2Name].id

    NodeDic[org1Name].links.append(org2Index)
    NodeDic[org2Name].links.append(org1Index)
    #compute weight according to formula :
    #NewValue = (((OldValue - OldMin) * (NewMax - NewMin)) / (OldMax - OldMin)) + NewMin
    linkWeight = (((float(lineList[3])-1)*(10-1))/(39-10)+1)
    linksList.append(JLink(org1Index, org2Index, linkWeight))


NodeList = [];
for key, value in NodeDic.items():
    NodeList.append(value)
WriteJSON("outputJS/" + name + ".json", NodeList, linksList)
NodeDic = {};
linksList = [];
