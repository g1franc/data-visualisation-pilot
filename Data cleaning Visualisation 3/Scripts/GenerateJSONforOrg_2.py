#Scripts/GenerateJSONfororg_2.py Output/Org.csv
import sys
import json
import operator

class JOrgList:
    def __init__(self, name, country, activity):
        self.name = name
        self.country = country
        self.activity = activity

    def toJSON(self):
        return json.dumps({'name': self.name, 'country': self.country, 'activity': self.activity},separators=(',', ':'),indent=4)

#write JSON output file
def WriteJSON(nameOutputFile, org_list):
    try:
        outputFile = open(nameOutputFile, 'w')
        outputFile.write('[\n')
        for i in range(len(org_list)-1):
            outputFile.write(org_list[i].toJSON())
            outputFile.write(',\n')
        outputFile.write(org_list[len(org_list)-1].toJSON())
        outputFile.write('\n')
        outputFile.write(']')
    except Exception as e:
        print(e)
        print(nameOutputFile)

#read file
FileOrg = sys.argv[1]
lines = [line.rstrip('\n') for line in open(FileOrg, encoding="utf-8")]

#define some parameters
sepChar = ";"
orgDictionary = {};

#Create a dictionary of all organizations with their "name, country and activity"
count = 0
for i in range(1, len(lines)):
    lineList = lines[i].split(sepChar)
    try:
        value = orgDictionary[lineList[0]];
    except KeyError:
        orgDictionary[lineList[0]] = JOrgList(lineList[0],lineList[4],lineList[3])
        count += 1

#write file
OrgList = [];
for key, value in orgDictionary.items():
    OrgList.append(value)
WriteJSON("outputJS/" + "organizations.json", OrgList)
