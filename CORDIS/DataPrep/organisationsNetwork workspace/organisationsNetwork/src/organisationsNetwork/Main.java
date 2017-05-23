package organisationsNetwork;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.io.File;

public class Main {
	public static void main(String[] args) throws IOException {
		if(args.length != 2) {
			System.out.println("This program needs three arguments: node file, edge file, and output file");
			System.out.println("Please run the program with the following command:");
			System.out.println("> jarname.jar nodeFile edgeFile outputFile");
			System.exit(0);
		}
		
    	org.joda.time.DateTime startTime = new org.joda.time.DateTime();
    	System.out.println("Started at" + startTime);
		
		String inputFolder = args[0];
		String outputFolder = args[1];
//		String inputFolder = "C:/data-visualisation-pilot/CORDIS/datasets/outputOrgNetwork" ;
//		String outputFolder = "C:/data-visualisation-pilot/CORDIS/datasets/output";
		

    	ArrayList<String> errors = new ArrayList<String>();
    	ArrayList<String> listFileName = new ArrayList<String>();
    	
    	{
	    	File inputFolderFile = new File(inputFolder);
	    	File[] listOfinputFiles = inputFolderFile.listFiles();
	    	
	    	for (int i = 1; i < listOfinputFiles.length; i++) {
	    		if (listOfinputFiles[i].isFile() && listOfinputFiles[i].getName().contains("_node.csv")) {
	    			listFileName.add(listOfinputFiles[i].getName());
	    		}
	    	}
	    	java.util.Collections.sort(listFileName);
	    	File outputFolderFile = new File(outputFolder);
	    	if(!outputFolderFile.exists()){
	    		outputFolderFile.mkdir();
	    	 }
    	}
    		
    	for (int i = 1; i < listFileName.size(); i++) {
    		String fileName = listFileName.get(i);
   			String nodeFile = inputFolder + '\\' + fileName;
   			String edgeFile = inputFolder + '\\' + fileName.replaceAll("node", "edge");
   			String outputFile= outputFolder + '\\' + fileName.replaceAll("_node.csv", ".gexf");
   	    	try {
   				buildGEXF a = new buildGEXF();
   				a.script(nodeFile, edgeFile, outputFile);
   	    	} catch (Exception ex) {
    			System.out.println(nodeFile);
    			errors.add(nodeFile);
    			ex.printStackTrace();
    		}
   			System.gc();   	    	
   			if (i % 100 == 0) {
   	    		System.out.println(i + " out of " + listFileName.size() + 
   	    				" organisations are processed at " + new org.joda.time.DateTime());
   	    	}
    	}
		if (errors.size()>0) {
			System.out.println("files with errors: ");
	    	for (int j=0; j < errors.size(); j++) { 
	    		System.out.println(errors.get(j).toString());
	    	}
		}
    	org.joda.time.DateTime endTime = new org.joda.time.DateTime();
    	System.out.println("Ended at: " + endTime);
		System.exit(0);
			
	}
}
