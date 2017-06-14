package organisationsNetwork;

import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.concurrent.TimeUnit;
import org.openide.util.Lookup;
import org.gephi.appearance.api.PartitionFunction;
import org.gephi.appearance.api.AppearanceController;
import org.gephi.appearance.api.AppearanceModel;
import org.gephi.appearance.api.Function;
import org.gephi.appearance.api.Partition;
import org.gephi.appearance.plugin.PartitionElementColorTransformer;
import org.gephi.appearance.plugin.RankingNodeSizeTransformer;
import org.gephi.appearance.plugin.palette.Palette;
import org.gephi.datalab.api.AttributeColumnsController;
import org.gephi.graph.api.Column;
import org.gephi.graph.api.Graph;
import org.gephi.graph.api.GraphController;
import org.gephi.graph.api.GraphModel;
import org.gephi.graph.api.Node;
import org.gephi.io.exporter.api.ExportController;
import org.gephi.project.api.ProjectController;
import org.gephi.layout.plugin.AutoLayout;
import org.gephi.layout.plugin.forceAtlas2.ForceAtlas2;
import org.gephi.layout.plugin.forceAtlas2.ForceAtlas2Builder;

public class buildGEXF {
	
	//declare class variables
	
	private GraphModel graphModel;
	private Graph graph;
	private AppearanceController appearanceController;
	private AppearanceModel appearanceModel;

	//main method
    public void script(String inputFileNodeName, String inputFileEdgeName, String outputFileName) throws IOException, IllegalArgumentException {
    	// print start time
    	if (inputFileNodeName == null || inputFileEdgeName == null || outputFileName == null) {
    		throw new IllegalArgumentException();
    	}
    	File nodeFile = new File(inputFileNodeName);
    	File edgeFile = new File(inputFileEdgeName);
   		initialiseProject();
   	    setupGraph();
   	    importFiles(nodeFile, edgeFile);
   	    setupAppearanceController();
   	    setAppearanceSizingNodes();
   	    setAppearanceColorNodes();
   	    setAppearanceColorEdges();
   	    setLabelsNodes();
   	    graph.removeNode(graph.getNode("TEST"));
   	    setAndExecuteLayout();
   	    ExportGraph(outputFileName);
    }
    
    //initialise the project
	private void initialiseProject() {
        ProjectController pc = Lookup.getDefault().lookup(ProjectController.class);
        pc.newProject();
	}
	
	//setup the graphModel and graph
	private void setupGraph() {
        this.graphModel = Lookup.getDefault().lookup(GraphController.class).getGraphModel();
        this.graph = graphModel.getGraph();
	}
	
	//import the files for edges and nodes
    private void importFiles(File nodeFile, File edgeFile) {
        try {
            AttributeColumnsController ac = Lookup.getDefault().lookup(AttributeColumnsController.class);
            
            String[] columnNamesEdges = new String[]{"Source", "Target", "Weight", "Type", "Org"};
            Class[] columnTypesEdges = new Class[]{String.class, String.class, double.class, String.class, String.class};
           
            ac.importCSVToEdgesTable(graph,
                    edgeFile,
                    ';',
                    Charset.defaultCharset(),
                    columnNamesEdges,
                    columnTypesEdges,
                    true);
            
            String[] columnNamesNodes = new String[]{"Id", "ActivityType", "NbProject"};
            Class[] columnTypesNodes = new Class[]{String.class, String.class, double.class};
            ac.importCSVToNodesTable(graph,
                    nodeFile,
                    ';',
                    Charset.defaultCharset(),
                    columnNamesNodes,
                    columnTypesNodes,
                    false);
            
        } catch (Exception ex) {
            ex.printStackTrace();
            return;
        }
	}
    

    //setup the appearanceController
	private void setupAppearanceController() {
        this.appearanceController = Lookup.getDefault().lookup(AppearanceController.class);
        this.appearanceModel = appearanceController.getModel();	
	}
	
	//set the sizing of the nodes depending on the number of projects
	private void setAppearanceSizingNodes() {
        Column columnNbProject = graphModel.getNodeTable().getColumn("NbProject");
        Function sizeRanking = appearanceModel.getNodeFunction(graph, columnNbProject, RankingNodeSizeTransformer.class);
       if (sizeRanking != null)
        {
	        RankingNodeSizeTransformer sizeTransformer = (RankingNodeSizeTransformer) sizeRanking.getTransformer();
	        sizeTransformer.setMinSize(100);
	        sizeTransformer.setMaxSize(500);
	        appearanceController.transform(sizeRanking);
        }

	}
	
	//set the colour of the nodes depending on the activity type (5 types)
	private void setAppearanceColorNodes() {
        Column columnActivityType = graphModel.getNodeTable().getColumn("ActivityType");
        Function funcColorNodes = appearanceModel.getNodeFunction(graph, columnActivityType, PartitionElementColorTransformer.class);
        Partition partition = ((PartitionFunction) funcColorNodes).getPartition();
        Color[] colorNodes = defineColors(partition);
        Palette paletteNodes = new Palette(colorNodes);
        partition.setColors(paletteNodes.getColors());
        appearanceController.transform(funcColorNodes);
	}
	
	//define the colours depending on the partition values
	private Color[] defineColors(Partition partition) {
		Color educationEstablishments = new Color(44, 160, 44); //green
		Color researchOrganisations = new Color(148, 103, 189); //purple
		Color privateForProfit = new Color(255, 127, 14); //orange
		Color publicBodies = new Color(	31, 119, 180); //blue
		Color other = new Color(255, 187, 120); //peach
		Object[] partitionValues = partition.getSortedValues().toArray();
		Color[] returnedColor = new Color[partitionValues.length];
		
		for (int i = 0; i < partitionValues.length; i++) {
			if ( partitionValues[i].toString().equals("PRC")) {
				returnedColor[i] = privateForProfit;
			} else if ( partitionValues[i].toString().equals("HES") ) {
				returnedColor[i] = educationEstablishments;
			} else if ( partitionValues[i].toString().equals("REC") ) {
				returnedColor[i] = researchOrganisations;
			} else if ( partitionValues[i].toString().equals("OTH") ) {
				returnedColor[i] = other;
			} else if ( partitionValues[i].toString().equals("PUB") ) {
				returnedColor[i] = publicBodies;
			}
		}
		
		return returnedColor;
	}
	
	//set the colour of the edges (grey)
	private void setAppearanceColorEdges() {
        for (Function edgeFunc : appearanceModel.getEdgeFunctions(graph)) 
        {
        	if (edgeFunc instanceof PartitionFunction)
        	{
	        	Partition edgePartition = ((PartitionFunction) edgeFunc).getPartition();
	        	if (edgePartition != null && edgePartition.size() != 0)
	        	{
	                Color[] colorEdge = new Color[]{new Color(179,179,179)};
	                Palette paletteEdge = new Palette(colorEdge);
	                edgePartition.setColors(paletteEdge.getColors());
	                appearanceController.transform(edgeFunc);
	        	}
        	}
        }
	}
	
	//set the labels of the nodes (equal to the id)
	private void setLabelsNodes() {
        for (Node node : graph.getNodes()) {
            node.setLabel(node.getId().toString());
        }
	}
	
	//set layout and execute
	private void setAndExecuteLayout() {
        AutoLayout layout = setLayout();
        layout.execute();
	}
	
	//set ForceAtlas2 layout and return it
	//set executionTime depending on the number of nodes
	private AutoLayout setLayout() {
		AutoLayout autoLayout = new AutoLayout(calculateExecutionTime(), TimeUnit.SECONDS);
        autoLayout.setGraphModel(graphModel);
        
        //ForceAtlas2 
        ForceAtlas2 forceAtlas2Layout = new ForceAtlas2(new ForceAtlas2Builder());
        forceAtlas2Layout.setThreadsCount(3);
        forceAtlas2Layout.setJitterTolerance(1d);
        forceAtlas2Layout.setOutboundAttractionDistribution(false);//repulsion
        forceAtlas2Layout.setBarnesHutTheta(1.2);//approximation 
        forceAtlas2Layout.setScalingRatio(70d);
        forceAtlas2Layout.setStrongGravityMode(false);
        forceAtlas2Layout.setGravity(1d);
        forceAtlas2Layout.setBarnesHutOptimize(false);
        forceAtlas2Layout.setLinLogMode(false);
        forceAtlas2Layout.setAdjustSizes(true);//Prevent Overlap
        forceAtlas2Layout.setEdgeWeightInfluence(1d);
        autoLayout.addLayout(forceAtlas2Layout, 1f);
        
        return autoLayout;
	}
	
	//calculate ExecutionTime depending on the number of nodes
	private int calculateExecutionTime() {
		int executionTime;
		int nodesCount = graph.getNodeCount();
		
		if (nodesCount < 10) {
			executionTime = 1;
		} else if (nodesCount < 100) {
			executionTime = 2;
		} else {
			executionTime = 5;
		}
		
		return executionTime;
	}
	
	//export the graph
	private void ExportGraph(String outputFileName) {
        ExportController ec = Lookup.getDefault().lookup(ExportController.class);
        try {
        	File file = new File(outputFileName);
        	//export the file
            ec.exportFile(file);
        } catch (IOException ex) {
            ex.printStackTrace();
            return;
        }
	}
}
