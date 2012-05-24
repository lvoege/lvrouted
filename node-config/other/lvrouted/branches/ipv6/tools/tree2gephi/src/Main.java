import java.io.File;

import org.gephi.graph.api.DirectedGraph;
import org.gephi.graph.api.GraphController;
import org.gephi.graph.api.GraphModel;
import org.gephi.io.exporter.api.ExportController;
import org.gephi.io.importer.api.Container;
import org.gephi.io.importer.api.EdgeDefault;
import org.gephi.io.importer.api.ImportController;
import org.gephi.io.processor.plugin.DynamicProcessor;
import org.gephi.project.api.ProjectController;
import org.gephi.project.api.Workspace;
import org.openide.util.Lookup;


public class Main {

    /**
     * @param args
     */
    public static void main(String[] args) {
        
        File folder = new File(".");
        File[] files = folder.listFiles();
        
        ProjectController pc = Lookup.getDefault().lookup(ProjectController.class);
        pc.newProject();
        Workspace workspace = pc.getCurrentWorkspace();
        
        ImportController importController = Lookup.getDefault().lookup(ImportController.class);
        Container container;
        try {
            container = importController.importFile(files[0]);
            container.getLoader().setEdgeDefault(EdgeDefault.DIRECTED);
            container.setAllowAutoNode(false);
        } catch (Exception ex) {
            ex.printStackTrace();
            return;
        }
        
        DynamicProcessor dynamicProcessor = new DynamicProcessor();
        dynamicProcessor.setDateMode(false);
        dynamicProcessor.setLabelmatching(true);
        dynamicProcessor.setDate("0");
        importController.process(container, dynamicProcessor, workspace);
        
        for (int i = 1; i < files.length; i++) {
	        try {
	            if (!files[i].getName().endsWith(".gml"))
	                continue;
	            container = importController.importFile(files[i]);
	            container.getLoader().setEdgeDefault(EdgeDefault.DIRECTED);
	            container.setAllowAutoNode(false);
	        } catch (Exception ex) {
	            ex.printStackTrace();
	            return;
	        }
	        importController.process(container, dynamicProcessor, workspace);
	        dynamicProcessor.setDate(new Integer(i).toString());
            
        }
        
        ExportController ec = Lookup.getDefault().lookup(ExportController.class);
        try {
            ec.exportFile(new File("c:\\temp\\out.gexf"));
        } catch (Exception ex) {
            ex.printStackTrace();
            return;
        }
    }

}
