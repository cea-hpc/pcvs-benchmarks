package gui;

import gui.controllers.MainController;
import gui.models.Database;
import gui.models.MainModel;
import gui.models.PersistentParams;
import gui.views.MainView;
import gui.views.ParameterView;

import java.io.File;
import java.io.IOException;

public class DbGui {

	public static void main(String[] args) {
		
		if (args.length > 0) {
		
			if (args.length != 6 && args.length != 4) {
				printUsage();
			}
			
			String configName = null;
			String outputName = null;
			int figureNumber = 1;
			
			for (int i=0; i<args.length; i++) {
				if (args[i].equals("-config")) {
					if (i<args.length-1) {
						configName = args[i+1];
					} else {
						printUsage();
					}
				}
				if (args[i].equals("-output")) {
					if (i<args.length-1) {
						outputName = args[i+1];
					} else {
						printUsage();
					}
				}
				if (args[i].equals("-figure")) {
					if (i<args.length-1) {
						figureNumber = Integer.parseInt(args[i+1]);
					} else {
						printUsage();
					}
				}
			}
			
			//TODO: get password if needed
			//TODO: have option to pass password in on command line
			
			File conf = new File(configName);
			if (!conf.exists()) {
				System.out.println("Please specify a valid config file");
				System.exit(1);
			}
			
			MainModel mainModel = new MainModel();
			MainView mainView = new MainView(mainModel);
			MainController mc = new MainController(mainModel, mainView);
			
			PersistentParams pp = null;
			ParameterView pv = mainView.getParameterView();
			Database db = null;
			try {
				pp = new PersistentParams(configName, pv);
				db = pp.loadParams();
			} catch (IOException ioe) {
				System.out.println("Unable to parse config file");
				System.exit(1);
			}
			
			mainModel.setDb(db);
			mainView.run();
			int x = pp.getXLegend();
			if (x >= 0) {
				mainView.setLegendXPosition(x);
			}
			int y = pp.getYLegend();
			if (y >= 0) {
				mainView.setLegendYPosition(y);
			}
			
			File f = new File(outputName);
			if (!f.exists()) {
				try {
					f.createNewFile();
				} catch (IOException e) {
					System.out.println("Unable to create output file");
					return;
				}
			}
			
			if (!f.getAbsoluteFile().getName().endsWith(".jpg")){
				f = new File(f.getAbsoluteFile().getName()+".jpg");
			}
			mc.createJPGReport(f, figureNumber);
			
			System.exit(0);
			
		} else {
			
			javax.swing.SwingUtilities.invokeLater(new Runnable() {
				public void run() {
	
					MainModel mainModel = new MainModel();
					MainView mainView = new MainView(mainModel);
					new MainController(mainModel, mainView);
	
					mainView.setVisible(true);
	
				}
			});
			
		}
		
	}

	private static void printUsage() {
		System.out.println("Command Line Usage: java -jar dbviz.jar -config foo.json -output foo.[pdf|jpg] [-figure #]");
		System.exit(1);
	}
}
