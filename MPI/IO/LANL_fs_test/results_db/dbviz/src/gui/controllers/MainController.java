package gui.controllers;

import gui.exceptions.DbConnectionException;
import gui.exceptions.QueryException;
import gui.models.Database;
import gui.models.Line;
import gui.models.LinePoint;
import gui.models.MainModel;
import gui.models.PersistentDefaultLabels;
import gui.models.PersistentParams;
import gui.models.PersistentReplacements;
import gui.models.PersistentTopUsed;
import gui.models.Replacements;
import gui.models.Line.PointType;
import gui.views.AdvancedQueryView;
import gui.views.DefaultLabelsView;
import gui.views.DescribeTableView;
import gui.views.DiffView;
import gui.views.DisplayDbsView;
import gui.views.DistinctValuesView;
import gui.views.GraphView;
import gui.views.ExtensionFileFilter;
import gui.views.MainView;
import gui.views.ParameterView;
import gui.views.ReplaceFieldView;
import gui.views.ReplacementConfigView;
import gui.views.SaveChooser;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Vector;

import javax.imageio.ImageIO;
import javax.swing.JFileChooser;

/**
 * This class acts as a controller between the views and the models.
 */
public class MainController {

	public class AdvancedQueryListener implements ActionListener {
		
		public class AqvRunListener implements ActionListener {
			public void actionPerformed(ActionEvent ae) {
				
				query = aqv.getQuery();
				aqv.closeWindow();
				
				mv.appendToHistory(query + "\n");
				
				executeQuery(true, null, 0, null, null);
				
			}
		}
		
		private AdvancedQueryView aqv;
		
		public void actionPerformed(ActionEvent ae) {
			aqv = new AdvancedQueryView();
			aqv.addRunListener(new AqvRunListener());
		}
	}
	
	public class ComboBoxFocusListener implements FocusListener {

		private String name;

		public ComboBoxFocusListener(String s) {
			super();
			name = s;
		}

		public void focusGained(FocusEvent e) {}

		public void focusLost(FocusEvent e) {
			String temp = null;
			boolean shouldSet = false;
			if (name.equals("xaxis")) {
				temp = pv.getXLabel();
				if (temp == null || temp.equals("")) {
					temp = pv.getXAxis();
					shouldSet = true;
				}
			} else if (name.equals("yaxis")) {
				temp = pv.getYLabel();
				if (temp == null || temp.equals("")) {
					temp = pv.getYAxis();
					shouldSet = true;
				}
			}
			
			if (shouldSet) {
				
				PersistentDefaultLabels pdl = null;
				String result = null;
				try {
					Database db = model.getDb();
					if (db != null) {
						pdl = new PersistentDefaultLabels();
						result = pdl.checkString(db.getTableName(), temp);
					}
				} catch (IOException ioe) {
					mv.appendToHistory("Unable to load default labels: "+ioe.getMessage()+"\n");
				}
				
				if (name.equals("xaxis")) {
					pv.setXLabel(result);
				} else if (name.equals("yaxis")) {
					pv.setYLabel(result);
				}
			}
		}

	}

	public class DefaultLabelListener implements ActionListener {

		public class ReplaceFieldOkListener implements ActionListener {
			public void actionPerformed(ActionEvent ae) {

				PersistentDefaultLabels pdl = null;

				try {
					Database db = model.getDb();
					if (db != null) {
						pdl = new PersistentDefaultLabels();
						pdl.addDefaultLabel(db.getTableName(), rfv
								.getToReplaceField(), rfv.getReplaceWithField());
					}
				} catch (IOException e) {
					mv
							.appendToHistory("Error while processing default label request: "
									+ e.getMessage() + "\n");
				}

				rfv.closeWindow();
			}
		}
		
		private ReplaceFieldView rfv;
		
		public void actionPerformed(ActionEvent e) {
			rfv = new ReplaceFieldView();
			rfv.addReplaceFieldOkListener(new ReplaceFieldOkListener());
		}
	}

	public class DescribeTableListener implements ActionListener {
		public void actionPerformed(ActionEvent e) {

			Database db = model.getDb();
			if (db == null) {
				mv.appendToHistory("Please select a table first.\n");
				return;
			}
			
			String query = "DESCRIBE " + db.getTableName();
			ResultSet result = null;
			try {
				result = model.queryDb(query);
			} catch (DbConnectionException e1) {
				mv
						.appendToHistory("Unable to connect to database to run describe table query\n");
				return;
			} catch (QueryException e1) {
				mv.appendToHistory("Invalid describe table query\n");
				return;
			}

			mv.appendToHistory(query + "\n");

			try {
				new DescribeTableView(db.getTableName(), result);
			} catch (SQLException e1) {
				mv.appendToHistory("Unable to process describe table query\n");
			}
		}
	}

	public class DiffListener implements ActionListener {
		
		public class DiffDisplayListener implements ActionListener {
			public void actionPerformed(ActionEvent e) {
				
				Database db = model.getDb();
				if (db == null) {
					mv.appendToHistory("Please select a table first.\n");
					return;
				}
				
				String selected = df.getSelected();
				if (selected == null) {
					mv.appendToHistory("Please select a field for which to display distinct values\n");
					return;
				}
				String diffQuery = "SELECT DISTINCT " + selected + " FROM "
						+ db.getTableName() + " WHERE " + pv.getWhere()
						+ " ORDER BY " + selected;
				ResultSet results = null;
				ArrayList<String> list = new ArrayList<String>();
				try {
					results = model.queryDb(diffQuery);
					mv.appendToHistory(diffQuery + "\n");
					while (results.next()) {
						try {
							list.add(String.valueOf(results.getDouble(1)));
						} catch (SQLException s) {
							list.add(results.getString(1));
						}
						if (results.wasNull()) {
							list.remove(list.size() - 1);
						}
					}
				} catch (DbConnectionException e1) {
					mv
							.appendToHistory("Unable to connect to database to get distinct values: "
									+ e1.getMessage() + "\n");
					return;
				} catch (QueryException e1) {
					mv.appendToHistory("Invalid diff query: " + e1.getMessage() + "\n");
					return;
				} catch (Exception e1) {
					mv
							.appendToHistory("Unable to process results from distinct values query: "
									+ e1.getMessage() + "\n");
					return;
				}

				new DistinctValuesView(list, selected);
			}
		}
		
		private DiffView df;
		
		public void actionPerformed(ActionEvent e) {
			
			Database db = model.getDb();
			if (db == null) {
				mv.appendToHistory("Please select a table first.\n");
				return;
			}

			String query = "DESCRIBE " + db.getTableName();
			ResultSet colNames = null;
			ArrayList<String> colNamesList = new ArrayList<String>();
			try {
				colNames = model.queryDb(query);
				mv.appendToHistory(query + "\n");
			} catch (DbConnectionException e1) {
				mv
						.appendToHistory("Unable to connect to database to run diff\n");
				return;
			} catch (QueryException e1) {
				mv.appendToHistory("Invalid diff query\n");
				return;
			}

			String curr = "";
			query = "SELECT";
			boolean first = true;
			try {
				while (colNames.next()) {
					if (!first) {
						query += ",";
					} else {
						first = false;
					}
					curr = colNames.getString("Field");
					colNamesList.add(curr);
					query += " COUNT(DISTINCT " + curr + ")";
				}
				query += " FROM " + db.getTableName();
				String where = pv.getWhere();
				if (where != null && !where.equals("")) {
					query += " WHERE " + where;
				}
			} catch (SQLException e1) {
				mv
						.appendToHistory("Error while processing results from initial diff query: "
								+ e1.getMessage() + "\n");
				return;
			}

			ResultSet countVals;
			try {
				mv.appendToHistory(query + "\n");
				countVals = model.queryDb(query);
			} catch (DbConnectionException e1) {
				mv
						.appendToHistory("Unable to connect to database to run diff\n");
				return;
			} catch (QueryException e1) {
				mv.appendToHistory("Invalid diff query\n");
				return;
			}

			int[] counts = null;
			int numCols = 0;
			try {
				ResultSetMetaData md = null;
				try {
					md = countVals.getMetaData();
				} catch (SQLException e1) {
					mv.appendToHistory("Unable to complete diff: "
							+ e1.getMessage() + "\n");
				}

				numCols = md.getColumnCount();
				counts = new int[numCols];
				while (countVals.next()) {
					for (int i = 0; i < numCols; i++) {
						counts[i] = countVals.getInt(i + 1);
					}
				}
			} catch (SQLException e1) {
				mv.appendToHistory("Error while processing results from diff: "
						+ e1.getMessage() + "\n");
			}

			df = new DiffView(colNamesList, counts);
			df.addDiffDisplayListener(new DiffDisplayListener());

		}
	}
	
	public class DisplayDefaultLabelsListener implements ActionListener {
		public void actionPerformed (ActionEvent e) {
			try {
				new DefaultLabelsView(new PersistentDefaultLabels(), model.getTableNames());
			} catch (IOException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
		}
	}
	
	public class DisplayReplacementFieldsListener implements ActionListener {
		public void actionPerformed (ActionEvent e) {
			try {
				new ReplacementConfigView(new PersistentReplacements(), model.getTableNames());
			} catch (IOException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
		}
	}
	
	public class CloseListener implements WindowListener {
		public void windowClosed(WindowEvent e) {
			//System.out.println("Window Closed");
			mv.saveAndClose();
		}
		public void windowClosing(WindowEvent e) {
			//System.out.println("Window Closing");
			mv.saveAndClose();
		}
		public void windowDeactivated(WindowEvent e) {}
		public void windowDeiconified(WindowEvent e) {}
		public void windowIconified(WindowEvent e) {}
		public void windowOpened(WindowEvent e) {}
		public void windowActivated(WindowEvent e) {}		
	}

	public class DisplayTablesListener implements ActionListener {
		
		public class AddDbOkListener implements ActionListener {
			public void actionPerformed(ActionEvent e) {

				int index = ddv.saveAndClose();
				
				if (index >= 0) {
					try {
						model.closeConnection();
					} catch (SQLException sqle) {
						mv
								.appendToHistory("Unable to close connection of previous db.\n");
					}
					
					boolean result = model.setDb(model.getTables().get(index)); // set table as currdb
					model.updateXML(); // add table to config file
					if (result) {
						saveOldTopUsed(oldDbName); // save old top used to file
						mv.setShouldLoadTopUsed(true);
						loadNewTopUsed(); // load top used from file
						pv.setComboFields(model.getTableColumnNamesNum(), model
								.getTableColumnNamesAll()); // populate x and y axis
															// input fields
					}
	
					Database db = model.getTables().get(index);
					//mv.appendToHistory("Added new table " + db.getTableName() + "\n");
					if (result) {
						mv.appendToHistory("Connected to host " + db.getHostName()
								+ " database " + db.getDbName() + " table "
								+ db.getTableName() + " as user "
								+ db.getUserName() + "\n");
					}
				}
			}
		}
		private String oldDbName;
		
		private DisplayDbsView ddv;
		
		public void actionPerformed (ActionEvent e) {
			try {
				oldDbName = model.getDb().getTableName();
			} catch (NullPointerException npe) {
				oldDbName = "";
			}
			ddv = new DisplayDbsView(model);
			ddv.addOkListener(new AddDbOkListener());
		}
	}
	
	public class ExitListener implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			
			Database db = model.getDb();
			if (db != null) {
				saveOldTopUsed(db.getTableName());
			}
			
			try {
				model.closeConnection();
			} catch (SQLException sqle) {
				mv.appendToHistory("Unable to close database connection.\n");
			}
			PersistentParams pp = null;
			try {
				pp = new PersistentParams("configs/exitParams.json", pv);
				pp.saveWindowSize(mv.getWindowSize());
				pp.saveParams();
				mv.appendToHistory("Saved parameter settings to "
						+ "configs/exitParams.json before exiting.\n");
			} catch (IOException ioe) {
				mv.appendToHistory("Unable to save parameters before exiting: "
						+ ioe.getMessage() + "\n");
			}
			mv.appendToHistory("Exit\n");
			System.exit(0);
		}
	}
	
	public class ExportDataListener implements ActionListener {
		public void actionPerformed(ActionEvent ae) {
			
			ArrayList<Line> lines = gv.getLines();
			
			if (lines == null || lines.size() == 0) {
				mv.appendToHistory("There are no data to export.\n");
				return;
			}
			
			//TODO: what extension should the files have
			ExtensionFileFilter eff = new ExtensionFileFilter(".dat");
			SaveChooser chooser = new SaveChooser();
			chooser.setCurrentDirectory(new File(defaultDir));
			chooser.setFileFilter(eff);
			int ret = chooser.showSaveDialog(mv);
			
			if (ret == SaveChooser.APPROVE_OPTION) {
				File f = chooser.getSelectedFile();
				FileWriter fw = null;
				BufferedWriter bw = null;
				try {
					fw = new FileWriter(f);
					bw = new BufferedWriter(fw);
					
					String caption = getCaption();
					caption = caption.replace("\n", "");
					bw.write("# " + caption + "\n");
					Line line = null;
					PointType pt = gv.getPointType();
					LinePoint point = null;
					String s = null;
					for (int i=0; i<lines.size(); i++) {
						line = lines.get(i);
						bw.write("# " + line.getCompares() + "\n");
						for (int j=0; j<line.size(pt); j++) {
							point = line.getPt(j, pt);
							s = point.getStringVal();
							if (s == null) {
								s = point.getXVal() + " ";
							} else {
								s += " ";
							}
							s += point.getYVal() + " ";		//TODO: check what to do with different point types
							if (pt != PointType.ALL) {
								s += line.getStdDev(j) + " " + line.getMin(j) + " " + line.getMax(j);
							}
							bw.write(s + "\n");
						}
						
						if (i < lines.size()-1) {
							bw.write("\n\n");
						}
					}
					bw.close();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				
				defaultDir = f.getParent();
			}
		}
	}

	public class ForceListener implements ActionListener {
		public void actionPerformed(ActionEvent ae) {
			oldcompare = null;
			mv.run();
		}
	}
	
	/**
	 * This class is a listener for the Load button in the parameter panel. It
	 * brings up a file chooser window to allow selection of a xml file to load
	 * parameters from.
	 */
	public class LoadListener implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			JFileChooser chooser = new JFileChooser();
			chooser.setCurrentDirectory(new File(defaultDir));
			ExtensionFileFilter jff = new ExtensionFileFilter(".json");
			chooser.setFileFilter(jff);
			int ret = chooser.showOpenDialog(mv);
			if (ret == JFileChooser.APPROVE_OPTION) {
				File f = chooser.getSelectedFile();

				PersistentParams pp = null;
				try {
					pp = new PersistentParams(f.getAbsolutePath(), pv);
					pp.loadParams();
					mv.appendToHistory("Loaded parameter settings from "
							+ f.getAbsolutePath() + "\n");
				} catch (IOException ioe) {
					mv.appendToHistory("Unable to load parameters: "
							+ ioe.getMessage() + "\n");
				}
				defaultDir = f.getParent();
			}
		}
	}
	
	public class ReplaceFieldListener implements ActionListener {
		
		public class ReplaceFieldOkListener implements ActionListener {
			public void actionPerformed(ActionEvent ae) {

				PersistentReplacements pr = null;

				try {
					Database db = model.getDb();
					if (db != null) {
						pr = new PersistentReplacements();
						pr.addReplacement(db.getTableName(), rfv
								.getToReplaceField(), rfv.getReplaceWithField());
					}
				} catch (IOException e) {
					mv
							.appendToHistory("Error while processing replacement field request: "
									+ e.getMessage() + "\n");
				}

				rfv.closeWindow();
			}
		}
		
		private ReplaceFieldView rfv;
		
		public void actionPerformed(ActionEvent e) {
			rfv = new ReplaceFieldView();
			rfv.addReplaceFieldOkListener(new ReplaceFieldOkListener());
		}	
	}

	/**
	 * This class is a listener for the Run button in the parameter panel. It
	 * uses the data entered in the parameter panel, queries the database, and
	 * plots the resulting data.
	 */
	public class RunListener implements ActionListener {

		public void actionPerformed(ActionEvent e) {

			double startTime = System.currentTimeMillis();
			
			if (justUpdate()) {
				mv.repaintGraph();
			} else {
				
				Database db = model.getDb();
				if (db == null) {
					mv.appendToHistory("Please select a table to query first.\n");
					return;
				}

				mv.initProgress();
				mv.setProgressText("Validating input");
				if (!checkValidInput()) {
					return;
				}
				mv.addProgress(10);
				
				mv.setProgressText("Updating top used fields");
				pv.updateTopUsed();
				mv.addProgress(10);
				
				mv.setProgressText("Setting up query to find table keys");

				run = true;

				ArrayList<String> keys = getKeys();
				if (keys == null) {
					return;
				}
				
				int numKeys = keys.size();
				mv.addProgress(10);

				mv.setProgressText("Setting up data query");
				String select;
				String from;
				String where;
				String compare;

				ArrayList<String> colNames = new ArrayList<String>();

				String[] yTokens = null;
				String yVal = pv.getYAxis();
				yTokens = yVal.split(",");
//				ArrayList<String> yVals = new ArrayList<String>();
				for (int j=0; j<yTokens.length; j++) {
					
					yTokens[j] = yTokens[j].trim();
					
					query = "";
					/*** prepare SELECT clause ***/
					select = "SELECT ";
					compare = pv.getCompare();
					String[] compTokens = null;
					int numCompares = 0;
					String token = null;
					int skips = 0;
					if (compare != null && !compare.equals("")) {
						compTokens = compare.split(",");
						compare = "";
						numCompares = compTokens.length;
						for (int i = 0; i < numCompares; i++) {
							token = compTokens[i];
							while (compTokens[i].contains("(")
									&& !compTokens[i].contains(")")
									&& i < numCompares - 1) {
								i++;
								skips++;
								token += "," + compTokens[i];
							}
							select += token + ",";
							compare += token;
							colNames.add(token);
							if (i < numCompares - 1) {
								compare += ",";
							}
						}
					}
					numCompares -= skips;
					compTokens = null;
					select += pv.getXAxis() + ",";
					select += yTokens[j];


					for (int i = 0; i < numKeys; i++) {
						select += "," + keys.get(i);
					}
					select += " ";
					query += select;

					/*** prepare FROM clause ***/
					from = "FROM ";
					from += db.getTableName();
					query += from;

					/*** prepare WHERE clause ***/
					where = pv.getWhere();
					query += " WHERE ";
					query += "!isnull(" + yTokens[j] + ")";
					query += " && !isnull(" + pv.getXAxis() + ")";
					if (where != null && !where.equals("")) {
						query += " && " + where;
					}

					/*** prepare ORDER BY clause ***/
					if (numCompares > 0) {
						query += " ORDER BY " + compare + "," + pv.getXAxis() + ","
						+ yTokens[j];
					}

					/*** display query in history ***/
					mv.appendToHistory(query + "\n");
					mv.addProgress(10);
					mv.setProgressText("Running data query");

					if (yTokens.length == 1) {
						executeQuery(true, null, numCompares, keys, colNames);
					} else {
						executeQuery(j==0, yTokens[j], numCompares, keys, colNames);
					}
					
				}
				
				
				//TODO: do line merging here
				ArrayList<Line> lines = gv.getLines();
				Line line = null;
				String key = null;
				HashMap<String, Line> map = new HashMap<String, Line>();
				Line hit = null;
				if (lines != null) {
					for (int i=0; i<lines.size(); i++) {
						line = lines.get(i);
						key = line.getCompares();
						hit = map.get(key);
						if (hit == null) {
							map.put(key, line);
						} else {
							hit.combineLines(line);
							map.put(key, hit);
							lines.remove(i);
							i--;
						}
					}
					
					Iterator<Line> iter = map.values().iterator();
					lines = new ArrayList<Line>();
					while (iter.hasNext()) {
						line = iter.next();
						line.calcStats();
						lines.add(line);
					}
					gv.setPoints(lines);
					
					/*** plot the data ***/
					mv.addProgress(10);
					mv.setProgressText("Painting results from query");
					gv.populateGraphElements();
					gv.paintGraph(gv.getGraphics());
					mv.addProgress(10);
					mv.setProgressText("Run complete");
					mv.resetProgress();
				}
			}
			mv.appendToHistory("Completed run request in "+((System.currentTimeMillis()-startTime)/1000) + " seconds.\n");
		}
	}
	
	public class SaveGraphListener implements ActionListener {
		public void actionPerformed(ActionEvent ae) {

			SaveChooser chooser = new SaveChooser();
			chooser.setCurrentDirectory(new File(defaultDir));
			ExtensionFileFilter jpff = new ExtensionFileFilter(".jpg");
			chooser.setFileFilter(jpff);
			int ret = chooser.showSaveDialog(mv);
			File f = null;
			if (ret == SaveChooser.APPROVE_OPTION) {
				f = chooser.getSelectedFile();
				defaultDir = f.getParent();
			} else {
				return;
			}

			if (!f.getName().endsWith("jpg")) {
				f = new File(f.getAbsolutePath() + ".jpg");
			}

			createJPGReport(f, 1);

			mv.appendToHistory("Saved graph to " + f.getAbsolutePath()
					+ "\n");
		}
	}
	
	/**
	 * This class is a listener for the Save button on the parameter panel. It
	 * opens a file chooser and saves the parameters to the selected json file.
	 */
	public class SaveListener implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			SaveChooser chooser = new SaveChooser();
			chooser.setCurrentDirectory(new File(defaultDir));
			ExtensionFileFilter jff = new ExtensionFileFilter(".json");
			chooser.setFileFilter(jff);
			int ret = chooser.showSaveDialog(mv);
			if (ret == SaveChooser.APPROVE_OPTION) {
				File f = chooser.getSelectedFile();
				PersistentParams pp = null;
				try {
					pp = new PersistentParams(f.getAbsolutePath(), pv);
					pp.saveParams();
				} catch (IOException ioe) {
					mv.appendToHistory("Unable to save parameters: "
							+ ioe.getMessage() + "\n");
				}

				String fileName = null;
				if (f.getAbsolutePath().endsWith(".json")) {
					fileName = f.getAbsolutePath();
				} else {
					fileName = f.getAbsolutePath() + ".json";
				}
				mv.appendToHistory("Saved parameter settings to " + fileName
						+ "\n");
				defaultDir = f.getParent();
			}
		}

	}
	
	public class UpdateButtonListener implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			if (run) {
				mv.repaintGraph();
			}
		}
	}

	private MainModel model;
	
	private MainView mv;
	
	private ParameterView pv;

	private GraphView gv;

	private boolean run = false;

	private String query;

	private String oldcompare = null;

	private String oldxaxis = null;

	private String oldyaxis = null;
	private String oldwhere = null;
	private boolean oldBarGraph = false;
	private Replacements oldReps = null;
	
	private String defaultDir;
	private int numRows;
	public MainController(MainModel mm, MainView mview) {

		model = mm;
		mv = mview;
		query = new String();

		numRows = 0;

		RunListener rl = new RunListener();
		pv = mv.getParameterView();
		pv.addRunListener(rl);
//		pv
//				.addComboBoxFocusListener(new ComboBoxFocusListener("xaxis"),
//						"xaxis");
//		pv
//				.addComboBoxFocusListener(new ComboBoxFocusListener("yaxis"),
//						"yaxis");

		mv.addSaveGraphListener(new SaveGraphListener());
		mv.addDiffListener(new DiffListener());
		mv.addLoadListener(new LoadListener());
		mv.addSaveListener(new SaveListener());
		mv.addRunListener(rl);
		mv.addDisplayTablesListener(new DisplayTablesListener());
		mv.addDisplayReplacementFieldsListener(new DisplayReplacementFieldsListener());
		mv.addDisplayDefaultLabelsListener(new DisplayDefaultLabelsListener());
		mv.addForceListener(new ForceListener());
		mv.addAdvancedListener(new AdvancedQueryListener());
		mv.addExportDataListener(new ExportDataListener());
		mv.addCloseListener(new CloseListener());

		mv.addDescribeTableListener(new DescribeTableListener());
		mv.addExitListener(new ExitListener());

		gv = mv.getGraphView();

		defaultDir = System.getProperty("user.dir");

		rl = null;

	}
	
	private boolean checkValidInput() {
		if (pv.getCompare().equals("----------")) {
			mv
					.appendToHistory("Please make sure a valid compare field is selected.\n");
			return false;
		} else if (pv.getXAxis().equals("----------")) {
			mv
					.appendToHistory("Please make sure a valid x axis field is selected.\n");
			return false;
		} else if (pv.getYAxis().equals("----------")) {
			mv
					.appendToHistory("Please make sure a valid y axis field is selected.\n");
			return false;
		}
		return true;
	}
	
	public void createJPGReport(File file, int figureNumber) {
		
		try {
			BufferedImage bimage = mv
					.getGraphImage("Figure "+figureNumber+": " + getCaption());
			ImageIO.write((RenderedImage) bimage, "jpg", file);
		} catch (IOException ioe) {
			ioe.printStackTrace();
		}
		
	}

	private void executeQuery(boolean firstPass, String currY, int numCompares, ArrayList<String> keys, ArrayList<String> colNames) {
		
		Database db = model.getDb();
		if (db == null) {
			mv.appendToHistory("Please select a table to query first.\n");
			return;
		}
		
		int numKeys = 0;
		if (keys != null) {
			numKeys = keys.size();
		}
		
		/*** execute the query ***/
		ResultSet result = null;
		try {
			result = model.queryDb(query);
		} catch (DbConnectionException e2) {
			mv.appendToHistory("Problem with database connection: "
					+ e2.getMessage());
			return;
		} catch (QueryException e2) {
			mv
					.appendToHistory("Problem with query: "
							+ e2.getMessage());
			return;
		}
		mv.addProgress(10);
		mv.setProgressText("Parsing results from data query");

		/*** get resulting metadata ***/
		ResultSetMetaData md = null;
		try {
			md = result.getMetaData();
		} catch (SQLException e1) {
			e1.printStackTrace();
		}

		/**** parse the results of the query ****/
		int numCols = 0;
		numRows = 0;
		try {

			numCols = md.getColumnCount();
			md = null;

			//int stringCount = 0;
			boolean first = true;
			Object[] oldComp = new Object[numCompares];
			Object[] currComp = new Object[numCompares];
			ArrayList<Line> allLines = null;
			HashMap<Integer, Line> map = new HashMap<Integer, Line>();
			HashMap<String, Integer> uniStrings = new HashMap<String, Integer>();
			Line currLine = new Line();
			LinePoint currPoint = null;

			PersistentReplacements pr = null;
			try {
				pr = new PersistentReplacements();
			} catch (IOException ioe) {
				mv
						.appendToHistory("Error while processing replacements config file: "
								+ ioe.getMessage() + "\n");
			}
			Replacements reps = pr.getReplacements(db.getTableName());
			pr = null;

			while (result.next()) {
				for (int i = 0; i < numCols; i++) {

					if (i <= numCols - numKeys - 3) { // compare values
						oldComp[i] = currComp[i];
						try {
							Double d = new Double(result.getDouble(i + 1));
							if (Math.floor(d) == d) {
								currComp[i] = String.valueOf((int)Math.floor(d));
							} else {
								if (d > 100000) {
									DecimalFormat df = new DecimalFormat("###");
									currComp[i] = df.format(d);
								} else {
									currComp[i] = d;
								}
							}
						} catch (SQLException se) {
							currComp[i] = result.getString(i + 1);
						}
						if (result.wasNull()) {
							currComp[i] = "null";
						}
					} else if (i == numCols - numKeys - 2) { // x values
						if (!first
								&& Arrays.hashCode(oldComp) != Arrays
										.hashCode(currComp)) {
							map.put(Arrays.hashCode(oldComp), currLine);
							currLine = new Line();
							//stringCount = 0;
							uniStrings = new HashMap<String, Integer>();
							for (int j = 0; j < numCompares; j++) {
								currLine.addCompare(colNames.get(j),
								currComp[j].toString());
							}
							currLine.setYVal(currY);
							currLine.setReplacements(reps);
						} else if (first) {
							for (int j = 0; j < numCompares; j++) {
								currLine.addCompare(colNames.get(j),
										currComp[j].toString());
							}
							currLine.setYVal(currY);
							currLine.setReplacements(reps);
							first = false;
						}
						currPoint = new LinePoint();
						try {
							currPoint.setXVal(result.getDouble(i + 1));
						} catch (SQLException sqle){
							long epoch = 0;
							try {
								//TODO: this doesn't handle dates before the epoch or dates that weren't initialized (i.e. 0000-00-00 00:00:00)
								epoch = new java.text.SimpleDateFormat("yyyy-mm-dd HH:mm:ss").parse(result.getTimestamp(i+1).toString()).getTime();
								currPoint.setXVal(epoch);
								pv.setFormatAsDate(true);
							} catch (ParseException e) {
								mv.appendToHistory("Error while processing results: "
										+ e.getMessage());
							} catch (SQLException se) {
								String s = result.getString(i+1);
								Integer in = uniStrings.get(s);
								if (in == null || in == 0) {
									in = new Integer(1);	
								} else {
									in = new Integer(++in);
								}
								uniStrings.put(s, in);
								currPoint.setXVal(s.hashCode());
								currPoint.setStringVal(s);
								pv.setBarGraph(true);
								//System.out.println("found string: " + s);
							}
						}
					} else if (i == numCols - numKeys - 1) { // y values
						currPoint.setYVal(result.getDouble(i + 1));
						// currLine.addPoint(currPoint);
					} else {
						try {
							Double d = result.getDouble(i + 1);
							if (d > 99999) {
								DecimalFormat df = new DecimalFormat("###");
								currPoint.addKey(String.valueOf(df.format(d)));
							} else {
								currPoint.addKey(String.valueOf(result.getDouble(i + 1)));
							}
						} catch (SQLException se) {
							currPoint.addKey(result.getString(i + 1));
						}
					}

					if (i == numCols - 1) {
						currPoint.setKeyNames(keys);
						currLine.addPoint(currPoint);
					}

				}
				numRows++;
			}
			mv.addProgress(10);
			mv.setProgressText("Wrapping up after queries");

			keys = null;
			reps = null;
			oldComp = null;
			currPoint = null;

			map.put(Arrays.hashCode(currComp), currLine);
			currComp = null;
			currLine = null;
			
			allLines = new ArrayList<Line>(map.values());
			for (int i = 0; i < allLines.size(); i++) {
				allLines.get(i).calcStats();
			}
			map = null;

			if (firstPass) {
				gv.setPoints(allLines);
			} else {
				gv.addPoints(allLines);
			}

			mv.appendToHistory(String.valueOf(numRows)
					+ " rows fetched\n");

			result.close();

			result = null;
			allLines = null;

		} catch (SQLException e1) {
			mv.appendToHistory("Error while processing results: "
					+ e1.getMessage());
		}
		oldcompare = pv.getCompare();
		oldxaxis = pv.getXAxis();
		oldyaxis = pv.getYAxis();
		oldwhere = pv.getWhere();
		oldBarGraph = pv.getBarGraph();
		PersistentReplacements pr = null;
		try {
			pr = new PersistentReplacements();
			oldReps = pr.getReplacements(db.getTableName());
		} catch (IOException ioe) {
			mv.appendToHistory("Unable to parse replacement fields config file.\n");
		}
	}

	private String getCaption() {

		Database db = model.getDb();
		if (db == null) {
			return null;
		}
		
		String table = db.getTableName();
		String xaxis = pv.getXAxis();
		String yaxis = pv.getYAxis();
		String where = pv.getWhere();
		String rows = String.valueOf(numRows);
		long time = System.currentTimeMillis() / 1000;
		String ts = String.valueOf(time);

		return table + ", " + xaxis + ":" + yaxis + ", where " + where + " ("
				+ rows + " rows, " + ts + ")";
	}

	private ArrayList<String> getKeys() {
		
		Database db = model.getDb();
		if (db == null) {
			return null;
		}
		
		query = "SHOW KEYS FROM  " + db.getTableName();
		mv.addProgress(10);
		mv.setProgressText("Running query to find table keys");

		/*** execute the query ***/
		ResultSet result = null;
		try {
			result = model.queryDb(query);
		} catch (DbConnectionException e2) {
			mv.appendToHistory("Problem with database connection: "
					+ e2.getMessage() + "\n");
			return null;
		} catch (QueryException e2) {
			mv.appendToHistory("Problem with query: " + e2.getMessage()
					+ "\n");
			return null;
		}
		mv.addProgress(10);
		mv.setProgressText("Parsing results from keys query");

		ArrayList<String> keys = new ArrayList<String>();
		try {
			while (result.next()) {
				keys.add(result.getString("Column_name"));
			}
		} catch (SQLException e3) {
			mv.appendToHistory("Problem completing query: "
					+ e3.getMessage() + "\n");
		}
		
		return keys;
		
	}

	private boolean justUpdate() {

		if (!run) {
			return false;
		} else if (oldcompare == null || oldxaxis == null || oldyaxis == null
				|| oldwhere == null) {
			return false;
		} else if (!oldcompare.equals(pv.getCompare())) {
			return false;
		} else if (!oldxaxis.equals(pv.getXAxis())) {
			return false;
		} else if (!oldyaxis.equals(pv.getYAxis())) {
			return false;
		} else if (!oldwhere.equals(pv.getWhere())) {
			return false;
		} else if (oldBarGraph != pv.getBarGraph()) {
			return false;
		}
		
		PersistentReplacements pr = null;
		try {
			Database db = model.getDb();
			if (db != null) {
				pr = new PersistentReplacements();
				if (pr.getReplacements(db.getTableName()) != null) {
					if (!pr.getReplacements(db.getTableName()).equals(oldReps)) {
						return false;
					}
				}
			}
		} catch (IOException ioe) {
			mv.appendToHistory("Unable to check for new replacement fields.\n");
		}

		return true;
	}

	private void loadNewTopUsed() {
		
		Database db = model.getDb();
		if (db == null) {
			return;
		}
		
		PersistentTopUsed ptu = null;
		try {
			ptu = new PersistentTopUsed(db.getTableName(), pv);
			pv.setXTopUsed(ptu.loadTopUsedX());
			pv.setYTopUsed(ptu.loadTopUsedY());
			pv.setCompareTopUsed(ptu.loadTopUsedCompare());
		} catch (IOException ioe) {
			mv.appendToHistory("Unable to load top used fields: "
					+ ioe.getMessage() + "\n");
		}
	}

	private void saveOldTopUsed(String oldTableName) {
		Vector<String> x = pv.getXTopUsed();
		Vector<String> y = pv.getYTopUsed();
		Vector<String> compare = pv.getCompareTopUsed();
		PersistentTopUsed ptu = null;
		try {
			ptu = new PersistentTopUsed(oldTableName, pv);
			ptu.flushToConfig(x, y, compare);
		} catch (IOException ioe) {
			mv.appendToHistory("Unable to save top used fields" + ": "
					+ ioe.getMessage() + "\n");
		}
	}

}
