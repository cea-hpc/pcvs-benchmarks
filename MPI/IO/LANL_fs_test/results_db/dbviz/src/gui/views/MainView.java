package gui.views;

import gui.exceptions.DbConnectionException;
import gui.exceptions.QueryException;
import gui.models.Database;
import gui.models.LinePoint;
import gui.models.MainModel;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowListener;
import java.awt.image.BufferedImage;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Vector;

import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

/*
 * This class is the main panel of the gui.
 */
public class MainView extends JFrame {

	private static final long serialVersionUID = 1L;
	
	private String version = "dbviz v1.0.8";

	// set window size
	private Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
	private int preferred_width = (int) (screenSize.width * .9);
	private int preferred_height = (int) (screenSize.width * .65);

	// Components
	private JMenuBar menuBar;
	
	private JMenu fileMenu;
	private JMenuItem loadConfigItem;
	private JMenuItem saveConfigItem;
	private JMenuItem saveGraphItem;
	private JMenuItem preferencesItem;
	private JMenuItem exitItem;
	
	private JMenu toolsMenu;
	private JMenuItem runItem;
	private JMenuItem advancedItem;
	private JMenuItem forceItem;
	private JMenuItem diffItem;
	private JMenuItem exportDataItem;
	private JMenuItem displayDbs;
	private JMenuItem displayReplacementFields;
	private JMenuItem displayDefaultLabels;
	
	private JMenu helpMenu;
	private JMenuItem describeTableItem;
	private JMenuItem helpItem;				//TODO: implement this
	
	private JPanel allContent;
	
	private HistoryView history;
	private ParameterView params;
	private GraphView graph;

	private MainModel model;

	public MainView(MainModel m) {

		model = m;

		/*********** Begin: Initialize components *****************/

		// create menu bar
		menuBar = new JMenuBar();

		// create file menu
		fileMenu = new JMenu("File");
		loadConfigItem = new JMenuItem("Load Parameters...");
		loadConfigItem.setAccelerator(KeyStroke.getKeyStroke("ctrl L"));
		saveConfigItem = new JMenuItem("Save Parameters...");
		saveConfigItem.setAccelerator(KeyStroke.getKeyStroke("ctrl S"));
		saveGraphItem = new JMenuItem("Save Graph...");
		saveGraphItem.setAccelerator(KeyStroke.getKeyStroke("ctrl G"));
		preferencesItem = new JMenuItem("Preferences...");
		preferencesItem.setAccelerator(KeyStroke.getKeyStroke("ctrl P"));
		exitItem = new JMenuItem("Exit");
		exitItem.setAccelerator(KeyStroke.getKeyStroke("ctrl Q"));
		fileMenu.add(loadConfigItem);
		fileMenu.add(saveConfigItem);
		fileMenu.addSeparator();
		fileMenu.add(saveGraphItem);
		fileMenu.addSeparator();
		fileMenu.add(preferencesItem);
		fileMenu.addSeparator();
		fileMenu.add(exitItem);

//		// create table menu
		Database db = model.populateTableList();
		if (db == null) {
			//TODO: do something
		} else {
			model.setDb(db);
			//TODO: do other stuff here
		}
		
		// create tools menu
		toolsMenu = new JMenu("Tools");
		runItem = new JMenuItem("Run Query");
		runItem.setAccelerator(KeyStroke.getKeyStroke("ctrl R"));
		advancedItem = new JMenuItem("Advanced Query");
		//TODO: add accelerator
		forceItem = new JMenuItem("Force Requery");
		forceItem.setAccelerator(KeyStroke.getKeyStroke("ctrl Y"));
		diffItem = new JMenuItem("Run Diff...");
		diffItem.setAccelerator(KeyStroke.getKeyStroke("ctrl I"));
		exportDataItem = new JMenuItem("Export Data...");
		exportDataItem.setAccelerator(KeyStroke.getKeyStroke("ctrl E"));
		displayDbs = new JMenuItem("Display Tables...");
		displayReplacementFields = new JMenuItem("Display Replacement Fields...");
		displayDefaultLabels = new JMenuItem("Display Default Labels...");
		toolsMenu.add(runItem);
		toolsMenu.add(advancedItem);
		toolsMenu.add(forceItem);
		toolsMenu.add(diffItem);
		//toolsMenu.add(preferencesItem);
		toolsMenu.add(exportDataItem);
		toolsMenu.addSeparator();
		toolsMenu.add(displayDbs);
		toolsMenu.add(displayReplacementFields);
		toolsMenu.add(displayDefaultLabels);

		// create help menu
		helpMenu = new JMenu("Help");
		describeTableItem = new JMenuItem("Describe Table");
		describeTableItem.setAccelerator(KeyStroke.getKeyStroke("ctrl T"));
		helpItem = new JMenuItem("Help Contents");
		helpItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent ae) {
				new HelpView();
			}
		});
		helpMenu.add(describeTableItem);
		helpMenu.add(helpItem);

		// add menus to menu bar
		menuBar.add(fileMenu);
		menuBar.add(toolsMenu);
		menuBar.add(helpMenu);

		// create main content panel
		allContent = new JPanel();

		// fetch info to populate parameter fields
		Vector<String> colNum = model.getTableColumnNamesNum();
		Vector<String> colAll = model.getTableColumnNamesAll();

		// setup history panel
		history = new HistoryView();

		// setup parameter panel
		params = new ParameterView(this, colNum, colAll);
		Dimension dim = params.getSavedWindowSize();
		if (dim != null) {
			preferred_height = (int) dim.getHeight();
			preferred_width = (int) dim.getWidth();
		}
		preferencesItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent ae) {
				new AdvancedOptions(params);
			}
		});

		// setup graph panel
		graph = new GraphView(this, params);

		JSplitPane splitPaneRight = new JSplitPane(JSplitPane.VERTICAL_SPLIT,
				graph, history);
		splitPaneRight.setOneTouchExpandable(true);
		splitPaneRight.setDividerLocation((int) (preferred_height * 0.7));
		graph.setMinimumSize(new Dimension((int) (0.5 * preferred_width),
				(int) (0.5 * preferred_height)));

		JSplitPane splitPaneLeft = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
				params, splitPaneRight);
		splitPaneLeft.setOneTouchExpandable(true);
		splitPaneLeft.setDividerLocation(333);
		params.setMinimumSize(new Dimension((int) (.2 * preferred_width),
				(preferred_height)));
		
		/*********** End: Initialize components *****************/

		/*********** Start: Layout components *****************/

		allContent.setPreferredSize(new Dimension(preferred_width,
				preferred_height));

		// initialize layout
		GridBagLayout gridbag = new GridBagLayout();
		allContent.setLayout(gridbag);
		GridBagConstraints c = new GridBagConstraints();

		// layout parameter panel
		c.fill = GridBagConstraints.BOTH;
		c.weightx = 1;
		c.weighty = 1;
		allContent.add(splitPaneLeft, c);

		// set menu bar and add panel to the content pane
		setJMenuBar(menuBar);
		setContentPane(allContent);

		
		/*********** End: Layout components *****************/

		setTitle(version);
		setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);

		pack();
		
		if (db == null) {
			appendToHistory("Please specify a database to use (Tools > Display Tables)\n");
		}
		
		gridbag = null;
		c = null;
		splitPaneRight = null;
		splitPaneLeft = null;
		colNum = null;

	}
	
	public void addAdvancedListener(ActionListener al) {
		advancedItem.addActionListener(al);
	}
	
	public void addCloseListener(WindowListener wl) {
		this.addWindowListener(wl);
	}
	
	public void addDescribeTableListener(ActionListener al) {
		describeTableItem.addActionListener(al);
	}
	
	public void addDiffListener(ActionListener al) {
		diffItem.addActionListener(al);
	}

	public void addDisplayDefaultLabelsListener(ActionListener al) {
		displayDefaultLabels.addActionListener(al);
	}
	
	public void addDisplayReplacementFieldsListener(ActionListener al) {
		displayReplacementFields.addActionListener(al);
	}
	
	public void addDisplayTablesListener(ActionListener al) {
		displayDbs.addActionListener(al);
	}
	
	public void addExitListener(ActionListener al) {
		exitItem.addActionListener(al);
	}

	public void addExportDataListener(ActionListener al) {
		exportDataItem.addActionListener(al);
	}

	public void addForceListener(ActionListener al) {
		forceItem.addActionListener(al);
	}

	public void addLoadListener(ActionListener al) {
		loadConfigItem.addActionListener(al);
	}

	public void addProgress(int n) {
		history.addProgress(n);
	}
	
	public void addRunListener(ActionListener al) {
		runItem.addActionListener(al);
	}
	
	public void addSaveGraphListener(ActionListener al) {
		saveGraphItem.addActionListener(al);
	}
	
	public void addSaveListener(ActionListener al) {
		saveConfigItem.addActionListener(al);
	}

	public void appendToHistory(String s) {
		history.addHist(s);
	}
	
	public void closeWindow() {
		Window window = SwingUtilities.getWindowAncestor(this);
		window.dispose();
	}
	
	public String[][] describePoint(LinePoint point) {
		Database db = model.getDb();
		if (db == null) {
			appendToHistory("Cannot describe point because no database is selected.\n");
			return null;
		}
		String query = "SELECT * FROM "+db.getTableName()+" WHERE ";
		ArrayList<String> keyNames = point.getKeyNames();
		ArrayList<String> keyValues = point.getKeyValues();
		String[][] array = null;
		for (int i=0; i<keyNames.size(); i++) {
			if (i!=0) {
				query += " && ";
			}
			try {
				query += keyNames.get(i)+"="+Double.valueOf(keyValues.get(i));
			} catch (NumberFormatException e) {
				query += keyNames.get(i)+" like '"+keyValues.get(i)+"'";
			}
		}
		appendToHistory(query + "\n");
		try {
			ResultSet result = model.queryDb(query);
			ResultSetMetaData md = result.getMetaData();
			int numCols = md.getColumnCount();
			array = new String[numCols][2];
			while (result.next()) {
				for (int i=0; i<numCols; i++) {
					array[i][0] = md.getColumnName(i+1);
					try {
						array[i][1] = String.valueOf(result.getDouble(i+1));
					} catch (SQLException e) {
						array[i][1] = result.getString(i+1);
					}
					if (result.wasNull()) {
						array[i][1] = "null";
					}
				}
			}
		} catch (DbConnectionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (QueryException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SQLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return array;
	}

//	public String getDbDbName() {
//		return model.getDbDbName();
//	}
//
//	public String getDbHostName() {
//		return model.getDbHostName();
//	}
//
//	public String getDbPassword() {
//		return model.getDbPassword();
//	}
//
//	public String getDbTableName() {
//		return model.getDbTableName();
//	}
//
//	public String getDbUserName() {
//		return model.getDbUserName();
//	}
	public Database getDb() {
		return model.getDb();
	}

	public Graphics getGraphGraphics() {
		return graph.getGraphGraphics();
	}

	public BufferedImage getGraphImage(String string) {
		return graph.getImage(string);
	}

	public String getGraphTitle() {
		return params.getGraphTitle();
	}

	public GraphView getGraphView() {
		return graph;
	}

	public int getLegendXPosition() {
		return graph.getLegendXPosition();
	}

	public int getLegendYPosition() {
		return graph.getLegendYPosition();
	}

	public ParameterView getParameterView() {
		return params;
	}
	
	public Image getScaledGraphImage(double width, double height) {
		return graph.getScaledImage(width, height);
	}

//	public String getTableName() {
//		return model.getDbTableName();
//	}

	public Dimension getWindowSize() {
		//the height correction accounts for the size of the border surrounding this frame
		return new Dimension(this.getWidth(), this.getHeight()-44);
	}

	public String getXLabel() {
		return params.getXLabel();
	}
	
	public String getXMax() {
		return params.getXMax();
	}
	
	public String getXMin() {
		return params.getXMin();
	}
	
	public String getYLabel() {
		return params.getYLabel();
	}
	
	public String getYMax() {
		return params.getYMax();
	}
	
	public String getYMin() {
		return params.getYMin();
	}
	
	public void initProgress() {
		history.init();
	}
	
	public void repaintGraph() {
		graph.populateGraphElements();
		graph.paintComponent(graph.getGraphics());
	}
	
	public void resetProgress() {
		history.resetProgress();
	}
	
	public void run() {
		params.hitRunButton();
	}
	
	public void saveAndClose() {
		exitItem.doClick();
	}

	public void setComboFields(Vector<String> tableColumnNamesNum,
			Vector<String> tableColumnNamesAll) {
		params.setComboFields(tableColumnNamesNum, tableColumnNamesAll);
		params.setShouldLoadTopUsed(true);
	}

	public void setErrorBars(int index) {
		graph.setErrorBars(index);
	}
	
	public void setLegendPosition(int selectedIndex) {
		graph.setLegendPosition(selectedIndex);
	}
	
	public void setLegendXPosition(int parseInt) {
		if (graph == null) {
			return;
		}
		graph.setLegendXBase(parseInt);
	}

	public void setLegendYPosition(int parseInt) {
		if (graph == null) {
			return;
		}
		graph.setLegendYBase(parseInt);
	}

	public void setPointType(int index) {
		graph.setPointType(index);
	}

	public void setProgressText(String s) {
		history.setProgressText(s);
	}

	public void setShouldLoadTopUsed(boolean b) {
		params.setShouldLoadTopUsed(true);
	}
	
	public void updateDbXML() {
		model.updateXML();
	}

}