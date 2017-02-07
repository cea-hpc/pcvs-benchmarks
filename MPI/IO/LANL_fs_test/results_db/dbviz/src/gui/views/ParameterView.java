package gui.views;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.awt.event.FocusListener;
import java.io.IOException;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;

import gui.models.Database;
import gui.models.PersistentParams;
import gui.models.PersistentTopUsed;
import gui.views.GraphView.LegendSortType;

public class ParameterView extends JPanel {

	private static final long serialVersionUID = 1L;
	private final int FIELD_WIDTH = 15;
	private MainView mv;

	/*** graph fields ***/
	private JTextField graphTitleField;
	private JTextField xlabelField;
	private JTextField ylabelField;
	private SteppedComboBox avgMaxMinField;// JComboBox avgMaxMinField;
	private SteppedComboBox errorBarsField;
	private SteppedComboBox legendPositionField;
	private JTextField xlogField;
	private JTextField ylogField;
	private JTextField xminField;
	private JTextField xmaxField;
	private JTextField yminField;
	private JTextField ymaxField;
//	private JTextField xTickMarksField;
//	private JTextField yTickMarksField;
	
	private String xTickMarks = "";
	private String yTickMarks = "";
	private int lineThickness = 1;
	private int pointSize = 10;
	private int fontSize;
	private LegendSortType legendSortType = LegendSortType.AVG_Y;
	private boolean formatAsDate = false;
	private boolean noLines = false;
	private boolean barGraph = false;
	
	private Dimension savedWindowSize = null;
	
	private boolean shouldLoadTopUsed = true;

	/*** query fields ***/
	private SteppedComboBox xaxisInput;// JComboBox xaxisInput;
	private SteppedComboBox yaxisInput;
	//private JTextField compareField;
	private SteppedComboBox compareField;
	// private SteppedComboBox strFields;//private JComboBox strFields;
	private JTextArea whereField;
	private JScrollPane whereScroll;

	/*** labels for graph fields ***/
	private JLabel graphTitleLabel;
	private JLabel xlabelLabel;
	private JLabel ylabelLabel;
	private JLabel avgMaxMinLabel;
	private JLabel errorBarsLabel;
	private JLabel legendPositionLabel;
	private JLabel xlogLabel;
	private JLabel ylogLabel;
	private JLabel xminLabel;
	private JLabel xmaxLabel;
	private JLabel yminLabel;
	private JLabel ymaxLabel;
//	private JLabel xTickMarksLabel;
//	private JLabel yTickMarksLabel;

	/*** labels for query fields ***/
	private JLabel xaxisLabel;
	private JLabel yaxisLabel;
	private JLabel compareLabel;
	private JLabel whereLabel;

	/*** buttons ***/
	private JButton runbutton;
	//private JButton advOptionsButton;

	private Vector<String> numFields;
	private Vector<String> allFields;
	private Vector<String> xTopUsedFields;
	private Vector<String> yTopUsedFields;
	private Vector<String> cTopUsedFields;

	public ParameterView(MainView mv) {
		// this(mv, gv, null, null);
		this(mv, null, null);
	}

	public ParameterView(MainView mv, Vector<String> nums, Vector<String> all) {

		this.mv = mv;
		
		fontSize = this.getFont().getSize();

		GridBagLayout gridbag = new GridBagLayout();
		setLayout(gridbag);

		GridBagConstraints c = new GridBagConstraints();

		graphTitleLabel = new JLabel("Graph Title");
		xlabelLabel = new JLabel("X Axis Title");
		ylabelLabel = new JLabel("Y Axis Title");
		xaxisLabel = new JLabel("X Axis");
		yaxisLabel = new JLabel("Y Axis");
		avgMaxMinLabel = new JLabel("Avg/Max/Min/All");
		errorBarsLabel = new JLabel("Error Bars");
		legendPositionLabel = new JLabel("Legend Position");
		compareLabel = new JLabel("Compare");
		xlogLabel = new JLabel("X Log Base");
		ylogLabel = new JLabel("Y Log Base");
		xminLabel = new JLabel("Min X Value");
		xmaxLabel = new JLabel("Max X Value");
		yminLabel = new JLabel("Min Y Value");
		ymaxLabel = new JLabel("Max Y Value");
		whereLabel = new JLabel("Where:");

		Vector<String> avgMaxMin = new Vector<String>(4);
		avgMaxMin.add("Average");
		avgMaxMin.add("Maximum");
		avgMaxMin.add("Minimum");
		avgMaxMin.add("All");

		Vector<String> stdDevRange = new Vector<String>(3);
		stdDevRange.add("Standard Deviation");
		stdDevRange.add("Range");
		stdDevRange.add("None");

		Vector<String> legendPositions = new Vector<String>(5);
		legendPositions.add("Top Left");
		legendPositions.add("Top Right");
		legendPositions.add("Bottom Left");
		legendPositions.add("Bottom Right");
		legendPositions.add("None");

		graphTitleField = new JTextField(FIELD_WIDTH);
		xlabelField = new JTextField(FIELD_WIDTH);
		ylabelField = new JTextField(FIELD_WIDTH);
		xaxisInput = new SteppedComboBox();
		yaxisInput = new SteppedComboBox();
		compareField = new SteppedComboBox();
		setComboFields(nums, all);
		Dimension d = yaxisInput.getPreferredSize();
		
		compareField.setPreferredSize(new Dimension(200, d.height));
		compareField.setPopupWidth(d.width);
		compareField.setEditable(true);
		
		xaxisInput.setPreferredSize(new Dimension(200, d.height));
		xaxisInput.setPopupWidth(d.width);
		xaxisInput.setEditable(true);
		
		yaxisInput.setPreferredSize(new Dimension(200, d.height));
		yaxisInput.setPopupWidth(d.width);
		yaxisInput.setEditable(true);

		avgMaxMinField = new SteppedComboBox(avgMaxMin);
		avgMaxMinField.setPreferredSize(new Dimension(200, d.height));
		errorBarsField = new SteppedComboBox(stdDevRange);
		errorBarsField.setPreferredSize(new Dimension(200, d.height));
		legendPositionField = new SteppedComboBox(legendPositions);
		legendPositionField.setPreferredSize(new Dimension(200, d.height));
		xlogField = new JTextField(FIELD_WIDTH);
		ylogField = new JTextField(FIELD_WIDTH);
		xminField = new JTextField(FIELD_WIDTH);
		xmaxField = new JTextField(FIELD_WIDTH);
		yminField = new JTextField(FIELD_WIDTH);
		ymaxField = new JTextField(FIELD_WIDTH);
		whereScroll = new JScrollPane();
		whereField = new JTextArea();
		whereField.setText("user like '" + System.getProperty("user.name") + "'");

		whereScroll.setViewportView(whereField);
		whereField.setEditable(true);

		runbutton = new JButton("Run Query");

		/*** place graph fields labels ***/
		c.gridx = 0;
		c.gridy = 0;
		c.insets = new Insets(2, 5, 0, 5);
		c.gridwidth = 1;
		c.anchor = GridBagConstraints.LINE_END;
		add(graphTitleLabel, c);
		c.gridy++;
		add(xlabelLabel, c);
		c.gridy++;
		add(ylabelLabel, c);
		c.gridy++;
		add(xminLabel, c);
		c.gridy++;
		add(xmaxLabel, c);
		c.gridy++;
		add(yminLabel, c);
		c.gridy++;
		add(ymaxLabel, c);
		c.gridy++;
		add(xlogLabel, c);
		c.gridy++;
		add(ylogLabel, c);
		c.gridy++;
		add(avgMaxMinLabel, c);
		c.gridy++;
		add(errorBarsLabel, c);
		c.gridy++;
		add(legendPositionLabel, c);

		/*** place graph fields fields ***/
		c.anchor = GridBagConstraints.LINE_START;
		c.gridy = 0;
		c.gridx = 1;
		add(graphTitleField, c);
		c.gridy++;
		add(xlabelField, c);
		c.gridy++;
		add(ylabelField, c);
		c.gridy++;
		add(xminField, c);
		c.gridy++;
		add(xmaxField, c);
		c.gridy++;
		add(yminField, c);
		c.gridy++;
		add(ymaxField, c);
		c.gridy++;
		add(xlogField, c);
		c.gridy++;
		add(ylogField, c);
		c.gridy++;
		add(avgMaxMinField, c);
		c.gridy++;
		add(errorBarsField, c);
		c.gridy++;
		add(legendPositionField, c);

		/*** place query fields labels ***/
		c.anchor = GridBagConstraints.LINE_END;
		c.gridx = 0;
		c.gridy++;
		add(compareLabel, c);
		c.gridy++;
		add(xaxisLabel, c);
		c.gridy++;
		add(yaxisLabel, c);

		c.anchor = GridBagConstraints.LINE_START;
		c.gridx = 1;
		c.gridy -= 3;
		c.gridy++;
		add(compareField, c);
		c.gridy++;
		add(xaxisInput, c);
		c.gridy++;
		add(yaxisInput, c);

		c.gridx = 0;
		c.gridy++;
		add(whereLabel, c);
		c.gridy++;
		c.gridwidth = 2;
		c.weighty = .9;
		c.weightx = 1;
		c.fill = GridBagConstraints.BOTH;
		add(whereScroll, c);

		c.gridy++;
		c.gridheight = 1;
		c.weightx = 0;
		c.weighty = 0;
		c.anchor = GridBagConstraints.LINE_START;
		add(runbutton, c);
		c.gridy++;
		
		loadExitParams();

	}
	
	public void addComboBoxFocusListener(FocusListener fl, String name) {
		Component[] comps = null;
		if (name.equals("xaxis")) {
			comps = xaxisInput.getComponents();
		} else if (name.equals("yaxis")) {
			comps = yaxisInput.getComponents();
		} else if (name.equals("compare")){
			comps = compareField.getComponents();
		}
		if (comps == null) {
			return;
		}
		for (int i=0; i<comps.length; i++) {
			comps[i].addFocusListener(fl);
		}
	}


	public void addRunListener(ActionListener al) {
		runbutton.addActionListener(al);
	}

	public String getAvgMaxMinAll() {
		return (String) avgMaxMinField.getSelectedItem();
	}

	public int getAvgMaxMinAllIndex() {
		return avgMaxMinField.getSelectedIndex();
	}

	public boolean getBarGraph() {
		return barGraph;
	}

	public String getCompare() {
		return (String) compareField.getSelectedItem();
	}

	public Vector<String> getCompareTopUsed() {
		return cTopUsedFields;
	}

	public Database getDb() {
		return mv.getDb();
	}

	public String getErrorBars() {
		return (String) errorBarsField.getSelectedItem();
	}

	public int getErrorBarsIndex() {
		return errorBarsField.getSelectedIndex();
	}

	public int getFontSize() {
		return fontSize;
	}

	public boolean getFormatAsDate() {
		return formatAsDate;
	}

	public String getGraphTitle() {
		return graphTitleField.getText();
	}
	
	public String getLegendPosition() {
		return (String) legendPositionField.getSelectedItem();
	}
	
	public int getLegendPositionIndex() {
		return legendPositionField.getSelectedIndex();
	}
	
	public String getLegendSortType() {
		if (legendSortType == LegendSortType.ALPHANUM) {
			return "Alphanumerically";
		} else if (legendSortType == LegendSortType.FIRST_Y) {
			return "First Y Value";
		} else if (legendSortType == LegendSortType.LAST_Y) {
			return "Last Y Value";
		} else {
			return "Average Y Value";
		}
	}
	
	public int getLegendXPosition() {
		return mv.getLegendXPosition();
	}

	public int getLegendYPosition() {
		return mv.getLegendYPosition();
	}

	public int getLineThickness() {
		return lineThickness;
	}

	public boolean getNoLines() {
		return noLines;
	}

	public int getPointSize() {
		return pointSize;
	}

	public Dimension getSavedWindowSize() {
		return savedWindowSize;
	}

	public String getWhere() {
		return whereField.getText();
	}

	public String getXAxis() {
		return (String) xaxisInput.getSelectedItem();
	}

	public int getXAxisIndex() {
		return xaxisInput.getSelectedIndex();
	}

	public String getXLabel() {
		return xlabelField.getText();
	}
	
	public String getXLog() {
		return xlogField.getText();
	}
	
	public String getXMax() {
		return xmaxField.getText();
	}

	public String getXMin() {
		return xminField.getText();
	}

	public String getXTickMarks() {
		return xTickMarks;
	}
	
	public Vector<String> getXTopUsed() {
		return xTopUsedFields;
	}
	
	public String getYAxis() {
		return (String) yaxisInput.getSelectedItem();
	}

	public int getYAxisIndex() {
		return yaxisInput.getSelectedIndex();
	}
	
	public String getYLabel() {
		return ylabelField.getText();
	}
	
	public String getYLog() {
		return ylogField.getText();
	}
	
	public String getYMax() {
		return ymaxField.getText();
	}

	public String getYMin() {
		return yminField.getText();
	}

	public String getYTickMarks() {
		return yTickMarks;
	}

	public Vector<String> getYTopUsed() {
		return yTopUsedFields;
	}

	public void hitRunButton() {
		runbutton.doClick();
	}

	private void loadExitParams() {
		PersistentParams pp = null;
		try {
			pp = new PersistentParams("configs/exitParams.json", this);
			pp.loadParams();
			savedWindowSize = pp.getSavedWindowSize();
		} catch (IOException ioe) {
			mv.appendToHistory("Unable to load parameters from last use: "
					+ ioe.getMessage() + "\n");
		}
		mv.appendToHistory("Loaded parameter settings from "
				+ "configs/exitParams.json\n");
	
		pp = null;
	}

	public void replaceColNames(Vector<String> tableColumnNamesNum) {
		xaxisInput.setSelectedItem(null);
		yaxisInput.setSelectedItem(null);
		xaxisInput.removeAllItems();
		yaxisInput.removeAllItems();

		String curr = null;
		for (int i = 0; i < tableColumnNamesNum.size(); i++) {
			curr = tableColumnNamesNum.get(i);
			xaxisInput.addItem(curr);
			yaxisInput.addItem(curr);
		}
		if (tableColumnNamesNum.size() >= 1) {
			xaxisInput.setSelectedIndex(0);
			yaxisInput.setSelectedIndex(0);
		}
	}

	public void setAvgMaxMinAll(int parseInt) {
		avgMaxMinField.setSelectedIndex(parseInt);
	}

	public void setAvgMaxMinAll(String s) {
		avgMaxMinField.setSelectedItem(s);
	}

	public void setBarGraph(boolean b) {
		barGraph = b;
	}

	//TODO: check that this is working properly
	public void setComboFields(Vector<String> nums, Vector<String> all) {
		
		numFields = nums;
		allFields = all;
		
		int count = xaxisInput.getItemCount();
		for (int i=0; i<count; i++) {
			xaxisInput.removeItemAt(0);
		}
		count = yaxisInput.getItemCount();
		for (int i=0; i<count; i++) {
			yaxisInput.removeItemAt(0);
		}
		count = compareField.getItemCount();
		for (int i=0; i<count; i++) {
			compareField.removeItemAt(0);
		}
		
		if (shouldLoadTopUsed) {
			PersistentTopUsed ptu = null;
			try {
				Database db = mv.getDb();
				if (db == null) {
					xTopUsedFields = null;
					yTopUsedFields = null;
					cTopUsedFields = null;
				} else {
					ptu = new PersistentTopUsed(db.getTableName(), this);
					xTopUsedFields = ptu.loadTopUsedX();
					yTopUsedFields = ptu.loadTopUsedY();
					cTopUsedFields = ptu.loadTopUsedCompare();
				}
			} catch (IOException ioe) {
				mv.appendToHistory("Unable to load top used fields from config file: "+ioe.getMessage()+"\n");
			} catch (NullPointerException npe) {
				//if this line is reached, then there is no current db set probably
			}
			shouldLoadTopUsed = false;
		}
		if (xTopUsedFields == null) {
			xTopUsedFields = new Vector<String>();
		}
		if (yTopUsedFields == null) {
			yTopUsedFields = new Vector<String>();
		}
		if (cTopUsedFields == null) {
			cTopUsedFields = new Vector<String>();
		}
		for (int i = 0; i < xTopUsedFields.size(); i++) {
			xaxisInput.addItem(xTopUsedFields.get(i));
		}
		if (xTopUsedFields.size() > 0) {
			xaxisInput.addItem("----------");
		}
		for (int i = 0; i < yTopUsedFields.size(); i++) {
			yaxisInput.addItem(yTopUsedFields.get(i));
		}
		if (yTopUsedFields.size() > 0) {
			yaxisInput.addItem("----------");
		}
		for (int i = 0; i < cTopUsedFields.size(); i++) {
			compareField.addItem(cTopUsedFields.get(i));
		}
		if (cTopUsedFields.size() > 0) {
			compareField.addItem("----------");
		}
		for (int i = 0; i<numFields.size(); i++) {
			xaxisInput.addItem(numFields.get(i));
			yaxisInput.addItem(numFields.get(i));
		}
		for (int i = 0; i<allFields.size(); i++) {
			compareField.addItem(allFields.get(i));
		}
		if (xaxisInput.getItemCount() > 0) {
			xaxisInput.setSelectedIndex(0);
		}
		if (yaxisInput.getItemCount() > 0) {
			yaxisInput.setSelectedIndex(0);
		}
		if (compareField.getItemCount() > 0) {
			compareField.setSelectedIndex(0);
		}
	}

	public void setCompare(String s) {
		compareField.setSelectedItem(s);
		//compareField.setText(s);
	}

	public void setCompareField(String s) {
		compareField.setSelectedItem(s);
	}
	
	public void setCompareTopUsed(Vector<String> compare) {
		cTopUsedFields = compare;
	}
	
	public void setErrorBars(int parseInt) {
		errorBarsField.setSelectedIndex(parseInt);
	}
	
	public void setErrorBars(String s) {
		errorBarsField.setSelectedItem(s);
	}
	
	public void setFontSize(int i) {
		fontSize = i;
	}
	
	public void setFormatAsDate(boolean b) {
		formatAsDate = b;
	}
	
	public void setGraphTitle(String s) {
		graphTitleField.setText(s);
	}
	
	public void setLegendPosition(int parseInt) {
		legendPositionField.setSelectedIndex(parseInt);
	}
	
	public void setLegendPosition(String s) {
		legendPositionField.setSelectedItem(s);
	}
	
	public void setLegendSortType(String sortType) {
		if (sortType.equals("Alphanumerically")) {
			legendSortType = LegendSortType.ALPHANUM;
		} else if (sortType.equals("First Y Value")) {
			legendSortType = LegendSortType.FIRST_Y;
		} else if (sortType.equals("Last Y Value")) {
			legendSortType = LegendSortType.LAST_Y;
		} else {
			legendSortType = LegendSortType.AVG_Y;
		}
	}
	
	public void setLegendXPosition(int parseInt) {
		mv.setLegendXPosition(parseInt);
	}

	public void setLegendYPosition(int parseInt) {
		mv.setLegendYPosition(parseInt);
	}
	
	public void setLineThickness(int i) {
		lineThickness = i;
	}
	
	public void setNoLines(boolean b) {
		noLines = b;
	}

	public void setPointSize(int i) {
		pointSize = i;
	}

	public void setShouldLoadTopUsed(boolean b) {
		shouldLoadTopUsed = b;
	}
	
	public void setWhere(String s) {
		whereField.setText(s);
	}
	
	public void setXAxis(int index) {
		xaxisInput.setSelectedIndex(index);
	}
	
	public void setXAxis(String string) {
		xaxisInput.setSelectedItem(string);
	}
	
	public void setXLabel(String s) {
		xlabelField.setText(s);
	}
	
	public void setXLog(String s) {
		xlogField.setText(s);
	}
	
	public void setXMax(String s) {
		xmaxField.setText(s);
	}
	
	public void setXMin(String s) {
		xminField.setText(s);
	}
	
	public void setXTickMarks(String s) {
		xTickMarks = s;
	}

	public void setXTopUsed(Vector<String> x) {
		xTopUsedFields = x;
	}
	
	public void setYAxis(int index) {
		yaxisInput.setSelectedIndex(index);
	}
	
	public void setYAxis(String string) {
		yaxisInput.setSelectedItem(string);
	}
	
	public void setYLabel(String s) {
		ylabelField.setText(s);
	}
	
	public void setYLog(String s) {
		ylogField.setText(s);
	}
	
	public void setYMax(String s) {
		ymaxField.setText(s);
	}

	public void setYMin(String s) {
		yminField.setText(s);
	}
	
	public void setYTickMarks(String s) {
		yTickMarks = s;
	}

	public void setYTopUsed(Vector<String> y) {
		yTopUsedFields = y;
	}

	public void updateTopUsed() {
		
		boolean replace = true;
		String comp = this.getCompare();
		for (int i=0; i<cTopUsedFields.size(); i++) {
			if (cTopUsedFields.get(i).equals(comp)) {
				replace = false;
				break;
			}
		}
		if (replace) {
			while (cTopUsedFields.size()>=10) {
				cTopUsedFields.remove(cTopUsedFields.size()-1);
			}
			cTopUsedFields.add(0, comp);
		}
		
		replace = true;
		String x = getXAxis();
		for (int i=0; i<xTopUsedFields.size(); i++) {
			if (xTopUsedFields.get(i).equals(x)) {
				replace = false;
				break;
			}
		}
		if (replace) {
			while (xTopUsedFields.size()>=10) {
				xTopUsedFields.remove(xTopUsedFields.size()-1);
			}
			xTopUsedFields.add(0, x);
		}
		
		replace = true;
		String y = getYAxis();
		for (int i=0; i<yTopUsedFields.size(); i++) {
			if (yTopUsedFields.get(i).equals(y)) {
				replace = false;
				break;
			}
		}
		if (replace) {
			while (yTopUsedFields.size()>=10) {
				yTopUsedFields.remove(yTopUsedFields.size()-1);
			}
			yTopUsedFields.add(0, y);
		}
		
		setComboFields(numFields, allFields);
		setCompareField(comp);
		setXAxis(x);
		setYAxis(y);
	}
	
}
