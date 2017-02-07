package gui.models;

import gui.views.ParameterView;

import java.awt.Dimension;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;

import net.sf.json.JSONObject;

public class PersistentParams implements Persistent {

	private File file;
	private String filePath;
	private ArrayList<String> keys;
	private ArrayList<String> values;
	private ParameterView pv;
	private int windowWidth = -1;
	private int windowHeight = -1;
	private int xLegend = -1;
	private int yLegend = -1;

	public PersistentParams(String fileName, ParameterView params) throws IOException {
		pv = params;
		filePath = fileName;
		if (!filePath.endsWith(".json")) {
			filePath += ".json";
		}
		file = new File(filePath);
		keys = new ArrayList<String>();
		values = new ArrayList<String>();
		if (!file.exists()) {
			File parentFile = new File(file.getParent());
			if (!parentFile.exists()) {
				parentFile.mkdirs();
			}
			file.createNewFile();
		}
	}

	@SuppressWarnings("unchecked")
	public void parse() throws IOException {
		BufferedReader br = null;
		String temp = null;
		String full = "";
		br = new BufferedReader(new FileReader(file));
		while ((temp = br.readLine()) != null) {
			full = full.concat(temp);
		}
		if (full.length() >= 0 && full.startsWith("{")) {
			JSONObject json = JSONObject.fromObject(full);
			Iterator<String> it = (Iterator<String>) json.keys();
			String key = null;
			while (it.hasNext()) {
				key = (String) it.next();
				keys.add(key);
				values.add((String) json.get(key));
			}
		}
	}

	public Database loadParams() throws IOException {
		
		Database db = new Database();
		
		parse();
		
		String currKey = null;
		for (int i=0; i<Math.min(keys.size(), values.size()); i++) {
			currKey = keys.get(i);
			if (currKey.equalsIgnoreCase("HostName")) {
				db.setHostname(values.get(i));
			} else if (currKey.equalsIgnoreCase("DatabaseName")) {
				db.setDatabasename(values.get(i));
			} else if (currKey.equalsIgnoreCase("TableName")) {
				db.setTablename(values.get(i));
			} else if (currKey.equalsIgnoreCase("UserName")) {
				db.setUsername(values.get(i));
			} else if (currKey.equalsIgnoreCase("Password")) {
				db.setPassword(values.get(i));
			} else if (currKey.equalsIgnoreCase("GraphTitle")) {
				pv.setGraphTitle(values.get(i));
			} else if (currKey.equalsIgnoreCase("XAxisTitle")) {
				pv.setXLabel(values.get(i));
			} else if (currKey.equalsIgnoreCase("YAxisTitle")) {
				pv.setYLabel(values.get(i));
			} else if (currKey.equalsIgnoreCase("MinX")) {
				pv.setXMin(values.get(i));
			} else if (currKey.equalsIgnoreCase("MaxX")) {
				pv.setXMax(values.get(i));
			} else if (currKey.equalsIgnoreCase("MinY")) {
				pv.setYMin(values.get(i));
			} else if (currKey.equalsIgnoreCase("MaxY")) {
				pv.setYMax(values.get(i));
			} else if (currKey.equalsIgnoreCase("XLog")) {
				pv.setXLog(values.get(i));
			} else if (currKey.equalsIgnoreCase("YLog")) {
				pv.setYLog(values.get(i));
			} else if (currKey.equalsIgnoreCase("NumXTickMarks")) {
				pv.setXTickMarks(values.get(i));
			} else if (currKey.equalsIgnoreCase("NumYTickMarks")) {
				pv.setYTickMarks(values.get(i));
			} else if (currKey.equalsIgnoreCase("LineThickness")) {
				pv.setLineThickness(Integer.parseInt(values.get(i)));
			} else if (currKey.equalsIgnoreCase("PointSize")) {
				pv.setPointSize(Integer.parseInt(values.get(i)));
			} else if (currKey.equalsIgnoreCase("FontSize")) {
				pv.setFontSize(Integer.parseInt(values.get(i)));
			} else if (currKey.equalsIgnoreCase("LegendSortType")) {
				pv.setLegendSortType(values.get(i));
			} else if (currKey.equalsIgnoreCase("FormatAsDate")) {
				pv.setFormatAsDate(Boolean.valueOf(values.get(i)));
			} else if (currKey.equalsIgnoreCase("NoLines")) {
				pv.setNoLines(Boolean.valueOf(values.get(i)));
			} else if (currKey.equalsIgnoreCase("BarGraph")) {
				pv.setBarGraph(Boolean.valueOf(values.get(i)));
			} else if (currKey.equalsIgnoreCase("AvgMaxMinAll")) {
				pv.setAvgMaxMinAll(values.get(i));
			} else if (currKey.equalsIgnoreCase("ErrorBars")) {
				pv.setErrorBars(values.get(i));
			} else if (currKey.equalsIgnoreCase("LegendPosition")) {
				pv.setLegendPosition(values.get(i));
			} else if (currKey.equalsIgnoreCase("Compare")) {
				pv.setCompare(values.get(i));
			} else if (currKey.equalsIgnoreCase("XAxis")) {
				pv.setXAxis(values.get(i));
			} else if (currKey.equalsIgnoreCase("YAxis")) {
				pv.setYAxis(values.get(i));
			} else if (currKey.equalsIgnoreCase("Where")) {
				pv.setWhere(values.get(i));
			} else if (currKey.equalsIgnoreCase("WindowWidth")) {
				windowWidth = Integer.parseInt(values.get(i));
			} else if (currKey.equalsIgnoreCase("WindowHeight")) {
				windowHeight = Integer.parseInt(values.get(i));
			} else if (currKey.equalsIgnoreCase("LegendXPosition")) {
				pv.setLegendXPosition(Integer.parseInt(values.get(i)));
				xLegend = Integer.parseInt(values.get(i));
			} else if (currKey.equalsIgnoreCase("LegendYPosition")) {
				pv.setLegendYPosition(Integer.parseInt(values.get(i)));
				yLegend = Integer.parseInt(values.get(i));
			}
		}
		
		return db;
	}
	
	public int getXLegend() {
		return xLegend;
	}
	
	public int getYLegend() {
		return yLegend;
	}
	
	public void saveParams() throws IOException {
		Database db = pv.getDb();
		if (db != null) {
			keys.add("HostName");
			values.add(db.getHostName());
			keys.add("DatabaseName");
			values.add(db.getDbName());
			keys.add("TableName");
			values.add(db.getTableName());
			keys.add("UserName");
			values.add(db.getUserName());
			keys.add("Password");
			values.add(db.getPassword());
		}
		keys.add("GraphTitle");
		values.add(pv.getGraphTitle());
		keys.add("XAxisTitle");
		values.add(pv.getXLabel());
		keys.add("YAxisTitle");
		values.add(pv.getYLabel());
		keys.add("MinX");
		values.add(pv.getXMin());
		keys.add("MaxX");
		values.add(pv.getXMax());
		keys.add("MinY");
		values.add(pv.getYMin());
		keys.add("MaxY");
		values.add(pv.getYMax());
		keys.add("XLog");
		values.add(pv.getXLog());
		keys.add("YLog");
		values.add(pv.getYLog());
		keys.add("NumXTickMarks");
		values.add(pv.getXTickMarks());
		keys.add("NumYTickMarks");
		values.add(pv.getYTickMarks());
		keys.add("LineThickness");
		values.add(String.valueOf(pv.getLineThickness()));
		keys.add("PointSize");
		values.add(String.valueOf(pv.getPointSize()));
		keys.add("FontSize");
		values.add(String.valueOf(pv.getFontSize()));
		keys.add("LegendSortType");
		values.add(pv.getLegendSortType());
		keys.add("FormatAsDate");
		values.add(String.valueOf(pv.getFormatAsDate()));
		keys.add("NoLines");
		values.add(String.valueOf(pv.getNoLines()));
		keys.add("BarGraph");
		values.add(String.valueOf(pv.getBarGraph()));
		keys.add("AvgMaxMinAll");
		//values.add(String.valueOf(pv.getAvgMinMaxAllIndex()));
		values.add(pv.getAvgMaxMinAll());
		keys.add("ErrorBars");
		//values.add(String.valueOf(pv.getErrorBarsIndex()));
		values.add(pv.getErrorBars());
		keys.add("LegendPosition");
		//values.add(String.valueOf(pv.getLegendPositionIndex()));
		values.add(pv.getLegendPosition());
		keys.add("Compare");
		values.add(pv.getCompare());
		keys.add("XAxis");
		values.add(pv.getXAxis());
		keys.add("YAxis");
		values.add(pv.getYAxis());
		keys.add("Where");
		values.add(pv.getWhere());
		keys.add("LegendXPosition");
		values.add(String.valueOf(pv.getLegendXPosition()));
		keys.add("LegendYPosition");
		values.add(String.valueOf(pv.getLegendYPosition()));
		
		writeOutJson();
	}
	
	public void writeOutJson() throws IOException {

		JSONObject json = new JSONObject();

		for (int i = 0; i < Math.min(keys.size(), values.size()); i++) {
			json.put(keys.get(i), values.get(i));
		}

		FileWriter fw = null;
		fw = new FileWriter(filePath);
		json.write(fw);
		fw.flush();
	}

	public ArrayList<String> getKeys() {
		return keys;
	}

	public ArrayList<String> getValues() {
		return values;
	}
	
	public void setKeys(ArrayList<String> array) {
		keys = array;
	}
	
	public void setValues(ArrayList<String> array) {
		values = array;
	}

	public Dimension getSavedWindowSize() {
		if (windowWidth < 0 || windowHeight < 0) {
			return null;
		}
		return new Dimension(windowWidth, windowHeight);
	}

	public void saveWindowSize(Dimension windowSize) {
		keys.add("WindowWidth");
		values.add(String.valueOf( (int)windowSize.getWidth()));
		keys.add("WindowHeight");
		values.add(String.valueOf( (int)windowSize.getHeight()));
	}

}
