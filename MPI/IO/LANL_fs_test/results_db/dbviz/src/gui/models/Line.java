package gui.models;

import java.awt.Color;
import java.util.ArrayList;
import java.util.regex.PatternSyntaxException;

public class Line {
	
	public enum ErrorBarType {
		STDDEV, RANGE, NONE
	}

	public enum PointStyle {
		CIRCLE, SQUARE, DIAMOND, UP_TRIANGLE, DOWN_TRIANGLE
	}

	public enum PointType {
		AVG, MAX, MIN, ALL
	}
	
	private HashTable hashTable;
	private ArrayList<String> compareNames;
	private ArrayList<String> compareValues;
	private Replacements reps;
	private Color color;
	private int pointStyle;
	private String compares;
	private String yVal = null;

	public Line() {
		compareNames = new ArrayList<String>();
		compareValues = new ArrayList<String>();
		hashTable = new HashTable();
		reps = new Replacements();
		compares = null;
	}
	
	public void setYVal(String s) {
		yVal = s;
	}
	
	public void addCompare(String name, String value) {
		compareNames.add(name);
		compareValues.add(value);
	}
	
	public void addPoint(LinePoint p) {
        hashTable.addToHash(p);
	}
	
	public void calcStats() {
	    hashTable.constructStats();
	}
	
	public void combineLines(Line line) {
		if (line == null) {
			return;
		}
		for (int i=0; i<line.size(PointType.ALL); i++) {
			hashTable.addToHash(line.getPtFromAll(i));
		}
		
	}

	public double getAvg(int j) {
		return hashTable.getAvg(j);
	}
	
	public Color getColor() {
		return color;
	}
	
	public String getCompares() throws PatternSyntaxException {
		if (compares == null) {
			compares = "";
			if (yVal != null) {
				compares += yVal;
				if (compareNames.size()>0) {
					compares += ", ";
				}
			}
			for (int i=0; i<compareNames.size(); i++) {
				compares += compareNames.get(i) + "=" + compareValues.get(i);
				if (i != compareNames.size()-1) {
					compares += ", ";
				}
			}
			if (reps != null && reps.size() > 0) {
				compares = reps.checkString(compares);
			}
		}
		return compares;
	}

	public double getMax(int j) {
		return hashTable.getMax(j);
	}
    
	public double getMin(int j) {
		return hashTable.getMin(j);
	}

	public int getNumCompares() {
		return compareNames.size();
	}

	public int getPointStyle() {
		return pointStyle;
	}

	public LinePoint getPt(int j, PointType pt) {
		if (pt != PointType.ALL) {
			return hashTable.getStatPoint(j, pt);
		} else {
			return this.getPtFromAll(j);
		}
	}
	
	public LinePoint getPtFromAll(int index) {
        return hashTable.getPtFromAll(index);
	}

	public double getStdDev(int j) {
		return hashTable.getStdDev(j);
	}

	public double getYVal(int j, PointType pt) {
		if (pt == PointType.AVG) {
			return getAvg(j);
		} else if (pt == PointType.MAX) {
			return getMax(j);
		} else if (pt == PointType.MIN) {
			return getMin(j);
		} else {
			return 0;
		}
	}

	public void removePoint(int j) {
        hashTable.removePoint(j);
	}

	public void removePointFromAll(int index) {
	    hashTable.removePointFromAll(index);
	}

	public void setColor(Color c) {
		color = c;
	}

	public void setCompares(String s) {
		compares = s;
	}

	public void setPointStyle(int i) {
		pointStyle = i;
	}

	public void setReplacements(Replacements reps) {
		this.reps = reps;
	}

	public int size(PointType pt) {
		if (pt == PointType.ALL) {
			return hashTable.getNumPoints();
		} else {
			return hashTable.getLoad();
		}
	}

	public int sizeOf(int j) {
        return hashTable.sizeOf(j);
	}

}
