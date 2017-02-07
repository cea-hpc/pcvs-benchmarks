package gui.models;

import java.awt.Point;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;

public class LinePoint {

	/*
	 * idea: have this implement mouse over listener -when mouse over occurs
	 * display info about point somewhere point would have to store all info
	 * retrieved from database -just create a list when looking over results in
	 * MainController the point would probably have to paint itself
	 */
	
	private ArrayList<String> keyNames;
	private ArrayList<String> keyVals;

	private double xVal;
	private double yVal;
	private String stringVal;

//	private double xPos;
//	private double yPos;

	private Rectangle2D position;

	public LinePoint() {
		this(0, 0);
	}

	public LinePoint(double newx, double newy) {
		
		keyNames = new ArrayList<String>();
		keyVals = new ArrayList<String>();
		
		xVal = newx;
		yVal = newy;
		
		position = new Rectangle2D.Double();

//		xPos = 0;
//		yPos = 0;

//		height = 0;
//		width = 0;
	}
	
	public void setStringVal(String s) {
		stringVal = s;
	}
	
	public String getStringVal() {
		return stringVal;
	}

	public void addXPos(double x) {
		position = new Rectangle2D.Double(position.getX()+x, position.getY(), position.getWidth(), position.getHeight());
		//xPos += x;
	}

	public void addYPos(double y) {
		position = new Rectangle2D.Double(position.getX(), position.getY()+y, position.getWidth(), position.getHeight());
		//yPos += y;
	}

	public boolean contains(Point p) {
		return position.contains(p);
//		if (p.x >= (xPos - width / (double)2) &&
//				p.x <= (xPos + width / (double)2) &&
//				p.y >= (yPos - height / (double)2) &&
//				p.y <= (yPos + height / (double)2)) {
//			return true;
//		} else {
//			return false;
//		}
	}

	public double getHeight() {
		return position.getHeight();
		//return height;
	}

	public double getWidth() {
//		return width;
		return position.getWidth();
	}

//	public double getXPos() {
//		return xPos;
//	}

	public double getXVal() {
		return xVal;
	}

//	public double getYPos() {
//		return yPos;
//	}

	public double getYVal() {
		return yVal;
	}

//	public void setHeight(double ht) {
//		height = ht;
//	}
//
//	public void setWidth(double wd) {
//		width = wd;
//	}

//	public void setXPos(double newx) {
//		xPos = newx;
//	}
	
	public void setPosition(double x, double y, double w, double h) {
		position = new Rectangle2D.Double(x, y, w, h);
	}

	public void setXVal(double newx) {
		xVal = newx;
	}

//	public void setYPos(double newy) {
//		yPos = newy;
//	}

	public void setYVal(double newy) {
		yVal = newy;
	}
	
	public void setKeyNames(ArrayList<String> names) {
		keyNames = names;
	}

	public void addKey(String string) {
		keyVals.add(string);
	}
	
	public ArrayList<String> getKeyNames() {
		return keyNames;
	}
	
	public ArrayList<String> getKeyValues() {
		return keyVals;
	}
	
	public ArrayList<String> getKeys() {
		ArrayList<String> keys = new ArrayList<String>();
		for (int i=0; i<Math.min(keyNames.size(), keyVals.size()); i++) {
			keys.add(keyNames.get(i)+"="+keyVals.get(i));
		}
		return keys;
	}
	
	public int getNumKeys() {
		return Math.min(keyNames.size(), keyVals.size());
	}
}
