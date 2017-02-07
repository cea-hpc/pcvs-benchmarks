package gui.views;

import gui.controllers.GraphManipListener;
import gui.models.Database;
import gui.models.Line;
import gui.models.LinePoint;
import gui.models.MainModel;
import gui.models.PersistentDefaultLabels;
import gui.models.Line.ErrorBarType;
import gui.models.Line.PointType;

import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.RenderingHints;
import java.awt.font.FontRenderContext;
import java.awt.font.LineMetrics;
import java.awt.geom.AffineTransform;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.regex.PatternSyntaxException;

import javax.imageio.ImageIO;
import javax.swing.JPanel;

/*
 * This class plots the data using the result from the query and the values
 * specified in the parameter panel.
 */
public class GraphView extends JPanel {

	// enumeration of possible legend locations
	public enum LegendLocation {
		TOP_LEFT, TOP_RIGHT, BOTTOM_LEFT, BOTTOM_RIGHT, NONE //PLACE, NONE
	}
	
	// enumeration of possible legend sort type
	public enum LegendSortType {
		AVG_Y, FIRST_Y, LAST_Y, ALPHANUM
	}

	private boolean barGraph = true;
	private boolean setLegend = false;
	private int xbase;
	private int ybase;
	
	private static final long serialVersionUID = 1L;

	// number of line types (e.g. square, circle, etc.)
	private static final int NUM_LINE_TYPES = 5;

	// half the width of a point (in pixels)
	private static int HALF_POINT_WIDTH = 5;

	// specified location of legend
	private LegendLocation legendPosition = LegendLocation.TOP_LEFT;

	private ArrayList<LinePoint> legendPoints;

	private PointType pt;

	private ErrorBarType ebt;

	private GraphManipListener graphManip; // listener for events on graph

	private double minX; // minimum x value
	private double maxX; // maximum x value
	private double xInt; // interval between x tick marks
	private double minY; // minimum y value
	private double maxY; // maximum y value
	private double yInt; // interval between y tick marks

	private int xdivisor; // number of intervals on x axis
	private int xlog; // log base for x axis
	private int ydivisor; // number of intervals on y axis
	private int ylog; // log base for y axis

	private String title; // graph title
	private String xLabel; // x axis label
	private String yLabel; // y axis label

	private MainView mv; // main panel of gui
	private ParameterView pv; // parameter panel for gui

	private ArrayList<Color> colors; // list of colors to paint with

	private ArrayList<Line> lines; // list of lines to draw

	private final int PAD = 100; // pad around edges of graph (in pixels)

	private Graphics2D g2; // graphics object to draw with

	// origin
	private double x0;
	private double y0;

	private double width;
	private double height;
	private double xInc;
	private double yInc;
	
	//private boolean forPDF;

	/**
	 * Constructs GraphView
	 * 
	 * @param mv
	 *            main panel of gui
	 * @param params
	 *            parameter panel of gui
	 */
	public GraphView(MainView mv, ParameterView params) {

		/*** initialize main view and parameter view ***/
		this.mv = mv;
		pv = params;

		width = getWidth();
		height = getHeight();

		/*** add listeners for events on graph ***/
		graphManip = new GraphManipListener(this);
		addMouseListener(graphManip);
		addMouseMotionListener(graphManip);
		addMouseWheelListener(graphManip);

		/*** set background color of graph ***/
		setBackground(Color.white);

		/*** add colors to paint with ***/
		colors = new ArrayList<Color>();
		colors.add(Color.red);
		colors.add(Color.blue);
		colors.add(Color.green);
		colors.add(Color.cyan);
		colors.add(Color.orange);
		colors.add(Color.magenta);
		colors.add(Color.lightGray);
		colors.add(Color.pink);
		colors.add(Color.darkGray);
		colors.add(Color.yellow);
		colors.add(Color.black);

		/*** initialize lines to draw ***/
		lines = new ArrayList<Line>();

		title = "";
		xLabel = "";
		yLabel = "";

		/*** initialize graphing parameters ***/
		minX = 0;
		maxX = 10;
		minY = 0;
		maxY = 10;
		xlog = 0;
		ylog = 0;
		xdivisor = 11;
		ydivisor = 11;
		pt = PointType.AVG;

		legendPoints = new ArrayList<LinePoint>();
		
//		forPDF = false;

	}

	public void addPoints(ArrayList<Line> newLines) {
		if (newLines == null) {
			return;
		}
		for (int i=0; i<newLines.size(); i++) {
			lines.add(newLines.get(i));
		}
		setPoints(lines);
	}
	
	public void combineLines(int lineIndex, int otherIndex) {
		lines.get(lineIndex).combineLines(lines.get(otherIndex));
		lines.get(lineIndex).calcStats();
		lines.remove(otherIndex);
	}
	
	public String[][] describePoint(LinePoint point) {
		return mv.describePoint(point);
	}

	private void drawAxes() {
		g2.draw(new Line2D.Double(x0, y0, width - PAD, y0)); // x axis
		g2.draw(new Line2D.Double(x0, y0, x0, PAD)); // y axis
	}

	private void drawErrorBar(Graphics2D g2, Line line, int j, double currX, double currY) {
		
		LinePoint currPt = line.getPt(j, pt);
		
		/** draw range bars **/
		if (pt == PointType.AVG) {
			if (ebt == ErrorBarType.STDDEV) {
				if (line.sizeOf(j) > 1) {
					double stddev = line.getStdDev(j);
					double topRange = Math.min(currPt.getYVal() + stddev, maxY);
					double topPixelY;
					if (ylog > 0) {
						double lower = Math.floor(log(topRange, ylog));
						double base = log(minY, ylog);
						double lowerPixel = ((lower - base) * yInc);
						topPixelY = y0
								- ((log(topRange, ylog) - lower) * yInc + lowerPixel);
					} else {
						topPixelY = y0 - (topRange - minY)
								* ((yInc * (ydivisor - 1)) / (maxY - minY));
					}
					g2.draw(new Line2D.Double(currX, currY, currX, topPixelY));
					g2.draw(new Line2D.Double(currX
								- HALF_POINT_WIDTH, topPixelY, currX
								+ HALF_POINT_WIDTH, topPixelY));
					double bottomRange = Math.max(currPt.getYVal() - stddev, minY);
					double bottomPixelY;
					if (ylog > 0) {
						double lower = Math
								.floor(log(bottomRange, ylog));
						double base = log(minY, ylog);
						double lowerPixel = ((lower - base) * yInc);
						bottomPixelY = y0
								- ((log(bottomRange, ylog) - lower)
										* yInc + lowerPixel);
					} else {
						bottomPixelY = y0 - (bottomRange - minY)
								* ((yInc * (ydivisor - 1)) / (maxY - minY));
					}
					g2.draw(new Line2D.Double(currX, currY, currX, bottomPixelY));
					g2.draw(new Line2D.Double(currX
								- HALF_POINT_WIDTH, bottomPixelY, currX
								+ HALF_POINT_WIDTH, bottomPixelY));
				}
			} else if (ebt == ErrorBarType.RANGE) {
				if (line.sizeOf(j) > 1) {
					double topRange = Math.min(line.getMax(j), maxY);
					double topPixelY;
					if (ylog > 0) {
						double lower = Math.floor(log(topRange, ylog));
						double base = log(minY, ylog);
						double lowerPixel = ((lower - base) * yInc);
						topPixelY = y0
								- ((log(topRange, ylog) - lower) * yInc + lowerPixel);
					} else {
						topPixelY = y0 - (topRange - minY)
								* ((yInc * (ydivisor - 1)) / (maxY - minY));
					}
					g2.draw(new Line2D.Double(currX, currY, currX, topPixelY));
					g2.draw(new Line2D.Double(currX
								- HALF_POINT_WIDTH, topPixelY, currX
								+ HALF_POINT_WIDTH, topPixelY));

					double bottomRange = Math.min(line.getMin(j), minY);
					double bottomPixelY;
					if (ylog > 0) {
						double lower = Math
								.floor(log(bottomRange, ylog));
						double base = log(minY, ylog);
						double lowerPixel = ((lower - base) * yInc);
						bottomPixelY = y0
								- ((log(bottomRange, ylog) - lower)
										* yInc + lowerPixel);
					} else {
						bottomPixelY = y0 - (bottomRange - minY)
								* ((yInc * (ydivisor - 1)) / (maxY - minY));
					}
					g2.draw(new Line2D.Double(currX, currY, currX, bottomPixelY));
					g2.draw(new Line2D.Double(currX
								- HALF_POINT_WIDTH, bottomPixelY, currX
								+ HALF_POINT_WIDTH, bottomPixelY));
				}
			}
		}
	}

	/**
	 * Draws the legend
	 * 
	 * @param g2
	 *            graphics object to draw with
	 */
	private void drawLegend() {
		
		//System.out.println("legendPosition " + legendPosition);
		
		if (lines == null) {
			return;
		}
		
		if (legendPosition == LegendLocation.NONE) {
			return;
		}
		
		Line line = null;
		ArrayList<String> finalCompares = new ArrayList<String>(lines.size());
		int ySpace = 20;
		int lineLength = 20;

		Font font = g2.getFont();
		FontMetrics metrics = g2.getFontMetrics(font);
		float textHt = metrics.getHeight();

		int maxWidth = -1;
		String allComps = null;
		for (int i = 0; i < Math.min(colors.size() * NUM_LINE_TYPES, lines
				.size()); i++) {
			
			legendPoints.add(new LinePoint());

			line = lines.get(i);
			//lineType = line.get 
			
			try {
				allComps = line.getCompares();
			} catch (PatternSyntaxException pse) {
				mv.appendToHistory(pse.getMessage() + "\n");
			}
			if (allComps == null || allComps.equals("")) {
				return;
			}
//			allComps = tempCompares;
//			for (int j = 0; j < tempCompares.size(); j++) {
//				allComps += tempCompares.get(j);
//				if (j != tempCompares.size() - 1) {
//					allComps += ", ";
//				}
//			}
			if (allComps.length() > maxWidth) {
				maxWidth = metrics.stringWidth(allComps);
			}
			finalCompares.add(allComps);
		}

		if (!setLegend) {
			if (legendPosition == LegendLocation.TOP_RIGHT) {
				xbase = (int) (width - (lineLength + 10 + maxWidth + PAD));
				ybase = (int) (PAD + textHt / 2);
			} else if (legendPosition == LegendLocation.TOP_LEFT) {
				xbase = PAD + 5;
				ybase = (int) (PAD + textHt / 2);
			} else if (legendPosition == LegendLocation.BOTTOM_LEFT) {
				xbase = PAD + 5;
				ybase = (int) (height - (PAD + textHt / 2 + textHt
						* Math.min(colors.size() * NUM_LINE_TYPES, lines.size())));
			} else if (legendPosition == LegendLocation.BOTTOM_RIGHT) {
				xbase = (int) (width - (lineLength + 10 + maxWidth + PAD));
				ybase = (int) (height - (PAD + textHt / 2 + textHt
						* Math.min(colors.size() * NUM_LINE_TYPES, lines.size())));
			}
		}

		for (int i = 0; i < Math.min(colors.size() * NUM_LINE_TYPES, lines
				.size()); i++) {

			line = lines.get(i);
			g2.setPaint(line.getColor());

			legendPoints.get(i).setPosition(xbase + (lineLength / 2), ybase + i * ySpace, 2 * HALF_POINT_WIDTH, 2 * HALF_POINT_WIDTH);

			
			if (barGraph) {
				drawPoint(g2, (xbase + (lineLength / 2)), ybase + i * ySpace, 1);
			} else {
				g2.draw(new Line2D.Double(xbase, ybase + i * ySpace, xbase
						+ lineLength, ybase + i * ySpace));
				drawPoint(g2, (xbase + (lineLength / 2)), ybase + i * ySpace, line.getPointStyle());
			}

			g2.setPaint(Color.black);
			g2.drawString(finalCompares.get(i), xbase + lineLength + 10, (ybase
					- 2 + (textHt / 3) + (i * ySpace)));
		}
	}
	
	/**
	 * Draws a data point
	 * 
	 * @param x
	 *            x position
	 * @param y
	 *            y position
	 * @param type
	 *            type of point to draw
	 */
	private void drawPoint(Graphics2D g2, double x, double y, int type) {

		//TODO: I lose precision if I cast these to ints
		int x_int = (int) Math.round(x);
		int y_int = (int) Math.round(y);
		int[] xpts;
		int[] ypts;
		Polygon poly;
		
		switch (type) {
		case 0: // draw circle
			g2.fill(new Ellipse2D.Double(x_int - HALF_POINT_WIDTH, y_int
					- HALF_POINT_WIDTH, 2 * HALF_POINT_WIDTH,
					2 * HALF_POINT_WIDTH));
			break;

		case 1: // draw square
			g2.fill(new Rectangle2D.Double((x-HALF_POINT_WIDTH), (y-HALF_POINT_WIDTH), 2*HALF_POINT_WIDTH, 2*HALF_POINT_WIDTH));
			//g2.fillRect((x_int-HALF_POINT_WIDTH), (y_int-HALF_POINT_WIDTH), 2*HALF_POINT_WIDTH, 2*HALF_POINT_WIDTH);
			break;

		case 2: // draw a diamond
			xpts = new int[4];
			ypts = new int[4];
			xpts[0] = x_int - HALF_POINT_WIDTH;
			xpts[1] = x_int;
			xpts[2] = x_int + HALF_POINT_WIDTH;
			xpts[3] = x_int;
			ypts[0] = y_int;
			ypts[1] = y_int - HALF_POINT_WIDTH;
			ypts[2] = y_int;
			ypts[3] = y_int + HALF_POINT_WIDTH;
			poly = new Polygon(xpts, ypts, 4);
			g2.fillPolygon(poly);
			/*g2.draw(new Line2D.Double(x - HALF_POINT_WIDTH, y
					+ HALF_POINT_WIDTH, x + HALF_POINT_WIDTH, y
					- HALF_POINT_WIDTH)); // draw an x
			g2.draw(new Line2D.Double(x - HALF_POINT_WIDTH, y
					- HALF_POINT_WIDTH, x + HALF_POINT_WIDTH, y
					+ HALF_POINT_WIDTH));*/
			break;

		case 3: // draw an upward pointing triangle

			xpts = new int[3];
			ypts = new int[3];
			xpts[0] = x_int - HALF_POINT_WIDTH;
			xpts[1] = x_int;
			xpts[2] = x_int + HALF_POINT_WIDTH;
			ypts[0] = y_int + HALF_POINT_WIDTH;
			ypts[1] = y_int - HALF_POINT_WIDTH;
			ypts[2] = y_int + HALF_POINT_WIDTH;
			
			poly = new Polygon(xpts, ypts, 3);
			g2.fillPolygon(poly);
			
			/*g2.draw(new Line2D.Double(x - HALF_POINT_WIDTH, y
					+ HALF_POINT_WIDTH, x + HALF_POINT_WIDTH, y
					+ HALF_POINT_WIDTH));
			g2.draw(new Line2D.Double(x - HALF_POINT_WIDTH, y
					+ HALF_POINT_WIDTH, x, y - HALF_POINT_WIDTH));
			g2.draw(new Line2D.Double(x + HALF_POINT_WIDTH, y
					+ HALF_POINT_WIDTH, x, y - HALF_POINT_WIDTH));*/
			break;

		default: // draw a downward pointing triangle

			xpts = new int[3];
			ypts = new int[3];
			xpts[0] = x_int - HALF_POINT_WIDTH;
			xpts[1] = x_int + HALF_POINT_WIDTH;
			xpts[2] = x_int;
			ypts[0] = y_int - HALF_POINT_WIDTH;
			ypts[1] = y_int - HALF_POINT_WIDTH;
			ypts[2] = y_int + HALF_POINT_WIDTH;
			
			poly = new Polygon(xpts, ypts, 3);
			g2.fillPolygon(poly);
			
			/*g2.draw(new Line2D.Double(x - HALF_POINT_WIDTH, y
					- HALF_POINT_WIDTH, x + HALF_POINT_WIDTH, y
					- HALF_POINT_WIDTH));
			g2.draw(new Line2D.Double(x - HALF_POINT_WIDTH, y
					- HALF_POINT_WIDTH, x, y + HALF_POINT_WIDTH));
			g2.draw(new Line2D.Double(x + HALF_POINT_WIDTH, y
					- HALF_POINT_WIDTH, x, y + HALF_POINT_WIDTH));*/
			break;

		}
	}

	private void drawPoints(Graphics2D g2) {
		
		if (lines == null) {
			return;
		}

		/*** makes features painted after this point transparent ***/
		float alpha = (float) .75;
		g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER,
				alpha));
		
		double currX = 0;
		double currY = 0;

		/**** plot data ****/
		Line line = null;
		LinePoint currPt = null;

		double oldX = 0;
		double oldY = 0;

		/*** loop through the lines to draw ***/
		for (int i = 0; i < Math.min(colors.size() * NUM_LINE_TYPES, lines
				.size()); i++) {

			line = lines.get(i);
			oldX = 0;
			oldY = 0;

			g2.setPaint(line.getColor());		//colors.get(i%colors.size()));
			
			/*** loop through the points in the current line ***/
			for (int j = 0; j < line.size(pt); j++) {

				currPt = line.getPt(j, pt);

				/** calculate position to draw current point **/
				if (xlog > 0) {
					double lower = Math.floor(log(currPt.getXVal(), xlog));
					double base = log(minX, xlog);
					double lowerPixel = ((lower - base) * xInc);
					currX = x0
							+ ((log(currPt.getXVal(), xlog) - lower) * xInc + lowerPixel);
				} else {
					currX = x0 + (currPt.getXVal() - minX)
							* ((xInc * (xdivisor - 1)) / (maxX - minX));
				}
				if (ylog > 0) {
					double lower = Math.floor(log(currPt.getYVal(), ylog));
					double base = log(minY, ylog);
					double lowerPixel = ((lower - base) * yInc);
					currY = y0
							- ((log(currPt.getYVal(), ylog) - lower) * yInc + lowerPixel);
				} else {
					currY = y0 - (currPt.getYVal() - minY)
							* ((yInc * (ydivisor - 1)) / (maxY - minY));
				}

				/** let the point know where it's going and how big it is **/
				currPt.setPosition(currX, currY, 2*HALF_POINT_WIDTH, 2*HALF_POINT_WIDTH);

				/** draw current point **/
				drawPoint(g2, currX, currY, line.getPointStyle());

				/** draw range bars **/
				drawErrorBar(g2, line, j, currX, currY);

				/** connect the points of this line with a line **/
				if ((j > 0) && (pt != PointType.ALL) && !pv.getNoLines()) {
					g2.draw(new Line2D.Double(oldX, oldY, currX, currY));
				}

				oldX = currX;
				oldY = currY;
			}
		}

		/***
		 * display error message if there aren't enough line colors and types to
		 * uniquely draw the lines
		 ***/
		if (lines.size() > (colors.size() * NUM_LINE_TYPES)) {
			mv.appendToHistory("There weren't enough line and color combinations to draw all of the lines.\n");
		}
		
	}

	private void drawTickMarks() {

		/*** temporary x and y position variables used when drawing ***/
		double currY = 0;
		double currX = 0;

		/*** draw tick marks along x axis ***/
		for (int i = 1; i < xdivisor; i++) {
			currX = x0 + i * xInc;
			g2.draw(new Line2D.Double(currX, y0, currX, y0 + 2));
		}

		/*** draw tick marks along y axis ***/
		for (int i = 1; i < ydivisor; i++) {
			currY = y0 - i * yInc;
			g2.draw(new Line2D.Double(x0, currY, x0 - 2, currY));
			float alpha = (float) .5;
			g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER,
					alpha));
			g2.draw(new Line2D.Double(x0 + 1, currY, width - PAD, currY));
			g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 1));
		}
	}

	private void drawTitle() {

		Font font = g2.getFont();
		FontRenderContext render = g2.getFontRenderContext();

		float textWidth = 0;
		float x = 0;
		float y = 0;

		/*** display title ***/
		textWidth = (float) font.getStringBounds(title, render).getWidth();
		x = (float) ((width / 2) - textWidth / 2);
		y = (float) PAD / 2;
		g2.drawString(title, x, y);

	}

	private void drawXLabel() {

		Font font = g2.getFont();
		FontRenderContext render = g2.getFontRenderContext();

		float textWidth = 0;
		float x = 0;
		float y = 0;

		/*** display x axis label ***/
		textWidth = (float) font.getStringBounds(xLabel, render).getWidth();
		x = (float) ((width / 2) - textWidth / 2);
		y = (float) (height - (PAD * .5));
		g2.drawString(xLabel, x, y);
	}

	private void drawYLabel() {

		Font font = g2.getFont();
		FontRenderContext render = g2.getFontRenderContext();
		LineMetrics metrics = font.getLineMetrics("TickMarkLabels", render);

		float textHt = metrics.getHeight();
		float textWidth = 0;
		float x = 0;
		float y = 0;

		/*** display y axis label ***/
		textHt = (float) font.getStringBounds(yLabel, render).getHeight();
		textWidth = (float) font.getStringBounds(yLabel, render).getWidth();
		x = (float) ((height / 2) + textWidth / 2);
		y = (5 + textHt);
		g2.rotate(-Math.PI / 2);
		g2.drawString(yLabel, -x, y);
		g2.rotate(Math.PI / 2);

	}
	
	public boolean getFormatAsDate() {
		return pv.getFormatAsDate();
	}

	public Graphics getGraphGraphics() {
		return g2;
	}

	public BufferedImage getImage(String caption) {
		
		BufferedImage bi = new BufferedImage(getWidth(), getHeight(),
				BufferedImage.TYPE_INT_RGB);
		Graphics2D g2d = bi.createGraphics();
//		this.forPDF = true;
		this.paintComponent(g2d);
//		this.forPDF = false;
		
		g2d.setPaint(Color.black);
		g2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 1));
		
		FontMetrics metrics = g2d.getFontMetrics();
		int adv = metrics.stringWidth(caption);
		int ht = (int) (height-32);
		
		if (adv > width-40) {
			int begin = 0;
			int textHt = metrics.getHeight();
			for (int i=0; i<caption.length(); i++) {
				adv = metrics.stringWidth(caption.substring(begin, i+1));
				if (adv > width-40) {
					g2d.drawString(caption.substring(begin, i), 20, ht);
					begin = i;
					ht += textHt;
					if (ht > height) {
						//System.out.println("Couldn't fit the entire caption on the picture");
						return bi;
					}
				} else if (i == caption.length()-1) {
					//System.out.println("drawing the last part of the caption");
					g2d.drawString(caption.substring(begin, caption.length()), 20, ht);
				}
			}
		} else {
			g2d.drawString(caption, 20, ht);
		}
		
		return bi;
	}

	public ArrayList<LinePoint> getLegendPoints() {
		return legendPoints;
	}

	public LegendLocation getLegendType() {
		return legendPosition;
	}

	public int getLegendXPosition() {
		return xbase;
	}

	public int getLegendYPosition() {
		return ybase;
	}

	/**
	 * Returns lines to draw
	 * 
	 * @return lines to draw
	 */
	public ArrayList<Line> getLines() {
		return lines;
	}

	public double getMaxX() {
		return maxX;
	}

	public double getMaxY() {
		return maxY;
	}

	public double getMinX() {
		return minX;
	}

	public double getMinY() {
		return minY;
	}

	public int getPadSize() {
		return PAD;
	}

	public double getPlotHeight() {
		return getHeight() - 2 * PAD;
	}
	
	public double getPlotWidth() {
		return getWidth() - 2 * PAD;
	}

	public LinePoint getPoint(int lineIndex, int pointIndex) {
		return lines.get(lineIndex).getPt(pointIndex, pt);
	}
	
	public PointType getPointType() {
		return pt;
	}

	public BufferedImage getScaledImage(double width2, double height2) {
		BufferedImage bi = new BufferedImage((int) Math.floor(width
				* getWidth()), (int) Math.floor(height2 * getHeight()),
				BufferedImage.TYPE_INT_RGB);
		Graphics2D g2d = bi.createGraphics();
		AffineTransform at = AffineTransform.getScaleInstance(width, height2);
		g2d.setTransform(at);
		this.paintComponent(g2d);
		return bi;
		/*
		 * BufferedImage scaledImage = new BufferedImage( 2. width, height,
		 * BufferedImage.TYPE_INT_RGB); 3.Graphics2D graphics2D =
		 * scaledImage.createGraphics(); 4.AffineTransform xform =
		 * AffineTransform.getScaleInstance(scale, scale);
		 * 5.graphics2D.setRenderingHint(RenderingHints.KEY_INTERPOLATION, 6.
		 * RenderingHints.VALUE_INTERPOLATION_BICUBIC);
		 * 7.graphics2D.drawImage(image, xform, null); 8.graphics2D.dispose();
		 */
	}

	public boolean inPlotArea(Point p) {
		if (p.x < PAD || p.x > (getWidth() - PAD) || p.y < PAD
				|| p.x > (getHeight() - PAD)) {
			return false;
		} else {
			return true;
		}
	}

	private void labelTickMarks(HashMap<Object, Integer> map) {
		
		/*** label x and y axis tick marks ***/
		Font font = g2.getFont();
		FontRenderContext render = g2.getFontRenderContext();
		LineMetrics metrics = font.getLineMetrics("TickMarkLabels", render);
		String s = "";

		float textHt = metrics.getHeight();
		float textWidth = 0;
		float x = 0;
		float y = 0;
		
//		if (lines != null && lines.size() > 0) {
//			if (lines.get(0).getPt(0, pt).getStringVal() != null) {
//				
//			}
//		}

		double numToWrite = 0;

		/** show two decimal places **/
		DecimalFormat df = new DecimalFormat("#.##");
		
		if (barGraph) {
			Object[] keys = map.keySet().toArray();
			Object key = null;
			int index = 0;
			for (int i=0; i<keys.length; i++) {
				key = keys[i];
				index = map.get(key);
				s = key.toString();

				textWidth = (float) font.getStringBounds(s, render).getWidth();
				x = (float) (PAD + (xInc/2) + index * xInc - (textWidth / 2));
				y = (float) (y0 + ((PAD + textHt) / 3.5) - metrics.getDescent());
				try {
					double d = Double.parseDouble(s);
					if (d == Math.floor(d)) {
						s = String.valueOf((int) d);
					}
				} catch (NumberFormatException nfe) {}
				g2.drawString(s, x, y);
			}
		} else {

			/** x axis tick mark labels **/
			for (int i = 0; i < xdivisor; i++) {
	
				if (xlog > 0) {
					numToWrite = minX * Math.pow(xlog, i);
				} else {
					numToWrite = minX + (i * xInt);
				}
	
				numToWrite = new Double(df.format(numToWrite)).doubleValue();
				if (pv.getFormatAsDate()) {
					s = MainModel.convertEpochToDate((long) numToWrite);
				} else {
					if (numToWrite > 99999) {
						DecimalFormat df2 = new DecimalFormat("###");
						s = String.valueOf(df2.format(numToWrite));
					} else if (Math.floor(numToWrite) == numToWrite) {
						s = String.valueOf((int)numToWrite);
					} else {
						s = String.valueOf(numToWrite);
					}
				}
				textWidth = (float) font.getStringBounds(s, render).getWidth();
				x = (float) (PAD + i * xInc - (textWidth / 2));
				y = (float) (y0 + ((PAD + textHt) / 3.5) - metrics.getDescent());
				g2.drawString(s, x, y);
	
			}
		}

		/** y axis tick mark labels **/
		for (int i = 0; i < ydivisor; i++) {

			if (ylog > 0) {
				numToWrite = minY * Math.pow(ylog, i);
			} else {
				numToWrite = minY + (i * yInt);
			}

			numToWrite = new Double(df.format(numToWrite)).doubleValue();
			if (numToWrite > 99999) {
				DecimalFormat df2 = new DecimalFormat("###");
				s = String.valueOf(df2.format(numToWrite));
			} else if (Math.floor(numToWrite) == numToWrite) {
				s = String.valueOf((int)numToWrite);
			} else {
				s = String.valueOf(numToWrite);
			}
			textWidth = (float) font.getStringBounds(s, render).getWidth();
			x = (float) ((PAD - textWidth) * .75);
			y = (float) (y0 - i * yInc + metrics.getDescent());
			g2.drawString(s, x, y);

		}
	}

	/**
	 * Calculates the log of a number with a given base
	 * 
	 * @param num
	 *            number to calculate log of
	 * @param base
	 *            base of log
	 * @return calculated log
	 */
	private double log(double num, double base) {
		return Math.log(num) / Math.log(base);
	}

	@Override
	/**
	 * Draws the graph
	 * 
	 * @param		graphics object to draw with
	 */
	protected void paintComponent(Graphics g) {
		
		//System.out.println("without dims Max Y: " + maxY);

		/*** paint base features of component (i.e. background, etc.) ***/
		g2 = (Graphics2D) g;
		super.paintComponent(g2);
		
		g2.setStroke(new BasicStroke(pv.getLineThickness()));
		HALF_POINT_WIDTH = (int) (pv.getPointSize() / 2.0);
		Font f = g2.getFont();
		f = new Font(f.getName(), f.getStyle(), pv.getFontSize());
		g2.setFont(f);

		width = getWidth();
		height = getHeight();

		/*** set rendering hints ***/
		setRenderingHints();

		barGraph = pv.getBarGraph();
		if (lines != null && lines.size() > 0) {
			if (lines.get(0).size(pt) > 0 && lines.get(0).getPt(0, pt).getStringVal() != null) {
				pv.setBarGraph(true);
				barGraph = true;
			}
		}
		
		if (barGraph && ylog > 0 && minY <= 0) {
			minY = 1;
			if (maxY < 1) {
				maxY = ylog;
			}
		}

		/*** initialize graph plotting info ***/
		setXDivisor();
		setYDivisor();
		setXInt();
		setYInt();
		setLegendPosition(pv.getLegendPositionIndex());
		
		/*** number of pixels between tick marks for x and y axis ***/
		xInc = (width - 2 * PAD) / (xdivisor - 1);
		yInc = (height - 2 * PAD) / (ydivisor - 1);

		/*** initialize origin of the plot ***/
		x0 = PAD;
		y0 = height - PAD;
		
		if (barGraph) {
			
			HashMap<Object, Integer> map = new HashMap<Object, Integer>();
			Line line = null;
			LinePoint point = null;
			int temp = 0;
			Integer val = null;
			Object key = null;
			for (int i=0; i<lines.size(); i++) {
				line = lines.get(i);
				for (int j=0; j<line.size(pt); j++) {
					point = line.getPt(j, pt);
					if (point.getStringVal() == null) {
						key = point.getXVal();
					} else {
						key = point.getStringVal();
					}
					val = map.get(key);
					if (val == null) {
						map.put(key, temp);
						temp++;
					} else {
						map.put(key, val);
					}
				}
			}
			if (lines == null || lines.size() <= 0) {
				return;
			}
			
			xdivisor = map.size() + 1;
			setXInt();
			xInc = (width - 2 * PAD) / (xdivisor - 1);
			
			g2.setPaint(Color.black);
			drawTickMarks();
			labelTickMarks(map);
			drawTitle();
			drawXLabel();
			drawYLabel();
			drawAxes();

			Graphics2D tempG = (Graphics2D) g2.create();
			tempG.setClip(PAD, PAD, (int) width-2*(PAD), (int) height-2*(PAD));
			float alpha = (float) .75;
			tempG.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER,
					alpha));
			double currY = 0;
			double currHt = 0;
			double currX = 0;
			int index = 0;
			double barWidth = xInc / ((double) lines.size() + 1);
			double midX = 0;
			
			if (barWidth < 1) {
				mv.appendToHistory("Too many lines to render bars properly.  Please modify query appopriately.\n");
				return;
			}
			
			for (int i=0; i<lines.size(); i++) {
				line = lines.get(i);
				tempG.setColor(line.getColor());
				for (int j=0; j<line.size(pt); j++) {
			 		point = line.getPt(j, pt);
					if (point.getStringVal() == null) {
						key = point.getXVal();
					} else {
						key = point.getStringVal();
					}
					
					/** calculate position to draw current point **/
					index = map.get(key);
					currX = (x0+(index*xInc)+(i*barWidth)) + (barWidth/2.0);
					
					if (ylog > 0) {
						double lower = Math.floor(log(point.getYVal(), ylog));
						double base = log(minY, ylog);
						double lowerPixel = ((lower - base) * yInc);
						currY = y0
								- ((log(point.getYVal(), ylog) - lower) * yInc + lowerPixel);
					} else {
						currHt = (point.getYVal() - minY) * ((yInc * (ydivisor - 1)) / (maxY - minY));
						currY = y0 - currHt;
					}
					
					tempG.fill(new Rectangle2D.Double(currX, currY, barWidth, currHt));
					point.setPosition(currX, currY, barWidth, currHt);
					midX = currX + barWidth/2.0;
					drawErrorBar(tempG, line, j, midX, currY);
				}
			}
			tempG.dispose();
			drawLegend();
			
		} else {
			
			/**** draw the graph ****/
			g2.setPaint(Color.black);
			drawTickMarks();
			labelTickMarks(null);
			drawTitle();
			drawXLabel();
			drawYLabel();
			drawAxes();
			Graphics2D tempG = (Graphics2D) g2.create();
			tempG.setClip(PAD, PAD, (int) width-2*(PAD), (int) height-2*(PAD));
			drawPoints(tempG);
			tempG.dispose();
			drawLegend();
		}
		
	}

	public void paintGraph(Graphics g) {
		paintComponent(g);
	}

	/**
	 * Pulls relevant parameters from parameter panel and initializes fields for
	 * graph
	 */
	public void populateGraphElements() {

		/* determine aggregation type */
		String avgMaxMin = pv.getAvgMaxMinAll();
		if (avgMaxMin.equalsIgnoreCase("average")) {
			pt = PointType.AVG;
		} else if (avgMaxMin.equalsIgnoreCase("maximum")) {
			pt = PointType.MAX;
		} else if (avgMaxMin.equalsIgnoreCase("minimum")) {
			pt = PointType.MIN;
		} else {
			pt = PointType.ALL;
		}

		/* determine error bar type */
		String errorBar = pv.getErrorBars();
		if (errorBar.equalsIgnoreCase("standard deviation")) {
			ebt = ErrorBarType.STDDEV;
		} else if (errorBar.equalsIgnoreCase("range")) {
			ebt = ErrorBarType.RANGE;
		} else {
			ebt = ErrorBarType.NONE;
		}
		
		/* get graph title */
		title = mv.getGraphTitle();
		
		/* get x axis label */
		xLabel = mv.getXLabel();
		if (xLabel == null || xLabel.equals("")) {
			xLabel = pv.getXAxis();
		}
		PersistentDefaultLabels pdl = null;
		try {
			Database db = mv.getDb();
			if (db != null) {
				pdl = new PersistentDefaultLabels();
				xLabel = pdl.checkString(db.getTableName(), xLabel);
			}
		} catch (IOException ioe) {
			mv.appendToHistory("Unable to load default labels: "+ioe.getMessage()+"\n");
		}
		
		/* get y axis label */
		yLabel = mv.getYLabel();
		if (yLabel == null || yLabel.equals("")) {
			yLabel = pv.getYAxis();
		}
		try {
			Database db = mv.getDb();
			if (db != null) {
				yLabel = pdl.checkString(db.getTableName(), yLabel);
			}
		} catch (IOException ioe) {
			mv.appendToHistory("Unable to load default labels: "+ioe.getMessage()+"\n");
		}
		
		/* get/set x and y axis min and max */
		try {
			minX = Double.parseDouble(mv.getXMin());
		} catch (NumberFormatException nfe) {
			setMinX();
		}
		try {
			maxX = Double.parseDouble(mv.getXMax());
		} catch (NumberFormatException nfe) {
			setMaxX();
		}
		try {
			minY = Double.parseDouble(mv.getYMin());
		} catch (NumberFormatException nfe) {
			// setMinY();
			minY = 0;
		}
		try {
			maxY = Double.parseDouble(mv.getYMax());
		} catch (NumberFormatException nfe) {
			setMaxY();
		}

		/* get x axis log base */
		try {
			xlog = Integer.parseInt(pv.getXLog());
			if (xlog <= 1) {
				xlog = 0;
				mv.appendToHistory("Y axis log base must be an integer greater than 1.\n");
			}
		} catch (NumberFormatException nfe) {
			xlog = 0;
		}
		
		/* get y axis log base */
		try {
			ylog = Integer.parseInt(pv.getYLog());
			if (ylog <= 1) {
				ylog = 0;
				mv.appendToHistory("Y axis log base must be an integer greater than 1.\n");
			}
		} catch (NumberFormatException nfe) {
			ylog = 0;
		}

		if (ylog > 0) {
			minY = Math.pow(ylog, Math.floor(log(minY, ylog)));
			maxY = Math.pow(ylog, Math.ceil(log(maxY, ylog)));
		}
		if (xlog > 0) {
			minX = Math.pow(xlog, Math.floor(log(minX, xlog)));
			maxX = Math.pow(xlog, Math.ceil(log(maxX, xlog)));
		}
		
		graphElementSanityChecks();
	}
	
	private void graphElementSanityChecks() {
		if (maxX <= minY) {
			maxX = Math.abs(10 * minY);
		}
		if (maxY <= minY) {
			maxY = Math.abs(10 * minY);
		}
		
		/* check for bargraph compatibilities */
		if (pt == PointType.ALL) {
			barGraph = false;
			mv.appendToHistory("\"All\" setting is not compatible with bar graphs.\n");
		}
		
		if (barGraph) {
			if (xlog > 0) {
				mv.appendToHistory("X log base is not compatible with bargraph mode.\n");
			} else if (ylog > 0) {
				double l = log(minY, ylog);
				if (l != (int) l) {
					//TODO: figure this out
					/*
					 *  
					 */
				}
			}
		}
	}

	public void removeLine(int i) {
		lines.remove(i);
	}

	/**
	 * Removes a point from a line
	 * 
	 * @param line
	 *            index of line from which to remove point
	 * @param point
	 *            index of point to remove
	 */
	public void removePoint(int line, int point) {
		if (pt == PointType.ALL) {
			lines.get(line).removePointFromAll(point);
		} else {
			lines.get(line).removePoint(point);
		}
	}

	public void saveImage() {
		BufferedImage bimage = new BufferedImage(getWidth(), getHeight(),
				BufferedImage.TYPE_INT_RGB);
		Graphics2D g2d = bimage.createGraphics();
		this.paintComponent(g2d);
		try {
			ImageIO.write(bimage, "jpg", new File("resources/graph.jpg"));
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public void setCompares(int lineIndex, String s) {
		lines.get(lineIndex).setCompares(s);
	}

	public void setErrorBars(int index) {
		switch (index) {
		case 0:
			ebt = ErrorBarType.STDDEV;
			break;
		case 1:
			ebt = ErrorBarType.RANGE;
			break;
		default:
			ebt = ErrorBarType.NONE;
			break;
		}
	}

	public void setLegendPosition(int selectedIndex) {
		switch (selectedIndex) {
		case 0:
			legendPosition = LegendLocation.TOP_LEFT;
			break;
		case 1:
			legendPosition = LegendLocation.TOP_RIGHT;
			break;
		case 2:
			legendPosition = LegendLocation.BOTTOM_LEFT;
			break;
		case 3:
			legendPosition = LegendLocation.BOTTOM_RIGHT;
			break;
		/*case 4:
			System.out.println("setting position to place");
			legendPosition = LegendLocation.PLACE;
			break;*/
		case 4:
			legendPosition = LegendLocation.NONE;
			break;
		default:
			legendPosition = LegendLocation.TOP_RIGHT;
			break;
		}
	}

	public void setLegendXBase(int x) {
		xbase = x;
		setLegend = true;
	}

	public void setLegendYBase(int y) {
		ybase = y;
		setLegend = true;
	}

	// TODO: combine setmaxminxy methods
	/**
	 * Finds the maximum x value
	 */
	private void setMaxX() {
		if (lines.size() == 0) {
			return;
		}
		double oldMaxX = maxX;
		boolean allEmpty = true;
		maxX = Double.MIN_VALUE;
		double x = 0;
		for (Line line : lines) {
			for (int i = 0; i < line.size(pt); i++) {
				x = line.getPt(i, pt).getXVal();
				allEmpty = false;
				if (x > maxX) {
					maxX = x;
				}
			}
		}
		if (allEmpty) {
			maxX = oldMaxX;
		}
		maxX = Math.ceil(maxX);
	}
	
	public void setMaxX(double x) {
		maxX = x;
	}

	/**
	 * Finds the maximum y value
	 */
	private void setMaxY() {
		if (lines.size() == 0) {
			return;
		}
		double oldMaxY = maxY;
		boolean allEmpty = true;
		maxY = Double.MIN_VALUE;
		double y = 0;
		for (Line line : lines) {
			for (int i = 0; i < line.size(pt); i++) {
				y = line.getPt(i, pt).getYVal();
				if (pt == PointType.AVG) {
					if (ebt == ErrorBarType.STDDEV) {
						y += line.getStdDev(i);
					} else if (ebt == ErrorBarType.RANGE) {
						y += line.getMax(i);
					}
				}
				allEmpty = false;
				if (y > maxY) {
					maxY = y;
				}
			}
		}
		if (allEmpty) {
			maxY = oldMaxY;
		}
		maxY = Math.ceil(maxY);
	}

	public void setMaxY(double y) {
		maxY = y;
	}
	
	/**
	 * Finds the minimum x value
	 */
	private void setMinX() {
		if (lines.size() == 0) {
			return;
		}
		double oldMinX = minX;
		boolean allEmpty = true;
		minX = Double.MAX_VALUE;
		double x = 0;
		for (Line line : lines) {
			for (int i = 0; i < line.size(pt); i++) {
				x = line.getPt(i, pt).getXVal();
				allEmpty = false;
				if (x < minX) {
					minX = x;
				}
			}
		}
		if (allEmpty) {
			minX = oldMinX;
		}
		minX = Math.floor(minX);
	}

	public void setMinX(double x) {
		minX = x;
	}
	
	/**
	 * Finds the minimum y value
	 */
	/*
	 * private void setMinY() { if (lines.size() == 0) { return; } double
	 * oldMinY = minY; boolean allEmpty = true; minY = Double.MAX_VALUE; double
	 * y = 0; for (Line line : lines) { for (int i = 0; i < line.size(pt); i++)
	 * { y = line.getPt(i, pt).getYVal(); allEmpty = false; if (y < minY) { minY
	 * = y; } } } if (allEmpty) { minY = oldMinY; } minY = Math.floor(minY); }
	 */

	public void setMinY(double y) {
		minY = y;
	}
	
	/**
	 * Sets the lines to draw
	 * 
	 * @param line
	 *            lines to draw
	 */
	public void setPoints(ArrayList<Line> newLines) {
		
		lines = newLines;
		Line line = null;
		int maxSize = 0;
		boolean text = false;
		
		if (lines != null) {
			if (lines.size() > 0) {
				if (lines.get(0).getPt(0, PointType.ALL).getStringVal() != null) {
					text = true;
				}
			}
			/*** loop through the lines to draw ***/
			for (int i = 0; i < lines.size(); i++) {	
				line = lines.get(i);
				line.setColor(colors.get(i%colors.size()));
				line.setPointStyle(i % NUM_LINE_TYPES);
				if (maxSize < line.size(PointType.AVG)) {
					maxSize = line.size(PointType.AVG);
				}
			}
		}
		
		if (text) {
			xdivisor = maxSize;
		}
		
		sortLines();
		
		setLegend = false;
	}
	
	public void setPointType(int index) {
		switch (index) {
		case 0:
			pt = PointType.AVG;
			break;
		case 1:
			pt = PointType.MAX;
			break;
		case 2:
			pt = PointType.MIN;
			break;
		default:
			pt = PointType.ALL;
			break;
		}
	}

	private void setRenderingHints() {
//		if (forPDF) {
//			RenderingHints hints = new RenderingHints(
//					RenderingHints.KEY_ANTIALIASING,
//					RenderingHints.VALUE_ANTIALIAS_OFF);
//			g2.setRenderingHints(hints);
//			return;
//		} else {
			RenderingHints hints = new RenderingHints(
					RenderingHints.KEY_ANTIALIASING,
					RenderingHints.VALUE_ANTIALIAS_ON);
			hints.put(RenderingHints.KEY_ALPHA_INTERPOLATION,
					RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY);
			hints.put(RenderingHints.KEY_RENDERING,
					RenderingHints.VALUE_RENDER_QUALITY);
			hints.put(RenderingHints.KEY_RENDERING,
					RenderingHints.VALUE_RENDER_QUALITY);
			hints.put(RenderingHints.KEY_INTERPOLATION,
					RenderingHints.VALUE_INTERPOLATION_BICUBIC);
			g2.setRenderingHints(hints);
//		}
	}

	/**
	 * Sets the number of intervals on the x axis
	 */
	private void setXDivisor() {
		try {
			xdivisor = Integer.parseInt(pv.getXTickMarks());
			if (xdivisor < 2) {
				throw new NumberFormatException();
			}
		} catch (NumberFormatException nfe) {
			if (xlog > 0) {
				xdivisor = (int) Math.ceil(log(maxX, xlog) - log(minX, xlog));
			} else {
				xdivisor = 11;
			}
		}
	}
	
	/**
	 * Sets the interval between x axis tick marks
	 */
	private void setXInt() {
		xInt = (maxX - minX) / (xdivisor-1);
	}
	
	/**
	 * Sets the number of intervals on the y axis
	 */
	private void setYDivisor() {
		try {
			ydivisor = Integer.parseInt(pv.getYTickMarks());
			if (ydivisor < 2) {
				throw new NumberFormatException();
			}
		} catch (NumberFormatException nfe) {
			if (ylog > 0) {
				ydivisor = (int) Math.ceil(log(maxY, ylog) - log(minY, ylog));
			} else {
				ydivisor = 11;
			}
		}
	}
	
	/**
	 * Sets the interval between y axis tick marks
	 */
	private void setYInt() {
		yInt = (maxY - minY) / (ydivisor - 1);
	}
	
	private void sortLines() {
		
		if (lines == null) {
			return;
		}
		
		double[] yVals = new double[lines.size()];
		Line line = null;
		Line tempLine = null;
		
		if (pv.getLegendSortType().equals("Alphanumerically")) {
			//System.out.println("sorting by alpha");
			for (int i=0; i<lines.size(); i++) {
				for (int j=i+1; j<lines.size(); j++) {
					tempLine = lines.get(j);
					if (lines.get(i).getCompares().compareTo(tempLine.getCompares()) > 0) {
						lines.set(j, lines.get(i));
						lines.set(i, tempLine);
					}
				}
			}
		} else if (pv.getLegendSortType().equals("First Y Value")) {
			//System.out.println("sorting by first y");
			for (int i=0; i<lines.size(); i++) {
				line = lines.get(i);
				if (line.size(pt) > 0) {
					if (pt == PointType.ALL) {
						yVals[i] = line.getMax(0);
					} else {
						yVals[i] = line.getYVal(0, pt);
					}
				}
			}
			
			for (int i=0; i<lines.size(); i++) {
				for (int j=i+1; j<lines.size(); j++) {
					if (yVals[i] < yVals[j]) {
						tempLine = lines.get(j);
						lines.set(j, lines.get(i));
						lines.set(i, tempLine);
					}
				}
			}
			
		} else if (pv.getLegendSortType().equals("Last Y Value")) {
			//System.out.println("sorting by last y");
			for (int i=0; i<lines.size(); i++) {
				line = lines.get(i);
				if (pt == PointType.ALL) {
					yVals[i] = line.getMax(line.size(PointType.AVG)-1);
				} else {
					yVals[i] = line.getYVal(line.size(pt)-1, pt);
				}
			}
			
			for (int i=0; i<lines.size(); i++) {
				for (int j=i+1; j<lines.size(); j++) {
					if (yVals[i] < yVals[j]) {
						tempLine = lines.get(j);
						lines.set(j, lines.get(i));
						lines.set(i, tempLine);
					}
				}
			}
		} else {
			//System.out.println("sorting by avg y");
			for (int i=0; i<lines.size(); i++) {
				line = lines.get(i);
				for (int j=0; j<line.size(pt); j++) {
					yVals[i] += line.getYVal(j, pt);
				}
				if (line.size(pt) > 0) {
					yVals[i] /= line.size(pt);
				}
				//System.out.println(yVals[i]);
			}
			
			for (int i=0; i<lines.size(); i++) {
				for (int j=i+1; j<lines.size(); j++) {
					if (yVals[i] < yVals[j]) {
						tempLine = lines.get(j);
						lines.set(j, lines.get(i));
						lines.set(i, tempLine);
						//System.out.println("swapped lines");
					}
				}
			}
		}
	}
}
