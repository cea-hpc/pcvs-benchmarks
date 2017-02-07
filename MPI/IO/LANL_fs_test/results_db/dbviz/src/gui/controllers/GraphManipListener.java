package gui.controllers;

import gui.models.Line;
import gui.models.LinePoint;
import gui.models.MainModel;
import gui.models.Line.PointType;
import gui.views.GraphView;
import gui.views.TextAreaRenderer;

import java.awt.Color;
import java.awt.Component;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.geom.AffineTransform;
import java.util.ArrayList;

import javax.swing.JColorChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JWindow;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.WindowConstants;
import javax.swing.event.MouseInputAdapter;
import javax.swing.table.TableColumnModel;

/**
 * This class listens for input events that occur within the
 * graph area and responds to them appropriately.
 */
public class GraphManipListener extends MouseInputAdapter implements
		MouseListener, MouseMotionListener, MouseWheelListener {

	public class DescribePointView extends JFrame {
		private static final long serialVersionUID = 1L;
		private LinePoint point;
		private JPanel mainPanel;
		
		public DescribePointView(LinePoint pt, Point p) {
			
			point = pt;
			mainPanel = new JPanel();
			this.setContentPane(mainPanel);
			this.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
			setLocation(p.x+gv.getLocationOnScreen().x, p.y+gv.getLocationOnScreen().y);
			
			GridBagLayout gb = new GridBagLayout();
			mainPanel.setLayout(gb);
			GridBagConstraints c = new GridBagConstraints();
			
			c.fill = GridBagConstraints.BOTH;
			c.weightx = 1;
			c.weighty = 1;
			c.anchor = GridBagConstraints.PAGE_START;

			String[] columnNames = {"Field","Value"};
			String[][] data = gv.describePoint(point);
			if (data == null) {
				data = new String[1][1];
			}
			data = sortData(data);
			JTable table = new JTable(data, columnNames);
			TableColumnModel tcm = table.getColumnModel();
			TextAreaRenderer tar = new TextAreaRenderer();
			for (int i=0; i<tcm.getColumnCount(); i++) {
				tcm.getColumn(i).setCellRenderer(tar);
			}
			table.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
			JScrollPane scroll = new JScrollPane(table);
			//table.setFillsViewportHeight(true);
			mainPanel.add(scroll, c);
			pack();
			setVisible(true);
		}
		
		private String[][] sortData(String[][] array) {
			String[] temp = null;
			for (int i=0; i<array.length; i++) {
				for (int j=0; j<array.length; j++) {
					if (array[i][0].compareTo(array[j][0]) < 0) {
						temp = array[i];
						array[i] = array[j];
						array[j] = temp;
					}
				}
			}
			return array;
		}
	}
	public class PointPopup extends JPopupMenu {
		private static final long serialVersionUID = 1L;
		private JMenuItem rmPoint;
		private JMenuItem rmLine;
		private JMenuItem describePoint;
		private JMenuItem placeLegend;
		private JMenuItem changeColor;
		private JMenu changeStyle;
		private JMenu combineLine;
		private int purpose = 0;
		
		public PointPopup(int popupPurpose, final int lineIndex, final int pointIndex, final Point p) {
			
			purpose = popupPurpose;
			
			if (purpose == 1 || purpose == 2) {
				changeColor = new JMenuItem("Change Line Color");
				changeColor.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent ae) {
						Color c = JColorChooser.showDialog(gv, "Choose Line Color", gv.getLines().get(lineIndex).getColor());
						gv.getLines().get(lineIndex).setColor(c);
						gv.paintGraph(gv.getGraphics());
					}
				});
				add(changeColor);
				
				changeStyle = new JMenu("Change Point Type");
				changeStyle.add(new JMenuItem("Circle"));
				changeStyle.add(new JMenuItem("Square"));
				changeStyle.add(new JMenuItem("Diamond"));
				changeStyle.add(new JMenuItem("Upward Triangle"));
				changeStyle.add(new JMenuItem("Downward Triangle"));
				for (int i=0; i<changeStyle.getItemCount(); i++) {
					final int tempInt = i;
					changeStyle.getItem(i).addActionListener(new ActionListener() {
						public void actionPerformed(ActionEvent ae) {
							gv.getLines().get(lineIndex).setPointStyle(tempInt);
							gv.paintGraph(gv.getGraphics());
						}
					});
				}
				add(changeStyle);
				
				combineLine = new JMenu("Combine Lines");
				add(combineLine);
				Line line = null;
				ArrayList<Line> lines = gv.getLines();
				JMenuItem tempItem = null;
				for (int i=0; i<lines.size(); i++) {
					if (i!=lineIndex) {
						line = lines.get(i);
						tempItem = new JMenuItem(line.getCompares());
						final int index = i;
						final String oldCompare = line.getCompares();
						tempItem.addActionListener(new ActionListener() {
							public void actionPerformed(ActionEvent ae) {
								String s = (String) JOptionPane.showInputDialog(
										gv, "Please set a new legend value for the new line.",
										"New Legend Value", JOptionPane.PLAIN_MESSAGE, null, null,
										oldCompare);
								gv.setCompares(lineIndex, s);
								gv.combineLines(lineIndex, index);
								gv.paintGraph(gv.getGraphics());
							}
						});
						combineLine.add(tempItem);
					}
				}
			}
			
			//if (gv.getLegendType() == LegendLocation.PLACE) {
				placeLegend = new JMenuItem("Place Legend");
				placeLegend.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent ae) {
						gv.setLegendXBase(p.x);
						gv.setLegendYBase(p.y);
						gv.setLegendPosition(4);
						gv.paintGraph(gv.getGraphics());
					}
				});
				add(placeLegend);
			//}
			
			if (purpose == 1) {
				rmPoint = new JMenuItem("Remove Point");
				rmPoint.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent ae) {
						gv.removePoint(lineIndex, pointIndex);
						gv.paintGraph(gv.getGraphics());
					}
				});
				add(rmPoint);
				if (gv.getPointType() == PointType.ALL) {
					describePoint = new JMenuItem("Describe Point");
					describePoint.addActionListener(new ActionListener() {
						public void actionPerformed(ActionEvent ae) {
							LinePoint lp = gv.getPoint(lineIndex, pointIndex);
							new DescribePointView(lp, p);
						}
					});
					add(describePoint);
				}
			} else if (purpose == 2) {
				rmLine = new JMenuItem("Remove Line");
				rmLine.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent ae) {
						gv.removeLine(lineIndex);
						gv.paintGraph(gv.getGraphics());
					}
				});
				add(rmLine);
			}
			
			pack();
		}
		
	}
	private int zoomLevel = 0;
	private Point dragStart;
	private Point dragEnd;
	private AffineTransform coordTrans = new AffineTransform();
	private JWindow toolTip;

	private JLabel label;

	private GraphView gv;
	//private boolean forKey = false;

	public GraphManipListener(Component target) {

		gv = (GraphView) target;

		label = new JLabel(" ");
		label.setOpaque(true);
		label.setBackground(UIManager.getColor("ToolTip.background"));
		toolTip = new JWindow(new Frame());
		toolTip.getContentPane().add(label);
	}

	public AffineTransform getCoordTransform() {
		return coordTrans;
	}

	@Override
	public void mouseClicked(MouseEvent e) {
	}

	@Override
	public void mouseDragged(MouseEvent e) {
	}

	@Override
	public void mouseEntered(MouseEvent e) {
	}

	@Override
	public void mouseExited(MouseEvent e) {
	}
	
	@Override
	public void mouseMoved(MouseEvent e) {
		PointType pt = gv.getPointType();
		Point ePoint = e.getPoint();
		ArrayList<Line> lines = gv.getLines();
		Line line = null;
		LinePoint point = null;
		boolean hovering = false;
		boolean found = false;
		String content = "<html>";
		if (lines == null) {
			return;
		}
		for (int i = 0; i < lines.size(); i++) {
			line = lines.get(i);
			for (int j = 0; j < line.size(gv.getPointType()); j++) {
				point = line.getPt(j, pt);
				if (point.contains(ePoint)) {
					if (!toolTip.isVisible()) {
						found = true;
						if (gv.getPointType() == PointType.ALL) {
							ArrayList<String> keys = point.getKeys();
							for (int k=0; k<keys.size(); k++) {
								content += keys.get(k)+"<br>";
							}
						}
						String compares = line.getCompares();
						compares += "<br>";
						content += compares;
						if (gv.getFormatAsDate()) {
							content += "x = " + MainModel.convertEpochToDate((long) point.getXVal());
						} else if (point.getStringVal() != null) {
							content += "x = " + point.getStringVal();
						} else {
							content += "x = " + point.getXVal();
						}
						content += "<br>y = " + point.getYVal();
						if (gv.getPointType() != PointType.ALL) {
							content += "<br>std dev = " + line.getStdDev(j);
							content += "<br>max = " + line.getMax(j);
							content += "<br>min = " + line.getMin(j);
							content += "<br>num pts = " + line.sizeOf(j);
						}
						content += "<br><br>";
					}
				}
			}
		}
		if (found) {
			hovering = true;
			SwingUtilities.convertPointToScreen(ePoint, gv);
			toolTip.setLocation(ePoint.x + 5, ePoint.y
					- toolTip.getHeight() - 5);
			label.setText(content);
			toolTip.pack();
			toolTip.setVisible(true);
		}
		if (!hovering && toolTip.isVisible()) {
			toolTip.setVisible(false);
		}
	}
	
	@Override
	public void mousePressed(MouseEvent e) {
		dragStart = e.getPoint();
		dragEnd = null;

		if (e.getButton() != MouseEvent.BUTTON1) {
			PointType pt = gv.getPointType();
			Point p = e.getPoint();
			ArrayList<Line> lines = gv.getLines();
			Line line = null;
			LinePoint point = null;
			for (int i = lines.size() - 1; i >= 0; i--) {
				line = lines.get(i);
				for (int j = 0; j < line.size(gv.getPointType()); j++) {
					point = line.getPt(j, pt);
					if (point.contains(p)) {
						PointPopup pp = new PointPopup(1, i, j, dragStart);
						pp.show(e.getComponent(), e.getX(), e.getY());
						return;
					}
				}
			}
			ArrayList<LinePoint> legendPoints = gv.getLegendPoints();
			for (int i = 0; i < legendPoints.size(); i++) {
				if (legendPoints.get(i).contains(p)) {
					PointPopup pp = new PointPopup(2, i, 0, e.getPoint());
					pp.show(e.getComponent(), e.getX(), e.getY());
					return;
				}
			}
			//if (gv.getLegendType() == LegendLocation.PLACE) {
				PointPopup pp = new PointPopup(0, 0, 0, e.getPoint());
				pp.show(e.getComponent(), e.getX(), e.getY());
			//}
		}
	}

	@Override
	public void mouseReleased(MouseEvent e) {
		moveCamera(e);
	}

	public void mouseWheelMoved(MouseWheelEvent e) {
		zoomCamera(e);
	}

	private void moveCamera(MouseEvent e) {

		if (!gv.inPlotArea(e.getPoint())) {
			return;
		}

//		System.out.println("Moving camera");
		dragEnd = e.getPoint();

		double dx = -1 * (dragEnd.getX() - dragStart.getX());
		double dy = dragEnd.getY() - dragStart.getY();

		double w = gv.getPlotWidth();
		double h = gv.getPlotHeight();

		double maxX = gv.getMaxX();
		double minX = gv.getMinX();
		double maxY = gv.getMaxY();
		double minY = gv.getMinY();

		double xrange = Math.abs(maxX - minX);
		double yrange = Math.abs(maxY - minY);

//		System.out.println("x = " + xrange * (dx / w) + "\ty = " + yrange
//				* (dy / h));

		gv.setMaxX(maxX + xrange * (dx / w));
		gv.setMinX(minX + xrange * (dx / w));
		gv.setMaxY(maxY + yrange * (dy / h));
		gv.setMinY(minY + yrange * (dy / h));

		gv.paintGraph(gv.getGraphics());

	}

	public void setCoordTransform(AffineTransform coordT) {
		coordTrans = coordT;
	}

	private void zoomCamera(MouseWheelEvent e) {

		int wheelRot = e.getWheelRotation();
		double maxX = gv.getMaxX();
		double minX = gv.getMinX();
		double maxY = gv.getMaxY();
		double minY = gv.getMinY();
		double diffX = maxX - minX;
		double diffY = maxY - minY;
		if (wheelRot > 0) {			//zoom out
			zoomLevel++;
			gv.setMaxX(maxX + (.1 * diffX));
			gv.setMinX(minX - (.1 * diffX));
			gv.setMaxY(maxY + (.1 * diffY));
			gv.setMinY(minY - (.1 * diffY));
		} else {					//zoom in
			zoomLevel--;
			gv.setMaxX(maxX - (.1 * diffX));
			gv.setMinX(minX + (.1 * diffX));
			gv.setMaxY(maxY - (.1 * diffY));
			gv.setMinY(minY + (.1 * diffY));
		}
		gv.paintGraph(gv.getGraphics());
	}

}
