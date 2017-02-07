package gui.models;

/*
 * import gui.Line.PointType;
 * 
 * import java.awt.Frame; import java.awt.Point; import
 * java.awt.event.MouseEvent; import java.util.ArrayList;
 * 
 * import javax.swing.JLabel; import javax.swing.JWindow; import
 * javax.swing.SwingUtilities; import javax.swing.UIManager; import
 * javax.swing.event.MouseInputAdapter;
 * 
 * //TODO: site source of help public class PointToolTip extends
 * MouseInputAdapter {
 * 
 * private GraphView gv; private JWindow toolTip; private JLabel label;
 * 
 * public PointToolTip(GraphView g) { gv = g; init(); }
 * 
 * private void init() { label = new JLabel(" "); label.setOpaque(true);
 * label.setBackground(UIManager.getColor("ToolTip.background")); toolTip = new
 * JWindow(new Frame()); toolTip.getContentPane().add(label); }
 * 
 * public void mousePressed(MouseEvent e) { if (e.getButton() !=
 * MouseEvent.BUTTON1) { //TODO: not sure if this is the best check; button2
 * didn't correspond to right click PointType pt = gv.getPointType(); Point p =
 * e.getPoint(); ArrayList<Line> lines = gv.getLines(); Line line = null;
 * LinePoint point = null; for (int i=lines.size()-1; i>=0; i--) { line =
 * lines.get(i); for (int j=line.size(gv.getPointType())-1; j>=0; j--) { point =
 * line.getPt(j, pt); if (point.contains(p)) { System.out.println("COMPARE: " +
 * line.getCompares().get(0)); gv.removePoint(i,j);
 * gv.paintComponent(gv.getGraphics()); return; } } } } }
 * 
 * @Override public void mouseMoved(MouseEvent e) { PointType pt =
 * gv.getPointType(); Point ePoint = e.getPoint(); ArrayList<Line> lines =
 * gv.getLines(); Line line = null; LinePoint point = null; boolean hovering =
 * false; for (int i = 0; i < lines.size(); i++) { line = lines.get(i); for (int
 * j = 0; j < line.size(gv.getPointType()); j++) { point = line.getPt(j, pt);
 * //System.out.println(ePoint.x + "\t" + point.getXPos()); if
 * (point.contains(ePoint)) { //System.out.println("in for loop"); if
 * (!toolTip.isVisible()) { String s = "<html>x = " + point.getXVal() +
 * "<br>y = " + point.getYVal(); label.setText(s); toolTip.pack();
 * toolTip.setVisible(true); } hovering = true;
 * SwingUtilities.convertPointToScreen(ePoint, gv); toolTip.setLocation(ePoint.x
 * + 5, ePoint.y - toolTip.getHeight() - 5); break; } } } if (!hovering &&
 * toolTip.isVisible()) { toolTip.setVisible(false); } }
 * 
 * }
 */