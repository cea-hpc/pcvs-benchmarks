package gui.views;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.Vector;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.WindowConstants;
import javax.swing.table.DefaultTableModel;

public class DescribeTableView extends JFrame {

	private static final long serialVersionUID = 1L;
	
	private JPanel mainPanel;
	private JScrollPane pane;
	private JTable table;

	public DescribeTableView(String tableName, ResultSet result) throws SQLException {
		
		mainPanel = new JPanel();
		setContentPane(mainPanel);
		mainPanel.setPreferredSize(new Dimension(500, 400));

		GridBagLayout gridbag = new GridBagLayout();
		setLayout(gridbag);
		GridBagConstraints c = new GridBagConstraints();
		
		DefaultTableModel dtm = new DefaultTableModel();
		ResultSetMetaData md = result.getMetaData();
		int numCols = md.getColumnCount();
		for (int i=0; i<numCols; i++) {
			dtm.addColumn(md.getColumnLabel(i+1));
		}
		table = new JTable(dtm);

		Vector<String> row = null;
		while (result.next()) {
			row = new Vector<String>(numCols);
			for (int i=0; i<numCols; i++) {
				row.addElement(result.getString(i+1));
				if (result.wasNull()) {
					row.set(i, "NULL");
				}
			}
			dtm.addRow(row);
		}
		
		pane = new JScrollPane(table);
		
		c.anchor = GridBagConstraints.PAGE_START;
		c.fill = GridBagConstraints.BOTH;
		c.weightx = 1;
		c.weighty = 1;
		c.gridx = 0;
		c.gridy = 0;
		mainPanel.add(pane, c);

		setTitle("Describe Table: " + tableName);
		setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		pack();
		setLocationRelativeTo(null);
		setVisible(true);
		
	}
	
}
