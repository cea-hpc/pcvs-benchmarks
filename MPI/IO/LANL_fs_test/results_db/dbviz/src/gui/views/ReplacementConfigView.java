package gui.views;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Vector;

import gui.models.Persistent;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;

public class ReplacementConfigView extends JFrame implements TableModelListener {
	
	private static final long serialVersionUID = 1L;
	private JPanel mainPanel;
	private JTable table;
	private JScrollPane scroll;
	private Persistent per;
	
	private JButton addButton;
	private JButton removeButton;
	private JButton okButton;
	private JButton cancelButton;
	
	public ReplacementConfigView(Persistent p, ArrayList<String> tableNames) {
		per = p;
		mainPanel = new JPanel();
		setContentPane(mainPanel);
		
		addButton = new JButton("Add Row");
		addButton.addActionListener(new AddListener());
		removeButton = new JButton("Remove Row");
		removeButton.addActionListener(new RemoveListener());
		okButton = new JButton("OK");
		okButton.addActionListener(new OkListener());
		cancelButton = new JButton("Cancel");
		cancelButton.addActionListener(new CancelListener());
		
		GridBagLayout gridbag = new GridBagLayout();
		mainPanel.setLayout(gridbag);
		GridBagConstraints c = new GridBagConstraints();
		
		DefaultTableModel dtm = new DefaultTableModel();
		dtm.addColumn("Table Name");
		dtm.addColumn("Find");
		dtm.addColumn("Replace With");
		table = new JTable(dtm);
		
		Vector<String> data = new Vector<String>();
		ArrayList<String> keys = per.getKeys();
		ArrayList<String> values = per.getValues();
		
		String s = null;
		boolean found = false;
		//int index = 0;
		for (int i=0; i<keys.size(); i++) {
			s = keys.get(i);
			for (int j=0; j<tableNames.size(); j++) {
				if (s.startsWith(tableNames.get(j))) {
					data.addElement(tableNames.get(j));
					data.addElement(s.substring(tableNames.get(j).length()));
					data.addElement(values.get(i));
					dtm.addRow(data);
					data = new Vector<String>();
					found = true;
					break;
					//index++;
				}
			}
			if (!found) {
				data.addElement(s);
				data.addElement("");
				data.addElement(values.get(i));
				dtm.addRow(data);
				data = new Vector<String>();
			}
			found = false;
		}
		//table = new JTable(data, columnHeaders);
		table.getModel().addTableModelListener(this);
		scroll = new JScrollPane(table);
		
		c.fill = GridBagConstraints.BOTH;
		c.weightx = 1;
		c.weighty = 1;
		c.gridx = 0;
		c.gridy = 0;
		c.gridwidth = 4;
		mainPanel.add(scroll, c);
		
		c.fill = GridBagConstraints.NONE;
		c.gridwidth = 1;
		c.weightx = .25;
		c.weighty = 0;
		c.gridy = 1;
		mainPanel.add(addButton, c);
		c.gridx = 1;
		mainPanel.add(removeButton, c);
		c.gridx++;
		mainPanel.add(okButton, c);
		c.gridx++;
		mainPanel.add(cancelButton, c);
		
		pack();
		setLocationRelativeTo(null);
		setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		setVisible(true);
		
	}

	public void tableChanged(TableModelEvent e) {
		TableModel model = (TableModel) e.getSource();
		ArrayList<String> keys = new ArrayList<String>();
		ArrayList<String> values = new ArrayList<String>();
		for (int i=0; i<model.getRowCount(); i++) {
			keys.add((String) model.getValueAt(i, 0) + model.getValueAt(i, 1));
			values.add((String) model.getValueAt(i, 2));
		}
		per.setKeys(keys);
		per.setValues(values);
	}
	
	public class AddListener implements ActionListener {
		public void actionPerformed(ActionEvent ae) {
			DefaultTableModel model = (DefaultTableModel) table.getModel();
			Vector<String> row = new Vector<String>();
			row.add("New Entry");
			row.add("New Entry");
			row.add("New Entry");
			model.addRow(row);
		}
	}
	
	public class RemoveListener implements ActionListener {
		public void actionPerformed(ActionEvent ae) {
			int row = table.getSelectedRow();
			if (row < 0) {
				return;
			}
			DefaultTableModel model = (DefaultTableModel) table.getModel();
			model.removeRow(row);
		}
	}
	
	public class OkListener implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			try {
				per.writeOutJson();
			} catch (IOException ioe) {
				//TODO: do something here
			}
			Window window = SwingUtilities.getWindowAncestor(mainPanel);
			window.dispose();
		}	
	}
	
	public class CancelListener implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			Window window = SwingUtilities.getWindowAncestor(mainPanel);
			window.dispose();
		}
	}
	
	
}
