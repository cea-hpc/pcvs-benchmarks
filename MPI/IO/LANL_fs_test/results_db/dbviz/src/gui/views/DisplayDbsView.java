package gui.views;

import gui.models.Database;
import gui.models.MainModel;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.LinkedList;

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

public class DisplayDbsView extends JFrame implements TableModelListener {

	private static final long serialVersionUID = 1L;
	
	private JPanel mainPanel;
	private JScrollPane pane;
	private JTable table;
	
	private JButton addTableButton;
	private JButton removeTableButton;
	private JButton okButton;
	private JButton cancelButton;
	
	private MainModel model;
	
	public DisplayDbsView(MainModel mm) {
		
		model = mm;
		
		mainPanel = new JPanel();
		setContentPane(mainPanel);
		
		GridBagLayout gridbag = new GridBagLayout();
		mainPanel.setLayout(gridbag);
		GridBagConstraints c = new GridBagConstraints();
		
		TableModel dtm = new TableModel();
		dtm.addColumn("Host Name");
		dtm.addColumn("Database Name");
		dtm.addColumn("Table Name");
		dtm.addColumn("User Name");
		dtm.addColumn("Password");		//TODO: include this or not???
		dtm.addColumn("Current Table");
		table = new JTable(dtm);
		
		Object[] data = null;
		LinkedList<Database> ll = model.getTables();		//TODO: change this so it returns a arraylist instead of a linkedlist
		Database currdb = model.getDb();
		if (ll != null) {
			Database db = null;
			for (int i=0; i<ll.size(); i++) {
				data = new Object[6];
				db = ll.get(i);
				data[0] = db.getHostName();
				data[1] = db.getDbName();
				data[2] = db.getTableName();
				data[3] = db.getUserName();
				data[4] = db.getPassword();
				if (db.getTableName().equals(currdb.getTableName())) {
					data[5] = new Boolean(true);
				} else {
					data[5] = new Boolean(false);
				}
				dtm.addRow(data);
			}
		}
		
		table.getModel().addTableModelListener(this);
		pane = new JScrollPane(table);
		
		addTableButton = new JButton("Add Table");
		addTableButton.addActionListener(new AddTableListener());
		removeTableButton = new JButton("Remove Table");
		removeTableButton.addActionListener(new RemoveTableListener());
		okButton = new JButton("OK");
		//okButton.addActionListener(new OkListener());
		cancelButton = new JButton("Cancel");
		cancelButton.addActionListener(new CancelListener());
		
		//layout components
		c.fill = GridBagConstraints.BOTH;
		c.weightx = 1;
		c.weighty = 1;
		c.gridwidth = 4;
		mainPanel.add(pane, c);
		
		c.fill = GridBagConstraints.NONE;
		c.weightx = .25;
		c.weighty = 0;
		c.gridy = 1;
		c.gridx = 0;
		c.gridwidth = 1;
		mainPanel.add(addTableButton, c);
		c.gridx = 1;
		mainPanel.add(removeTableButton, c);
		c.gridx = 2;
		mainPanel.add(okButton, c);
		c.gridx = 3;
		mainPanel.add(cancelButton, c);
		
		pack();
		setLocationRelativeTo(null);
		setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		setVisible(true);
		
	}
	
	public void tableChanged(TableModelEvent e) {
		int row = e.getFirstRow();
		int col = e.getColumn();
		TableModel tm = (TableModel) e.getSource();
		if (col == 5) {
			boolean b = (Boolean) tm.getValueAt(row, col);
			if (b) {
				for (int i=0; i<tm.getRowCount(); i++) {
					if (i != row) {
						tm.setValueAt(new Boolean(false), i, 5);
					}
				}
			}
		}
	}
	
	public class TableModel extends DefaultTableModel {
		private static final long serialVersionUID = 1L;
		@SuppressWarnings("unchecked")
		public Class getColumnClass (int colIndex) {
			if (colIndex == 5) {
				return Boolean.class;
			} else {
				return String.class;
			}
		}
	}
	
	public class AddTableListener implements ActionListener {
		public void actionPerformed(ActionEvent ae) {
			DefaultTableModel model = (DefaultTableModel) table.getModel();
			for (int i=0; i<model.getRowCount(); i++) {
				model.setValueAt(new Boolean(false), i, 5);
			}
			Object[] row = new Object[6];
			row[0] = "New Entry";
			row[1] = "New Entry";
			row[2] = "New Entry";
			row[3] = "New Entry";
			row[4] = "New Entry";
			row[5] = new Boolean(true);
			model.addRow(row);
		}
	}
	
	public class RemoveTableListener implements ActionListener {
		public void actionPerformed(ActionEvent ae) {
			int row = table.getSelectedRow();
			if (row < 0) {
				return;
			}
			TableModel model = (TableModel) table.getModel();
			if ((Boolean) model.getValueAt(row, 5) && model.getRowCount() > 1) {
				if (row == 0) {
					model.setValueAt(new Boolean(true), 1, 5);
				} else {
					model.setValueAt(new Boolean(true), 0, 5);
				}
			}
			model.removeRow(row);
		}
	}
	
	public int saveAndClose() {
		
		LinkedList<Database> ll = new LinkedList<Database>();
		DefaultTableModel dtm = (DefaultTableModel) table.getModel();
		Database db = null;
		int index = -1;
		
		for (int i=0; i<dtm.getRowCount(); i++) {
			db = new Database();
			db.setHostname((String) dtm.getValueAt(i,0));
			db.setDatabasename((String) dtm.getValueAt(i,1));
			db.setTablename((String) dtm.getValueAt(i,2));
			db.setUsername((String) dtm.getValueAt(i,3));
			db.setPassword((String) dtm.getValueAt(i,4));

			if ((Boolean) dtm.getValueAt(i, 5)) {
				index = i;
			}
			ll.add(db);
		}
		if (index >= 0) {
			model.setTables(ll);
//			if (model.setDb(ll.get(index))) {
//				view.setShouldLoadTopUsed(true);
//				view.setComboFields(model.getTableColumnNamesNum(), model.getTableColumnNamesAll());
//				view.appendToHistory("Connected to table " + model.getDbTableName() + ".\n");
//			} else {
//				view.appendToHistory("Could not connect to table " + ll.get(index).getTableName() +
//				". Please try connecting to another table.");
//			}
		}
		Window window = SwingUtilities.getWindowAncestor(mainPanel);
		window.dispose();
		
		return index;
	}
	
	public class CancelListener implements ActionListener {
		public void actionPerformed(ActionEvent ae) {
			Window window = SwingUtilities.getWindowAncestor(mainPanel);
			window.dispose();
		}
	}

	public void addOkListener(ActionListener al) {
		okButton.addActionListener(al);
	}

}
