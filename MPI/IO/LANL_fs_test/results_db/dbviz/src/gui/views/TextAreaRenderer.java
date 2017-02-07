package gui.views;

import javax.swing.*;
import javax.swing.table.*;
import java.awt.*;
import java.util.*;
 
/**
  * This class is complements of Heinz Kabutz and was obtained from 
  * http://www.javaspecialists.eu/archive/Issue106.html
  * 
  */

public class TextAreaRenderer extends JTextArea implements TableCellRenderer {
	
	private static final long serialVersionUID = 1L;
	private final DefaultTableCellRenderer adaptee = new DefaultTableCellRenderer();
	/** map from table to map of rows to map of column heights */
	@SuppressWarnings("unchecked")
	private final Map cellSizes = new HashMap();

	public TextAreaRenderer() {
		setLineWrap(true);
		setWrapStyleWord(true);
	}

	public Component getTableCellRendererComponent(
			//
			JTable table, Object obj, boolean isSelected, boolean hasFocus,
			int row, int column) {
		// set the colours, etc. using the standard for that platform
		adaptee.getTableCellRendererComponent(table, obj, isSelected, hasFocus,
				row, column);
		setForeground(adaptee.getForeground());
		setBackground(adaptee.getBackground());
		setBorder(adaptee.getBorder());
		setFont(adaptee.getFont());
		setText(adaptee.getText());

		// This line was very important to get it working with JDK1.4
		TableColumnModel columnModel = table.getColumnModel();
		setSize(columnModel.getColumn(column).getWidth(), 100000);
		int height_wanted = (int) getPreferredSize().getHeight();
		addSize(table, row, column, height_wanted);
		height_wanted = findTotalMaximumRowSize(table, row);
		if (height_wanted != table.getRowHeight(row)) {
			table.setRowHeight(row, height_wanted);
		}
		return this;
	}

	@SuppressWarnings("unchecked")
	private void addSize(JTable table, int row, int column, int height) {
		Map rows = (Map) cellSizes.get(table);
		if (rows == null) {
			cellSizes.put(table, rows = new HashMap());
		}
		Map rowheights = (Map) rows.get(new Integer(row));
		if (rowheights == null) {
			rows.put(new Integer(row), rowheights = new HashMap());
		}
		rowheights.put(new Integer(column), new Integer(height));
	}

	/**
	 * Look through all columns and get the renderer. If it is also a
	 * TextAreaRenderer, we look at the maximum height in its hash table for
	 * this row.
	 */
	@SuppressWarnings("unchecked")
	private int findTotalMaximumRowSize(JTable table, int row) {
		int maximum_height = 0;
		Enumeration columns = table.getColumnModel().getColumns();
		while (columns.hasMoreElements()) {
			TableColumn tc = (TableColumn) columns.nextElement();
			TableCellRenderer cellRenderer = tc.getCellRenderer();
			if (cellRenderer instanceof TextAreaRenderer) {
				TextAreaRenderer tar = (TextAreaRenderer) cellRenderer;
				maximum_height = Math.max(maximum_height, tar
						.findMaximumRowSize(table, row));
			}
		}
		return maximum_height;
	}

	@SuppressWarnings("unchecked")
	private int findMaximumRowSize(JTable table, int row) {
		Map rows = (Map) cellSizes.get(table);
		if (rows == null)
			return 0;
		Map rowheights = (Map) rows.get(new Integer(row));
		if (rowheights == null)
			return 0;
		int maximum_height = 0;
		for (Iterator it = rowheights.entrySet().iterator(); it.hasNext();) {
			Map.Entry entry = (Map.Entry) it.next();
			int cellHeight = ((Integer) entry.getValue()).intValue();
			maximum_height = Math.max(maximum_height, cellHeight);
		}
		return maximum_height;
	}
}
