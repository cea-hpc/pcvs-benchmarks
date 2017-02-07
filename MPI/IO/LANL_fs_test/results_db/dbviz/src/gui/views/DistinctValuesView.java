package gui.views;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.util.ArrayList;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.WindowConstants;

public class DistinctValuesView extends JFrame {
	
	private static final long serialVersionUID = 1L;
	
	public DistinctValuesView(ArrayList<String> list, String selected) {
		
		JFrame valsWindow = new JFrame();
		JPanel mainPanel = new JPanel();
		
		valsWindow.setContentPane(mainPanel);
		mainPanel.setPreferredSize(new Dimension(200, 250));
		
		GridBagLayout gridbag = new GridBagLayout();
		valsWindow.setLayout(gridbag);
		GridBagConstraints c = new GridBagConstraints();
		c.anchor = GridBagConstraints.PAGE_START;
		c.fill = GridBagConstraints.BOTH;
		c.weightx = 1;
		c.weighty = 1;
		c.gridx = 0;
		c.gridy = 0;

		String[] colHeaders = {"Distinct Values for "+selected};
		String[][] data = new String[list.size()][1];
		for (int i = 0; i < list.size(); i++) {
			data[i][0] = list.get(i);
		}
		JTable table = new JTable(data, colHeaders);
		JScrollPane pane = new JScrollPane(table);
		
		mainPanel.add(pane, c);
		
		valsWindow.pack();
		valsWindow.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		valsWindow.setLocationRelativeTo(null);
		valsWindow.setVisible(true);
		
	}
}
