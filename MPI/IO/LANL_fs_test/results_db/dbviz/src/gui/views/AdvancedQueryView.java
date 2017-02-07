package gui.views;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;

public class AdvancedQueryView extends JFrame {

	private static final long serialVersionUID = 1L;
	
	private JPanel mainPanel;
	private JScrollPane scroll;
	private JTextArea text;
	private JButton runButton;
	private JButton cancelButton;

	public AdvancedQueryView() {
		
		mainPanel = new JPanel();
		mainPanel.setPreferredSize(new Dimension(450, 350));
		setContentPane(mainPanel);
		
		text = new JTextArea();
		scroll = new JScrollPane();
		scroll.setViewportView(text);
		text.setEditable(true);
		text.append("SELECT experiment.num_hosts, mdtest.file_stat_mean \n");
		text.append("FROM experiment \n");
		text.append("INNER JOIN mdtest \nON experiment.test_type=mdtest.collective_creates \n");
		text.append("WHERE experiment.user like 'rrkroiss' \n");
		
		runButton = new JButton("Run");
		cancelButton = new JButton("Cancel");
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent ae) {
				closeWindow();
			}
		});
		
		GridBagLayout gridbag = new GridBagLayout();
		mainPanel.setLayout(gridbag);
		GridBagConstraints c = new GridBagConstraints();
		
		c.fill = GridBagConstraints.BOTH;
		c.gridwidth = 2;
		c.weightx = 1;
		c.weighty = 1;
		mainPanel.add(scroll, c);
		
		c.fill = GridBagConstraints.NONE;
		c.weightx = .5;
		c.weighty = 0;
		c.gridwidth = 1;
		c.gridy = 1;
		c.gridx = 0;
		mainPanel.add(runButton, c);
		c.gridx = 1; 
		mainPanel.add(cancelButton, c);
		
		pack();
		setLocationRelativeTo(null);
		setVisible(true);
		
	}
	
	public void closeWindow() {
		Window window = SwingUtilities.getWindowAncestor(mainPanel);
		window.dispose();
	}
	
	public void addRunListener(ActionListener al) {
		runButton.addActionListener(al);
	}
	
	public String getQuery() {
		return text.getText();
	}
	
}
