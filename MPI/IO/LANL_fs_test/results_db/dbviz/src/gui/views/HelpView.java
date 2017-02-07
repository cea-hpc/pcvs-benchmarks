package gui.views;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextPane;

public class HelpView extends JFrame {

	private static final long serialVersionUID = 1L;
	private JPanel mainPanel;
	//private JScrollPane scroll;
	private JTextPane text;

	public HelpView() {
		
		mainPanel = new JPanel();
		setContentPane(mainPanel);
		mainPanel.setPreferredSize(new Dimension(450, 550));

		text = new JTextPane();
		text.setEditable(false);
		
		
		GridBagLayout gridbag = new GridBagLayout();
		mainPanel.setLayout(gridbag);
		GridBagConstraints c = new GridBagConstraints();
		
		c.fill = GridBagConstraints.BOTH;
		c.weightx = 1;
		c.weighty = 1;
		mainPanel.add(text, c);
		
		pack();
		setLocationRelativeTo(null);
		setVisible(true);
	}
}
