package gui.views;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

//public class HistoryView extends JTextArea {
public class HistoryView extends JPanel { //extends JScrollPane {

	private static final long serialVersionUID = 1L;

	private JTextArea text;
	private JScrollPane scroll;
	private JProgressBar progress;

	public HistoryView() {

		//super();

		scroll = new JScrollPane();
		text = new JTextArea();
		scroll.setViewportView(text);
		text.setEditable(false);
		text.append("History:\n");
		
		progress = new JProgressBar(JProgressBar.HORIZONTAL, 0, 100);
		
		GridBagLayout gridbag = new GridBagLayout();
		setLayout(gridbag);
		GridBagConstraints c = new GridBagConstraints();
		
		c.anchor = GridBagConstraints.PAGE_START;
		c.gridx = 0;
		c.gridy = 0;
		c.weightx = 1;
		c.weighty = 1;
		c.fill = GridBagConstraints.BOTH;
		add(scroll, c);
		
		c.gridy = 1;
		c.weighty = 0;
		c.fill = GridBagConstraints.HORIZONTAL;
		add(progress, c);
		
		gridbag = null;
		c = null;

	}

	public void addHist(String s) {
		text.append(s);
	}
	
	public void init() {
		progress.setValue(0);
		progress.setStringPainted(true);
	}
	
	public void setProgressText(String s) {
		progress.setStringPainted(true);
		progress.setString(s);
	}
	
	public void addProgress(int n) {
		if (n<=0) {
			return;
		}
		int current = progress.getValue();
		int max = progress.getMaximum();
		if (current >= max) {
			return;
		} else {
			progress.setValue(current+n);
		}
		if (progress.getValue() >= (.95*max)) {
			progress.setValue(max);
		}
	}
	
	public void resetProgress() {
		progress.setValue(0);
		progress.setStringPainted(false);
	}

}
