package gui.views;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

public class AdvancedOptions extends JFrame {

	private static final long serialVersionUID = 1L;
	private JPanel mainPanel;
	private ParameterView pv;
	
	private JButton okButton;
	private JButton cancelButton;
	
	private JLabel xTickMarksLabel;
	private JLabel yTickMarksLabel;
	private JLabel lineThickLabel;
	private JLabel pointSizeLabel;
	private JLabel fontSizeLabel;
	private JLabel sortLegendLabel;
	private JLabel formatAsDateLabel;
	private JLabel noLinesLabel;
	private JLabel barGraphLabel;
	
	private JTextField xTickMarksField;
	private JTextField yTickMarksField;
	private JTextField lineThickField;
	private JTextField pointSizeField;
	private JTextField fontSizeField;
	private SteppedComboBox sortLegendField;
	private JCheckBox formatAsDateBox;
	private JCheckBox noLinesBox;
	private JCheckBox barGraphBox;
	
	public AdvancedOptions(ParameterView params) {
		
		pv = params;
		
		mainPanel = new JPanel();
		setContentPane(mainPanel);
		setTitle("Advanced Options");
		
		xTickMarksLabel = new JLabel("# X Tick Marks");
		yTickMarksLabel = new JLabel("# Y Tick Marks");
		lineThickLabel = new JLabel("Line Thickness");
		pointSizeLabel = new JLabel("Point Size");
		fontSizeLabel = new JLabel("Font Size");
		sortLegendLabel = new JLabel("Legend Sort Order");
		formatAsDateLabel = new JLabel("Format X Labels As Dates");
		noLinesLabel = new JLabel("Scatter Plot (No Lines)");
		barGraphLabel = new JLabel("Bar Graph");
		
		Vector<String> legendSortTypes = new Vector<String>();
		legendSortTypes.add("Average Y Value");
		legendSortTypes.add("First Y Value");
		legendSortTypes.add("Last Y Value");
		legendSortTypes.add("Alphanumerically");
		
		xTickMarksField = new JTextField(10);
		yTickMarksField = new JTextField(10);
		lineThickField = new JTextField(10);
		pointSizeField = new JTextField(10);
		fontSizeField = new JTextField(10);
		sortLegendField = new SteppedComboBox(legendSortTypes);
		formatAsDateBox = new JCheckBox();
		noLinesBox = new JCheckBox();
		barGraphBox = new JCheckBox();
		
		sortLegendField.setPreferredSize(new Dimension(200, fontSizeField.getPreferredSize().height));
		sortLegendField.setPopupWidth(fontSizeField.getPreferredSize().width);
		sortLegendField.setEditable(false);
		
		xTickMarksField.setText(pv.getXTickMarks());
		yTickMarksField.setText(pv.getYTickMarks());
		lineThickField.setText(String.valueOf(pv.getLineThickness()));
		pointSizeField.setText(String.valueOf(pv.getPointSize()));
		fontSizeField.setText(String.valueOf(pv.getFontSize()));
		sortLegendField.setSelectedItem(pv.getLegendSortType());
		formatAsDateBox.setSelected(pv.getFormatAsDate());
		noLinesBox.setSelected(pv.getNoLines());
		barGraphBox.setSelected(pv.getBarGraph());
		
		okButton = new JButton("OK");
		cancelButton = new JButton("Cancel");
		
		okButton.addActionListener(new OkListener());
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent ae) {
				closeWindow();
			}
		});
		
		GridBagLayout gridbag = new GridBagLayout();
		mainPanel.setLayout(gridbag);
		GridBagConstraints c = new GridBagConstraints();
		
		c.anchor = GridBagConstraints.LINE_END;
		c.insets = new Insets(2, 5, 0, 5);
		c.gridx = 0;
		c.gridy = 0;
		mainPanel.add(xTickMarksLabel, c);
		c.gridy++;
		mainPanel.add(yTickMarksLabel, c);
		c.gridy++;
		mainPanel.add(lineThickLabel, c);
		c.gridy++;
		mainPanel.add(pointSizeLabel, c);
		c.gridy++;
		mainPanel.add(fontSizeLabel, c);
		c.gridy++;
		mainPanel.add(sortLegendLabel, c);
		c.gridy++;
		mainPanel.add(formatAsDateLabel, c);
		c.gridy++;
		mainPanel.add(noLinesLabel, c);
		c.gridy++;
		mainPanel.add(barGraphLabel, c);
		c.gridy++;
		mainPanel.add(okButton, c);
		
		c.anchor = GridBagConstraints.LINE_START;
		c.gridx = 1;
		c.gridy = 0;
		mainPanel.add(xTickMarksField, c);
		c.gridy++;
		mainPanel.add(yTickMarksField, c);
		c.gridy++;
		mainPanel.add(lineThickField, c);
		c.gridy++;
		mainPanel.add(pointSizeField, c);
		c.gridy++;
		mainPanel.add(fontSizeField, c);
		c.gridy++;
		mainPanel.add(sortLegendField, c);
		c.gridy++;
		mainPanel.add(formatAsDateBox, c);
		c.gridy++;
		mainPanel.add(noLinesBox, c);
		c.gridy++;
		mainPanel.add(barGraphBox, c);
		c.gridy++;
		mainPanel.add(cancelButton, c);
		
		pack();
		setLocationRelativeTo(null);
		setVisible(true);
		
	}
	
	public class OkListener implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			pv.setXTickMarks(xTickMarksField.getText());
			pv.setYTickMarks(yTickMarksField.getText());
			pv.setLineThickness(Integer.parseInt(lineThickField.getText()));
			pv.setPointSize(Integer.parseInt(pointSizeField.getText()));
			pv.setFontSize(Integer.parseInt(fontSizeField.getText()));
			pv.setLegendSortType((String) sortLegendField.getSelectedItem());
			pv.setFormatAsDate(formatAsDateBox.isSelected());
			pv.setNoLines(noLinesBox.isSelected());
			pv.setBarGraph(barGraphBox.isSelected());
			closeWindow();
		}
	}
	
	private void closeWindow() {
		Window window = SwingUtilities.getWindowAncestor(mainPanel);
		window.dispose();
	}
	
}
