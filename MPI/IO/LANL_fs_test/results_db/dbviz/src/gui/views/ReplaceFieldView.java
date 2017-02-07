package gui.views;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

public class ReplaceFieldView extends JFrame {

	private static final long serialVersionUID = 1L;

	private JPanel mainPanel = new JPanel();
	private JLabel toReplaceLabel;
	private JLabel replaceWithLabel;
	private JTextField toReplaceField;
	private JTextField replaceWithField;
	private JButton okButton;
	private JButton cancelButton;

	public ReplaceFieldView() {

		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.PAGE_AXIS));
		setContentPane(mainPanel);

		GridBagLayout gridbag = new GridBagLayout();
		setLayout(gridbag);

		GridBagConstraints c = new GridBagConstraints();

		toReplaceLabel = new JLabel("Field to Replace");
		replaceWithLabel = new JLabel("Replace With");

		toReplaceField = new JTextField(10);
		replaceWithField = new JTextField(10);

		c.gridx = 0;
		c.gridy = 0;
		mainPanel.add(toReplaceLabel, c);
		c.gridy = 1;
		mainPanel.add(replaceWithLabel, c);

		c.gridx = 1;
		c.gridy = 0;
		mainPanel.add(toReplaceField, c);
		c.gridy = 1;
		mainPanel.add(replaceWithField, c);

		okButton = new JButton("OK");

		// okButton.addActionListener(new ReplaceFieldOkListener());
		cancelButton = new JButton("Cancel");

		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Window window = SwingUtilities.getWindowAncestor(mainPanel);
				window.dispose();
			}
		});

		c.gridx = 0;
		c.gridy = 2;
		c.gridwidth = 2;
		JPanel buttonPane = new JPanel();
		buttonPane.setLayout(new BoxLayout(buttonPane, BoxLayout.LINE_AXIS));
		buttonPane.add(Box.createHorizontalGlue());

		buttonPane.add(okButton);
		buttonPane.add(Box.createRigidArea(new Dimension(10, 0)));
		buttonPane.add(cancelButton);
		mainPanel.add(buttonPane, c);

		pack();
		setLocationRelativeTo(null);
		setVisible(true);
	}

	public void addReplaceFieldOkListener(ActionListener rf) {
		okButton.addActionListener(rf);
	}

	public void closeWindow() {
		Window window = SwingUtilities.getWindowAncestor(mainPanel);
		window.dispose();
	}

	public String getReplaceWithField() {
		return replaceWithField.getText();
	}

	public String getToReplaceField() {
		return toReplaceField.getText();
	}
}
