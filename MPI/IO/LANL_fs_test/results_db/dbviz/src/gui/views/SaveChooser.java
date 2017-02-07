package gui.views;

import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

public class SaveChooser extends JFileChooser {

	private static final long serialVersionUID = 1L;

	public void approveSelection() {
		File selected = getSelectedFile();
		if (selected != null && selected.exists()) {
			int response = JOptionPane.showConfirmDialog(this, 
					"The file "+selected.getName()+" already exists.  Do you want to overwrite the existing file?",
					"",
					JOptionPane.YES_NO_OPTION);
			if (response != JOptionPane.YES_OPTION) {
				return;
			}
		}
		super.approveSelection();
	}
}
