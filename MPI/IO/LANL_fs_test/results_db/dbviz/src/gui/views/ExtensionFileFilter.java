package gui.views;

import java.io.File;

import javax.swing.filechooser.FileFilter;

public class ExtensionFileFilter extends FileFilter {
	
	private String extension;
	
	public ExtensionFileFilter(String ext) {
		super();
		extension = ext;
	}

	@Override
	public boolean accept(File f) {
		return f.isDirectory() || f.getName().endsWith(extension);
	}

	@Override
	public String getDescription() {
		return extension + " files";
	}

}
