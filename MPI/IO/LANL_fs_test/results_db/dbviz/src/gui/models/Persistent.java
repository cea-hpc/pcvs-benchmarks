package gui.models;

import java.io.IOException;
import java.util.ArrayList;

public interface Persistent {
	
	void parse() throws IOException;
	void writeOutJson() throws IOException;
	ArrayList<String> getKeys();
	ArrayList<String> getValues();
	void setKeys(ArrayList<String> keys);
	void setValues(ArrayList<String> values);

}
