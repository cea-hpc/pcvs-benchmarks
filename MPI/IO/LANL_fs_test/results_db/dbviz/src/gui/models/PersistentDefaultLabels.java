package gui.models;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;

import net.sf.json.JSONObject;

public class PersistentDefaultLabels implements Persistent {

	private File file;
	private String filePath = "configs/defaultLabels.json";
	private ArrayList<String> keys;
	private ArrayList<String> values;

	public PersistentDefaultLabels() throws IOException {
		file = new File(filePath);
		keys = new ArrayList<String>();
		values = new ArrayList<String>();
		if (!file.exists()) {
			File parentFile = new File(file.getParent());
			if (!parentFile.exists()) {
				parentFile.mkdirs();
			}
			file.createNewFile();
		}
	}

	@SuppressWarnings("unchecked")
	public void parse() throws IOException {
		BufferedReader br = null;
		String temp = null;
		String full = "";
		br = new BufferedReader(new FileReader(file));
		while ((temp = br.readLine()) != null) {
			full = full.concat(temp);
		}
		if (full.length() >= 0 && full.startsWith("{")) {
			JSONObject json = JSONObject.fromObject(full);
			Iterator<String> it = (Iterator<String>) json.keys();
			String key = null;
			while (it.hasNext()) {
				key = (String) it.next();
				keys.add(key);
				values.add((String) json.get(key));
			}
		}
	}

	public void addDefaultLabel(String tablename, String toReplace, String replaceWith) throws IOException {
		
		parse();
		
		keys.add(tablename+","+toReplace);
		values.add(replaceWith);
		
		writeOutJson();		
		
	}
	
	public String checkString(String tablename, String toCheck) throws IOException {
		
		parse();
		
		String key = tablename+","+toCheck;
		for (int i=0; i<Math.min(keys.size(), values.size()); i++) {
			if (keys.get(i).equals(key)) {
				return values.get(i);
			}
		}
		
		return toCheck;
	}
	
	public void writeOutJson() throws IOException {

		JSONObject json = new JSONObject();

		for (int i = 0; i < Math.min(keys.size(), values.size()); i++) {
			json.put(keys.get(i), values.get(i));
		}

		FileWriter fw = null;
		fw = new FileWriter(filePath);
		json.write(fw);
		fw.flush();
	}
	
	public ArrayList<String> getKeys() {
		try {
			parse();
		} catch (IOException ioe) {
			//TODO: do something here
		}
		return keys;
	}
	
	public ArrayList<String> getValues() {
		return values;
	}
	
	public void setKeys(ArrayList<String> array) {
		keys = array;
	}
	
	public void setValues(ArrayList<String> array) {
		values = array;
	}

}
