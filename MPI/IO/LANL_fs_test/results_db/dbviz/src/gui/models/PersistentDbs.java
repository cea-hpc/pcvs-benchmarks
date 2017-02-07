package gui.models;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import net.sf.json.JSONObject;

public class PersistentDbs implements Persistent {

	private File file;
	private String filePath;
	private MainModel mm;
	private ArrayList<String> keys;
	private ArrayList<String> values;
	
	public PersistentDbs(String filename, MainModel model) throws IOException {
		keys = new ArrayList<String>();
		values = new ArrayList<String>();
		filePath = filename;
		mm = model;
		file = new File(filename);
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
	
	public Database loadDbs() throws IOException {
		
		parse();
		
		String[] tokens;
		String currVal = null;
		String currTable = "";
		String currDb = "";
		String currHost = "";
		String currUser = "";
		String currPW = "";
		int index = -1;
		
		for (int i=0; i<Math.min(keys.size(), values.size()); i++) {
			
			currTable = keys.get(i);
			currVal = values.get(i);
			tokens = currVal.split(",");
			
			for (int j=0; j<tokens.length; j++) {
				switch (j) {
				case 0:
					currHost = tokens[j];
					break;
				case 1:
					currDb = tokens[j];
					break;
				case 2:
					currUser = tokens[j];
					break;
				case 3:
					currPW = tokens[j];
					break;
				case 4:
					if (tokens[j].equals("true")) {
						index = i;
					}
				}
			}
			
			mm.addTable(new Database(currHost, currDb, currTable,
					currUser, currPW));

			currHost = "";
			currDb = "";
			currTable = "";
			currUser = "";
			currPW = "";
			
		}
		
		if (index >= 0) {
			return mm.getTables().get(index);
		} else {
			if (keys.size() > 0) {
				return mm.getTables().get(0);
			} else {
				return null;
			}
		}
	}
	
	public void flushDbsToConfig() throws IOException {
	
		List<Database> dbs = mm.getTables();
		Database curr;
		String currVal;
		keys = new ArrayList<String>(dbs.size());
		values = new ArrayList<String>(dbs.size());

		for (int i = 0; i < dbs.size(); i++) {
			curr = dbs.get(i);
			keys.add(curr.getTableName());
			currVal = curr.getHostName();
			currVal += ","+curr.getDbName();
			currVal += ","+curr.getUserName();
			currVal += ","+curr.getPassword();
			if (curr.isPrimary()) {
				currVal += ",true";
			} else {
				currVal += ",false";
			}
			values.add(currVal);
		}
		
		writeOutJson();
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
