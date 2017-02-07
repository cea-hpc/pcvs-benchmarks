package gui.models;

import gui.views.ParameterView;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import net.sf.json.JSONObject;

public class PersistentTopUsed implements Persistent {
	
	private ArrayList<String> keys;
	private ArrayList<String> values;
	private String tableName;
	private String filePath = "configs/topUsed.json";
	private File file;
	//private ParameterView pv;
	
	public PersistentTopUsed(String tablename, ParameterView pv) throws IOException {
		//this.pv = pv;
		keys = new ArrayList<String>();
		values = new ArrayList<String>();
		tableName = tablename;
		file = new File(filePath);
		if (!file.exists()) {
			File parentFile = new File(file.getParent());
			if (!parentFile.exists()) {
				parentFile.mkdirs();
			}
			file.createNewFile();
		}
		parse();
	}
	
	public Vector<String> loadTopUsedX() throws IOException {
		//parse();
		String currKey = null;
		Vector<String> v = new Vector<String>();
		for (int i=0; i<Math.min(keys.size(), values.size()); i++) {
			currKey = keys.get(i);
			if (currKey.startsWith(tableName) && currKey.endsWith(",x")) {
				v.add(values.get(i));
			}
		}
		return v;
	}
	
	public Vector<String> loadTopUsedY() throws IOException {
		//parse();
		String currKey = null;
		Vector<String> v = new Vector<String>();
		for (int i=0; i<Math.min(keys.size(), values.size()); i++) {
			currKey = keys.get(i);
			//System.out.println(currKey);
			if (currKey.startsWith(tableName) && currKey.endsWith(",y")) {
				//System.out.println(values.get(i));
				v.add(values.get(i));
			}
		}
		return v;
	}

	public Vector<String> loadTopUsedCompare() throws IOException {
		//parse();
		String currKey = null;
		Vector<String> v = new Vector<String>();
		for (int i=0; i<Math.min(keys.size(), values.size()); i++) {
			currKey = keys.get(i);
			if (currKey.startsWith(tableName) && currKey.endsWith(",c")) {
				v.add(values.get(i));
			}
		}
		return v;
	}
	
	public void flushToConfig(List<String> x, List<String> y, List<String> compare) throws IOException {
		
		if (x!=null) {
			for (int i=0; i<Math.min(x.size(), 10); i++) {
				keys.add(tableName+","+i+",x");
				values.add(x.get(i));
			}
		}
		
		if (y != null) {
			for (int i=0; i<Math.min(y.size(), 10); i++) {
				keys.add(tableName+","+i+",y");
				values.add(y.get(i));
			}
		}
		
		if (compare != null) {
			for (int i=0; i<Math.min(compare.size(), 10); i++) {
				keys.add(tableName+","+i+",c");
				values.add(compare.get(i));
			}
		}
		writeOutJson();
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
