package gui.models;

import java.io.File;
import java.io.FileReader;
import java.io.BufferedReader;
import java.io.FileWriter;
import java.io.IOException;

import java.util.ArrayList;
import java.util.Iterator;

import net.sf.json.JSONObject;

public class PersistentReplacements implements Persistent {
    
    private File file;
    private String filePath = "configs/replacementsConfig.json";
    private ArrayList<String> keys;
    private ArrayList<String> values;
    
    public PersistentReplacements() throws IOException {
        file = new File(filePath);
        keys = new ArrayList<String>();
        values = new ArrayList<String>();
        if (!file.exists()) {
			File parentFile = new File(file.getParent());
			if (!parentFile.exists()) {
				parentFile.mkdirs();
			}
			file.createNewFile();
		} else {
        	parse();
        }
    }
    
    public Replacements getReplacements(String tableName) {
    	
    	if (Math.min(keys.size(), values.size()) <= 0) {
    		return null;
    	}
    	
    	Replacements reps = new Replacements();
    	
    	String currKey = null;
    	for (int i=0; i<Math.min(keys.size(), values.size()); i++) {
    		currKey = keys.get(i);
    		if (currKey.startsWith(tableName)) {
    			reps.addReplacement(currKey.substring(tableName.length()), values.get(i));
    		}
    	}
    	
    	return reps;
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
    
	public void addReplacement(String tableName, String toReplace, String replaceWith) throws IOException {
		
		if (tableName == null || toReplace == null || replaceWith == null) {
			return;
		}
		
		parse();
		
		String key = tableName.concat(toReplace);
		keys.add(key);
		values.add(replaceWith);
		
		writeOutJson();
		
	}
	
	public void writeOutJson() throws IOException {
		
		JSONObject json = new JSONObject();
		
		for (int i=0; i<Math.min(keys.size(), values.size()); i++) {
			json.put(keys.get(i), values.get(i));
		}

		FileWriter fw = null;
		fw = new FileWriter(filePath);
		json.write(fw);
		fw.flush();
	}
	
	public void removeReplacement(String tableName, String toReplace, String replaceWith) throws IOException {
		
		if (tableName == null || toReplace == null || replaceWith == null) {
			return;
		}
		
		parse();
		
		String searchKey = tableName.concat(toReplace);
		String currKey = null;
		for (int i=0; i<Math.min(keys.size(), values.size()); i++) {
			currKey = keys.get(i);
			if (currKey.equals(searchKey)) {
				keys.remove(i);
				values.remove(i);
			}
		}
		
		writeOutJson();
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
