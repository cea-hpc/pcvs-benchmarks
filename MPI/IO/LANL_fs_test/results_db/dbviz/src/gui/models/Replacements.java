package gui.models;

import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

public class Replacements {

	private ArrayList<String> comps;
	private ArrayList<String> reps;
	
	public Replacements() {
		comps = new ArrayList<String>();
		reps = new ArrayList<String>();
	}
	
	public void addReplacement(String toRep, String repWith) {
		comps.add(toRep);
		reps.add(repWith);
	}
	
	public String checkString(String s) throws PatternSyntaxException {
		
		Pattern p = null;
		Matcher m = null;
		int start = 0;
		int end = 0;

		for (int i=0; i<comps.size(); i++) {
			try {
				p = Pattern.compile(comps.get(i));
			} catch (PatternSyntaxException pse) {
				throw new PatternSyntaxException("Invalid search pattern", comps.get(i), i);
			}
			m = p.matcher(s);
			while (m.find()) {
				start = m.start();
				end = m.end();
				if (start == 0 && end == s.length()) {
					s = reps.get(i);
				} else if (start == 0) {
					s = reps.get(i) + s.substring(end);
				} else if (end == s.length()) {
					s = s.substring(0, start) + reps.get(i);
				} else {
					s = s.substring(0, start) + reps.get(i) + s.substring(end);
				}
			}
		}
		return s; 
	}
	
	public void setComps(ArrayList<String> al) {
		comps = al;
	}
	
	public void setReps(ArrayList<String> al) {
		reps = al;
	}

	public int size() {
		return comps.size();
	}
	
	public boolean equals(Replacements other) {
		if (other == null) {
			return false;
		}
		ArrayList<String> otherComps = other.comps;
		ArrayList<String> otherReps = other.reps;
		if (otherComps == null && comps != null) {
			return false;
		} else if (otherComps != null && comps == null) {
			return false;
		} else if (otherReps == null && reps != null) {
			return false;
		} else if (otherReps != null && reps == null) {
			return false;
		}
		if (comps.size() != otherComps.size()) {
			return false;
		}
		if (reps.size() != otherReps.size()) {
			return false;
		}
		for (int i=0; i<comps.size(); i++) {
			if (!comps.get(i).equals(otherComps.get(i))) {
				return false;
			}
		}
		for (int i=0; i<reps.size(); i++) {
			if (!reps.get(i).equals(otherReps.get(i))) {
				return false;
			}
		}
		return true;
	}
	
}
