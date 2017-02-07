package gui.models;

import gui.exceptions.DbConnectionException;
import gui.exceptions.QueryException;

import java.io.IOException;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.Vector;

public class MainModel {

	//TODO: handle possibility that db config file is empty - dbviz doesn't load if this is the case
	
	private Database currdb;
	private LinkedList<Database> tables;

	public MainModel() {
		currdb = null;
		tables = new LinkedList<Database>();
	}

	public void addTable(Database db) {
		tables.add(db);
		//return setDb(db);
	}
	
	public void closeConnection() throws SQLException {
		if (currdb != null) {
			currdb.closeConnection();
		}
	}

	private void connectDb() throws DbConnectionException,
			ClassNotFoundException, SQLException {
		if (currdb.dbconnect() == null) {
			throw new DbConnectionException();
		}
	}
	
	public Database getDb() {
		return currdb;
	}

	// gets the column names for the string fields of the db table
	public Vector<String> getTableColumnNamesAll() {
		if (currdb == null) {
			return new Vector<String>();
		}
		Vector<String> cols = currdb.getColNameAll();
		if (cols != null) {
			Collections.sort(cols);
		}
		return cols;
	}

	// gets the column names for the numerical fields of the db table
	public Vector<String> getTableColumnNamesNum() {
		if (currdb == null) {
			return new Vector<String>();
		}
		Vector<String> cols = currdb.getColNameNum();
		if (cols != null) {
			Collections.sort(cols);
		}
		return currdb.getColNameNum();
	}

	public ArrayList<String> getTableNames() {
		ArrayList<String> names = new ArrayList<String>();
		for (int i=0; i<tables.size(); i++) {
			names.add(tables.get(i).getTableName());
		}
		return names;
	}

	public LinkedList<Database> getTables() {
		return tables;
	}

	public Database populateTableList() {

		PersistentDbs pd = null;
		try {
			pd = new PersistentDbs("configs/dbConfig.json", this);
			currdb = pd.loadDbs();
		} catch (IOException ioe) {
			//TODO: do something here
		}

		//TODO: handle null db
		return currdb;
	}

	public ResultSet queryDb(String query) throws DbConnectionException,
			QueryException {
		return currdb.query(query);
	}

	public boolean removeDb(Database dbm) throws DbConnectionException {
		boolean curr = false;
		if (dbm.getTableName().equals(currdb.getTableName())) {
			curr = true;
		}
		for (int i = 0; i < tables.size(); i++) {
			if (tables.get(i).equals(dbm)) {
				tables.remove(i);
			}
		}
		if (tables.size() == 0) {
			throw new DbConnectionException();
		}
		return curr;
	}

	//TODO: fix this up
	public boolean setDb(Database db) {
		//TODO: if db isn't already in list, add it to the list
		Database olddb = currdb;
		try {
			currdb = db;
			connectDb();
			for (int i=0; i<tables.size(); i++) {
				tables.get(i).setPrimary(false);
			}
			currdb.setPrimary(true);
			return true;
		} catch (DbConnectionException dce) {
			currdb = olddb;
			return false;
		} catch (SQLException e) {
			currdb = olddb;
			return false;
		} catch (ClassNotFoundException e) {
			currdb = olddb;
			return false;
		}
	}

	public void setTables(LinkedList<Database> dbs) {
		tables = dbs;
	}

	public int size() {
		if (tables==null) {
			return 0;
		}
		return tables.size();
	}

	public void updateDb(Database olddb, Database newdb) {
		for (int i = 0; i < tables.size(); i++) {
			if (tables.get(i).equals(olddb)) {
				tables.get(i).setHostname(newdb.getHostName());
				tables.get(i).setDatabasename(newdb.getDbName());
				tables.get(i).setTablename(newdb.getTableName());
				tables.get(i).setUsername(newdb.getUserName());
				tables.get(i).setPassword(newdb.getPassword());
				return;
			}
		}
	}
	
	public void updateXML() {
		
		PersistentDbs pd = null;
		try {
			pd = new PersistentDbs("configs/dbConfig.json", this);
			pd.flushDbsToConfig();
		} catch (IOException ioe) {
			//TODO: do something here
		}
	}
	
	public static String convertEpochToDate(long epoch) {
		String date = new java.text.SimpleDateFormat("dd/MM/yyyy HH:mm:ss").format(new java.util.Date (epoch*1000));
		return date;
	}
}
