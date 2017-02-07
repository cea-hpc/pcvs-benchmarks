package gui.models;

import gui.exceptions.DbConnectionException;
import gui.exceptions.QueryException;

import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.SwingUtilities;

public class Database {

	public class EnterPassword extends JFrame {
		private static final long serialVersionUID = 1L;
		private JPanel mainPanel;
		private JLabel label;
		private JPasswordField passwordField;
		private JButton okButton;

		public EnterPassword() {

			mainPanel = new JPanel();
			setContentPane(mainPanel);

			label = new JLabel("Enter Password: ");
			passwordField = new JPasswordField(10);
			okButton = new JButton("OK");

			mainPanel.add(label);
			mainPanel.add(passwordField);
			mainPanel.add(okButton);

			pack();
			setLocationRelativeTo(null);
			setVisible(true);

		}

		public void addOkListener(ActionListener al) {
			okButton.addActionListener(al);
			okButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					Window window = SwingUtilities.getWindowAncestor(mainPanel);
					window.dispose();
				}
			});
		}

		public String getPassword() {
			String pass = "";
			char[] array = passwordField.getPassword();
			if (array == null || array.length == 0) {
				return "";
			}
			for (int i = 0; i < array.length; i++) {
				pass += array[i];
			}
			return pass;
		}

	}

	private String hostname;
	private String dbname;
	private String tablename;

	private String username;

	private String password;
	private boolean retry = false;

	private String driverName = "com.mysql.jdbc.Driver";
	private Connection conn;

	private Vector<String> colNameAll;

	private Vector<String> colNameNum;
	
	private boolean isPrimary = false;
//	private boolean isEnabled = false;

	public Database() {
		this("", "", "", "", "");
	}

	public Database(String hname, String dname, String t, String uname,
			String pass) {

		hostname = hname;
		dbname = dname;
		username = uname;
		password = pass;
		tablename = t;
		conn = null;
		colNameAll = new Vector<String>();
		colNameNum = new Vector<String>();

	}
	
	public boolean isPrimary() {
		return isPrimary;
	}
	
	public void setPrimary(boolean b) {
		isPrimary = b;
	}
//	
//	public boolean isEnabled() {
//		return isEnabled;
//	}
//	
//	public void setEnabled(boolean b) {
//		isEnabled = b;
//	}

	public void closeConnection() throws SQLException {
		if (conn == null) {
			return;
		}
		conn.close();
	}

	public Connection dbconnect() throws ClassNotFoundException, SQLException {

		Class.forName(driverName);

		String port = "3306";
		String url = "jdbc:mysql://" + hostname + ":" + port + "/" + dbname;

		//password = "";

		retry = true;
		int numTries = 0;
		while (retry && numTries < 3) {
			try {
				conn = DriverManager.getConnection(url, username, password);
				retry = false;
			} catch (SQLException s) {
				numTries++;
				password = JOptionPane.showInputDialog(null, "Please enter your password:", "Authenticate", JOptionPane.PLAIN_MESSAGE);
			
				try {
					conn = DriverManager.getConnection(url, username, password);
					retry = false;
				} catch (SQLException se) {
					;
				}
				
				// TODO: check that the exception was actually caused by
				// password failure

			}
		}
		
		//password = "";

		if (conn == null) {
			return null;
		}

		Statement s = conn.createStatement();

		String query = "DESCRIBE " + tablename;
		s.executeQuery(query);
		ResultSet result = s.getResultSet();
		colNameAll = new Vector<String>();
		colNameNum = new Vector<String>();

		while (result.next()) {
			if (result.getString(2).startsWith("tinyint")
					|| result.getString(2).startsWith("smallint")
					|| result.getString(2).startsWith("mediumint")
					|| result.getString(2).startsWith("int")
					|| result.getString(2).startsWith("bigint")
					|| result.getString(2).startsWith("float")
					|| result.getString(2).startsWith("double")
					|| result.getString(2).startsWith("datetime")
					|| result.getString(2).startsWith("timestamp")) {
				colNameNum.add(result.getString(1));
			}
			colNameAll.add(result.getString(1));
		}

		s.close();

		return conn;

	}

	public boolean equals(Database db) {
		if (this.hostname.equals(db.getHostName())
				&& this.dbname.equals(db.getDbName())
				&& this.tablename.equals(db.getTableName())
				&& this.username.equals(db.getUserName())
				&& this.password.equals(db.getPassword())) {
			return true;
		} else {
			return false;
		}
	}

	public Vector<String> getColNameAll() {
		return colNameAll;
	}

	public Vector<String> getColNameNum() {
		return colNameNum;
	}

	public String getDbName() {
		return dbname;
	}

	public String getHostName() {
		return hostname;
	}

	public String getPassword() {
		return password;
	}

	public String getTableName() {
		return tablename;
	}

	public String getUserName() {
		return username;
	}

	public ResultSet query(String query) throws DbConnectionException,
			QueryException {

		if (conn == null) {
			throw new DbConnectionException(
					"Connection has not been initialized");
		} else
			try {
				if (conn.isClosed()) {
					throw new DbConnectionException(
							"Connection to database is closed");
				}
			} catch (SQLException sqle) {
				throw new DbConnectionException(sqle.getMessage());
			}

		ResultSet result = null;

		Statement s;
		try {
			s = conn.createStatement();
		} catch (SQLException e) {
			throw new QueryException(e.getMessage());
		}

		try {
			s.executeQuery(query);
		} catch (SQLException e) {
			throw new QueryException(e.getMessage());
		}

		try {
			result = s.getResultSet();
		} catch (SQLException e) {
			throw new QueryException(e.getMessage());
		}

		return result;

	}

	public void setDatabasename(String s) {
		dbname = s;
	}

	public void setHostname(String s) {
		hostname = s;
	}

	public void setPassword(String s) {
		password = s;
	}

	public void setTablename(String s) {
		tablename = s;
	}

	public void setUsername(String s) {
		username = s;
	}

}
