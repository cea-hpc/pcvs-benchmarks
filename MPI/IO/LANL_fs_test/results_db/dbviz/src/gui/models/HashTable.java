package gui.models;

import java.util.ArrayList;
import gui.models.Line.PointType;

public class HashTable {

    private ArrayList<ArrayList<LinePoint>> hashTable;
    private ArrayList<Statistics> stats;
    private ArrayList<LinePoint> allPoints;
    private int numPoints;
    private int capacity;
    private double loadBalance;
    private int load;
    
    public HashTable() {
        this(17);
    }
    
    public HashTable(int size) {
        capacity = size;
        hashTable = new ArrayList<ArrayList<LinePoint>>(capacity);
        for (int i=0; i<capacity; i++) {
            hashTable.add(new ArrayList<LinePoint>());
        }
        numPoints = 0;
        load = 0;
        loadBalance = .75;
    }
    
    public int getLoad() {
    	if (stats==null) {
    		return 0;
    	}
        return stats.size();
    }
    
    public int getNumPoints() {
        return numPoints;
    }
    
    private void rehash() {

        int newCap = (capacity * 3) + 1;
        int index = 0;
        load = 0;
        ArrayList<ArrayList<LinePoint>> newHash = new ArrayList<ArrayList<LinePoint>>(newCap);
        ArrayList<LinePoint> array = null;

        for (int i=0; i<newCap; i++) {
            newHash.add(new ArrayList<LinePoint>());
        }
        
        for (int i=0; i<capacity; i++) {
            array = hashTable.get(i);
            if (array.size() > 0) {
            	index = hashVal(array.get(0).getXVal(), newCap);
            	while (newHash.get(index).size() > 0) {
            		index = (++index) % newCap;
            	}
            	newHash.set(index, array);
            }
        }
        
        hashTable = newHash;
        capacity = newCap;
        
        newHash = null;
        array = null;
    }
    
    public void addToHash(LinePoint point) {
        if (((double)load / (double)capacity) >= loadBalance) {
        	rehash();
        }
        int index = hashVal(point.getXVal(), capacity);
        ArrayList<LinePoint> array = hashTable.get(index);
        if (array.size() > 0) {
            if (array.get(0).getXVal() != point.getXVal()) {
                //faulty collision - so find new ll
                int count = 0;
                while (count < capacity) {
                	index++;
                    index %= capacity;
                    array = hashTable.get(index);
                    if (array.size() <= 0) {
                        load++;
                        break;
                    } else if (array.get(0).getXVal() == point.getXVal()) {
                        break;
                    }
                    count++;
                }
            }
        } else {
            load++;
        }
        
        array.add(point);
        numPoints++;
        array = null;

    }
    
    public int hashVal(double d, int max) {
        Double doub = new Double(d);
        int hash = doub.hashCode() % max;
        if (hash < 0) {
            hash += max;
        }
        return hash;
    }
    
    public double getAvg(int i) {
        return stats.get(i).getAvg();
    }
    
    public double getMax(int i) {
        return stats.get(i).getMax();
    }
    
    public double getMin(int i) {
        return stats.get(i).getMin();
    }
    
    public double getStdDev(int i) {
        return stats.get(i).getStdDev();
    }
    
    public LinePoint getStatPoint(int index, PointType pt) {
        return stats.get(index).getPoint(pt);
    }
    
    public LinePoint getPtFromAll(int index) {
        return allPoints.get(index);
    }
    
    public void removePoint(int index) {
    	double xVal = stats.get(index).getXVal();
        stats.remove(index);
        removeFromAllPoints(xVal);
        int hashVal = hashVal(xVal, capacity);
        hashTable.set(hashVal, new ArrayList<LinePoint>());
    	load--;
    }
    
    private void removeFromAllPoints(double xVal) {
    	LinePoint point = null;
    	for (int i=0; i<numPoints; i++) {
    		point = allPoints.get(i);
    		if (point.getXVal() == xVal) {
    			allPoints.remove(i);
    			i--;
    			numPoints--;
    		}
    	}
    }
    
    private int findStatIndex(double xVal) {
    	Statistics stat = null;
    	for (int i=0; i<stats.size(); i++) {
    		stat = stats.get(i);
    		if (stat.getXVal() == xVal) {
    			return i;
    		}
    	}
    	return -1;
    }

	public void removePointFromAll(int index) {
		LinePoint point = allPoints.get(index);
		double xVal = point.getXVal();
		ArrayList<LinePoint> array = hashTable.get(hashVal(xVal, capacity));
		if (array.size() <= 1) {
			hashTable.set(hashVal(xVal, capacity), new ArrayList<LinePoint>());
			stats.remove(findStatIndex(xVal));
			load--;
		} else {
			LinePoint temp = null;
			for (int i=0; i<array.size(); i++) {
				temp = array.get(i);
				if (temp.getYVal() == point.getYVal()) {
					array.remove(i);
					break;
				}
			}
			int i = findStatIndex(xVal);
			stats.set(i, new Statistics(array));
		}
		allPoints.remove(index);
		numPoints--;
	}
    
    public int sizeOf(int index) {
        return stats.get(index).size();
    }
    
    private void compress() {
        allPoints = new ArrayList<LinePoint>(numPoints);
        ArrayList<LinePoint> array = null;
        //int temp = 0;
        for (int i=0; i<hashTable.size(); i++) {
            array = hashTable.get(i);
            for (int j=0; j<array.size(); j++) {
//            	if (array.get(j).getStringVal() != null) {
//            		array.get(j).setXVal(temp);
//            	}
                allPoints.add(array.get(j));
            }
//            temp++;
        }
    }
    
    public void constructStats() {
    
        stats = new ArrayList<Statistics>();
        
        ArrayList<LinePoint> array = null;
        for (int i=0; i<hashTable.size(); i++) {
            array = hashTable.get(i);
            if (array.size() > 0) {
            	//System.out.println("Making new stat..." + i);
                stats.add(new Statistics(array));
            }
        }
        
        sortStats();
        compress();
    }
    
    //uses bubble sort
    private void sortStats() {
        boolean swapped = true;
        Statistics temp = null;
        do {
            swapped = false;
            for (int i=0; i<stats.size()-1; i++) {
                if (stats.get(i).getXVal() > stats.get(i+1).getXVal()) {
                    temp = stats.get(i);
                    stats.set(i, stats.get(i+1));
                    stats.set(i+1, temp);
                    swapped = true;
                }
            }
        } while (swapped);
    }
    
    private class Statistics {
        
        private LinePoint point;
        
        private double avg;
        private double max;
        private double min;
        private double stddev;
        private int size;
        
        public Statistics(ArrayList<LinePoint> array) {

            point = new LinePoint();
            
            avg = stddev = 0;
            max = Double.MIN_VALUE;
            min = Double.MAX_VALUE;
            size = array.size();
            double sum = 0;
            double currVal = 0;
            LinePoint currPoint = null;
            
            for (int i=0; i<size; i++) {
                currPoint = array.get(i);
                if (i == 0) {
                    point.setXVal(currPoint.getXVal());
                    point.setStringVal(currPoint.getStringVal());
                }
                //System.out.println(currPoint.getXVal());
                currVal = currPoint.getYVal();
                sum += currVal;
                if (currVal > max) {
                    max = currVal;
                }
                if (currVal < min) {
                    min = currVal;
                }
            }
            
            if (size > 0) {
                avg = sum / (double)size;
            }
            
            sum = 0;
            for (int i=0; i<size; i++) {
                currVal = array.get(i).getYVal();
                sum += Math.pow((currVal - avg), 2);
            }
            if (size > 0) {
                sum /= size;
                stddev = Math.sqrt(sum);
            }
        }
        
        public int size() {
            return size;
        }
        
        public double getAvg() {
            return avg;
        }
        
        public double getMax() {
            return max;
        }
        
        public double getMin() {
            return min;
        }
        
        public double getStdDev() {
            return stddev;
        }
        
        public double getXVal() {
            return point.getXVal();
        }
        
        public LinePoint getPoint(PointType pt) {
            if (pt == PointType.AVG) {
                point.setYVal(avg);
            } else if (pt == PointType.MAX) {
                point.setYVal(max);
            } else {
                point.setYVal(min);
            }
            
            return point;
        }
    }
}
