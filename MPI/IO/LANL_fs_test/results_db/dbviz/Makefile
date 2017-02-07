JAR = dbviz.jar
JCLASSPATH = ./libs/mysql-connector-java-5.1.7-bin.jar
JCLASSPATH := $(JCLASSPATH):./libs/commons-beanutils-1.7.jar
JCLASSPATH := $(JCLASSPATH):./libs/commons-collections.jar
JCLASSPATH := $(JCLASSPATH):./libs/commons-lang.jar
JCLASSPATH := $(JCLASSPATH):./libs/commons-logging-1.1.1.jar
JCLASSPATH := $(JCLASSPATH):./libs/ezmorph.jar
JCLASSPATH := $(JCLASSPATH):./libs/json-lib-2.3-jdk15.jar


all:
	mkdir -p bin
	javac -classpath $(JCLASSPATH)  -sourcepath src/ -d bin src/gui/*.java
	jar cfm  $(JAR) resources/Manifest -C bin gui
	jar i $(JAR) 

run:
	java -Xms64M -Xmx256M -jar $(JAR)

clean:
	find . -name \*.class -exec rm {} \;
	rm -f $(JAR)
	rm -rf bin

