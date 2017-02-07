function submit(){
	var res = document.getElementById("result");
	var val;
	res.value = "";
	if(!check_valid_data())
		return;

	res.focus();
	res.value = "<configuration>\n\t<job>\n";
	
	res.value += "\t\t<verbosity>"+document.forms["fillingForm"]["verbosity"].selectedIndex+"</verbosity>\n";
	res.value += "\t\t<logging>"+document.forms["fillingForm"]["keep"].selectedIndex+"</logging>\n";

	if((val = document.forms["fillingForm"]["whitelist"].value) != ""){
		res.value += "\t\t<whitelist>"+val+"</whitelist>\n";
	}
	if((val = document.forms["fillingForm"]["blacklist"].value) != ""){
		res.value += "\t\t<blacklist>"+val+"</blacklist>\n";
	}
	if((val = document.forms["fillingForm"]["job"].value) != ""){
		res.value += "\t\t<maxJobTime>"+val+"</maxJobTime>\n";
	}
	if(document.forms["fillingForm"]["fake"][0].checked){
		res.value += "\t\t<fakeExecution>true</fakeExecution>\n";
	}
	res.value += "\t\t<jobslist>"+document.forms["fillingForm"]["jobfiles"].value.replace(/\n/g, " ")+"</jobslist>\n";
	
	res.value += "\t</job>\n\t<system>\n";

	if((val = document.forms["fillingForm"]["build"].value) != ""){
		res.value += "\t\t<build>"+val+"</build>\n";
	}
	if((val = document.forms["fillingForm"]["output"].value) != ""){
		res.value += "\t\t<output>"+val+"</output>\n";
	}
	if((val = document.forms["fillingForm"]["resources"].value) != ""){
		res.value += "\t\t<maxResources>"+val+"</maxResources>\n";
	}
	if((val = document.forms["fillingForm"]["maxslaves"].value) != ""){
		res.value += "\t\t<maxSlaves>"+val+"</maxSlaves>\n";
	}
	if((val = document.forms["fillingForm"]["maxtslave"].value) != ""){
		res.value += "\t\t<maxSlaveTime>"+val+"</maxSlaveTime>\n";
	}
	if((val = document.forms["fillingForm"]["mintslave"].value) != ""){
		res.value += "\t\t<minSlaveTime>"+val+"/<minSlaveTime>\n";
	}
	if((val = document.forms["fillingForm"]["launcher"].value) != ""){
		res.value += "\t\t<jobsCommand>"+val+"</jobsCommand>\n";
	}
	if((val = document.forms["fillingForm"]["clauncher"].value) != ""){
		res.value += "<compilationCommand>"+val+"</compilationCommand>\n";
	}
	res.value += "\t\t<policy>"+document.forms["fillingForm"]["policy"].selectedIndex+"</autokill>\n";

	if((val = document.forms["fillingForm"]["autokill"].value) != ""){
		res.value += "\t\t<autokill>"+val+"</autokill\n";
	}
	
	res.value += "\t</system>\n</configuration>\n";
}

function check_valid_data() {
	if(/^[0-9]+$/.test(document.forms["fillingForm"]["resources"].value)){
		document.getElementById("resources-info").style.color="black";
	} else {
		document.getElementById("resources-info").style.color="red";
		return false;
	}

	return true;
}
