var tab = [
"This option activates the help menu <br>( <em>--help | -h</em> )",
"This option selects if JCHRONOSS have to be run in master or slave mode. Careful about this option, master option is probably what you want to do <br>( <em>--master | --slave</em> )",
"This option allows you to specify the configuration file to load with JCHRONOSS as default global configuration. Will be overrided by command line options <br>( <em>--config-file</em> )",
"This option sets the verbosity level for the whole execution <br>( <em>--verbosity</em> )",
"This option sets the file logging level for the whole execution <br>( <em>--keep</em> )",
"You can specify as many files as you want, separated with a comma. These files have to contain jobs you want to execute <br>( <em>--white</em> )",
"You can specify as many files as you want, separated with a comma. These files have to contain jobs you <strong>DO NOT</strong> want to execute <br>( <em>--black</em> )",
"Used for debug, this option choose a random value and use it as job execution. This is useful to quickly check a scheduling policy and see its behaviour on a new machine <br>( <em>--fake | --no-fake</em> )",
"This option specifies where build files (temporary, artefacts), will be stored <br>( <em>--build</em> )",
"This option specifies where JUnit and traces files will have to be stored <br>( <em>--output</em> )",
"The most important option. Need to be set (except if you have provided a configuration file). This option set the max number of resources JCHRONOSS can use <br>( <em>--nb-resources</em> )",
"This option defines the number of simulatenous instance of slave JCHRONOSS which can be launched. A slave is a request to the job manager. You can use this option if your job manager limits the max number of simultaneous requests <br>( <em>--max-slaves</em> )",
"This option sets the max time allowed to a slave instance, an autokill is set and abort the slave if not finished. In case of not run jobs, they will be scheduled again <br>( <em>--maxt-slave</em> )",
"This option set the min time for a slave instance. This option is used by some scheduling policy to avoid request wasting : not enough work for a specific launch. <br>( <em>--mint-slave</em> )",
"This option set the temporary script to use between master and slave instance. This script depends on your job manager. It is used as the interface between JCHRONOSS and your machine. Some scripts have been written, check tools/launchers folder. <br>( <em>--launcher</em> )",
"This option set the temporary script to use between master and slave instance. This script depends on your job manager. It is used as the interface between JCHRONOSS and your machine <strong>ONLY for jobs marked as compilation</strong>. Thus, JCHRONOSS can optimise dependance resolution and execution time. Some scripts have been written, check tools/launchers/compilation folder. <br>( <em>--compil-launcher</em> )",
"This option allows to select scheduling policy to use: <ul><li>The default one is sequential ;</li><li>The scheduling by resources gathers similar jobs to optimize job manager workload ;</li><li>The scheduling by time is the most implemented scheduling policy, gathers jobs whatever their specifications, looking for minimize execution time ;</li><li>The custom policy is defined by the user.</li></ul> <br>( <em>--policy</em> )",
"This option define the max time allowed to JCHRONOSS to pass the whole validation. In case of overtime, JCHRONOSS is stopped and current results are stored in file <br>( <em>--autokill</em> )",
"No help info have been found for this option. Sorry :(",
"This option sets the JCHRONOSS path in order to generate a valid command, working with a simple copy-paste.",
"This option sets max time allowed to a job for execution. This is used by somes policies to estimate global execution time. <br>( <em>--maxt-job</em> )"
];

function onMouseOver(element, e){
	var div = document.getElementById("popup");
	div.style.position = 'absolute';
	div.style.display='';
	div.style.left = e.clientX+"px";
	div.style.top = e.clientY+"px";

	switch(element.name){
		case "help":
			div.innerHTML = tab[0];
			break;
		case "exec":
			div.innerHTML = tab[1];
			break;
		case "config":
			div.innerHTML = tab[2];
			break;
		case "verb":
			div.innerHTML = tab[3];
			break;
		case "log":
			div.innerHTML = tab[4];
			break;
		case "white":
			div.innerHTML = tab[5];
			break;
		case "black":
			div.innerHTML = tab[6];
			break;
		case "fake":
			div.innerHTML = tab[7];
			break;
		case "build":
			div.innerHTML = tab[8];
			break;
		case "output":
			div.innerHTML = tab[9];
			break;
		case "maxres":
			div.innerHTML = tab[10];
			break;
		case "maxslaves":
			div.innerHTML = tab[11];
			break;
		case "maxtslave":
			div.innerHTML = tab[12];
			break;
		case "mintslave":
			div.innerHTML = tab[13];
			break;
		case "launcher":
			div.innerHTML = tab[14];
			break;
		case "clauncher":
			div.innerHTML = tab[15];
			break;
		case "sched":
			div.innerHTML = tab[16];
			break;
		case "autokill":
			div.innerHTML = tab[17];
			break;
		case "jchronoss":
			div.innerHTML = tab[19];
			break;
		case "job":
			div.innerHTML = tab[20];
			break;
		default:
			div.innerHTML = tab[18]; 
			break;
	}
}

function onMouseOut(){
	document.getElementById("popup").style.display = 'none';
}

function submit(){
	var res = document.getElementById("result");
	var val;
	
	res.value = "";
	if(!check_valid_data())
		return;

	res.focus();
	if((val = document.forms["fillingForm"]["execfile"].value) != ""){
		res.value = val;
	} else {
		res.value = "./jchronoss";
	}

	if(document.getElementById("short").checked){
		if(document.forms["fillingForm"]["help"][0].checked){
			res.value += " -h";
		}
		if(document.forms["fillingForm"]["execmode"][1].checked){
			res.value += " -S";
		}
		if((val = document.forms["fillingForm"]["configfile"].value) != ""){
			res.value += " -c"+val;
		}
	
		res.value += " -Y"+document.forms["fillingForm"]["verbosity"].selectedIndex;
		res.value += " -K"+document.forms["fillingForm"]["keep"].selectedIndex;
	
		if((val = document.forms["fillingForm"]["whitelist"].value) != ""){
			res.value += " -w"+val;
		}
		if((val = document.forms["fillingForm"]["blacklist"].value) != ""){
			res.value += " -b"+val;
		}
		if((val = document.forms["fillingForm"]["job"].value) != ""){
			res.value += " -t"+val;
		}
		if(document.forms["fillingForm"]["fake"][0].checked){
			res.value += " -f";
		}
		if((val = document.forms["fillingForm"]["build"].value) != ""){
			res.value += " -B"+val;
		}
		if((val = document.forms["fillingForm"]["output"].value) != ""){
			res.value += " -o"+val;
		}
		if((val = document.forms["fillingForm"]["resources"].value) != ""){
			res.value += " -r"+val;
		}
		if((val = document.forms["fillingForm"]["maxslaves"].value) != ""){
			res.value += " -j"+val;
		}
		if((val = document.forms["fillingForm"]["maxtslave"].value) != ""){
			res.value += " -T"+val;
		}
		if((val = document.forms["fillingForm"]["mintslave"].value) != ""){
			res.value += " -m"+val;
		}
		if((val = document.forms["fillingForm"]["launcher"].value) != ""){
			res.value += " -L"+val;
		}
		if((val = document.forms["fillingForm"]["clauncher"].value) != ""){
			res.value += " -C"+val;
		}
		
		res.value += " -p"+document.forms["fillingForm"]["policy"].selectedIndex;
	
		if((val = document.forms["fillingForm"]["autokill"].value) != ""){
			res.value += " -k"+val;
		}
	} else {
		if(document.forms["fillingForm"]["help"][0].checked){
			res.value += " --help ";
		}
		if(document.forms["fillingForm"]["execmode"][1].checked){
			res.value += " --slave";
		}
		if((val = document.forms["fillingForm"]["configfile"].value) != ""){
			res.value += " --config-file="+val;
		}
	
		res.value += " --verbosity="+document.forms["fillingForm"]["verbosity"].selectedIndex;
		res.value += " --keep="+document.forms["fillingForm"]["keep"].selectedIndex;
	
		if((val = document.forms["fillingForm"]["whitelist"].value) != ""){
			res.value += " --white="+val;
		}
		if((val = document.forms["fillingForm"]["blacklist"].value) != ""){
			res.value += " --black="+val;
		}
		if((val = document.forms["fillingForm"]["job"].value) != ""){
			res.value += " --maxt-job="+val;
		}
		if(document.forms["fillingForm"]["fake"][0].checked){
			res.value += " --fake";
		}
		if((val = document.forms["fillingForm"]["build"].value) != ""){
			res.value += " --build="+val;
		}
		if((val = document.forms["fillingForm"]["output"].value) != ""){
			res.value += " --output="+val;
		}
		if((val = document.forms["fillingForm"]["resources"].value) != ""){
			res.value += " --nb-resources="+val;
		}
		if((val = document.forms["fillingForm"]["maxslaves"].value) != ""){
			res.value += " --nb-slaves="+val;
		}
		if((val = document.forms["fillingForm"]["maxtslave"].value) != ""){
			res.value += " --maxt-slave="+val;
		}
		if((val = document.forms["fillingForm"]["mintslave"].value) != ""){
			res.value += " --mint-slave="+val;
		}
		if((val = document.forms["fillingForm"]["launcher"].value) != ""){
			res.value += " --launcher="+val;
		}
		if((val = document.forms["fillingForm"]["clauncher"].value) != ""){
			res.value += " --compil-launcher="+val;
		}
		
		res.value += " --policy="+document.forms["fillingForm"]["policy"].selectedIndex;
	
		if((val = document.forms["fillingForm"]["autokill"].value) != ""){
			res.value += " --autokill="+val;
		}
	}			
	res.value += " "+document.forms["fillingForm"]["jobfiles"].value.replace(/\n/g, " ");
}

function check_valid_data() {
	if(!document.forms["fillingForm"]["configfile"].value == "" || /^[0-9]+$/.test(document.forms["fillingForm"]["resources"].value)){
		document.getElementById("resources-info").style.color="black";
	} else {
		document.getElementById("resources-info").style.color="red";
		return false;
		}

	return true;
}
