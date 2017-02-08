var connector;

function attempt_disconnect()
{
	connector.close();
}
function attempt_connect()
{
	var resbox = document.getElementById("boxinfo");
	var button = document.getElementById("button-form");
	var input = document.getElementById("url");
	var regex = "^.*:[0-9]+$";
	var url = "ws://" + input.value.replace(/\s/g, '');
	var reg = new RegExp(regex);

	if(! reg.test(input.value))
	{
		update_warn("Wrong format ! Your URL has to match with <strong>'"+regex+"'</strong>'");
		return;
	}

	try
	{
		connector = new WebSocket(url);
	}catch(e)
	{
		if(e.name == 'SecurityError')
		{
			resbox.innerHTML = "<strong>Error: </strong> The browser returned a Security Error. This probably happens because you specified a port below 1024 but JCHRONOSS should not be bound to this port range.";
			resbox.className = "alert alert-danger";
			button.className="btn btn-danger";
		}
		else{
			resbox.innerHTML = "<strong>Error: </strong> The browser returned an error blocking the WebSocket creation. Be sure of the URL you provided and that JCHRONOSS Log Server is up.";
			resbox.className = "alert alert-danger";
			button.className="btn btn-danger";
		};
		return;
	}


	button.className = "btn btn-default disabled";
	button.firstChild.data = "Connecting";
	connector.onerror = onerror_handler;
	connector.onopen = onopen_handler;
	connector.onmessage = onmessage_handler;
	connector.onclose = onclose_handler;
}

var timeout;
var timeout_enabled = 0;
var refresh_value = 2000; /* 10 seconds */

function refresh_timeout()
{
	var button = document.getElementById("refreshButton");
	connector.send('polling');
	timeout = setTimeout("refresh_timeout()", refresh_value);
	button.className = "btn btn-default pull-right";
	button.firstChild.data = "Disable Auto-refresh";

}
function toggle_timeout()
{
	var button = document.getElementById("refreshButton");
	if(timeout_enabled == 0)
	{
		timeout = setTimeout("refresh_timeout()",refresh_value);
		button.className = "btn btn-default pull-right";
		button.firstChild.data = "Disable Auto-refresh";
	}
	else
	{
		clearTimeout(timeout);
		button.className = "btn btn-info pull-right";
		button.firstChild.data = "Enable Auto-refresh";
	}
	timeout_enabled = !timeout_enabled;
}

function reset(message)
{
	var resbox = document.getElementById("boxinfo");
	var button = document.getElementById("button-form");

	if(message != "")
	{
		resbox.innerHTML = "<Strong>Info:</strong> " + message;
		resbox.className = "alert alert-info";
	}
	button.className = "btn btn-primary";
	button.firstChild.data = "Connect";
	button.onclick = attempt_connect;
}

function init()
{
	var resbox = document.getElementById("boxinfo");
	var button = document.getElementById("button-form");

	if(!window.WebSocket)
	{
		resbox.innerHTML = "Your browser does not handle HTML 5 WebSockets implementation.";
		resbox.className = "alert alert-danger disabled";
		button.className = "btn btn-primary disabled";
		button.onclick = function() {};
	}
	else
	{
		reset("");
	}
}

function update_bad(text)
{
	var resbox = document.getElementById("boxinfo");
	var button = document.getElementById("button-form");

	resbox.innerHTML = "<strong>Error: </strong>The connection failed with error '"+text+"'. Please be sure to provide a valid address following the <strong>'address:port'</strong> format and check the JCHRONOSS Log Server is still up.";
	resbox.className = "alert alert-danger";
	button.className="btn btn-danger";
	button.firstChild.data = "Connect";
	button.onclick = attempt_connect;
}

function update_good(text)
{
	var resbox = document.getElementById("boxinfo");
	var button = document.getElementById("button-form");

	resbox.innerHTML = "<strong>Success: </strong>" + text;
	resbox.className = "alert alert-success";
	button.className="btn btn-success";
	button.firstChild.data = "Disconnect";
	button.onclick = attempt_disconnect;
}

function update_warn(text)
{
	var resbox = document.getElementById("boxinfo");
	var button = document.getElementById("button-form");

	resbox.innerHTML = "<strong>Warning: </strong>"+text;
	resbox.className = "alert alert-warning";
	button.className="btn btn-warning";
}

function onerror_handler(e)
{
	console.log("OnError()");
	update_bad(e.name);
	last_bad = 1;
}

var last_bad = 0;
var just_open = 0;
function onopen_handler(e)
{
	var resbox = document.getElementById("boxinfo");
	var button = document.getElementById("button-form");
	console.log("OnOpen()");
	update_good("Connection Successful !");
	just_open = 1;
	toggle_timeout();
}

function build_config_table(obj)
{
	var table = document.getElementById("configTable");
	var row_count = 0;
	var row, key, value;

	//row = table.insertRow(row_count++); key = row.insertCell(0); value = row.insertCell(1);
	//key.innerHTML = "Total number of jobs";
	//value.innerHTML = obj["nbJobs"];

	row = table.insertRow(row_count++); key = row.insertCell(0); value = row.insertCell(1);
	key.innerHTML = "Total number of Resources";
	value.innerHTML = obj["configSystem"]["maxResources"];

	row = table.insertRow(row_count++); key = row.insertCell(0); value = row.insertCell(1);
	key.innerHTML = "Max number of concurrent slaves";
	value.innerHTML = obj["configSystem"]["nbmaxslaves"] + " (0 meaning 'unlimited')";

	row = table.insertRow(row_count++); key = row.insertCell(0); value = row.insertCell(1);
	key.innerHTML = "Overall Autokill";
	value.innerHTML = obj["configSystem"]["autokill"] + " second(s)";

	row = table.insertRow(row_count++); key = row.insertCell(0); value = row.insertCell(1);
	key.innerHTML = "Output Formats";
	value.innerHTML = obj["configSystem"]["outputFormats"];

	row = table.insertRow(row_count++); key = row.insertCell(0); value = row.insertCell(1);
	key.innerHTML = "Max output log size";
	value.innerHTML = obj["configSystem"]["maxbytes"] + " byte(s)";

	row = table.insertRow(row_count++); key = row.insertCell(0); value = row.insertCell(1);
	key.innerHTML = "Build Directory";
	value.innerHTML = obj["configSystem"]["build"];

	row = table.insertRow(row_count++); key = row.insertCell(0); value = row.insertCell(1);
	key.innerHTML = "Output Directory";
	value.innerHTML = obj["configSystem"]["output"];

	row = table.insertRow(row_count++); key = row.insertCell(0); value = row.insertCell(1);
	key.innerHTML = "Selected Policy";
	value.innerHTML = obj["configSystem"]["policy"];

	row = table.insertRow(row_count++); key = row.insertCell(0); value = row.insertCell(1);
	key.innerHTML = "Verbosity Level";
	value.innerHTML = obj["configJob"]["verbosity"];

	row = table.insertRow(row_count++); key = row.insertCell(0); value = row.insertCell(1);
	key.innerHTML = "Average time per Job";
	value.innerHTML = obj["configJob"]["maxJobTime"];

	row = table.insertRow(row_count++); key = row.insertCell(0); value = row.insertCell(1);
	key.innerHTML = "Output Logging level";
	value.innerHTML = obj["configJob"]["logging"];
}

function build_test_table(obj)
{
	var content = document.getElementById("data_array");
	content.className = "";

	for(var test in obj["testsuite"])
	{
		var row_short, row_details, name, statut, timei, details;
		var test_name = obj["testsuite"][test]["name"];
		var status_name = obj["testsuite"][test]["status"]

		row = content.insertRow(-1); name = row.insertCell(0); statut = row.insertCell(1); time = row.insertCell(2);
		name.innerHTML   = "<strong>"+test_name+"</strong>";
		statut.innerHTML = "<em>"+status_name+"</em>";
		time.innerHTML   = obj["testsuite"][test]["time"].toFixed(2);
		
		row.onclick = function(s) {return function() {toggle_visibility( s + "-details");};}(test_name);
		
		row_details = content.insertRow(-1); details = row_details.insertCell(0);
		row_details.style.display = "none";
		row_details.id = obj["testsuite"][test]["name"]+"-details";
		details.colSpan = 3;
		details.innerHTML="<h4>Command : </h4><pre>"+obj["testsuite"][test]["command"]+"</pre>";
		
		if("log" in obj["testsuite"][test])
		{
		details.innerHTML += "<h4>Trace : </h4><pre>"+obj["testsuite"][test]["log"]+"</pre>";
		}


		if(status_name == "success")
		{
			row.className = "success";
			row_details.className = "succeed-details success";
		}
		else if(status_name == "skipped")
		{
			row.className = "warning";
			row_details.className = "skipped-details warning";
		}
		else if(status_name == "failure")
		{
			row.className = "danger";
			row_details.className = "failed-details danger";
		}
		else
		{
			row.className = "danger";
			row_details.className = "error-details danger";
		}
	}
}

function onmessage_handler(e)
{
	if(e.data == "EOS")
	{
		connector.close();
		return;
	}
	
	console.log("OnMessage()");
	var jsontab = JSON.parse(e.data);
	if(just_open == 1)
	{
		just_open = 0;
		var config = document.getElementById("config");
		config.style.display = "block";
		build_config_table(jsontab);
	}
	else
	{
		build_test_table(jsontab);
	}
}

function onclose_handler(e)
{
	var resbox = document.getElementById("boxinfo");
	var button = document.getElementById("button-form");
	console.log("OnClose()");
	if(last_bad == 0)
		reset("Connection to remote Log Server has been closed.");

	last_bad = 0;


}

$(document).ready(function() {
	$('form input').keydown(function(event){
		if(event.keyCode == 13) {
			event.preventDefault();
			return false;
		}
	});
});
