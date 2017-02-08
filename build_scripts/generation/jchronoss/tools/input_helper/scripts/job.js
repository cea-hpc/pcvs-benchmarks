function submit(){
	var res = document.getElementById("result");
	var val;
	
	res.value = "";
	if(!check_valid_data())
		return;
	res.focus();
	res.value = "<jobSuite package=\""+document.forms["jobForm"]["package"].value+"\">\n";
	res.value += "    <job>\n";
	res.value += "        <name>"+escapeHTML(document.forms["jobForm"]["name"].value)+"</name>\n";
	res.value += "        <command>"+escapeHTML(document.forms["jobForm"]["command"].value)+"</command>\n";
	if(document.forms["jobForm"]["rc"].value == "")
		val = "0";
	else val = document.forms["jobForm"]["rc"].value;
	res.value += "        <rc>"+val+"</rc>\n";
	res.value += "        <resources>"+document.forms["jobForm"]["resources"].value+"</resources>\n";
	
	val ="";
	var array = document.forms["jobForm"]["deps"].value.replace(/\n/g, " ").split(" ");
	for(i = 0; i < array.length; i++){ 
		val += "            <dep>"+array[i]+"</dep>\n"
	}
	res.value += "        <deps>\n"+val+"        </deps>\n";
	res.value += "        <constraints>"+document.forms["jobForm"]["constraints"].options[document.forms["jobForm"]["constraints"].selectedIndex].value+"</constraints>\n";
	res.value += "    </job>\n";
	res.value += "</jobSuite>";
}

function escapeHTML(s) { 
    return s.replace(/&/g, '&amp;')
            .replace(/"/g, '&quot;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;');
}
function check_valid_data() {
	if(/^[0-9]+$/.test(document.forms["jobForm"]["resources"].value)){
		document.getElementById("resources-info").style.color="black";
	} else {
		document.getElementById("resources-info").style.color="red";
		return false;
	}

	return true;
}
