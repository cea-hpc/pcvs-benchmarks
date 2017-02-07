function generate_diff_summary(){
	var nbUnchangedSuccess = document.getElementsByClassName('unchanged_success').length;
	var nbUnchangedFailure = document.getElementsByClassName('unchanged_failure').length;
	var nbFixed = document.getElementsByClassName('fixed').length;
	var nbRegressFail = document.getElementsByClassName('regress_failure').length;
	var nbRegressPerf = document.getElementsByClassName('regress_perf').length;
	var nbTotal = nbUnchangedSuccess+nbUnchangedFailure+nbFixed+nbRegressFail+nbRegressPerf;

	document.getElementById('nb_unchanged_success').innerHTML = nbUnchangedSuccess;	
	document.getElementById('nb_unchanged_failure').innerHTML = nbUnchangedFailure;	
	document.getElementById('nb_fixed').innerHTML = nbFixed;	
	document.getElementById('nb_regress_failure').innerHTML = nbRegressFail;	
	document.getElementById('nb_regress_perf').innerHTML = nbRegressPerf;	
	
	document.getElementById('nb_total').innerHTML = nbTotal;
	
	document.getElementById('bar_unchanged_success').setAttribute("style", "width: " + Math.round(100*(nbUnchangedSuccess/nbTotal)) + '%;');	
	document.getElementById('bar_unchanged_failure').setAttribute("style", "width: " + Math.round(100*(nbUnchangedFailure/nbTotal)) + '%;');	
	document.getElementById('bar_fixed').setAttribute("style", "width: " + Math.round(100*(nbFixed/nbTotal)) + '%;');	
	document.getElementById('bar_regress_perf').setAttribute("style", "width: " + Math.round(100*(nbRegressPerf/nbTotal)) + '%;');
	document.getElementById('bar_regress_failure').setAttribute("style", "width: " + Math.round(100*(nbRegressFail/nbTotal)) + '%;');	

	document.getElementById('percent_unchanged_success').innerHTML = Math.round(100*(nbUnchangedSuccess/nbTotal)) + ' %';	
	document.getElementById('percent_unchanged_failure').innerHTML = Math.round(100*(nbUnchangedFailure/nbTotal)) + ' %';	
	document.getElementById('percent_fixed').innerHTML = Math.round(100*(nbFixed/nbTotal)) + ' %';	
	document.getElementById('percent_regress_failure').innerHTML = Math.round(100*(nbRegressFail/nbTotal)) + ' %';	
	document.getElementById('percent_regress_perf').innerHTML = Math.round(100*(nbRegressPerf/nbTotal)) + ' %';	
}

function toggle_visibility(id)
{
	var e = document.getElementById(id);
	if(e.style.display == 'none')
		e.style.display = '';
	else
		e.style.display = 'none';
}

function showOnly(message)
{
	var tabMessage = new Array("success","error","failed","skipped", "unchanged_success","unchanged_failure", "fixed", "regress_failure", "regress_perf");
	for(var cpt = 0; cpt<tabMessage.length; cpt++){
		if(message == tabMessage[cpt] || message == "*"){
			var Obj1 = document.getElementsByClassName(tabMessage[cpt]+"-details");
			var Obj2 = document.getElementsByClassName(tabMessage[cpt]);
			for(var i=0 ; i<Obj1.length ; i++)
			{
				if (Obj1[i].tagName == 'TR') Obj1[i].style.display = 'none';
			}
			for(var i=0 ; i<Obj2.length ; i++)
			{
				if (Obj2[i].tagName == 'TR') Obj2[i].style.display = '';
			}
		} else {
			var Obj1 = document.getElementsByClassName(tabMessage[cpt]+"-details");
			var Obj2 = document.getElementsByClassName(tabMessage[cpt]);
		
			for(var i=0 ; i<Obj1.length ; i++)
			{
				if (Obj1[i].tagName == 'TR') Obj1[i].style.display = 'none';
			}
			for(var i=0 ; i<Obj2.length ; i++)
			{
				if (Obj2[i].tagName == 'TR') Obj2[i].style.display = 'none';
			}
		}

		var title = document.getElementById("title_results");
		if(message == '*') title.innerHTML = "";
		else title.innerHTML = " (Only "+message+")";
	}
}
