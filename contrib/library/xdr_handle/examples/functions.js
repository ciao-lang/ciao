<script language="JavaScript">

function do_and(field) {
  opts=document.data['on_'+field];
  if ((opts.options[opts.selectedIndex].value != "ignore") &&
      (document.data['exp_'+field].value != ""))
      {	
       document.data['exp_'+field].value += ' and ' ;
       }
}

function do_or(field) {
  opts=document.data['on_'+field];
  if ((opts.options[opts.selectedIndex].value != "ignore") &&
      (document.data['exp_'+field].value != ""))
      {	
       document.data['exp_'+field].value += ' or ' ;
       }
}

function do_ok(field, name) {
  opts=document.data['on_'+field];	
  if (document.data[field].value != "")
     {
      if (opts.options[opts.selectedIndex].value != "ignore")
	 {
	 document.data['exp_'+field].value += 
	 name +opts.options[opts.selectedIndex].value + 
         document.data[field].value ; 
	 }
      }
}
function do_lpar(field) {
  opts=document.data['on_'+field];
  if (opts.options[opts.selectedIndex].value != "ignore")
      {	
       document.data['exp_'+field].value += '(' ;
       }
}

function do_rpar(field) {
  opts=document.data['on_'+field];
  if ((opts.options[opts.selectedIndex].value != "ignore") &&
      (document.data['exp_'+field].value != ""))
      {	
       document.data['exp_'+field].value += ')' ;
       }
}

function do_ok_date(sep, field, field1, field2, field3, name) {
  opts=document.data['on_'+field];
  if ((document.data[field1].value != "") &&
      (document.data[field2].value != "") &&
      (document.data[field3].value != ""))
     {
      if (opts.options[opts.selectedIndex].value != "ignore")
	 {
	 document.data['exp_'+field].value += 
	 name +opts.options[opts.selectedIndex].value + 
	 document.data[field1].value + sep +
	 document.data[field2].value + sep +
	 document.data[field3].value ; 
	 }
     }	 
}


</script>
