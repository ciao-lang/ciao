function set_status(msg) {
  var div = document.getElementById('status');
  div.innerHTML = "<b>Ciao</b>JS - " + msg;
  if (div.style.display === 'none') {
    div.style.display = 'block';
  }
}
function clear_status() {
  var div = document.getElementById('status');
  div.style.display = 'none';
}
set_status("Loading... Please wait");
