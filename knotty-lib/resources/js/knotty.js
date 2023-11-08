function toggleVisibility(id) {
  var panel = document.getElementById(id);
  var check = document.getElementById(id + '_checkbox');
  panel.style.display = (panel.style.display == 'block') ? 'none' : 'block';
  check.checked = (panel.style.display == 'block');
}

function zoom() {
  var slider = document.getElementById('slider');
  var chart = document.getElementById('figure');
  var x = slider.value;
  chart.style.scale = x / 80 + ' ' + x / 100;
  chart.style.transition = '0.4s';
}

function inputFloat() {
  var span = document.getElementById('float-span');
  var float = document.getElementById('float');
  var button = document.getElementById('button');
  float.style.display = 'block';
  button.innerHTML = 'Max float length';
  button.disabled = true;
  span.classList.add('active');
}

function submitForm() {
  var div = document.getElementById('resizable');
  var input = document.getElementById('size');
  var form = document.getElementById('form');
  input.value = div.style.height.replace('px', '');
  form.submit();
}

onload = (event) => {
  zoom();
}