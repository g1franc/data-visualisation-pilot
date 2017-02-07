$(document).ready(function(){

  var FJS = FilterJS.auto(organisations);
  FJS.filter();

  window.FJS = FJS;

  FJS.addCriteria({field: 'country', ele: '#country_criteria input:checkbox'});

});

function filterOrgSelected(element) {
  alert(element.getElementsByTagName('p')[0].innerHTML);
}

function displayList() {
  $('#filterOrgsList').show()
}

function hideList() {
  $('#filterOrgsList').hide()
}
