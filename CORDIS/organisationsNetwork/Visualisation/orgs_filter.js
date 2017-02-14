$(document).ready(function(){

  var FJS = FilterJS.auto(organisations);
  FJS.filter();

  window.FJS = FJS;

  FJS.addCriteria({field: 'country', ele: '#country_criteria input:checkbox'});

  $(document).bind('click', function(e) {
      var $clicked = $(e.target);
      if (! ( $clicked.hasClass("form-control searchBar") || $clicked.hasClass("FakeDropdownList") ) )
          $("#filterOrgsList").hide();
    });

});

function filterOrgSelected(element) {
  //alert(element.getElementsByTagName('p')[0].innerHTML);
  currentOrg = element.getElementsByTagName('p')[0].innerHTML;
  $("#org_label").text(currentOrg);
  currentOrg = currentOrg.replace(/ /g,'_');
  currentOrg = currentOrg.replace(/\*/g,'')
	currentOrg = currentOrg.replace(/\//g,'')
	currentOrg = currentOrg.replace(/\'/g,'')
	currentOrg = currentOrg.replace(/\"/g,'')
	currentOrg = currentOrg.replace(/,/g,'')
	currentOrg = currentOrg.replace(/&/g,'')
  D3ok();
}

function displayList() {
  $('#filterOrgsList').show()
}

function hideList() {
  $('#filterOrgsList').hide()
}
