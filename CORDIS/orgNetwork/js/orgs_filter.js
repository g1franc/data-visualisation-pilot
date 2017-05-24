$(document).ready(function(){

  var FJS = FilterJS.auto(organisations);
  FJS.filter();

  window.FJS = FJS;

  FJS.addCriteria({field: 'country', ele: '#country_criteria input:checkbox'});

  $(document).bind('click', function(e) {
    console.log("function");
      var $clicked = $(e.target);
      if (! ( $clicked.hasClass("searchicon") || $clicked.hasClass("searchwindow")
          || $clicked.hasClass("search-tooltip")
          || ($(e.target).parents('.searchwindow').length > 0)
          || ($(e.target).parents('#searchbar-container').length > 0))
          || ($(e.target).parents('.FakeDropdownList').length > 0))
      {
        $('#search').hide();
        $('#dummy_div').hide();
      }

    });


    /*var currentOrg="ADANA_METROPOLITAN_MUNICIPALITY";
    $("#org_label").text("ADANA METROPOLITAN MUNICIPALITY");*/

});

function filterOrgSelected(element) {
  $("#chartID").html("");
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
	currentOrg = currentOrg.replace(/:/g,'')
  parseGexf();
}

function displayList() {
  $('#search').show();
  $('#dummy_div').show();
}

function hideList() {
    $('#search').hide();
    $('#dummy_div').hide();
}
