$(document).ready(function(){

  var organisations = [
  {
    "name": "UCL",
    "country": "Belgium",
    "activity": "University"
  },
  {
    "name": "PwC",
    "country": "United Kingdom",
    "activity": "Consulting"
  },
  {
    "name": "University of Vienna",
    "country": "Austria",
    "activity": "University"
  },
  {
    "name": "French consulting",
    "country": "France",
    "activity": "Consulting"
  },
  {
    "name": "ULB",
    "country": "Belgium",
    "activity": "University"
  },
  {
    "name": "University of Paris",
    "country": "France",
    "activity": "University"
  },
  {
    "name": "ULg",
    "country": "Belgium",
    "activity": "University"
  },
  {
    "name": "Austia Consulting",
    "country": "Austria",
    "activity": "Consulting"
  }
];

  /*var FJS = FilterJS(organisations, '#orgs', {
    template: '#org-template',
    filter_on_init: true, // Default filter_on_init is false
  });*/

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
