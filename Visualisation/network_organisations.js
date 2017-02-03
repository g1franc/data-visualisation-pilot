// For MSIE < 9, forget it
function D3notok() {
  document.getElementById('sidepanel').style.visibility = 'hidden';
  var nocontent = document.getElementById('nocontent');
  nocontent.style.visibility = 'visible';
  nocontent.style.pointerEvents = 'all';
  var t = document.getElementsByTagName('body');
  var body = document.getElementsByTagName('body')[0];
  body.style.backgroundImage = "url('movie-network-screenshot-d.png')";
  body.style.backgroundRepeat = "no-repeat";
}

// -------------------------------------------------------------------
// A number of forward declarations. These variables need to be defined since
// they are attached to static code in HTML. But we cannot define them yet
// since they need D3.js stuff. So we put placeholders.

// Highlight a movie in the graph. It is a closure within the d3.json() call.
var SelectCountry = undefined;

// Change status of a panel from visible to hidden or viceversa
var toggleDiv = undefined;

// The call to set a zoom value -- currently unused
// (zoom is set via standard mouse-based zooming)
var zoomCall = undefined;

//selected contract
var e = document.getElementById("orgDropdown");
var currentOrg = e.options[e.selectedIndex].value;

// -------------------------------------------------------------------

// Do the stuff -- to be called after D3.js has loaded
function D3ok() {

  // Some constants
  var WIDTH = 988,
      HEIGHT = 700,
      SHOW_THRESHOLD = 2.5;
      ANIMATION_TIME = 100;

  // Variables keeping graph state
  var activeCountry = undefined;
  var currentZoom = 0.5;
  var currentOffset = { x : 250, y : 150 };

  // The D3.js scales
  var xScale = d3.scale.linear()
    .domain([0, WIDTH])
    .range([0, WIDTH]);
  var yScale = d3.scale.linear()
    .domain([0, HEIGHT])
    .range([0, HEIGHT]);
  var zoomScale = d3.scale.linear()
    .domain([0.25,10])
    .range([0.25,10])
    .clamp(true);

/* .......................................................................... */

  // The D3.js force-directed layout
  var force = d3.layout.force()
    .charge(-320)
    .size( [WIDTH, HEIGHT] )
    .linkStrength( function(d,idx) { return /*d.weight*/ 1; } );
    //.friction(0.5);

  var myNode = document.getElementById("chartID");
  while (myNode.firstChild) {
      myNode.removeChild(myNode.firstChild);
  }

  // Add to the page the SVG element that will contain the network
  var svg = d3.select("#chartID").append("svg:svg")
    .attr('xmlns','http://www.w3.org/2000/svg')
    .attr("width", WIDTH)
    .attr("height", HEIGHT)
    .attr("id","graph")
    .attr("viewBox", "0 0 " + WIDTH + " " + HEIGHT )
    .attr("preserveAspectRatio", "xMidYMid meet");

  CountryInfoDiv = d3.select("#countryInfo");

  /* ....................................................................... */

  // Get the current size & offset of the browser's viewport window
  function getViewportSize( w ) {
    var w = w || window;
    if( w.innerWidth != null )
      return { w: w.innerWidth,
	       h: w.innerHeight,
	       x : w.pageXOffset,
	       y : w.pageYOffset };
    var d = w.document;
    if( document.compatMode == "CSS1Compat" )
      return { w: d.documentElement.clientWidth,
	       h: d.documentElement.clientHeight,
	       x: d.documentElement.scrollLeft,
	       y: d.documentElement.scrollTop };
    else
      return { w: d.body.clientWidth,
	       h: d.body.clientHeight,
	       x: d.body.scrollLeft,
	       y: d.body.scrollTop};
  }

  function getQStringParameterByName(name) {
    var match = RegExp('[?&]' + name + '=([^&]*)').exec(window.location.search);
    return match && decodeURIComponent(match[1].replace(/\+/g, ' '));
  }

  /* Change status of a panel from visible to hidden or viceversa
     id: identifier of the div to change
     status: 'on' or 'off'. If not specified, the panel will toggle status
  */
  toggleDiv = function( id, status ) {
    d = d3.select('div#'+id);
    console.log( 'TOGGLE', id, d.attr('class'), '->', status );
    if( status === undefined )
      status = d.attr('class') == 'panel_on' ? 'off' : 'on';
    d.attr( 'class', 'panel_' + status );
    return false;
  }

  /* Select a country in the network and in the country details panel
  */
  clearAndSelect = function (id) {
    SelectCountry(id,true); // we use here the SelectCountry() closure
  }

  /* Compose the content for the panel with country details.
     Parameters: the node data, and the array containing all nodes
  */
  function getCountryInfo( n, nodeArray ) {
    info = '<div id="cover">';
    info += '<div class=t style="float: right">' + n.label + '</div>';
    info += '<img src="close.png" class="action" style="top: 0px;" title="close panel" onClick="toggleDiv(\'countryInfo\');"/>'
    info += '<br/></div>';
    return info;
  }


  // *************************************************************************

  var datafile = "./Data/"+currentOrg+".json";

  d3.json(
    datafile,
    function(data) {
      // Declare the variables pointing to the node & link arrays
      var nodeArray = data.nodes;
      var linkArray = data.links;

      minLinkWeight =
        Math.min.apply( null, linkArray.map( function(n) {return n.weight;} ) );
      maxLinkWeight =
        Math.max.apply( null, linkArray.map( function(n) {return n.weight;} ) );

      // Add the node & link arrays to the layout, and start it
      force.nodes(nodeArray).links(linkArray).start();

      // A couple of scales for node radius & edge width
      var node_size = d3.scale.linear()
        .domain([0,50])	// we know score is in this domain
        .range([1,50])
        .clamp(true);
      var edge_width = d3.scale.pow().exponent(8)
        .domain( [minLinkWeight,maxLinkWeight] )
        .range([1,39])
        .clamp(true);

      /* Add drag & zoom behaviours */
      svg.call( d3.behavior.drag().on("drag",dragmove) );

      // ------- Create the elements of the layout (links and nodes) ------
      var networkGraph = svg.append('svg:g').attr('class','grpParent');

      // links: simple lines
      var graphLinks = networkGraph.append('svg:g').attr('class','grp gLinks')
        .selectAll("line")
        .data(linkArray, function(d) {return d.source.id+'-'+d.target.id;} )
        .enter().append("line")
        .style('stroke-width', function(d) { return edge_width(d.weight);} )
        .attr("class", "link");

      // nodes: an SVG circle
      var graphNodes = networkGraph.append('svg:g').attr('class','grp gNodes')
        .selectAll("circle")
        .data( nodeArray, function(d){ return d.id; } )
        .enter().append("svg:circle")
        .attr('id', function(d) { return "c" + d.index; } )
        .attr('class', function(d) { console.log(d.level);return 'level'+d.level;} )
        .attr('r', function(d) { return node_size(parseFloat(d.score) || 3); } )
        .attr('pointer-events', 'all')
        .on("click", function(d) { showMoviePanel(d); } )
        .on("mouseover", function(d) { highlightGraphNode(d,true,this);  } )
        .on("mouseout",  function(d) { highlightGraphNode(d,false,this); } );

      // labels: a group with two SVG text: a title and a shadow (as background)
      var graphLabels = networkGraph.append('svg:g').attr('class','grp gLabel')
        .selectAll("g.label")
        .data( nodeArray, function(d){return d.label} )
        .enter().append("svg:g")
        .attr('id', function(d) { return "l" + d.index; } )
        .attr('class','label');

      shadows = graphLabels.append('svg:text')
        .attr('x','-2em')
        .attr('y','-.3em')
        .attr('pointer-events', 'none') // they go to the circle beneath
        .attr('id', function(d) { return "lb" + d.index; } )
        .attr('class','nshadow')
        .text( function(d) { return d.label; } );

      labels = graphLabels.append('svg:text')
        .attr('x','-2em')
        .attr('y','-.3em')
        .attr('pointer-events', 'none') // they go to the circle beneath
        .attr('id', function(d) { return "lf" + d.index; } )
        .attr('class','nlabel')
        .text( function(d) { return d.label; } );

        /*var n =1000;
        force.start();
        for (var i = n * n; i > 0; --i) force.tick();
        force.stop();*/


      /* --------------------------------------------------------------------- */
      /* Select/unselect a node in the network graph.
         Parameters are:
         - node: data for the node to be changed,
         - on: true/false to show/hide the node
      */
      function highlightGraphNode( node, on )
      {
        // If we are to activate a movie, and there's already one active,
        // first switch that one off
        if( on && activeCountry !== undefined ) {
          console.log("..clear: ",activeCountry);
          highlightGraphNode( nodeArray[activeCountry], false );
          console.log("..cleared: ",activeCountry);
        }

        // locate the SVG nodes: circle & label group
        circle = d3.select( '#c' + node.index );
        label  = d3.select( '#l' + node.index );

        // activate/deactivate the node itself
        circle.classed( 'main', on );
        label.classed( 'on', on || currentZoom >= SHOW_THRESHOLD );
        label.selectAll('text').classed( 'main', on );

        // activate all siblings
        /*Object(node.links).forEach( function(id) {
        	d3.select("#c"+id).classed( 'sibling', on );
        	label = d3.select('#l'+id);
        	label.classed( 'on', on || currentZoom >= SHOW_THRESHOLD );
        	label.selectAll('text.nlabel').classed( 'sibling', on );
        });*/

        // set the value for the current active movie
        activeCountry = on ? node.index : undefined;
        console.log("SHOWNODE finished: "+node.index+" = "+on );
      }


    /* --------------------------------------------------------------------- */
    /* Show the movie details panel for a given node
     */
    function showMoviePanel( node ) {
      CountryInfoDiv.html( getCountryInfo(node,nodeArray) ).attr("class","panel_on");
    }


    /* --------------------------------------------------------------------- */
    /* Move all graph elements to its new positions. Triggered:
       - on node repositioning (as result of a force-directed iteration)
       - on translations (user is panning)
       - on zoom changes (user is zooming)
       - on explicit node highlight (user clicks in a movie panel link)
       Set also the values keeping track of current offset & zoom values
    */
    function repositionGraph( off, z, mode ) {
      // do we want to do a transition?
      var doTr = (mode == 'move');

      //initial draw, repositioning
      if (mode == 'tick') {
        g = d3.select('g.grpParent')
        g = g.transition().duration(ANIMATION_TIME);
        g.attr("transform", function(d) { return "translate("+ off.x+","+off.y+")" } );
      }

      // drag: translate to new offset
      if( off !== undefined && (off.x != currentOffset.x || off.y != currentOffset.y ) ) {
        g = d3.select('g.grpParent')
        if( doTr )
        g = g.transition().duration(ANIMATION_TIME);
        g.attr("transform", function(d) { return "translate("+ off.x+","+off.y+")" } );
        currentOffset.x = off.x;
        currentOffset.y = off.y;
      }

      // zoom: get new value of zoom
      if( z === undefined ) {
        z = currentZoom;
        if( mode != 'tick' )
        return;	// no zoom, no tick, we don't need to go further
      }
      else
        currentZoom = z;

      // move edges
      e = doTr ? graphLinks.transition().duration(ANIMATION_TIME) : graphLinks;
      e.attr("x1", function(d) { return z*(d.source.x); })
        .attr("y1", function(d) { return z*(d.source.y); })
        .attr("x2", function(d) { return z*(d.target.x); })
        .attr("y2", function(d) { return z*(d.target.y); });

      // move nodes
      n = doTr ? graphNodes.transition().duration(ANIMATION_TIME) : graphNodes;
      n.attr("transform", function(d) { return "translate(" +z*d.x+","+z*d.y+")" } );
      // move labels
      l = doTr ? graphLabels.transition().duration(ANIMATION_TIME) : graphLabels;
      l.attr("transform", function(d) { return "translate(" +z*d.x+","+z*d.y+")" } );
    }


    /* --------------------------------------------------------------------- */
    /* Perform drag
     */
    function dragmove(d) {
      offset = { x : currentOffset.x + d3.event.dx, y : currentOffset.y + d3.event.dy };
      repositionGraph( offset, undefined, 'drag' );
    }


    /* --------------------------------------------------------------------- */
    /* Perform zoom. We do "semantic zoom", not geometric zoom
     * (i.e. nodes do not change size, but get spread out or stretched
     * together as zoom changes)
     */
    function doZoom( increment ) {
      newZoom = increment === undefined ? d3.event.scale : zoomScale(currentZoom+increment);
      if( currentZoom == newZoom )
      return;	// no zoom change

       // See if we cross the 'show' threshold in either direction
      if( currentZoom<SHOW_THRESHOLD && newZoom>=SHOW_THRESHOLD )
        svg.selectAll("g.label").classed('on',true);
      else if( currentZoom>=SHOW_THRESHOLD && newZoom<SHOW_THRESHOLD )
        svg.selectAll("g.label").classed('on',false);

      // See what is the current graph window size
      s = getViewportSize();
      width  = s.w<WIDTH  ? s.w : WIDTH;
      height = s.h<HEIGHT ? s.h : HEIGHT;

      // Compute the new offset, so that the graph center does not move
      zoomRatio = newZoom/currentZoom;
      newOffset = { x : currentOffset.x*zoomRatio + width/2*(1-zoomRatio), y : currentOffset.y*zoomRatio + height/2*(1-zoomRatio)};

      // Reposition the graph
      repositionGraph( newOffset, newZoom, "zoom" );
    }

    /* --------------------------------------------------------------------- */

    /* process events from the force-directed graph */
    force.on("tick", function() {
      //repositionGraph(undefined,undefined,'tick');
      repositionGraph(currentOffset,undefined,'tick');
    });

    //repositionGraph(currentOffset,undefined,'move');

    document.getElementById("zoomPlus").addEventListener("click", function(){
      doZoom(0.1);
    });

    document.getElementById("zoomMinus").addEventListener("click", function(){
      doZoom(-0.1);
    });

  }); // end of function(data

} // end of D3ok()

function orgSelected(option) {
  currentOrg = option.value;
  D3ok();
}
