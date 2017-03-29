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
/*var e = document.getElementById("orgDropdown");
var currentOrg = e.options[e.selectedIndex].value;*/
//var currentOrg = $('#orgDropdown').val();


// -------------------------------------------------------------------

// Do the stuff -- to be called after D3.js has loaded
function D3ok() {
	var myNode = document.getElementById("chartID");
	while (myNode.firstChild) {
	    myNode.removeChild(myNode.firstChild);
	}

	var w = $("#chartID").width();
	var h = $("#chartID").height();

	var keyc = true, keys = true, keyt = true, keyr = true, keyx = true, keyd = true, keyl = true, keym = true, keyh = true, key1 = true, key2 = true, key3 = true, key0 = true

	var focus_node = null, highlight_node = null;

	var text_center = false;
	var outline = false;

	var min_score = 0;
	var max_score = 1;

	var color = d3.scale.linear()
		.domain([min_score, (min_score+max_score)/2, max_score])
		.range(["lime", "yellow", "red"]);

	var highlight_color = "blue";
	var highlight_trans = 0.1;

	var size = d3.scale.pow().exponent(1)
		.domain([10,50])
		.range([10,50]);

	var force = d3.layout.force()
		.linkDistance(200)
		.charge(-600)
		.size([w,h]);

	var default_node_color = "#ccc";
	var default_link_color = "#888";
	var nominal_base_node_size = 8;
	var nominal_text_size = 12;
	var max_text_size = 24;
	var nominal_stroke = 1.5;
	var max_stroke = 1.5;
	var max_base_node_size = 36;
	var min_zoom = 0.1;
	var max_zoom = 7;
	var svg = d3.select("#chartID").append("svg");
	var zoom = d3.behavior.zoom().scaleExtent([min_zoom,max_zoom])
	var g = svg.append("g");

	svg.style("cursor","move");

	var dataFile = typeof currentOrg !== "undefined" ? "./Data/"+currentOrg+'.json' : "./Data/ADANA_METROPOLITAN_MUNICIPALITY.json";

	d3.json(dataFile, function(error, graph)
	{	var linkedByIndex = {};
			graph.links.forEach(function(d){
				linkedByIndex[d.source + "," + d.target] = true;
			});

		function isConnected(a, b) {
			return linkedByIndex[a.index + "," + b.index] || linkedByIndex[b.index + "," + a.index] || a.index == b.index;
		}

		force
			.nodes(graph.nodes)
			.links(graph.links)
			.start();

		var link = g.selectAll(".link")
			.data(graph.links)
			.enter().append("line")
			.attr("class", "link")
			.style("stroke-width",function(d) { return d.weight*.75;})
			.style("stroke", default_link_color)
			.style("visibility", "hidden")

		var node = g.selectAll(".node")
			.data(graph.nodes)
			.enter().append("g")
			.attr("class", "node")
			.attr("id", function(d) { return "c" + d.index; } )
			.style("visibility", "hidden")
			.call(force.drag)


		node.on("dblclick.zoom", function(d){
			d3.event.stopPropagation();
			var dcx = ($("#chartID").width()/2-d.x*zoom.scale());
			var dcy = ($("#chartID").height()/2-d.y*zoom.scale());
			zoom.translate([dcx,dcy]);
			g.attr("transform", "translate("+ dcx + "," + dcy  + ")scale(" + zoom.scale() + ")");
		});


		var tocolor = "fill";
		var towhite = "stroke";

		if (outline){
			tocolor = "stroke"
			towhite = "fill"
		}


		var circle = node.append("path")
			.attr("d", d3.svg.symbol()
				.size(function(d) { return d.size*25; })
				.type(function(d) { return "circle"; })
			)
			.style(tocolor, function(d) {
				switch (d.activity) {
					case "PUB": return "#1f77b4";
					case "PRC": return "#ff7f0e";
					case "OTH": return "#ffbb78";
					case "HES": return "#2ca02c";
					case "REC": return "#9467bd";
				}
			})
			.style("stroke-width", nominal_stroke)
			.style(towhite, "white");


		var text = g.selectAll(".text")
			.data(graph.nodes)
			.enter().append("text")
			.attr("dy", ".35em")
			.attr("id", function(d) { return "l" + d.index; } )
			.style("font-size", nominal_text_size + "px")
			.style("opacity", 0)
			.style("font-family", "Lucida Sans Unicode")
			.style("font-weight", "bold");


		if (text_center)
			text.text(function(d) { return (d.label + " - " + d.country).toLowerCase(); })
				.style("text-anchor", "middle");
		else
			text.attr("dx", function(d) {return (size(d.size)||nominal_base_node_size)+6;})
				.text(function(d) { return '\u2002'+(d.label + " - " + d.country).toLowerCase(); });

		node.on("mouseover", function(d) {
				set_highlight(d);
			})
			.on("mousedown", function(d) {
				d3.event.stopPropagation();
				focus_node = d;
				set_focus(d)
				if (highlight_node === null)
					set_highlight(d)
			})
			.on("mouseout", function(d) {
				exit_highlight();
			});


		d3.select(window).on("mouseup",  function() {
			if (focus_node!==null){
				focus_node = null;
				if (highlight_trans<1){
					circle.style("opacity", 1);
					text.style("opacity", 1);
					link.style("opacity", 1);
				}
			}
			if (highlight_node === null)
				exit_highlight();
		});


		function exit_highlight(){
			highlight_node = null;
			if (focus_node===null){
				svg.style("cursor","move");
				if (highlight_color!="white"){
					circle.style(towhite, "white");
					text.style("font-weight", "normal")
						.style("opacity", 0);
					link.style("stroke", function(o) {return (isNumber(o.score) && o.score>=0)?color(o.score):default_link_color});
				}
			}
		}


		function set_focus(d){
			if (highlight_trans<1){
				circle.style("opacity", function(o) {
						return isConnected(d, o) ? 1 : highlight_trans;
					});/*
					text.style("opacity", function(o) {
						return isConnected(d, o) ? 1 : highlight_trans;
					});*/
					link.style("opacity", function(o) {
						return o.source.index == d.index || o.target.index == d.index ? 1 : highlight_trans;
					});
			}
		}


		function set_highlight(d){
			/*$("#infoTitle").text(d.label);
			$("#infoText").text(d.country);
			$("#infoBubble").show();*/
			$("#l"+d.index).css("opacity", 1);
			svg.style("cursor","pointer");
			if (focus_node!==null)
				d = focus_node;
			highlight_node = d;
			if (highlight_color!="white") {
				circle.style(towhite, function(o) {
					return isConnected(d, o) ? highlight_color : "white";
				});
				link.style("stroke", function(o) {
					return o.source.index == d.index || o.target.index == d.index ? highlight_color : ((isNumber(o.score) && o.score>=0)?color(o.score):default_link_color);
				});
			}
		}


		zoom.on("zoom", function(){
			var stroke = nominal_stroke;
			if (nominal_stroke*zoom.scale()>max_stroke)
				stroke = max_stroke/zoom.scale();
			circle.style("stroke-width",stroke);
			var base_radius = nominal_base_node_size;
			if (nominal_base_node_size*zoom.scale()>max_base_node_size)
				base_radius = max_base_node_size/zoom.scale();
			circle.attr("d", d3.svg.symbol()
				.size(function(d) { return d.size*25; })
				.type(function(d) { return "circle"; }))
			if (!text_center)
				text.attr("dx", function(d){
					return (size(d.size)*base_radius/nominal_base_node_size||base_radius);
				});
			var text_size = nominal_text_size;
			if (nominal_text_size*zoom.scale()>max_text_size)
				text_size = max_text_size/zoom.scale();
			text.style("font-size",text_size + "px");
			g.attr("transform", "translate(" + d3.event.translate + ")scale(" + d3.event.scale + ")");
		});

		svg.call(zoom);

		resize();

		d3.select(window)
			.on("resize", resize);

		force.on("tick", function(){
			if (force.alpha() < 0.05) {
				node.style("visibility", "visible");
			    link.style("visibility", "visible");
			    $("#loading").fadeOut();
			} 
			node.attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; });
			text.attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; });
			link.attr("x1", function(d) { return d.source.x; })
				.attr("y1", function(d) { return d.source.y; })
				.attr("x2", function(d) { return d.target.x; })
				.attr("y2", function(d) { return d.target.y; });
			node.attr("cx", function(d) { return d.x; })
				.attr("cy", function(d) { return d.y; });
		});


/*		force.on('end', function() {
		    // layout is done
		    node.style("visibility", "visible");
		    link.style("visibility", "visible");
		    $("#loading").fadeOut();
		    console.log("here");
		  });*/

		function resize() {
			console.log("resize")
			var width = $("#chartID").width(), height = $("#chartID").height();
			svg.attr("width", width).attr("height", height);
			force.size([force.size()[0]+(width-w)/zoom.scale(),force.size()[1]+(height-h)/zoom.scale()]).resume();
			w = width;
			h = height;
		}

	});

	function vis_by_node_score(score){
		if (isNumber(score))
		{
			if (score>=0.666)
				return keyh;
			else if (score>=0.333)
				return keym;
			else if (score>=0)
				return keyl;
		}
		return true;
	}

	function vis_by_link_score(score)
	{
		if (isNumber(score))
		{
			if (score>=0.666)
				return key3;
			else if (score>=0.333)
				return key2;
			else if (score>=0)
				return key1;
		}
		return true;
	}

	function isNumber(n) {
	  return !isNaN(parseFloat(n)) && isFinite(n);
	}
/*
	$.fn.d3Click = function () {
		this.each(function (i, e) {
			console.log("click");
			var evt = new MouseEvent("dblclick");
			e.dispatchEvent(evt);
		});
	};
	$(".node").d3Click();*/

} // end of D3ok()