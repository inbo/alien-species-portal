Shiny.addCustomMessageHandler("updateLegendOpacity", function(message) {
  console.log("Opacity mapping received:", message.opacities);

  // Wait a bit for legend to appear
  setTimeout(function() {
		
		// Locate the map container by its ID
		  var mapDiv = document.getElementById(message.mapId);
		  if (!mapDiv) {
		    console.warn("No map element found with id:", message.mapId);
		    return;
		  }
			
    var legendDiv = mapDiv.querySelector('.legend');
    if (!legendDiv) {
      console.warn("No legend found in DOM.");
      return;
    }

    var symbols = legendDiv.querySelectorAll('i');

    // Extract the text labels next to the symbols
    var labels = Array.from(symbols).map(function(symbol) {
      var textNode = symbol.nextSibling;
      return textNode ? textNode.textContent.trim() : '';
    });

    symbols.forEach(function(symbol, index) {
      var label = labels[index];
      if (!label) return;

      var fillColor = window.getComputedStyle(symbol).backgroundColor;
      var opacity = message.opacities[label];

      console.log("Processing:", {
        label: label,
        originalColor: fillColor,
        opacity: opacity
      });

      // Extract RGB components
      var rgb = fillColor.match(/\d+/g);
      if (rgb && rgb.length >= 3) {
        var borderColor = `rgb(${rgb[0]}, ${rgb[1]}, ${rgb[2]})`;
        var transparentFill =
          opacity !== undefined && opacity > 0
            ? `rgba(${rgb[0]}, ${rgb[1]}, ${rgb[2]}, ${opacity})`
            : "transparent";

        console.log("Applying styles:", {
          border: borderColor,
          background: transparentFill
        });

        // Force style overrides
        symbol.style.setProperty("border", `2px solid ${borderColor}`, "important");
        symbol.style.setProperty("background", transparentFill, "important");
        symbol.style.boxSizing = "border-box";
      }
    });
  }, 100);
});


Shiny.addCustomMessageHandler("updateLegendBlueTransparent", function(message) {
	 setTimeout(function() {
	    var symbols = document.querySelectorAll('.legend i');

	    symbols.forEach(function(symbol) {
	      var fillColor = window.getComputedStyle(symbol).backgroundColor;
	      var rgb = fillColor.match(/\d+/g);

	      // Apply only if blue component == 255
	      if (rgb && rgb.length >= 3 && parseInt(rgb[2]) === 255) {
	        var transparentFill = 'rgba(' + rgb[0] + ', ' + rgb[1] + ', ' + rgb[2] + ', 0)';
					
					if (transparentFill !== fillColor) {
	        symbol.style.setProperty('border', '2px solid ' + fillColor, 'important');
	        symbol.style.setProperty('background', transparentFill, 'important');
	        symbol.style.boxSizing = 'border-box';
					}
	      }
	    });
	  }, 100);
	});