    //Creating the Reactome Diagram widget
    //Take into account a proxy needs to be set up in your server side pointing to www.reactome.org
    function onReactomeDiagramReady(name,genes){  //This function is automatically called when the widget code is ready to be used
        var diagram = Reactome.Diagram.create({
            "placeHolder" : "diagramHolder",
            "width" : 950,
            "height" : 545
        });

        //Initialising it to the "Hemostasis" pathway
        diagram.loadDiagram(name);

        //Adding different listeners

        diagram.onDiagramLoaded(function (loaded) {
            console.info("Loaded ", loaded);
            //genes.forEach((item, i) => {
              //diagram.flagItems(item)
            //});

            diagram.flagItems(genes);
            if (loaded == name) diagram.selectItem(name);
        });

        diagram.onObjectHovered(function (hovered){
            console.info("Hovered ", hovered);
        });

        diagram.onObjectSelected(function (selected){
            console.info("Selected ", selected);
        });
     }
