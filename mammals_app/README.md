This is a small shiny app on mammal traits that uses the COMBINE database, available here https://esajournals.onlinelibrary.wiley.com/doi/10.1002/ecy.3344. 

This app has 5 panels:
* **General Overview**: It shows the number of taxa for each taxonomic rank.

* **Weight & Length**: It shows the weight and length range, min, and max values for each order.

* **Life History**: It shows a list of life history traits for each mammal species.

* **Diet**: It shows an estimate of species diet compositon.

* **Range & Dispersal**: It shows the altitudinal range and biogeographical realm of each species.

## Docker version

There is a docker version of this app, found at mcrotti1/mammals-app.

To get the app, run:

```bash
docker pull mcrotti1/mammals-app
```

To start the app, run: 

```bash
docker run --rm -p 3838:3838 mcrotti1/mammals-app
```

To access the locally running app open a web browser to http://localhost:3838. 
To stop the container, head back to the terminal where docker is running and press ctrl+c.