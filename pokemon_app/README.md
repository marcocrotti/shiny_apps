This is a small shiny app that uses the Pokémon dataset from Kaggle, available here https://www.kaggle.com/datasets/rounakbanik/pokemon.

This app has 5 panels:
* **Pokémon stats**: It shows the stats for each pokémon, and compare up to three pokémon in a radar plot.

* **Type vs Stats**: It shows the different stats by type, and compare up to three types up closely.

* **Type effectiveness**: It shows the effectiveness of each type against the chosen pokémon.

* **Pokémon by type**: It shows the number of pokémon by first and second type, for all pokémon and for legendary only.

* **Height & Weight**: It shows an histogram of height and weight (log-transformed) for all pokémon, coloured by type, and highlights the pokémon with highest and lowest values.

## Docker version

There is a docker version of this app, found at mcrotti1/pokemon-app.

To get the app, run:

```bash
docker pull mcrotti1/pokemon-app
```

To start the app, run: 

```bash
docker run --rm -p 3838:3838 mcrotti1/pokemon-app
```

To access the locally running app open a web browser to http://localhost:3838. 
To stop the container, head back to the terminal where docker is running and press ctrl+c.