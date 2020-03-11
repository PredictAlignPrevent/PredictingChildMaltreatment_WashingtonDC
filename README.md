# Predicting Child Maltreatment Washington, D.C.

This repo hosts the code to develop a geospatial risk prediction model that estimates maltreatment risk across Washington, D.C.

The finished report can be found in the `docs` folder. To view, navigate to the docs folder > right click on 'index.html' > save link as > and then open in a new window. Required source code is located in the `source_files` folder and the R markdown (.Rmd) can be found in the `markdown` folder. A shapefile of the masked risk predictions is in the `data` folder and includes the fields:

<ul>
   - `net_id`: unique id given to each fishnet grid cell
   
   - `pred`: predicted count of maltreatment events
   
   - `pred_bin_class`: the predicted risk category
   
   - `WARD`: the ward the fishnet grid cell is in
   
   - `NBH_CLUSTER`: the neighborhood cluster the grid cell is in
   
   - `NBH_NAMES`: the group of neighborhoods the grid cell is located
   
</ul>

All the tools required for replicating our analysis can be found on [GitHub](https://github.com/urbanSpatial/spatialML_package). 
