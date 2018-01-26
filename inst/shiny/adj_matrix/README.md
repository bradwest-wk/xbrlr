## Visualizing Financial Concept (Element) Relationships with Adjacency Matrices

#### Purpose

Large financial taxonomies, such as the U.S. GAAP, are too large to visualize with traditional network graphs techniques.  The result -- even with thoughtful grouping, coloring, node sizing, layout algorithms, and interactive zoom -- is typically a hairball. In addition, the final product is highly dependent on the layout algorithm, a choice which often depends more on developer/data analyst asthetic preference than the underlying data.  This abstraction serves to complicate analysis and leaves us no closer to the end goal: insight.  

An alternative to network graphs are adjacency matrices. Although less pretty, these matrices can contain just as much information as a network tree.  They too are an abstraction of the data, but in a much simpler structure and are scalable to large sizes.

This app used heatmap matrices to visualize relationships between elements. The impetus for this project is a problem in visualizing elements that were used together in the same filings.

#### Packages

Requires ggplot2 and plotly to generate the plots.


