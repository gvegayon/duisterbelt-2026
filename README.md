# Conditional Exponential-Family Random Graph Models: Does Conditioning on Edge Counts Help?

## Abstract

Exponential-family random graph models (ERGMs) provide a flexible framework for asking how local network features—such as reciprocity, clustering, or homophily—are associated with the formation of ties. Despite their appeal, ERGMs can be difficult to fit: their likelihood contains an intractable normalizing constant, so estimation typically relies on Markov chain Monte Carlo (MCMC) simulation. In practice, this can lead to slow convergence, unstable estimates, and degeneracy, in which simulated networks concentrate on implausibly sparse or dense graphs.

This talk examines whether conditioning on a network’s total number of ties—the edge count—can make ERGM estimation more stable. Conditioning treats the edge count, or density, as fixed rather than modeled; consequently, the density parameter is removed as a nuisance parameter, and MCMC sampling is restricted to networks with the same number of ties as the observed network. This removes the density direction that often drives degeneracy while allowing inference about other structural features of interest.

Conditional ERGMs are already natural in settings where data collection or the underlying process constrains network density. For example, name-generator surveys commonly limit how many people a respondent may nominate, motivating models that respect those limits. We present preliminary results from a large simulation study comparing conventional and edge-conditional ERGMs across seven model settings. We evaluate convergence, bias, confidence-interval coverage, and computation time. This work is part of a broader effort to make ERGMs more reliable and accessible beyond their traditional social-science applications, with planned applications to brain networks derived from fMRI data, gene-interaction networks, and potentially chemical-interaction networks.

## About the speaker

George G. Vega Yon, Ph.D., is an Associate Research Professor in the Division of Epidemiology at the University of Utah, with adjunct appointments in Population Health Sciences and the Kahlert School of Computing. He develops statistical and computational methods for studying networks and complex systems, with particular expertise in exponential-family random graph models, agent-based simulation, and mechanistic machine learning. Dr. Vega Yon develops open-source tools for network analysis, simulation, and high-performance computing, including epiworld, ergmito, netdiffuseR, rgexf, and slurmR. His work also focuses on making advanced statistical and computational methods accessible to broader scientific and public audiences. He has served both the Chilean and U.S. governments on education, pension, and public-health policy, most recently as a data scientist with CDC's Center for Forecasting and Outbreak Analytics. He holds an M.A. in Public Policy from Universidad Adolfo Ibáñez, an M.Sc. in Economics from Caltech, and a Ph.D. in Biostatistics from USC.

https://ggvy.cl

