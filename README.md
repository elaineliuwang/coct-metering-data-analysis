# City of Cape Town: Advanced Metering Infrastructure Data Analysis

## Background 
The City of Cape Town meters its commercial customers using Advanced Metering Infrastructure (AMI) also called Smart Meters. These record real, reactive, and apparent power in 4 quadrants every thirty or fifteen minutes depending on the meter for internal and external customers. These are mostly but not all meters used for billing. Some of the meters sub-load within a facility. The electricity load data is stored on an enterprise database called MDUS which acquires the data from the industrial automation system (SCADA) that the meters are connected. MDUS also stores a number of meter and meter site attributes. We can leverage MDUS for many applications, for example, the SmartFacility business reporting application. We can write our own code to understand what it costs to supply different types of customers with electricity if we know which meter numbers are on which tariff says. 

## Code Project Components: 
1. A bit that programmatically gets the data using the PNP SCADA API 
2. A bit that manipulates the data into the required form. 
3. A user interface that allows a user to set parameters and initiate an extraction

## AMI Meter Data
- P1: Real Power (kW) in the forward (consumption) direction 
- P2: Real Power (kW) in the reverse (generation) direction e.g. on-site solar 
- Q: Reactive Power (KVAr) 
- S: Apparent Power (KVA) --> S^2 = P^2 + Q^2	(the vector sum) 
