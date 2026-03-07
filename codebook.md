#Dataset_1: Final_shipment.csv
| Variable           | Description                                                                    
| ------------------ | ------------------------------------------------------------------------------ 
| shipment_id        | Unique identifier for each shipment                                            
| shipment_value     | Monetary value of the shipment (USD)                                           
| departure_time     | Time the shipment left the origin                                              
| shipping_method    | Transportation method used (Truck, Rail, Sea, Air)                             
| weather_condition  | Weather condition affecting the shipment route (Clear, Rain, Storm, Fog, Snow) 
| shipping_speed     | Estimated transport speed                                                      
| origin             | Shipment origin location                                                       
| destination        | Final delivery location                                                        
| shipping_distance  | Distance of the shipment route                                                 
| shipping_time      | Estimated travel time                                                          
| arrival_time       | Actual arrival timestamp                                                       
| delay_flag         | Binary indicator of shipment delay (1 = delayed, 0 = on time)                  
| delay_hours        | Number of hours delayed                                                        
| risk_score         | Composite shipment risk score (calculated from multiple risk factors)          
| financial_exposure | Estimated financial impact if disruption occurs                                
| weather_risk       | Risk component derived from weather conditions                                 
| shipping_risk      | Risk component based on transportation method                                  
| distance_risk      | Risk associated with route length                                              
| delay_signal       | Signal variable indicating delay severity                                      
| supplier_risk      | Risk factor associated with supplier reliability                               

risk_score = weather_risk + shipping_risk + distance_risk + delay_signal + supplier_risk
financial_exposure = shipment_value × risk_score

#Dataset_2: historical_shipments.csv
| Variable       | Description                                    
| -------------- | ---------------------------------------------- 
| route          | Shipment route (origin → destination)          
| shipment_count | Number of shipments on this route              
| delay_rate     | Percentage of shipments delayed                
| avg_risk       | Average risk score for shipments on this route 
| avg_exposure   | Average financial exposure                     
| date           | Observation time period                        

route_reliability = 1 - delay_rate

#Dataset_3: 
warehouse_status_by_cargo.csv
| Variable     | Description                      
| ------------ | -------------------------------- 
| warehouse    | Regional distribution hub name   
| cargo_type   | Type of cargo stored at the hub  
| capacity     | Maximum storage capacity         
| current_load | Current amount of stored cargo   
| utilization  | Warehouse utilization percentage 

utilization = current_load / capacity
Capacity_stress_classification:
| Utilization | Stress Level  
| ----------- | ------------- 
| < 70%       | Normal        
| 70–85%      | Medium Stress 
| > 85%       | High Stress   

#Data integration
The three datasets are integrated to power the RiskRoute AI dashboard, which provides:
- Shipment risk monitoring
- Financial exposure tracking
- Warehouse congestion detection
- Route reliability analytics
- Delay trend analysis

#Analaysis
Using these datasets, the system produces:
- High-risk shipment alerts
- Route reliability leaderboards
- Delay trend monitoring
- Warehouse capacity stress detection
- Financial exposure prioritization
