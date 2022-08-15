# DOLL Montecarlo Reinforcement Learning (DMRL)
## The Gym plant
### Prerequisites
gym
pika
matplotlib
pygame
numpy
json

## The Clojure learner
### Prerequisites

### Building
In the Clojure directory that contains the build.boot file, build the jar file.
	boot build-jar
	
## Running
1. Ensure that the RabbitMQ server is running
2. In one window start the gym plant
	python3 misty_plant.py
3. In another window start the learner
	java -jar target/dmrl.jar qlearn
	