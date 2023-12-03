# DOLL Montecarlo Reinforcement Learning (DMRL)
## The Gym plant
### Prerequisites
gymnasium
pika
matplotlib
pygame
numpy
json - seems to be already in python

## The Clojure learner
### Prerequisites

### Building
In the Clojure directory that contains the build.boot file, build the jar file.
	boot build-jar

## Running
1. Ensure that the RabbitMQ server is running
2. In one window start the gym plant
	python3 gym_plant.py
3. In another window start the learner
	java -jar target/dmrl.jar qlearn
