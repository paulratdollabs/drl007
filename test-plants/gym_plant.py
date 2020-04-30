#!/usr/bin/env python

# Copyright 2020 Dynamic Object Language Labs Inc.
#
# This software is licensed under the terms of the
# Apache License, Version 2.0 which can be found in
# the file LICENSE at the root of this distribution.

import argparse
import sys
import time
from pprint import pprint
import gym
import numpy as np
import matplotlib.pyplot as plt

try:
    import plant
except ImportError as error:
    # Output expected ImportErrors.
    print(error.__class__.__name__ + ": " + error.message)
    print('Ensure PYTHONPATH has plant.py')
except Exception as exception:
    # Output unexpected Exceptions.
    print(exception, False)
    print(exception.__class__.__name__ + ": " + exception.message)

# Globals
rmq = None

class Rmq:
    """
    Class to interface RMQ plant messaging
    """

    # gym environment

    env = None
    gym_reward = 0
    gym_new_state = None
    gym_done = False
    gym_goal_position = 0

    obs_high = None
    obs_low = None
    num_obs = 0
    num_acts = 0
    high0 = 0
    high1 = 0
    high2 = 0
    high3 = 0
    low0 = 0
    low1 = 0
    low2 = 0
    low3 = 0
    state0 = 0
    state1 = 1
    state2 = 2
    state3 = 3
    done = False

    def __init__(self, plantid, exchange, host, port):
        self.plant = plant.Plant(plantid, exchange, host, port)
        # self.plant.connection.add_callback_threadsafe(self.rmq_call_back) # Not needed
        self.done = False
        self.last_rmq_call_back = time.time()

    def make_env(self, msg):
        self.plant.started(msg)
        envname, = msg['args']
        #print('gym make: ', envname)
        self.env=gym.make(envname)
        self.plant.finished(msg)
        #print('make_env, env=', self.env)
        self.publish_data_obs_rmq()

    def reset(self, msg):
        self.plant.started(msg)
        # no args for reset -- alt, = msg['args']
        if not self.env==None:
            self.gym_new_state=self.env.reset()
            #print('reset') #, alt
            self.publish_state_obs_rmq()
        self.plant.finished(msg)
        #print('done reset')

    def render(self, msg):
        self.plant.started(msg)
        # no args for render -- alt, = msg['args']
        self.env.render()
        #print('render') #, alt
        self.plant.finished(msg)
        #print('done render')

    def perform_action(self, msg):
        action_name, = msg['args']
        action_number = int(action_name)
        if self.env.action_space.n >= action_number >= 0:
            self.gym_new_state, self.gym_reward, self.gym_done, _ = self.env.step(action_number)
            self.gym_goal_position = 0 #self.env.goal_position
            self.publish_step_obs_rmq()
        else:
            print('Bad action specified:', action_name)
        self.plant.finished(msg)
        #print('done perform_action')

    def publish_data_obs_rmq(self):
        gym_data_observations = self.make_gym_data_observation()
        pprint(gym_data_observations)
        self.plant.observations(None, gym_data_observations, copy_observations=False, plantid="gym")

    def make_gym_data_observation(self):
        self.obs_high = self.env.observation_space.high
        self.obs_low = self.env.observation_space.low
        self.num_acts = self.env.action_space.n
        self.num_obs = len(self.obs_high)

        p1=[self.plant.make_observation('numacts', int(self.num_acts)),
            self.plant.make_observation('numobs',  int(self.num_obs))]
        p2=[self.plant.make_observation('high0', float(self.obs_high[0]))] if self.num_obs>0 else []
        p3=[self.plant.make_observation('high1', float(self.obs_high[1]))] if self.num_obs>1 else []
        p4=[self.plant.make_observation('high2', float(self.obs_high[2]))] if self.num_obs>2 else []
        p5=[self.plant.make_observation('high3', float(self.obs_high[3]))] if self.num_obs>3 else []
        p6=[self.plant.make_observation('low0',  float(self.obs_low[0]))]  if self.num_obs>0 else []
        p7=[self.plant.make_observation('low1',  float(self.obs_low[1]))]  if self.num_obs>1 else []
        p8=[self.plant.make_observation('low2',  float(self.obs_low[2]))]  if self.num_obs>2 else []
        p9=[self.plant.make_observation('low3',  float(self.obs_low[3]))]  if self.num_obs>3 else []
        return p1+p2+p3+p4+p5+p6+p7+p8+p9

    def publish_step_obs_rmq(self):
        gym_step_observations = self.make_step_observation()
        #pprint(gym_step_observations)
        self.plant.observations(None, gym_step_observations, copy_observations=False, plantid="gym")

    def make_step_observation(self):
        p1=[self.plant.make_observation('reward',  float(self.gym_reward))]
        p2=[self.plant.make_observation('state0',  float(self.gym_new_state[0]))] if self.num_obs>0 else []
        p3=[self.plant.make_observation('state1',  float(self.gym_new_state[1]))] if self.num_obs>1 else []
        p4=[self.plant.make_observation('state2',  float(self.gym_new_state[2]))] if self.num_obs>2 else []
        p5=[self.plant.make_observation('state3',  float(self.gym_new_state[3]))] if self.num_obs>3 else []
        p6=[self.plant.make_observation('done',          self.gym_done)]
        p7=[self.plant.make_observation('goal_position', self.gym_goal_position)]
        return p1+p2+p3+p4+p5+p6+p7

    def publish_state_obs_rmq(self):
        gym_state_observations = self.make_state_observation()
        #pprint(gym_state_observations)
        self.plant.observations(None, gym_state_observations, copy_observations=False, plantid="gym")

    def make_state_observation(self):
        p2=[self.plant.make_observation('state0',  float(self.gym_new_state[0]))] if self.num_obs>0 else []
        p3=[self.plant.make_observation('state1',  float(self.gym_new_state[1]))] if self.num_obs>1 else []
        p4=[self.plant.make_observation('state2',  float(self.gym_new_state[2]))] if self.num_obs>2 else []
        p5=[self.plant.make_observation('state3',  float(self.gym_new_state[3]))] if self.num_obs>3 else []
        return p2+p3+p4+p5

    def dispatch_func(self, msg, rkey_):
        if 'function-name' in msg:
            self.handle_fn(msg)
        # elif 'observations' in msg:
        #     self.handle_observation(msg)
        # elif 'state' in msg:
        #     st = msg['state']
        # noop
        else:
            print('unhandled message')
            pprint(msg)

    def handle_fn(self, msg):
        # print 'Got message routing key', routing_key
        # print 'handle rmq message:', utils.get_current_thread_name()
        #pprint(msg)
        fn_name = msg['function-name']
        if fn_name == 'make_env':
            self.make_env(msg)
        elif fn_name == 'reset':
            self.reset(msg)
        elif fn_name == 'render':
            self.render(msg)
        elif fn_name == 'perform-action':
            self.perform_action(msg)
        else:
            print('RMQ Unknown function', msg['function-name'])
            self.plant.failed(msg, "Unknown function for cps ros plant" + msg['function-name'])

    def subscribe_and_wait(self):
        self.plant.wait_for_messages(self.dispatch_func)
        # print('gym_plant.py done subscribe_and_wait')

    def shutdown(self):
        self.done = True
        #print('RMQ Shut down')
        self.plant.close()


def on_gym_shutdown():
    try:
        rmq.shutdown()
    except Exception as e:
        print('Ignoring as we are shutting down', e.__class__.__name__ + ": " + e.message)


def main(args):
    global rmq

    print("plantid=", args.plantid, "exchange=", args.exchange, "host=", args.host, "port=", args.port)
    rmq = Rmq(args.plantid, args.exchange, args.host, args.port)

    try:
        rmq.subscribe_and_wait()
    except Exception as e:
        print('Ignoring exception as we are shutting down', e.__class__.__name__ + ": " + e.message)

    print('Done gym_plant main')

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Gym Plant')
    parser.add_argument('--host', default='localhost', help='RMQ host')
    parser.add_argument('-p', '--port', default=5672, help='RMQ Port', type=int)
    parser.add_argument('-e', '--exchange', default='dmrl', help='RMQ Exchange')
    parser.add_argument('--plantid', default="dmrl", help='default plant id')

    args = parser.parse_args()
    pprint(args)
    main(args)
    sys.exit(0)
