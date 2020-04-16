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

# gym environment

env = None
gym_reward = 0
gym_new_state = None
gym_done = False

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

class Rmq:
    """
    Class to interface RMQ plant messaging with Ardu Copter
    """

    def __init__(self, plantid, exchange, host, port):
        self.plant = plant.Plant(plantid, exchange, host, port)
        # self.plant.connection.add_callback_threadsafe(self.rmq_call_back) # Not needed
        self.done = False
        self.last_rmq_call_back = time.time()

    def make_env(self, msg):
        self.plant.started(msg)
        envname, = msg['args']
        print('gym make: ', envname)
        env=gym.make(envname)
        self.plant.finished(msg)
        print('make_env')
        self.publish_data_obs_rmq()

    def reset(self, msg):
        self.plant.started(msg)
        # no args for reset -- alt, = msg['args']
        if not env==None:
            env.reset()
            print('reset') #, alt
        self.plant.finished(msg)
        print('done reset')

    def render(self, msg):
        self.plant.started(msg)
        # no args for render -- alt, = msg['args']
        env.render()
        print('render') #, alt
        self.plant.finished(msg)
        print('done render')

    def perform_action(self, msg):
        action_name, = msg['args']
        action_number = int(action_name)
        if env.action_space.n >= action_number >= 0:
            gym_new_state, gym_reward, gym_done, _ = env.step(action_number)
            publish_step_obs_rmq(self)
        else:
            print('Bad action specified:', action_name)
        self.plant.finished(msg)
        print('done perform_action')

    def publish_data_obs_rmq(self):
        gym_data_observations = self.make_gym_data_observation()
        pprint(gym_data_observations)
        self.plant.observations(None, gym_data_observations, copy_observations=False, plantid="gym")

    def make_gym_data_observation(self):
        obs_high = env.observation_space.high
        obs_low = env.observation_space.low
        actions = env.action_space.n
        numobs = len(obs_high)

        return [self.plant.make_observation('numacts', actions),
                self.plant.make_observation('numobs',  numobs),
                self.plant.make_observation('high0', obs_high[0] if numobs>0 else 0),
                self.plant.make_observation('high1', obs_high[1] if numobs>1 else 0),
                self.plant.make_observation('high2', obs_high[2] if numobs>2 else 0),
                self.plant.make_observation('high3', obs_high[3] if numobs>3 else 0),
                self.plant.make_observation('low0',  obs_low[0]  if numobs>0 else 0),
                self.plant.make_observation('low1',  obs_low[1]  if numobs>1 else 0),
                self.plant.make_observation('low2',  obs_low[2]  if numobs>2 else 0),
                self.plant.make_observation('low3',  obs_low[3]  if numobs>3 else 0)
               ]

    def publish_step_obs_rmq(self):
        gym_step_observations = self.make_step_observation()
        pprint(gym_step_observations)
        self.plant.observations(None, gym_step_observations, copy_observations=False, plantid="gym")

    def make_step_observation(self):
        return [self.plant.make_observation('reward',  gym_reward),
                self.plant.make_observation('state0',  gym_new_state[0] if numobs>0 else 0),
                self.plant.make_observation('state1',  gym_new_state[1] if numobs>1 else 0),
                self.plant.make_observation('state2',  gym_new_state[2] if numobs>2 else 0),
                self.plant.make_observation('state3',  gym_new_state[3] if numobs>3 else 0),
                self.plant.make_observation('done',    gym_done)
                ]

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
        print('RMQ Shut down')
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
    parser.add_argument('--host', default='192.168.11.100', help='RMQ host')
    parser.add_argument('-p', '--port', default=5672, help='RMQ Port', type=int)
    parser.add_argument('-e', '--exchange', default='dmrl', help='RMQ Exchange')
    parser.add_argument('--plantid', default="gym", help='default plant id')

    args = parser.parse_args()
    pprint(args)
    main(args)
    sys.exit(0)
