#!/usr/bin/env python

# Copyright 2020 Dynamic Object Language Labs Inc.
#
# This software is licensed under the terms of the
# Apache License, Version 2.0 which can be found in
# the file LICENSE at the root of this distribution.

import argparse
import os
import sys
import time
import openai
from pprint import pprint
import gymnasium as gym
from openai import OpenAI
import numpy as np
import matplotlib.pyplot as plt
import json
#from dotenv import load_dotenv

# openai.api_key = "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" # DrPaulRobertson@gmail.com key
# paulr@dollabs.com key ="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"

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
    gym_done = False            # terminated
    gym_truncated = False
    gym_info = None
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

    gpt_says = None

    def __init__(self, plantid, exchange, host, port):
        self.plant = plant.Plant(plantid, exchange, host, port)
        # self.plant.connection.add_callback_threadsafe(self.rmq_call_back) # Not needed
        self.done = False
        self.last_rmq_call_back = time.time()

    envname = None
    humanmode = False
    rendermode = -1

    def make_env(self, msg):
        self.plant.started(msg)
        self.envname, self.rendermode = msg['args']
        print('gym make: ', self.envname, 'render-mode = ', self.rendermode)
        if self.rendermode == -1:
            self.env=gym.make(self.envname, render_mode='human')
        else:
            self.env=gym.make(self.envname)
        self.plant.finished(msg)
        #print('make_env, env=', self.env)
        self.publish_data_obs_rmq()

    def reset(self, msg):
        self.plant.started(msg)
        # no args for reset -- alt, = msg['args']
        if not self.env==None:
            if (self.rendermode > 0) and self.humanmode:
                self.env.close()             # Close the rendering
                self.env = gym.make(self.envname) # Make a new envinonment without rendering
                self.humanmode = False
            self.gym_new_state=self.env.reset()
            #print('reset') #, alt
            self.publish_state_obs_rmq()
        self.plant.finished(msg)
        #print('done reset')

    def close(self, msg):
        self.plant.started(msg)
        # no args for reset -- alt, = msg['args']
        if not self.env==None:
            self.env.close()
            self.env=None
            self.publish_state_obs_rmq()
        self.plant.finished(msg)
        #print('done close')

    def render(self, msg):
        self.plant.started(msg)
        # no args for render -- alt, = msg['args']
        if (self.rendermode > 0) and (self.humanmode == False):
            self.env.close() # close the existing environment
            self.env=gym.make(self.envname, render_mode='human')
            self.humanmode = True
            self.gym_new_state=self.env.reset()
        self.env.render()
        #print('render') #, alt
        self.plant.finished(msg)
        #print('done render')

    def perform_action(self, msg):
        action_name, = msg['args']
        action_number = int(action_name)
        if self.env.action_space.n >= action_number >= 0:
            self.gym_new_state, self.gym_reward, self.gym_done, gym_truncated, gym_info  = self.env.step(action_number)
            self.gym_goal_position = 0 #self.env.goal_position
            self.publish_step_obs_rmq()
        else:
            print('Bad action specified:', action_name)
        self.plant.finished(msg)
        #print('done perform_action')

    def gpt_ask(self, msg):
        prompt, = msg['args']
        self.gpt_says = self.get_gpt4_json_response(prompt)
        if not (self.gpt_says==None):
            print(self.gpt_says) # +++ we need to publish this back to the caller
        else:
            print('GPT did not find anything to say in response to ', prompt)
        self.publish_gpt_obs_rmq()
        self.plant.finished(msg)
        #print('done ask_gpt action')

    def publish_gpt_obs_rmq(self):
        obs=[self.plant.make_observation('gpt-response',  self.gpt_says)] if self.gpt_says else []
        self.plant.observations(None, obs, copy_observations=False, plantid="gym")

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
        p2=[self.plant.make_observation('state0',  float(self.gym_new_state[0][0]))] if self.num_obs>0 else []
        p3=[self.plant.make_observation('state1',  float(self.gym_new_state[0][1]))] if self.num_obs>1 else []
        p4=[self.plant.make_observation('state2',  float(self.gym_new_state[0][2]))] if self.num_obs>2 else []
        p5=[self.plant.make_observation('state3',  float(self.gym_new_state[0][3]))] if self.num_obs>3 else []
        return p2+p3+p4+p5

############################################
# OpenAI

    #load_dotenv()

    if not os.getenv("OPENAI_API_KEY"):
        print("WARNING: the OPENAI_API_KEY environment variable was not found")

    def get_gpt4_json_response(self, prompt):
        our_messages=[{"role": "system",
                   "content": "You are a robot and to express your understanding of recommandations by summarizing them as in json form."},
                  {"role": "system",
                   "content": "You deliver your responses in json as follows { 'precondition': 'speed=high', 'avoid': 'actuation-changes', 'do': 'misdemeanor' }'"},
                  {"role": "system",
                   "content" : "Example: when the speed is high. avoid changing direction { 'precondition': 'speed=high', 'avoid' : 'changing-direction'}"},
                  {"role": "user",
                   "content": "REspond to this advice: "+prompt}]
        openai.api_key = os.getenv("OPENAI_API_KEY")

        response = openai.chat.completions.create(
            model="gpt-4-1106-preview",
            messages=our_messages,
            response_format={"type": "json_object"})
        print("A total of "+str(response.usage.total_tokens)+" tokens used")
        if response.choices[0].finish_reason=="stop":
            result=json.loads(response.choices[0].message.content)
            return result
        else:
            return None

    # startup test
    # pprint(get_gpt4_json_response("I advise you to avoid using the do_nothing action"))

##################################################"
# Dispatch

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
        elif fn_name == 'ask-gpt':
            self.gpt_ask(msg)
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
