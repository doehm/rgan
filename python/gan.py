import numpy as np
import pandas as pd
import os
from datetime import datetime

import keras
from keras.layers import Dense, Dropout, Input
from keras.models import Model,Sequential
from keras.layers.advanced_activations import LeakyReLU
from keras.optimizers import adam


class GAN():
  
    def __init__(self):
        
        # parameters
        self.random_draws = 1
        self.samples = 42
        self.trace = 100
        self.plot_layout = (7, 6)
        self.fig_size = (32, 18)
        self.date = datetime.now().strftime("%Y-%m-%d %Hh%Mm%Ss")
        
        # create path and log file
        self.path = os.getcwd() + "\\log\\"
        if not os.path.isdir(self.path):
            os.mkdir(self.path)
        self.log_file = open(self.path + "log %s.txt" % self.date, 'w+')
        self.log_file.write('%s created\n' % self.date)
        self.log_file.close()
        
        # get data which was created in R
        self.x_train = r.x_train
        
        # output
        # store as a list and plot in R
        self.output = {}
        
        # Creating GAN
        self.generator = self.create_generator()
        self.discriminator = self.create_discriminator()
        self.gan = self.create_gan(self.discriminator, self.generator)
    
        
    # the generator function
    def create_generator(self):
        
        # build generator graph
        generator = Sequential()
        generator.add(Dense(units = 16, input_dim = self.random_draws))
        generator.add(LeakyReLU(0.2))
        generator.add(Dropout(0.2))
        generator.add(Dense(units = 32))
        generator.add(LeakyReLU(0.2))
        generator.add(Dropout(0.2))
        generator.add(Dense(units = 64))
        generator.add(LeakyReLU(0.2))
        generator.add(Dropout(0.2))
        generator.add(Dense(units = self.x_train.shape[1]))
        
        # compile
        generator.compile(loss = 'binary_crossentropy', optimizer = self.optimizer())
        # generator.compile(loss = 'binary_crossentropy', optimizer = 'adam')
        
        return generator
    
        
    # the discriminator function
    def create_discriminator(self):
        
        # build discriminator graph
        discriminator=Sequential()
        discriminator.add(Dense(units = self.x_train.shape[1]))
        discriminator.add(LeakyReLU(0.2))
        discriminator.add(Dropout(0.2))
        discriminator.add(Dense(units = 32))
        discriminator.add(LeakyReLU(0.2))
        discriminator.add(Dropout(0.2))
        discriminator.add(Dense(units = 16))
        discriminator.add(LeakyReLU(0.2))
        discriminator.add(Dropout(0.2))
        discriminator.add(Dense(units = 1, activation='sigmoid'))
        
        # compile
        discriminator.compile(loss = 'binary_crossentropy', optimizer = self.optimizer())
        # discriminator.compile(loss = 'binary_crossentropy', optimizer = 'adam')
        
        return discriminator
    
        
    # the gan function combining the generator and the discriminator
    def create_gan(self, discriminator, generator):
        
        discriminator.trainable = False
        gan_input = Input(shape=(self.random_draws,))
        x = generator(gan_input)
        gan_output = discriminator(x)
        gan = Model(inputs = gan_input, outputs = gan_output)
        gan.compile(loss = 'binary_crossentropy', optimizer = 'adam')
        # gan.compile(loss = 'binary_crossentropy', optimizer = self.optimizer())
        
        return gan
        
        
    # adam optimiser function
    def optimizer(self):

        return adam(lr=0.2, beta_1=0.5)
        
    
    # returns the details of the keras model
    def details(self):
        
        print(self.generator.summary())
        print(self.discriminator.summary())
        
    
    def slice_generated_sequence(self, epoch):
      
        # generate noise as input
        noise = np.random.normal(0, 1, size = [np.prod(self.plot_layout), self.random_draws])
        
        # generate sequences from the generator
        generated_sequences = self.generator.predict(noise)
        
        # save to a list for plotting in R
        self.output.update({'epoch %d' % epoch:generated_sequences})
      
      
    
    def logger(self, k, epochs):
      
        self.log_file = open(self.path + "log %s.txt" % self.date, 'a')
        self.log_file.write(datetime.now().strftime("%Y-%m-%d %Hh%Mm%Ss") + ' epoch %d of %d\n' % (k, epochs))
        self.log_file.close()
        
    
    # GAN training function
    def train(self, epochs = 500, batch_size = 16):
        
        for k in range(1, epochs + 1):

            for j in range(batch_size):
              
                # generate noise as input to the generator
                noise = np.random.normal(0, 1, [batch_size, self.random_draws])
                
                # generate sequences and take a sample of real sequences from the training data of the same size
                generated_sequences = self.generator.predict(noise)
                batch = self.x_train[np.random.randint(low = 0, high = self.x_train.shape[0], size = batch_size)]
                x = np.concatenate([batch, generated_sequences])
                
                # create labels for the real and generated sequences
                y_dis = np.zeros(2*batch_size)
                y_dis[:batch_size] = 1
                
                # train discriminator
                self.discriminator.trainable = True
                self.discriminator.train_on_batch(x, y_dis)
                
                # train the GAN
                noise = np.random.normal(0, 1, [batch_size, self.random_draws])
                y_gen = np.ones(batch_size)
                self.discriminator.trainable = False
                self.gan.train_on_batch(noise, y_gen)
                
            # this is mostly to track progress and view some interim output    
            if k % self.trace == 0:
           
                # self.plot_generated_sequences(epoch = k, path = path)
                self.logger(k, epochs)
                self.slice_generated_sequence(k)

        


        
