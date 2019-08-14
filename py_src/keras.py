# TensorFlow and tf.keras
import tensorflow as tf
from tensorflow.python.keras import Sequential
from tensorflow.python.keras.layers import Dense

# Helper libraries
import random
import numpy as np

print(tf.__version__)

# =============================================================
# Prepare data set
# =============================================================

Inputs = np.array([[random.uniform(0, 0.5), random.uniform(0, 0.5)] for i in range(200)])
Outputs = np.array([[I1 + I2] for [I1, I2] in Inputs])

# =============================================================
# Build the model
# =============================================================

# _____________________________________________________________
# Setup the layers
model = Sequential([
    Dense(4, input_shape=(2,)),
    Dense(1, activation='relu')
])

# _____________________________________________________________
# Compile the model
model.compile(optimizer='adam',
              loss='mean_squared_error',
              metrics=['accuracy'])

# =============================================================
# Train the model
# =============================================================

model.fit(Inputs, Outputs, epochs=4, batch_size=2)

# =============================================================
# Make predictions
# =============================================================

Predictions = model.predict(Inputs[0:10])
Results = list(zip(Inputs.tolist(), Outputs.tolist(), Predictions.tolist()))

print("Results: ")
for aTuple in Results:
    print(aTuple)

print("Layers: ")
for layer in model.layers:
    print(layer.get_weights())

pause = True
