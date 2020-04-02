from keras.models import Sequential
from keras.layers.core import Dense
import numpy as np

# =============================================================
# Prepare data set
# =============================================================
N_Inputs  = 2
N_Outputs = 1
N_Loops   = 8000

Inputs  = np.random.rand(N_Loops,N_Inputs) -0.5
OSum    = np.sum(Inputs, axis=1)
Outputs = np.repeat(OSum.reshape([-1,1]),N_Outputs,axis=1)

# =============================================================
# Build the model
# =============================================================

# _____________________________________________________________
# Setup the layers
model = Sequential([
    Dense(1, input_shape=(N_Inputs,), 
          # kernel_initializer='random_uniform',
          # bias_initializer='zeros',
          # activation='elu'
          )
])

# _____________________________________________________________
# Compile the model
model.compile(optimizer='sgd',
              loss='mean_squared_error',
              metrics=['accuracy'])

# =============================================================
# Train the model
# =============================================================
model.fit(Inputs, Outputs, epochs=1, batch_size=1)

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
