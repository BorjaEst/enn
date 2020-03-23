from keras.models import Sequential
from keras.layers.core import Dense
import numpy as np

# =============================================================
# Prepare data set
# =============================================================
N_Loops   = 8000

Inputs  = np.round(np.random.rand(N_Loops, 2))*2 -1
OLogic  = np.logical_xor(Inputs[:,0] > 0, Inputs[:,1] > 0)
Outputs = OLogic.reshape(-1,1).astype(float)*2 -1

# =============================================================
# Build the model
# =============================================================

# _____________________________________________________________
# Setup the layers
model = Sequential([
    Dense(2, activation='elu', input_shape=(2,)),
    Dense(1, activation='elu')
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
