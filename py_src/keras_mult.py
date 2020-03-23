from keras.models import Sequential
from keras.layers.core import Dense
import numpy as np

# =============================================================
# Prepare data set
# =============================================================
N_Loops   = 2000

Inputs  = np.random.rand(N_Loops,2)
OSum    = np.multiply(Inputs[:,0], Inputs[:,1])
Outputs = OSum.reshape([-1,1])

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

model.fit(Inputs, Outputs, epochs=1, batch_size=2)

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
