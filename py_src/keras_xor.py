from keras.models import Sequential
from keras.layers.core import Dense
from keras.optimizers import SGD
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
    Dense(2, input_shape=(2,),
          # kernel_initializer='random_uniform',
          # bias_initializer='zeros',
          activation='elu'),
    Dense(1,
          # kernel_initializer='random_uniform',
          # bias_initializer='zeros',
          activation='elu')
])

# _____________________________________________________________
# Setup the Optimizer
Optimizer = SGD(learning_rate=0.01, 
                momentum=0.0, 
                nesterov=False)

# _____________________________________________________________
# Compile the model
model.compile(optimizer=Optimizer,
              loss='mean_squared_error',
              metrics=['accuracy'])

# =============================================================
# Train the model
# =============================================================
print("Initialitation values Layers: ")
for layer in model.layers:
    print(layer.get_weights())

model.fit(Inputs, Outputs, epochs=1, batch_size=1)

print("After training values Layers: ")
for layer in model.layers:
    print(layer.get_weights())


# =============================================================
# Make predictions
# =============================================================

Predictions = model.predict(Inputs[0:10])
Results = list(zip(Inputs.tolist(), Outputs.tolist(), Predictions.tolist()))

print("Results: ")
for aTuple in Results:
    print(aTuple)
