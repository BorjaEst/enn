# enn
Erlang Neural Networks (enn) is an application to implement artificial inteligence based on artificial neural networks (ANN).


## Installation
Create your own project with rebar3.
 ```sh
 $ rebar3 new app yourapp
 ```

Then in your project path find rebar.config file and add enn as dependency under the deps key:
```erlang
{deps, 
    [
        {enn, {git, "https://github.com/BorjaEst/enn.git", {tag, "<version>"}}}
    ]}.
```

Then using compile command, rebar3 will fetch the defined dependencies and compile them as well for your application.
``sh
$ rebar3 compile
```

At the end for making a release you first need to create your release structure and then making a release with following commands.
```sh
$ rebar3 new release yourrel
$ rebar3 release
```

>You can find more information about dependencies in [rebar3 - dependencies](https://www.rebar3.org/docs/dependencies). 


## Usage
All user functions are defined inside the module enn:
```
TBW
```

>To simplify usage you can load the predefined makros at layers.hrl: 
```
-include_lib("enn/include/layers.hrl").

Model_Example = enn:sequential(
    [
        ?input(10),
        ?dense(10, #{activation => sigmoid}),
        ?output(2)
    ]).
```


## Examples
The folder test indludes a module *test_architectures.erl* where you can find a lot of useful examples of models. Feel free to propose yours.


## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.


### Improvement ideas
In progress:
-Momentum, not well implemented on enn, review. This helps a lot when not using batch normalisation.

Erlang performance:
- Use binary for long messages to speedup communications.

Speed-up training:
- ELU activation function seems by papers to perform better than Sigmoid and ReLU. Leaky ReLU is good as well.

Find correct solution:
- 


Importants to be clasified:
- Normalisation: Batch normalisation
- Regularization: dropout
- Optimizer: Adam
- Learning rate schedule: None
- Dropout, every training step (except output neurons) has a probability to be ignored during that training step. The weights have to be multiplied by this factor after training.
- Play with learning rate:
    - After some trainings, reduce the value a defined value
    - Reduce if the error grows / Proportional to error
    - Exponential reduction
    - Power scheduling
- Weights close to 0 must be set to 0, so in the next construction are removed
- Implement optimisers:
    -Nesterov Accelerated Gradient, performs better than momentum
    -AdaGrad, good idea but not efficient on the last steps
    -RMSProp, improvement of AdaGrad
    -Adam Optimization, adaptative moment estimation. This is the best in almost all cases.


Nice to try:
- Residual Network?, add a transfer function to the activation function to make it time dependent
- Max-Norm regularisation,.      ||w||2 <= r
- Training with Kalman filter
- A DNN can be trained with unlabeled data, it is call unsupervised pertaining.
- To integrate easily keras, use stochastic gradient descent (SGD) together with eager execution.
- Nonsaturating activation functions.
- Gradient Clipping.


Others:
- Implement reusage of networks for drivers etc. Normally the lower layers are the important.
- OPC UA client to get inputs from industry


## License
This software is under [GPL-3.0](https://www.gnu.org/licenses/gpl-3.0.en.html) license.

