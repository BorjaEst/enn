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
```sh
$ rebar3 compile
```

At the end for making a release you first need to create your release structure and then making a release with following commands.
```sh
$ rebar3 new release yourrel
$ rebar3 release
```

>You can find more information about dependencies in [rebar3 - dependencies](https://www.rebar3.org/docs/dependencies). 


## Usage
Load the app using your prefered method. For example in the project folder executing  rebar3 shell:
```sh
$ rebar3 shell
===> Booted enn
```


All user functions are defined inside the module [src/enn](https://github.com/BorjaEst/enn/blob/master/src/enn.erl), however here is an example:



### Measure performance and resources
First of all I woudl initialize the observer, so you can see the loads of the 
system and the ETS tables:
```erl
1> observer:start().
ok
```
> Here you can find a table nn_pool with all the currently enabled neural netwroks.

### Define and start your erlang neural network
You can create a neural network simply with enn:start/1:
```erl
2> Network = enn:start(
2>     model:sequential([
2>         layer:input( 2                       ),
2>         layer:dense( 3, #{activation => tanh}),
2>         layer:output(2                       )
2>     ])).
{network, #Ref<0.367976965.4190896130.201756>}
```
> It is important to save the "Network id", you will need it to stop the network.

Another option is to first compile the model and run it after in 2 steps:
```erl
2> Network = enn:compile(
2>     model:sequential([
2>         layer:input( 2                       ),
2>         layer:dense( 3, #{activation => tanh}),
2>         layer:output(2                       )
2>     ])),
2> enn:start(Network).
{network, #Ref<0.367976965.4190896130.204258>}
```

### Generate/load your training data
Then you should gerenate your training, for example:
```erl
3> Loops  = 2000,
3> Inputs = [[rand:uniform()-0.5, rand:uniform()-0.5] || _ <- lists:seq(1, Loops)],
3> Optima = [[I1+I2, I1-I2] || [I1, I2] <- Inputs],
3> ok.
ok
```

### Train your neural network
This operation is done by enn:fit/3:
```erl
4> enn:fit(Network, Inputs, Optima),
4> ok.
200     [==>.................]  loss:   0.7023357850785679      
400     [====>...............]  loss:   0.4756651445573089      
600     [======>.............]  loss:   0.4007308266912763      
800     [========>...........]  loss:   0.352693640197892       
1000    [==========>.........]  loss:   0.2836852528877784      
1200    [============>.......]  loss:   0.23039384337127725     
1400    [==============>.....]  loss:   0.18402347148602524     
1600    [================>...]  loss:   0.14579458406750548     
1800    [==================>.]  loss:   0.1043275109902257      
2000    [====================]  loss:   0.08482314127899158    
ok
```

### Do some predictions
This operation is done by enn:predict/2:
```erl
5> enn:predict(Network, [
5>     [0.1, 0.6],
5>     [0.3, 0.2],
5>     [0.1, 0.1]
5> ]).
[[0.5807413412972212,-0.453551952162384],
 [0.4702901667692009,0.18243722520393055],
 [0.1914040508238413,0.051403340047410706]]
```

> For more options such log the cycles, or do not do not print the results, explore the options in the function enn:run/4. 


### Stop your neural network
You can easily do it with enn:stop/1:
```erl
6> enn:stop(Network).
ok
```
You will see in the observer window the network is gone.
> You can resume your network with enn:start/1, the last network status is saved!


### Clone your work into a different network
Just call enn:clone/1
```erl
7> Clone_id = enn:clone(Network).
{#Ref<0.367976965.4190896130.204297>,cortex}
```


### Resume your neural network

```erl
8> enn:start(Network),
8> enn:predict(Network, [
8>     [0.1, 0.6],
8>     [0.3, 0.2],
8>     [0.1, 0.1]
8> ]).
[[0.5807413412972212,-0.453551952162384],
 [0.4702901667692009,0.18243722520393055],
 [0.1914040508238413,0.051403340047410706]]
```


## Examples
The folder test includes a module *test_architectures.erl* where you can find a lot of useful examples of models. Feel free to propose yours.


## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.


### Improvement ideas and requests
In progress:
- Momentum, not well implemented on enn, review. This helps a lot when not using batch normalisation.
- Boolean activation function.
- Try which derivade behaves better (and if can be common to some activations).

Erlang performance:
- Use binary for long messages to speedup communications.
- Runtime connections change: ETS table with connections [{{in,N2},N1}, {{out,N1},N2}]

Speed-up training:
- ELU activation function seems by papers to perform better than Sigmoid and ReLU. Leaky ReLU is good as well.

Find correct solution:
- none for now


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
- New Input attribute; Correlation, measures how much the input changed in relation with the error. For example a neuron with inputs A,B,C but optima is A+B, C would have a "correlation" near to 0 and should be deleted.

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

