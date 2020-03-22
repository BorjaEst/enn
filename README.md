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


## License
This software is under [GPL-3.0](https://www.gnu.org/licenses/gpl-3.0.en.html) license.

