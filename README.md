<h1 align="center">
  hant
</h1>

<p align="center">
  <img src="./img/logo.jpg" width="200" />
</p>

`hant` is a testing framework for partially verifying hybrid automata networks. It utilizes event and bound sequences as test cases, encoding both these test cases and the HAN under test as SMT formulas. 

### Preliminary

Install Z3 SMT solver on your computer.

#### Ubuntu
```bash
sudo apt update
sudo apt -y install z3
```

#### macOS
```bash
brew install z3
```

#### Windows
Follow the official [installation guide](https://github.com/Z3Prover/z3).

### Installation

To run `hant`, follow these steps:
1. Install Haskell toolchains using [GHCup](https://www.haskell.org/ghcup/).
  - GHC 9.2.7
  - cabal 3.10.1.0
  - Stack 2.9.3

2. Clone this repository.


3. Navigate to the directory where the repository was downloaded.
```bash
cd shan
```

### Reproducing experimental results

#### Observing the experimental results

- Run the first experiment with the following command.
```bash
stack run +RTS -N -- observe experiment1
```

- Run the second experiment with the following command.
```bash
stack run +RTS -N -- observe experiment2
```

#### Measuring the coverage of `hant`

- Run the following command that measures the coverage of the experiment 1.
```bash
stack run +RTS -N -- coverage experiment1
```

#### Measuring the performance of `hant`

- Run the following command that measures the verification time of the experiment 1.
```bash
stack run +RTS -N -- parallel experiment1 --output experiment1.html
```
- Run the following command that measures the verification time of the experiment2.
```bash
stack run +RTS -N -- parallel experiment2 --output experiment2.html
```

Once you execute these two commands, `hant` will produce two HTML files named `experiment1.html` and `experiment2.html`, which depict the experimental results.
To visualize these experimental results, open these files using a web browser.

### Raw Data

The raw data of the experimental results are available in the `data` directory.
The execution time reports for the experiments can be accessed online through the following links: [experiment1](https://rico1900.github.io/shan/experiment1) and [experiment2](https://rico1900.github.io/shan/experiment2).