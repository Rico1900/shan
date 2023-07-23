# shan

`shan` is a verification tool for verifying scenario-controlled hybrid automata network, which combines interrupt sequence diagrams and hybrid automata networks to facilitate modelling of distributed hybrid systems.

### Installation

To run `shan`, follow these steps:
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

```bash
stack run +RTS -N -- parallel single1 --output single1.html
```

```bash
stack run +RTS -N -- parallel single2 --output single2.html
```
- Run the first experiment with the following command.
```bash
stack run +RTS -N -- parallel experiment1 --output experiment1.html
```
- Run the second experiment with the following command.
```bash
stack run +RTS -N -- parallel experiment2 --output experiment2.html
```

Once you execute these two commands, `shan` will produce two HTML files named `experiment1.html` and `experiment2.html`, which depict the experimental results.
To visualize these experimental results, open these files using a web browser.