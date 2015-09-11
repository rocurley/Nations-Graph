To install:

```
git clone https://github.com/rocurley/Nations-Graph.git
cd Nations-Graph
cabal sandbox init
cabal install --only-dependencies
```

To download the graph:

```
#the download command requres a maximum number of pages to grab:
cabal run download 10000
```

This is the sad part: by far the best graph layout algorithm I know of is only to be found for free in the [yEd Graph Editor](http://www.yworks.com/en/products/yfiles/yed/). The download command should have created a file named `out.tgf`, which can be imported into yEd. Run the hirearchical layout algorithm on `out.tgf` and save the result as `out.graphml`. Now run:

```
cabal run join
```

This will join the layout information in the graphml file with the data saved in `out.json`, yielding a new file `out2.json` which can be displayed using the [frontend](https://github.com/rocurley/Nations-Graph-Frontend).
