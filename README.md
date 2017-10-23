# mindsnap
A task management platform similar to trello, built for fun.

https://mindsnap.herokuapp.com/

(the deployment build might be broken due to configurations with postgres)

to build locally
```SHELL
git clone https://github.com/showlet/mindsnap
cd mindsnap
cabal sandbox init
cabal install
```

This creates the sandbox and the project binairies

To run the site
```SHELL
.cabal-sandbox/bin/mindsnap -p 3001
```
