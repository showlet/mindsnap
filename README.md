# mindsnap
A task management platform similar to trello, built for fun.

https://mindsnap.herokuapp.com/

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
 DATABASE_URL="host=localhost user=<user> password=<password> port=5432 dbname=mindsnap_db" .cabal-sandbox/bin/mindsnap -p 3001
```
