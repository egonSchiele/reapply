all:
	cabal install && .hsenv/cabal/bin/reapply
repo:
	new_bitbucket_repo reapply
spec:
	cabal install && .hsenv/cabal/bin/reapply-spec
