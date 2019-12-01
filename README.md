# Ye

A TCP load balancer in Haskell


## Usage

To install, run

```
$ cabal install
```


This will build and copy the binary to $HOME/.cabal/bin/ye (or to the cabal installdir if that is configured)

to run this use:


```
ye  --listenPort 4242 --remoteHost mybackend.com --remotePort 80 --debug
```

or

```
ye  --l 4242 -r mybackend.com -p 80 -d
```



This starts a listener at `localhost:4242` which forwards traffic to `mybackend.com:80`
