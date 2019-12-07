# Ye

A simple TCP load balancer in Haskell


## Usage

To install, run

```
$ cabal install
```


This will build and copy the binary to $HOME/.cabal/bin/ye (or to the cabal installdir if that is configured)

to run this use:


```
ye  --listenPort 4242 --remoteHosts mybackend-1.com:4000,mybackend-2.com:3000,mybackend-3.com:80 --debug --balancingStrategy ROUND_ROBIN
```

or

```
ye -l 4242 -r mybackend-1.com:4000,mybackend-2.com:3000,mybackend-3.com:80   -d -t ROUND_ROBIN
```



This starts a listener at `localhost:4242` which forwards traffic to the backends:
 -  mybackend-1.com:4000
 -  mybackend-2.com:3000
 -  mybackend-3.com:80

in a round robin distribution

## Available balancing strategies
- ROUND_ROBIN
- SOURCE_IP_HASH 
