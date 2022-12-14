# Token Ring Architecture

Erlang program that mimics the work distribution and communication of a [Token Ring network](https://en.wikipedia.org/wiki/Token_Ring).  

## How it works

A ”control” process has to offer a user function `go(N,M)` that generates a lists L of M random
integer numbers in the set `{1,2,...,M}`, sets up ring of N processes (so-called ”workers”) and
sends a token to the first worker. Token possession grants the worker possessor permission to eat.
When worker k receives a token, it sends a message `{self(), eat}` to control and sends the
token to next worker.

When control receives a message `{Pid, eat}`, it withdraws the head H of the list L and
appends to a result list the tuple `{Pid, H}`. When list L is empty, control sends a stop message
to the ring that terminates the workers and prints the result list.

## Example run

You run the program by calling `control:go/2`

```erlang
 1> control:go(3,4). 
Targets [4,2,2,1]
Workers [<0.148.0>,<0.147.0>,<0.146.0>]
<0.148.0> eats
<0.147.0> eats
<0.146.0> eats
<0.148.0> eats
[{<0.148.0>,1},{<0.146.0>,2},{<0.147.0>,2},{<0.148.0>,4}]
stop
```