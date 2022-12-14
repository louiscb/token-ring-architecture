# Token Ring Architecture

A ”control” process has to offer a user function go(N,M) that generates a lists L of M random
integer numbers in the set {1,2,...,M}, sets up ring of N processes (so-called ”workers”) and
sends a token to the first worker. Token possesion grants the worker possesor permission to eat.
When worker k receives a token, it sends a message {self(), eat} to control and sends the
token to next worker.

When control receives a message {Pid, eat}, it withdraws the head H of the list L and
appends to a result list the tuple {Pid, H}. When list L is empty, control sends a stop message
to the ring that terminates the workers and prints the result list.