counting_semaphore
=====

counting_semaphore is meant to be used for cases where N of M state machines
need to acquire rights to some shared resource. I.e only 4 workers can be in
a specific state out of 10.

Build
-----

    $ rebar3 compile
