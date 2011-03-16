Socket.IO for Erlang
====================

## what is socket.io for Erlang?

*TODO: add description*

### socket.io

[Socket.IO](http://socket.io/) aims to make realtime apps possible in every browser and mobile device, blurring the differences between the different transport mechanisms.





## Dependencies

You need crypto/ssl support in Erlang or you will get errors like this:

    Uncaught error in rebar_core: {'EXIT',
                                      {undef,
                                          [{crypto,start,[]},
                                           {rebar,run_aux,1},
                                           {rebar,main,1},
                                           {escript,run,2},
                                           {escript,start,1},
                                           {init,start_it,1},
                                           {init,start_em,1}]}}
    make: *** [deps] Error 1

see [https://github.com/basho/riak_wiki/issues/45](https://github.com/basho/riak_wiki/issues/45)

### OSX: 
Installing SSL for Erlang:

    sudo port install erlang +ssl

### Linux
Make sure you have the erlang-crypto and erlang-dev packages installed if you're on Debian, erlang-crypto and erlang-devel on Redhat/Fedora.

## TODO

- License
- How can I contribute?
- Roadmap
- Known Issues