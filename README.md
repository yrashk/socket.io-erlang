Socket.IO for Erlang
====================

## Socket.IO

[Socket.IO](http://socket.io/) aims to make live apps possible in every browser and mobile device, blurring the differences between the different transport mechanisms.

## What is socket.io for Erlang?

Socket.IO-erlang is a full-blown socket.io server reimplementation in
Erlang that is fully compatible with socket.io's javascript client
library.

## How to use

For the time being, take a look at demo/demo.erl

## Building

Depending on a way you have your Erlang distribution installed, you might need crypto/ssl support in Erlang or you will get errors like this:

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
If you use MacPorts to install Erlang instead of Homebrew or manual builds, this is how you install SSL for Erlang:

    sudo port install erlang +ssl

### Linux
Make sure you have the erlang-crypto and erlang-dev packages installed if you're on Debian, erlang-crypto and erlang-devel on Redhat/Fedora. Or better yet, consider building
your Erlang manually, as Erlang packages in Linux distros tend to be
either outdated or broken. Or both.

## Roadmap
Socket.io-erlang has entered maintenance mode. The original [socket.io](http://socket.io) library has been upgraded to version 0.7.x (and the oddly similar version 0.8.x), which is incompatible with major ways with the 0.6.x versions that socket.io-erlang implements. The latest versions took more and more the shape of an entire framework that breaks backwards compatibility on the client-side as well as the server side, which the current socket.io-erlang team of maintainers disagree with.

Because of this, we will be keeping the socket.io-erlang features as they are. We will still maintain the application and try to fix all issues and bugs to keep it working, but the development itself will be suspended.

## Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Don't forget to write tests for your changes
4. Commit changes (`git commit -am 'Add some feature'`)
5. Push to the branch (`git push origin my-new-feature`)
6. Create new Pull Request

## TODO

- License
- Known Issues
