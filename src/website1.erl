-module(website1).

%% Copyright (c) 2012 Joe Armstrong
%% Subject to MIT license
%% See http://www.opensource.org/licenses/mit-license.php

%% This is a website made using cowboy
%% To deploy the website do the following:

%% Edit port/0 and root/0 to appropriate values
%% In this example my server runs on port 8080
%% and the rood of my server is ~/nobackup/fetch_quest/unpack

%% To run the server compile this module together with
%% web_server.erl then run give the command
%% website1:start().

%% What might go wrong?
%% You paths might be wrong
%% Make sure the ebin directorys for cowboy and cowboy_examples
%% is in your path 

-export([start/0, port/0, root/0]).

start() -> web_server:start(?MODULE).

root() ->
    os:getenv("HOME") ++ "/nobackup/fetch_quest/unpack".

port() ->
    8080.
 
