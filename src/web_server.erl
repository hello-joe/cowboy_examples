-module(web_server).

%% Copyright (c) 2012 Joe Armstrong
%% Subject to MIT license
%% See http://www.opensource.org/licenses/mit-license.php

%% An example web-server using cowboy
%% To use the web server see the instructions in
%% website1.erl

-compile(export_all).

%% batch is provided so you can call web_server start from a makefile

batch([Mod]) ->
    start(Mod).

%% Mod is a module providing a port() and root() callbaks.
%% for example:
%%  -module(site1).
%%  -export([port/0, root/0]).
%%
%%  port() -> 8080.
%%  root() -> os:getenv("HOME") ++ "/nobackup/fetch_quest/unpack".

start(Mod) ->
    Port = Mod:port(),
    ok = application:start(cowboy),
    Dispatch = [{'_', [{'_', ?MODULE, Mod}]}],  
    NumberOfAcceptors = 100,
    Status = cowboy:start_listener(my_named_thing,
				   NumberOfAcceptors,
				   cowboy_tcp_transport, [{port, Port}],
				   cowboy_http_protocol, [{dispatch, Dispatch}]),
    case Status of
	{error, _} ->
	    io:format("web_server could not be started -- port probably in use~n"),
	    init:stop();
	{ok, _Pid} ->
	    ok
    end.

init(_, Req, E0) -> 
    %% io:format("init Env=~p~n",[E0]),
    {ok, Req, E0}.

terminate(_, _) ->  
    %% io:format("we terminate~n"),
    ok.

handle(Req, Env) ->
    Resource = path(Req),
    %% io:format("Resource=~p~n",[Resource]),
    handle(Resource, Req, Env).

handle(["root",File], Req, Mod) ->
    send_file_or_dir(File, Req, Mod);
handle([], Req, Mod) ->
    send_page(html, list_dir(Mod:root()), Req, Mod);
handle(Path, Req, Mod) ->
    M = filename:split(Mod:root()),
    File = filename:join(M ++  Path),
    send_file_or_dir(File, Req, Mod).

send_file_or_dir(File, Req, Mod) ->
    case filelib:is_dir(File) of
	true ->
	    send_page(html, list_dir(File), Req, Mod);
	false ->
	    %% io:format("get_file:~p~n",[File]),
	    case file:read_file(File) of
		{ok, Bin} ->
		    case classify_extension(filename:extension(File)) of
			text ->
			    Data = ["<pre>",quote(binary_to_list(Bin)),"</pre>"],
			    send_page(html, Data, Req, Mod);
			Type ->
			    send_page(Type, Bin, Req, Mod)
		    end;
		{error, _}->
		    io:format("*** Missing page:~p~n",[File]),
		    send_page(html, pre({no_such_page,File}), Req, Mod)
	    end
    end.

list_dir(Dir) ->
    io:format("list_dir:~p~n",[Dir]),
    case file:list_dir(Dir) of
	{ok, L} ->
	    L1 = [format_child(Dir, I) || I <- lists:sort(L)],
	    Links = [["<a href='./",I,"'>",I,"</a></br>\n"]||I <- L1],
	    Links;
	_ ->
	    {pre,{no_such_directory,Dir}}
    end.

format_child(Dir, I) ->
    C=filename:join(Dir,I),
    %% io:format("child=~p~n",[{Dir,I,C}]),
    case filelib:is_dir(C) of
	true  -> I ++ "/";
	false -> I
    end.

path(Req) ->
    {Path, _} = cowboy_http_req:path(Req),
    Path1 = [binary_to_list(I) || I <- Path],
    Path1.

pre(X) ->
    ["<pre>\n",quote(lists:flatten(io_lib:format("~p",[X]))), "</pre>"].

quote("<" ++ T) -> "&lt;" ++ quote(T);
quote("&" ++ T) -> "&amp;" ++ quote(T);
quote([H|T])    -> [H|quote(T)];
quote([])       -> [].

send_page(Type, Data, Req, Mod) ->
    {_, Req1} = cowboy_http_req:reply(200, [{<<"Content-Type">>,
					     list_to_binary(mime_type(Type))}],
				      Data, Req),
    {ok, Req1, Mod}.

mime_type(gif)     -> "image/gif";
mime_type(jpg)     -> "image/jpeg";
mime_type(png)     -> "image/png";
mime_type(css)     -> "text/css";
mime_type(special) -> "text/plain; charset=x-user-defined";
mime_type(json)    -> "application/json";
mime_type(swf)     -> "application/x-shockwave-flash";
mime_type(html)    -> "text/html";
mime_type(xul)     -> "application/vnd.mozilla.xul+xml";
mime_type(js)      -> "application/x-javascript";
mime_type(svg)     -> "image/svg+xml";
mime_type(woff)    -> "application/x-font-woff";
mime_type(ttf)     -> "application/x-font-ttf";
mime_type(mp3)     -> "audio/mpeg";
mime_type(_)       -> "text/html".

classify_extension(X) ->
    classify_extension1(elib1_misc:to_lower(X)).

classify_extension1(".gif")  -> gif;
classify_extension1(".jpg")  -> jpg;
classify_extension1(".jpeg") -> jpg;
classify_extension1(".js")   -> js;
classify_extension1(".css")  -> css;
classify_extension1(".html") -> html;
classify_extension1(".png")  -> png;
classify_extension1(".json") -> json;
classify_extension1(".svg")  -> svg;
classify_extension1(".mp3")  -> mp3;
classify_extension1(".woff") -> woff;
classify_extension1(".ttf")  -> ttf;
classify_extension1(_)       -> text. 
