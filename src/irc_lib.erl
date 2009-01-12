%% -------------------------------------------------------------------
%% @author Will Boyce <mail@willboyce.com> [http://willboyce.com]
%% @copyright 2008 Will Boyce
%% @doc Erb IRC Library
%% -------------------------------------------------------------------
-module(irc_lib).
-author("Will Boyce").

%% API
-export([operation_to_atom/1, pong/1, register/1, register/4, join/1,
			privmsg/2, kick/2, kick/3, mode/2, shun/2, ban/2]).

%% ===================================================================
%% API Functions
%% ===================================================================
%% -------------------------------------------------------------------
%% @spec operation_to_atom() -> atom
%% @doc Parse an Operation into a representive atom as RFC2812
%% -------------------------------------------------------------------
operation_to_atom("001") ->
	rpl_welcome;
operation_to_atom("002") ->
	rpl_yourhost;
operation_to_atom("003") ->
	rpl_created;
operation_to_atom("004") ->
	rpl_myinfo;
operation_to_atom("005") ->
	rpl_bounce;

operation_to_atom("200") ->
	rpl_tracelink;
operation_to_atom("201") ->
	rpl_traceconnecting;
operation_to_atom("202") ->
	rpl_tracehandshake;
operation_to_atom("203") ->
	rpl_traceunknown;
operation_to_atom("204") ->
	rpl_traceoperator;
operation_to_atom("205") ->
	rpl_traceuser;
operation_to_atom("206") ->
	rpl_traceserver;
operation_to_atom("207") ->
	rpl_traceservice;
operation_to_atom("208") ->
	rpl_tracenewtype;
operation_to_atom("209") ->
	rpl_traceclass;
operation_to_atom("210") ->
	rpl_tracereconnect;
operation_to_atom("211") ->
	rpl_tracestatslinkinfo;
%% @todo lots more useless numerics I need to add here.	
operation_to_atom("433") ->
	err_nicknameinuse;

operation_to_atom("PRIVMSG") ->
	privmsg;

operation_to_atom(Other) ->
	Other.
	
%% -------------------------------------------------------------------
%% @spec pong() -> string
%% @doc Reply to a server PING
%% -------------------------------------------------------------------
pong(Server) ->
	"PONG " ++ Server.

%% -------------------------------------------------------------------
%% @spec register() -> string
%% @doc Register with the server
%% -------------------------------------------------------------------
register(Nick) ->
	"NICK " ++ Nick.
register(User, Host, Server, RealName) ->
	"USER " ++ User ++ " " ++ Host ++ " " ++ Server ++ " :" ++ RealName.

%% -------------------------------------------------------------------
%% @spec join(Result, Channels) -> string
%% @doc Join the channel(s)
%% -------------------------------------------------------------------
join(Channels) ->
	join("", Channels).
join(Result, []) ->
	Result;
join(Result, [Channel | Channels]) ->
	join(Result ++ "\r\nJOIN " ++ Channel, Channels).
	
%% -------------------------------------------------------------------
%% @spec privmsg(Dest, Msg) -> string
%% @doc Send Msg to Dest(ination) via PRIVMSG
%% -------------------------------------------------------------------
privmsg(Dest, Msg) ->
	"PRIVMSG " ++ Dest ++ " :" ++ Msg.
	
%% -------------------------------------------------------------------
%% @spec kick(Chan, Nick | Chan, Nick, Reason) -> string
%% @doc Send Msg to Dest(ination) via PRIVMSG
%% -------------------------------------------------------------------
kick(Chan, Nick) ->
	kick(Chan, Nick, Nick).
kick(Chan, Nick, Reason) ->
	"KICK " ++ Chan ++ " " ++ Nick ++ " :" ++ Reason.

%% -------------------------------------------------------------------
%% @spec mode(Dest, Mode) -> string
%% @doc Change mode for Dest(ination)
%% -------------------------------------------------------------------
mode(Dest, Mode) ->
	"MODE " ++ Dest ++ " " ++ Mode.

%% -------------------------------------------------------------------
%% @spec shun(Chan, Mask) -> string
%% @doc Prevent users matching Mask from talking in Chan
%% -------------------------------------------------------------------
shun(Chan, Mask) ->
	mode(Chan, "+q " ++ Mask).
	
%% -------------------------------------------------------------------
%% @spec ban(Chan, Mask) -> string
%% @doc Ban mask from joining Chan
%% -------------------------------------------------------------------
ban(Chan, Mask) ->
	mode(Chan, "+b " ++ Mask).