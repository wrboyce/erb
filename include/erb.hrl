%% -------------------------------------------------------------------
%% @author Will Boyce <me@willboyce.com> [http://willboyce.com]
%% @copyright 2008-11 Will Boyce
%% @doc Erb Record Definitions
%% -------------------------------------------------------------------
-author("Will Boyce").

%% @doc Active Bot
-record(bot, {
        id,
        network, %% #network.id
        connected_ts=undefined,
        nick,
        chans,
        router=undefined,
        dispatcher=undefined}).

%% @doc Data record representing a line from the server
-record(data, {
        bot,
        datetime,
        origin,
        operation,
        destination,
        options,
        body,
        account}).
