%% -------------------------------------------------------------------
%% @author Will Boyce <mail@willboyce.com> [http://willboyce.com]
%% @copyright 2008 Will Boyce
%% @doc Erb Record Definitions
%% -------------------------------------------------------------------
-author("Will Boyce").

%% @doc Data record representing a line from the server
-record(data, {
        datetime,
        origin,
        operation,
        destination,
        options,
        body}).
