-module(zlogc).
-export([
		 init/0, 
		 init_db/0, 
		 main/0, 
		 log_null/2, 
		 log_io/2, 
		 log_dallas/2, 
		 log_file/2, 
		 log_udp/2, 
		 log_msid/3, 
		 set_msid_trace/1,
		 log_st/0,
		 test/0,
		 make_timestamp/0
		]).
-define(ZLOG(Fmt, Args), zlogc:log_file("~w, ~w ~w:~w, ln ~w, " ++ Fmt, [node(), self(), ?MODULE, ?FUNCTION_NAME, ?LINE] ++ Args)).
-define(ZLOGM(MsId, Fmt, Args), zlogc:log_msid(MsId, "~w, pid ~w, ~w:~w, ln ~w, " ++ Fmt, [node(), self(), ?MODULE, ?FUNCTION_NAME, ?LINE] ++ Args)).
-define(ZLOGC_PROC, zlogc_proc).


init() ->
	case whereis(?ZLOGC_PROC) of
		undefined ->
			%%log_file("init proc", []),
			Pid = spawn(?MODULE, main, []),
			register(?ZLOGC_PROC, Pid),

			timer:sleep(100),
			log_io("~w: zlog client initiated. ~n", [node()]);
		_Pid ->
			do_nothing
	end.

init_db() ->
	%%log_file("init db", []),
	ets:new(zlog_db, [public, named_table]).

main() ->
	init_db(),
	case get(socket) of
		undefined ->
			{ok, Socket} = gen_udp:open(0, [binary]),
			put(socket, Socket);
		Socket ->
			void
	end,
	loop(Socket).

loop(Socket) ->
	receive
		Str ->
			log_null("~w received msg: ~w ~n", [self(), Str]),
			case gen_udp:send(Socket, "epgtool9-40", 8888, Str) of
				{error, Reason} ->
					log_io("udp send failed: ~w ~n", [Reason]);
				ok ->
					ok
			end,
			loop(Socket)
	end.

-spec set_msid_trace(MsId :: integer()) -> term().
set_msid_trace(MsId) ->
	Entry = ets:lookup(zlog_db, msid),
	case Entry of
		[{msid, MsList}] ->
			case lists:member(MsId, MsList) of
				true ->
					do_nothing;
				false ->
					NewMsList = [MsId | MsList],
					ets:insert(zlog_db, {msid, NewMsList})
			end;
		_ ->
			ets:insert(zlog_db, {msid, [MsId]})
	end.

-spec get_msid_trace(MsId :: integer()) -> boolean().
get_msid_trace(MsId) ->
	Entry = ets:lookup(zlog_db, msid),
	case Entry of
		[{msid, MsList}] ->
			lists:member(MsId, MsList);
		_ ->
			false
	end.


log_st() ->
	log_file("~w ~p ~n", [?LINE, (catch 1 div 0)]).

log_msid(MsId, Fmt, Args) ->
	case get_msid_trace(MsId) of
		false ->
			do_nothing;
		true ->
			log_udp(Fmt ++ " msid ~w", Args ++ [MsId])
	end.

%% log nothing
log_null(_Fmt, _Args) ->
	void.

%% log to stdio
log_io(Fmt, Args) ->
	io:format("~s: " ++ Fmt ++ "~n", [make_timestamp() | Args]).

%% log using msg_trace 
log_dallas(Fmt, Args) ->
	msg_trace:warning(zws, Fmt, Args).

%% log to /tmp/zlog
log_file(Fmt, Args) ->
	Str = io_lib:format("~s: " ++ Fmt, [make_timestamp() | Args]),
	StrFlat = lists:flatten(Str),
	os:cmd("echo '" ++ StrFlat ++ "' >> /tmp/zlog").

%% log to zlogd (c application)
log_udp(Fmt, Args) ->
	Str = io_lib:format("~s: " ++ Fmt, [make_timestamp() | Args]),
	StrFlat = lists:flatten(Str),
	?ZLOGC_PROC ! StrFlat.

test() ->
	make_timestamp().

make_timestamp() ->
	Now = erlang:timestamp(),
	{_, _, MicroSec} = Now,
	MilliSec = MicroSec div 1000,
	{{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(Now),
	Str = io_lib:format("~2.10.0b/~2.10.0b/~4.10.0b ~2.10.0b:~2.10.0b:~2.10.0b.~3.10.0b",
						[Month,Day,Year,Hour,Minute,Second,MilliSec]),
	lists:flatten(Str).

