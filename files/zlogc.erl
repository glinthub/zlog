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
		 log_by_id/3,
		 get_id_trace/1,
		 set_id_trace/1,
		 log_st/0,
		 test/0,
		 get_local_dpn_list/0,
		 is_local_dp/1
		]).
-define(ZLOGF(Fmt, Args), zlogc:log_file("~w, ~w ~w:~w, ln ~w, " ++ Fmt, [node(), self(), ?MODULE, ?FUNCTION_NAME, ?LINE] ++ Args)).
-define(ZLOGU(Fmt, Args), zlogc:log_udp("~w, ~w ~w:~w, ln ~w, " ++ Fmt, [node(), self(), ?MODULE, ?FUNCTION_NAME, ?LINE] ++ Args)).
-define(ZLOGM(_Module, Fmt, Args), zlogc:log_file("~w ~w, ~w ~w:~w, ln ~w, " ++ Fmt, [node(), self(), ?MODULE, ?FUNCTION_NAME, ?LINE] ++ Args)).
-define(ZLOG_ID(Id, Fmt, Args), zlogc:log_by_id(Id, "~w, pid ~w, ~w:~w, ln ~w, " ++ Fmt, [node(), self(), ?MODULE, ?FUNCTION_NAME, ?LINE] ++ Args)).

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
	CsHost = os:cmd("pgrep dpn -a | sed -e 's/.*--l \\([^ ]\\+\\) .*/\\1/g'"),
	CsAddr = lists:subtract(CsHost, "\n"),
	loop(Socket, CsAddr).

loop(Socket, CsAddr) ->
	receive
		Str ->
			log_null("~w received msg: ~w ~n", [self(), Str]),
			case gen_udp:send(Socket, CsAddr, 8888, Str) of
				{error, Reason} ->
					log_io("udp send failed: ~w ~n", [Reason]);
				ok ->
					ok
			end,
			loop(Socket, CsAddr)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Log by ID
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec set_id_trace(Id :: term()) -> term().
set_id_trace(Id) ->
	Entry = ets:lookup(zlog_db, log_obj_id),
	case Entry of
		[{log_obj_id, IdList}] ->
			case lists:member(Id, IdList) of
				true ->
					do_nothing;
				false ->
					NewIdList = [Id | IdList],
					ets:insert(zlog_db, {log_obj_id, NewIdList})
			end;
		_ ->
			ets:insert(zlog_db, {log_obj_id, [Id]})
	end.

-spec get_id_trace(Id :: term()) -> boolean().
get_id_trace(Id) ->
	Entry = ets:lookup(zlog_db, log_obj_id),
	case Entry of
		[{log_obj_id, IdList}] ->
			lists:member(Id, IdList);
		_ ->
			false
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Log calling backtrace to file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_st() ->
	log_st(0).

log_st(Num) ->
	log_file("~w ~p ~n", [?LINE, (catch 1 div Num)]).

log_by_id(Id, Fmt, Args) ->
	case get_id_trace(Id) of
		false ->
			do_nothing;
		true ->
			log_udp(Fmt ++ " logObjId ~w", Args ++ [Id])
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
	init(),
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

get_local_dpn_list() ->
	{ok, LocalHostName} = inet:gethostname(),
	[{all_dpns, AllDpnList}] = ets:lookup(arch_db, all_dpns),
	LocalDpnIdList = lists:foldr(
		fun({DpnId, _RawOrOtp, HostName, _Port}, AccIn) ->
			case HostName =:= LocalHostName of
				true ->
					[DpnId | AccIn];
				false ->
					AccIn
			end
		end, [], AllDpnList),
	LocalDpnIdList.

is_local_dp(DpId) ->
	LocalDpnList = get_local_dpn_list(),
	[DpInfo] = ets:lookup(arch_db, {dp, DpId}),
	{{dp, DpId}, {_, DpnId, _, _CapList}} = DpInfo,
	lists:any(
	  	fun(EntryDpnId) -> 
			EntryDpnId =:= DpnId
		end, LocalDpnList).
