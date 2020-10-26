-module(zlogc).

-include("zlogc.hrl").

-export([
	debug/3,
	info/3,
	event/3,
	error/3,
	warning/3,
	critical/3
]).

-export([
		 init/0, 
		 init_db/0, 
		 zlogc_task/0, 
		 main/1,
		 log_null/2, 
		 log_io/2, 
		 log_file/2, 
		 log_udp/2, 
		 log_by_id/3,
		 get_id_trace/1,
		 set_id_trace/1,
		 get_st/0,
		 test/0
		]).

-define(ZLOGC_PROC, zlogc_proc).

debug(_Logkey, Fmt, Args) ->
	log_udp(Fmt, Args).

info(_Logkey, Fmt, Args) ->
	log_udp(Fmt, Args).

event(_Logkey, Fmt, Args) ->
	log_udp(Fmt, Args).

error(_Logkey, Fmt, Args) ->
	log_udp(Fmt, Args).

warning(_Logkey, Fmt, Args) ->
	log_udp(Fmt, Args).

critical(_Logkey, Fmt, Args) ->
	log_udp(Fmt, Args).

init() ->
	case whereis(?ZLOGC_PROC) of
		undefined ->
			%%log_file("init proc", []),
			Pid = spawn(?MODULE, zlogc_task, []),
			register(?ZLOGC_PROC, Pid),

			timer:sleep(100),
%%			log_io("~w: zlog client initiated. ~n", [node()]);
			ok;
		_Pid ->
			do_nothing
	end.

init_db() ->
	%%log_file("init db", []),
	ets:new(zlog_db, [public, named_table]).

zlogc_task() ->
	init_db(),
	case get(socket) of
		undefined ->
			{ok, Socket} = gen_udp:open(0, [binary]),
			put(socket, Socket);
		Socket ->
			void
	end,
	%CsHost = "selnpctool-071-02-001",
	CsHost = "localhost",
	zlogc_loop(Socket, CsHost, 0).

zlogc_loop(Socket, CsAddr, Seq) ->
	receive
		exit ->
			exit;
		Str ->
			SeqInsertedStr = "[" ++ erlang:integer_to_list(Seq) ++ "] " ++ Str,
			log_null("~w received msg: ~w ~n", [self(), SeqInsertedStr]),
			case gen_udp:send(Socket, CsAddr, 8888, SeqInsertedStr) of
				{error, Reason} ->
					log_io("udp send failed: ~w ~n", [Reason]);
				ok ->
					ok
			end,
			zlogc_loop(Socket, CsAddr, Seq+1)
	after 2000 ->
			  %%log_io("timeout", []),
			  zlogc_loop(Socket, CsAddr, Seq)
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

get_st() ->
	erlang:process_info(self(), current_stacktrace).

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

%% log to /tmp/zlog
log_file(Fmt, Args) ->
	{ok, IoDevice} = file:open("/tmp/zlog", [write,append]),
	FmtWithNewLine =
		case lists:suffix("~n", Fmt) of
			true ->
				Fmt;
			false ->
				Fmt ++ "~n"
		end,
	io:fwrite(IoDevice, "~s: " ++ FmtWithNewLine, [make_timestamp() | Args]),
	file:close(IoDevice).

%% log to zlogd (c application)
log_udp(Fmt, Args) ->
	init(),
	Str = io_lib:format("~s ~s: " ++ Fmt, [node(), make_timestamp() | Args]),
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

main(Args) ->
	log_file("erlang zlogc file: ~w", [Args]),
	log_udp("erlang zlogc udp: ~w", [Args]),
	ok.
