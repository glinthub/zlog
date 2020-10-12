-ifndef(ZLOGC_HRL).
-define(ZLOGC_HRL, true).

-define(ZLOGF(Fmt, Args), zlogc:log_file("~w, ~w ~w:~w, ln ~w, " ++ Fmt, [node(), self(), ?MODULE, ?FUNCTION_NAME, ?LINE] ++ Args)).
-define(ZLOGU(Fmt, Args), zlogc:log_udp("~w, ~w ~w:~w, ln ~w, " ++ Fmt, [node(), self(), ?MODULE, ?FUNCTION_NAME, ?LINE] ++ Args)).
-define(ZLOG_ST(), zlogc:log_udp("~w, ~w ~w:~w, ln ~w, ~p", [node(), self(), ?MODULE, ?FUNCTION_NAME, ?LINE, erlang:process_info(self(), current_stacktrace)])).
-define(ZLOG_ID(Id, Fmt, Args), zlogc:log_by_id(Id, "~w, pid ~w, ~w:~w, ln ~w, " ++ Fmt, [node(), self(), ?MODULE, ?FUNCTION_NAME, ?LINE] ++ Args)).

-define(msg_trace, zlogc).

-endif.
