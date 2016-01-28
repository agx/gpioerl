%%% @author Guido Günther <agx@sigxcpu.org>
%%% @copyright (C) 2016, Guido Günther
%%% @doc
%%% Read and write GPIO values from userspace
%%%
%%% @reference https://www.kernel.org/doc/Documentation/gpio/sysfs.txt
%%% @end
%%% License: LGLv3
-module(gpio).

-export([export/1,
         get_exported/0,
         get_direction/1,
         read/1,
         set_direction/2,
         unexport/1,
         write/2]).

-define(GPIO_BASE, "/sys/class/gpio/").
-define(GPIO_EXPORT, ?GPIO_BASE ++ "export").
-define(GPIO_UNEXPORT, ?GPIO_BASE ++ "unexport").

-type direction() :: in | out | low | high.
-type pin() :: pos_integer().

% @doc: write to GPIO pin
-spec write(Pin::pin(), 0 | 1) -> ok | {error, any()}.
write(Pin, Value) ->
    Sysfs = pin_sysfs_path(Pin, value),
    Out = io_lib:format("~p", [Value]),
    file:write_file(Sysfs, Out).

% @doc: read from GPIO pin
-spec read(Pin::pin()) -> {ok, 0 | 1} | {error, atom()}.
read(Pin) ->
    Sysfs = pin_sysfs_path(Pin, value),
    {ok, Data} = file:read_file(Sysfs),
    case Data of
        <<"0">> -> {ok, 0};
        <<"1">> -> {ok, 1};
        _-> {error, badval}
    end.

% @doc: export GPIO pin
-spec export(Pin::pin()) -> ok | {error, any()}.
export(Pin) ->
    Out = io_lib:format("~p", [Pin]),
    file:write_file(?GPIO_EXPORT, Out).

% @doc: get list of exported GPIO pins
-spec get_exported() -> {ok, list(pin())}.
get_exported() ->
    {ok, Files} = file:list_dir(?GPIO_BASE),
    {ok, R} = re:compile("^gpio[0-9]+$"),
    PinNames = [ X || X <- Files, nomatch /= re:run(X, R)],
    {ok, [ Num || {Num, []} <-
      [ string:to_integer(lists:nthtail(4, Pin)) || Pin <- PinNames ]]}.

% @doc: unexport GPIO pin
-spec unexport(Pin::pin()) -> ok | {error, any()}.
unexport(Pin) ->
    Out = io_lib:format("~p", [Pin]),
    file:write_file(?GPIO_UNEXPORT, Out).

% @doc: set GPIO pin direction
-spec set_direction(Pin::pin(), D::direction()) -> ok | {error, any()}.
set_direction(Pin, D) ->
    Sysfs = pin_sysfs_path(Pin, direction),
    Out = io_lib:format("~p", [D]),
    file:write_file(Sysfs, Out).

% @doc: get GPIO pin direction
-spec get_direction(Pin::pin()) -> {ok, direction()} | {error, any()}.
get_direction(Pin) ->
    Sysfs = pin_sysfs_path(Pin, direction),
    {ok, Data} = file:read_file(Sysfs),
    {ok, list_to_atom(string:strip(binary:bin_to_list(Data), right, $\n))}.

%% Private functions
-spec pin_sysfs_path(Pin::pin(), atom() | binary()) -> string().
pin_sysfs_path(Pin, File) ->
    io_lib:format(?GPIO_BASE ++ "gpio~p/~p", [Pin, File]).
