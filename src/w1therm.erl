%%% @author Guido Günther <agx@sigxcpu.org>
%%% @copyright (C) 2016, Guido Günther
%%% @doc
%%% Read and write GPIO values from userspace
%%%
%%% @reference http://www.raspberrypi-spy.co.uk/2013/03/raspberry-pi-1-wire-digital-thermometer-sensor/
%%% @end
%%% License: LGLv3
-module(w1therm).

-define(W1_BASE, "/sys/bus/w1/devices/").

-export([get_therms/0,
         read_temp/1]).

-type therm() :: string().

% @doc: Get 1wire temperature famliy sensors
-spec get_therms() -> {ok, list(therm())} | {error, atom()}.
get_therms() ->
    case file:list_dir(?W1_BASE) of
        {error, enoent} -> {ok, []};
        {error, Error} -> {error, Error};
        {ok, Files} ->
            {ok, R} = re:compile("^[0-9]{2}-.*"),
            Names = [ X || X <- Files, nomatch /= re:run(X, R)],
            {ok, Names}
    end.

% @doc: Read temperature from 1wire therm device in ⁰C
-spec read_temp(Therm::therm()) -> {ok, float()} | {error, atom()}.
read_temp(Therm) ->
    Sysfs = therm_sysfs_path(Therm, w1_slave),
    {ok, Data} = file:read_file(Sysfs),
    Last = string:sub_word(binary:bin_to_list(Data), 2, $\n),
    TempStr = string:sub_word(Last, 2, $=),
    {Temp, []} = string:to_integer(TempStr),
    {ok, Temp / 1000.0}.

% private functions
-spec therm_sysfs_path(Therm::therm(), atom() | binary()) -> string().
therm_sysfs_path(Therm, File) ->
    io_lib:format(?W1_BASE ++ Therm ++ "/~p", [File]).
