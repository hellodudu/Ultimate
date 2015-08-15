-module(config_to_beam).

-export([config_to_beam/0, config_to_beam/1]).
config_to_beam() ->
    config_to_beam(".").

config_to_beam(ConfigDirectory) ->
    {ok, FileNames} = file:list_dir(ConfigDirectory),
    ProtoFiles = lists:filter(fun(File) -> filename:extension(File) =:= ".config" end, FileNames),
    GenerateBeam = 
        fun( FileName ) ->
                {ok, Terms} = file:consult(FileName),
                BaseName = filename:basename( FileName, ".config"),
                ModuleName = BaseName ++ "_config",
                ErlFileHeader = "-module(" ++ ModuleName ++" )." ++ "\n" ++ " -export([get/1])." ++"\n",
                GetFunctions = lists:foldl( fun( {Key, Value}, Acc ) ->
                                                    KeyString = erlang:bitstring_to_list(erlang:list_to_bitstring(io_lib:format("~w",[Key]))),
                                                    ValueString = erlang:bitstring_to_list(erlang:list_to_bitstring(io_lib:format("~p", [Value]))),
                                                    Acc ++ "get(" ++ KeyString ++ ")->" ++ "\n    " ++ValueString ++".\n"
                                            end,
                                            "",
                                            Terms),
                file:write_file(ModuleName++".erl", ErlFileHeader++GetFunctions),
                compile:file( ModuleName ++".erl", [{outdir, "../ebin"}] ),
                file:delete( ModuleName ++ ".erl" )
        end,
    lists:foreach( GenerateBeam, ProtoFiles ).
