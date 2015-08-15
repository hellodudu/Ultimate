-module(proto).

-export([compile_all/0]).
-export([compile/0]).

compile_all() ->
    {ok, FileList} = file:list_dir("proto"),
    lists:foreach( fun(File)->
                           case filename:extension(File) of
                               ".proto" ->
                                   CompilingFile = "proto/"++File,
                                   protobuffs_compile:generate_source(CompilingFile, 
                                                                      [{output_src_dir, "src/proto"},
                                                                       {output_include_dir, "include"},
                                                                       {imports_dir, ["proto"]}]);
                               _ ->
                                   ignore
                           end
                   end,
                   FileList),
    io:format("Proto Compiled!~n"),
    erlang:halt(0).

compile() ->
    {ok, FileList} = file:list_dir("proto"),
    lists:foreach(fun(SourceFile)->
                          case filename:extension(SourceFile) of
                              ".proto" ->
                                  [FileName, "proto"] = string:tokens(SourceFile, "."),
                                  TargetFile="ebin/" ++ FileName ++ "_pb.beam",
                                  SourceLastModifiedDatetime = filelib:last_modified(SourceFile),
                                  TargetLastModifiedDatetime = filelib:last_modified(TargetFile),
                                  case SourceLastModifiedDatetime > TargetLastModifiedDatetime of
                                      true ->
                                          CompilingFile = "proto/"++SourceFile,
                                          protobuffs_compile:generate_source(CompilingFile, 
                                                                             [{output_src_dir, "src/proto"},
                                                                              {output_include_dir, "include"},
                                                                              {imports_dir,["proto"]}]);
                                      _ ->
                                          ignore
                                  end;
                              _ ->
                                  ignore
                          end
                  end,
                  FileList),
    io:format("Proto Compiled!~n"),
    erlang:halt(0).
