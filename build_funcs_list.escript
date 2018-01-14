#!/usr/bin/env escript

%% Usage:
%% cd ${ERL_TOP}
%% make docs
%% ./build_funcs_list.escript ${ERL_TOP}/erts/doc/ ${ERL_TOP}/lib/*/doc/ > functions_and_types.plist
%% built-in types are not included as specifications of modules, they should be added manually.
%%
%% http://www.erlang.org/doc/reference_manual/typespec.html
%% 
%% 
-mode(compile).
-export([main/1]).
-include_lib("xmerl/include/xmerl.hrl").

main(Dirs) ->
    AllSpecs = lists:flatmap(fun(Dir) ->
        SpecFiles = filelib:wildcard(filename:join([Dir, "specs", "*.xml"])),
        SpecsFromSpecFiles = lists:flatmap(fun(XMLSpec) ->
            try
                process_xml_spec_file(XMLSpec)
            catch T:V ->
                io:format(standard_error, "Could not parse spec file ~s\n~p:~p\n~p\n", [XMLSpec, T, V, erlang:get_stacktrace()]),
                []
            end
        end, SpecFiles),
        SrcFiles = filelib:wildcard(filename:join([Dir, "src", "*.xml"])),
        SpecsFromSrcFiles = lists:flatmap(fun(XMLSrc) ->
            try
                process_xml_src_file(XMLSrc)
            catch T:V ->
                io:format(standard_error, "Could not parse src file ~s\n~p:~p\n~p\n", [XMLSrc, T, V, erlang:get_stacktrace()]),
                []
            end
        end, SrcFiles),
        merge_specs(SpecsFromSpecFiles, SpecsFromSrcFiles)
    end, Dirs),
    SortedSpecs = lists:sort(AllSpecs),
    io:put_chars("\t<key>ErlangFunctionsForCompletion</key>\n"),
    io:put_chars("\t<array>\n"),
    io:put_chars("\t\t<!-- exported and documented functions -->\n"),
    lists:foreach(fun({_ModuleNameL, _ModuleName, Specs}) ->
        Functions = [Formatted || {function, _Name, _Arity, Formatted} <- Specs],
        io:put_chars(Functions)
    end, SortedSpecs),
    io:put_chars("\t</array>\n"),
    io:put_chars("\t<key>ErlangTypesForCompletion</key>\n"),
    io:put_chars("\t<array>\n"),
    io:put_chars("\t\t<!-- built-in types -->\n"),
    io:put_chars("\t\tSee: http://www.erlang.org/doc/reference_manual/typespec.html\n"),
    io:put_chars("\t\t<!-- exported and documented types -->\n"),
    lists:foreach(fun({_ModuleNameL, _ModuleName, Specs}) ->
        Types = [Formatted || {type, _Name, _Arity, Formatted} <- Specs],
        io:put_chars(Types)
    end, SortedSpecs),
    io:put_chars("\t</array>\n").

process_xml_src_file(XMLFile) ->
    ErlDocGenPriv = code:lib_dir(erl_docgen, priv),
    DTDPaths = [filename:join(ErlDocGenPriv, "dtd"), filename:join(ErlDocGenPriv, "dtd_man_entities")],
    {DOM, _Rest} = do_xmerl_scan(XMLFile, [{fetch_path, DTDPaths}]),
    case DOM of
        #xmlElement{name = erlref, content = Content} ->
            #xmlElement{name = module, content = ModuleNameElements} = lists:keyfind(module, #xmlElement.name, Content),
            ModuleName = lists:append(lists:map(fun(#xmlText{value = ModuleNameTextValue}) -> ModuleNameTextValue end, ModuleNameElements)),
            FormattedFunctionsR = case lists:keyfind(funcs, #xmlElement.name, Content) of
                #xmlElement{name = funcs, content = Funcs} ->
                    lists:foldl(fun(Func, Acc) ->
                        case Func of
                            #xmlElement{name = func, content = FuncElements} ->
                                [process_docsrc_function(XMLFile, ModuleName, FuncElements) | Acc];
                            _ -> Acc
                        end
                    end, [], Funcs);
                _ ->
                    io:format("no function found in ~s\n", [XMLFile]),
                    [] % no function was found (e.g. erlang_stub.xml)
            end,
            [{string:to_lower(ModuleName), ModuleName, lists:reverse(FormattedFunctionsR)}];
        _ ->
            []  % not an erl_ref file.
    end.

process_docsrc_function(XMLFile, ModuleName, FuncElements) ->
    FunctionNames = lists:foldl(fun(FuncElement, Acc) ->
        case FuncElement of
            #xmlElement{name = name, content = []} ->
                % New documentation format, specification is generated from source code
                Acc;
            #xmlElement{name = name, content = FuncNameElements} ->
                FuncName = xml_flatten(FuncNameElements, []),
                Parsed = parse_function_head(ModuleName, FuncName),
                case Parsed of
                    {error, cannot_process} ->
                        io:format(standard_error, "Module ~s : cannot process src file ~s\nfuncname = ~s, funcelements = ~1000p\n", [ModuleName, XMLFile, FuncName, FuncNameElements]),
                        Acc;
                    _ -> [Parsed | Acc]
                end;
            _ -> Acc
        end
    end, [], FuncElements),
    {_ResultType, Result} = lists:foldl(fun(ParsedFunctionName, {PreviousReturnType, Acc}) ->
        case ParsedFunctionName of
            {Name, Args, ReturnType} ->
                FormattedFunctionName = format_function(Name, Args, ReturnType),
                {ReturnType, [{function, Name, compute_arity(Args), FormattedFunctionName} | Acc]};
            {Name, Args} ->
                FormattedFunctionName = format_function(Name, Args, PreviousReturnType),
                {PreviousReturnType, [{function, Name, compute_arity(Args), FormattedFunctionName} | Acc]}
        end
    end, {unknown_return_type, []}, FunctionNames),
    Result.


process_xml_spec_file(XMLFile) ->
    {DOM, _Rest} = do_xmerl_scan(XMLFile, []),
    #xmlElement{name = module, attributes = Attributes, content = Content} = DOM,
    #xmlAttribute{value = ModuleName} = lists:keyfind(name, #xmlAttribute.name, Attributes),
    Specifications = process_specs(ModuleName, Content, []),
    [{string:to_lower(ModuleName), ModuleName, lists:reverse(Specifications)}].

process_specs(_Module, [], Acc) -> Acc;
process_specs(Module, [XMLElement | XMLTail], Acc) ->
    case XMLElement of
        #xmlElement{name = type, content = SubElements} ->
            NewAcc = case process_type(Module, SubElements) of
                {value, TypeSpec} -> [TypeSpec | Acc];
                false -> Acc
            end,
            process_specs(Module, XMLTail, NewAcc);
        #xmlElement{name = spec, content = SubElements} ->
            NewAcc = case process_function(Module, SubElements) of
                {value, FunctionSpec} -> [FunctionSpec | Acc];
                false -> Acc
            end,
            process_specs(Module, XMLTail, NewAcc);
        _ ->
            process_specs(Module, XMLTail, Acc)
    end.

process_function(Module, Elements) ->
    case parse_xml_spec(Elements, name, arity, [contract, clause, head]) of
        {ok, {FunctionName, FunctionArity, FunctionDef}} ->
            case parse_function_head(Module, FunctionDef) of
                {error, cannot_process} ->
                    io:format(standard_error, "Module ~s : cannot process ~s (~s)\n", [Module, xml_escape(FunctionName), xml_escape(FunctionDef)]),
                    false;
                {Name, Args, ReturnType} ->
                    {value, {function, Name, FunctionArity, format_function(Name, Args, ReturnType)}}
            end;
        {error, Reason} ->
            io:format(standard_error, "Module ~s : cannot process spec (~100p)\n", [Module, Reason]),
            false
    end.

process_type(Module, Elements) ->
    case parse_xml_spec(Elements, name, n_vars, [typedecl, typehead, marker]) of
        {ok, {TypeName, Arity, TypeDef}} ->
            case parse_type_marker(Module, TypeDef) of
                {error, cannot_process} ->
                    io:format(standard_error, "Module ~s : cannot process ~s (~s)\n", [Module, xml_escape(TypeName), xml_escape(TypeDef)]),
                    false;
                {Name, Args} ->
                    {value, {type, Name, Arity, format_type(Name, Args)}}
            end;
        {error, Reason} ->
            io:format(standard_error, "Module ~s : cannot process type (~p)\n", [Module, Reason]),
            false
    end.

parse_xml_spec(Elements, NameTag, ArityTag, DefPath) ->
    NameElem = lists:keyfind(NameTag, #xmlElement.name, Elements),
    NameContent = NameElem#xmlElement.content,
    Name = xml_flatten(NameContent, []),
    ArityElem = lists:keyfind(ArityTag, #xmlElement.name, Elements),
    ArityContent = ArityElem#xmlElement.content,
    Arity = list_to_integer(unicode:characters_to_list(xml_flatten(ArityContent, []))),
    DefContentT = lists:foldl(fun(PathElement, AccContent) ->
        case AccContent of
            {ok, AccElems} ->
                Elem = lists:keyfind(PathElement, #xmlElement.name, AccElems),
                case Elem of
                    false -> {error, {incomplete_spec, Name}};
                    #xmlElement{} ->
                        ContentWithoutElem = lists:keydelete(PathElement, #xmlElement.name, AccElems),
                        case lists:keyfind(PathElement, #xmlElement.name, ContentWithoutElem) of
                            false -> ok;
                            _Element ->
                                throw({more_than_one_typedecl, Elements})
                        end,
                        {ok, Elem#xmlElement.content}
                end;
            {error, _} -> AccContent
        end
    end, {ok, Elements}, DefPath),
    case DefContentT of
        {ok, DefContent} ->
            Def = xml_flatten(DefContent, []),
            {ok, {Name, Arity, Def}};
        {error, _Reason} ->
            DefContentT
    end.

xml_flatten([], Acc) -> iolist_to_binary(lists:reverse(Acc));
xml_flatten([#xmlText{value = Value} | Tail], Acc) ->
    xml_flatten(Tail, [Value | Acc]);
xml_flatten([#xmlElement{content = Content} | Tail], Acc) ->
    xml_flatten(Content ++ Tail, Acc).

parse_type_marker(Module, Type) ->
    parse_function_head(Module, Type).

parse_function_head("erlang", FuncName) ->
    parse_function_head0(iolist_to_binary(FuncName));
parse_function_head(Module, FuncName) ->
    parse_function_head0(iolist_to_binary([Module, ":", FuncName])).

parse_function_head0(FuncName0) ->
    % Some initial cleanup.
    FuncName1 = re:replace(FuncName0, <<"\n">>, <<>>, [global, {return, binary}]),
    FuncName2 = re:replace(FuncName1, <<",\\s*">>, <<", ">>, [global, {return, binary}]),
    FuncName = re:replace(FuncName2, <<"\\s*\\|\\s*">>, <<" | ">>, [global, {return, binary}]),
    % Parse FuncName to generate three lines:
    % <string>alarm_handler:clear_alarm</string>
    % <string>alarm_handler:clear_alarm(&lt;#AlarmId#&gt;)</string>
    % <string>alarm_handler:clear_alarm(AlarmId) -&gt; void()</string>
    case re:run(FuncName, <<"^(.*?)\\(([^-]*)\\)(?:\\s*(?:->|=)\\s*(.*))?\\s*$">>, [unicode, {capture, all_but_first, binary}]) of
        nomatch -> {error, cannot_process};
        {match, [Name, Args]} -> {Name, Args};
        {match, [Name, Args, <<>>]} -> {Name, Args};
        {match, [Name, Args, ReturnType]} -> {Name, Args, ReturnType}
    end.

format_type(Name, Args) ->
    ArgsWithPlaceHolders = put_placeholders(Args),
    [
        io_lib:format("\t\t<string>~s()</string>\n", [xml_escape(Name)]),
        io_lib:format("\t\t<string>~s(~s)</string>\n", [xml_escape(Name), xml_escape(ArgsWithPlaceHolders)]),
        io_lib:format("\t\t<string>~s(~s)</string>\n", [xml_escape(Name), xml_escape(Args)])
    ].

format_function(Name, Args, unknown_return_type) ->
    ArgsWithPlaceHolders = put_placeholders(Args),
    [
        io_lib:format("\t\t<string>~s</string>\n", [xml_escape(Name)]),
        io_lib:format("\t\t<string>~s(~s)</string>\n", [xml_escape(Name), xml_escape(ArgsWithPlaceHolders)]),
        io_lib:format("\t\t<string>~s(~s)</string>\n", [xml_escape(Name), xml_escape(Args)])
    ];
format_function(Name, Args, ReturnType) ->
    ArgsWithPlaceHolders = put_placeholders(Args),
    [
        io_lib:format("\t\t<string>~s</string>\n", [xml_escape(Name)]),
        io_lib:format("\t\t<string>~s(~s)</string>\n", [xml_escape(Name), xml_escape(ArgsWithPlaceHolders)]),
        io_lib:format("\t\t<string>~s(~s) -&gt; ~s</string>\n", [xml_escape(Name), xml_escape(Args), xml_escape(ReturnType)])
    ].

xml_escape(String0) ->
    String1 = re:replace(String0, <<"&">>, <<"\\&amp;">>, [unicode, global]),
    String2 = re:replace(String1, <<"<">>, <<"\\&lt;">>, [unicode, global]),
    String3 = re:replace(String2, <<">">>, <<"\\&gt;">>, [unicode, global]),
    String3.

put_placeholders(Args) ->
    case re:split(Args, <<",\s*">>, [unicode, {return, binary}]) of
        [<<>>] -> Args;
        [FirstArg | OtherArgs] ->
            OtherArgsWithPlaceHolders = [[<<", <\#">>, OtherArg, <<"\#>">>] || OtherArg <- OtherArgs],
            [<<"<\#">>, FirstArg, <<"\#>">> | OtherArgsWithPlaceHolders]
    end.

%% xmerl accepts encoding="utf-8" but rejects encoding="utf8" as can be found in some system modules
do_xmerl_scan(File, Options) ->
    try
        xmerl_scan:file(File, Options)
    catch exit:{bad_character_code, Content, utf8} ->
        xmerl_scan:string(Content, [{encoding, 'utf-8'} | Options])
    end.

merge_specs(SpecsFromSpecFiles, SpecsFromSrcFiles) ->
    merge_specs0(lists:sort(SpecsFromSpecFiles), lists:sort(SpecsFromSrcFiles), []).

merge_specs0([{ModuleLower, Module, GeneratedSpecs} | GeneratedTail], [{ModuleLower, Module, SrcSpecs} | SrcTail], Acc) ->
    MergedSpecs = merge_module_specs(lists:sort(GeneratedSpecs), lists:sort(SrcSpecs), []),
    NewAcc = [{ModuleLower, Module, MergedSpecs} | Acc],
    merge_specs0(GeneratedTail, SrcTail, NewAcc);
merge_specs0([{GeneratedModuleLower, GeneratedModule, GeneratedSpecs} | GeneratedTail], [{SrcModuleLower, SrcModule, SrcSpecs} | SrcTail], Acc) ->
    if
        GeneratedModuleLower < SrcModuleLower ->
            merge_specs0(GeneratedTail, [{SrcModuleLower, SrcModule, SrcSpecs} | SrcTail], [{GeneratedModuleLower, GeneratedModule, GeneratedSpecs} | Acc]);
        true ->
            merge_specs0([{GeneratedModuleLower, GeneratedModule, GeneratedSpecs} | GeneratedTail], SrcTail, [{SrcModuleLower, SrcModule, SrcSpecs} | Acc])
    end;
merge_specs0(GeneratedSpecs, [], Acc) -> lists:reverse(Acc, GeneratedSpecs);
merge_specs0([], SrcSpecs, Acc) -> lists:reverse(Acc, SrcSpecs).

merge_module_specs([{Type, Name, Arity, Spec} | GeneratedTail], [{Type, Name, Arity, _SrcSpec} | SrcTail], Acc) ->
    % Favor generated specs for now.
    merge_module_specs(GeneratedTail, SrcTail, [{Type, Name, Arity, Spec} | Acc]);
merge_module_specs([GeneratedSpecT | GeneratedTail], [SrcSpecT | SrcTail], Acc) ->
    if
        GeneratedSpecT < SrcSpecT ->
            merge_module_specs(GeneratedTail, [SrcSpecT | SrcTail], [GeneratedSpecT | Acc]);
        true ->
            merge_module_specs([GeneratedSpecT | GeneratedTail], SrcSpecT, [SrcSpecT | Acc])
    end;
merge_module_specs(GeneratedSpecs, [], Acc) -> lists:reverse(Acc, GeneratedSpecs);
merge_module_specs([], SrcSpecs, Acc) -> lists:reverse(Acc, SrcSpecs).

compute_arity(Expr) ->
    length(string:tokens(unicode:characters_to_list(Expr), ",")).


    