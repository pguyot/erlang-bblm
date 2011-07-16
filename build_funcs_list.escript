#!/opt/erlang/erlang-current/bin/escript

%% Usage: ./build_funcs_list.escript otp/erts/doc/src/*.xml otp/lib/*/doc/src/*.xml > all_functions.plist
-mode(compile).
-export([main/1]).
-include_lib("xmerl/include/xmerl.hrl").

main(XMLFiles) ->
    AllFunctions = lists:flatmap(fun(XMLFile) ->
        process_xml_file(XMLFile)
    end, XMLFiles),
    lists:foreach(fun({_ModuleNameL, _ModuleName, Functions}) ->
        io:put_chars(Functions)
    end, lists:sort(AllFunctions)).

process_xml_file(XMLFile) ->
    {DOM, _Rest} = xmerl_scan:file(XMLFile, [{fetch_path, [code:lib_dir(docbuilder, dtd)]}]),
    case DOM of
        #xmlElement{name = erlref, content = Content} ->
            #xmlElement{name = module, content = ModuleNameElements} = lists:keyfind(module, #xmlElement.name, Content),
            ModuleName = lists:append(lists:map(fun(#xmlText{value = ModuleNameTextValue}) -> ModuleNameTextValue end, ModuleNameElements)),
            FormattedFunctionsR = case lists:keyfind(funcs, #xmlElement.name, Content) of
                #xmlElement{name = funcs, content = Funcs} ->
                    lists:foldl(fun(Func, Acc) ->
                        case Func of
                            #xmlElement{name = func, content = FuncElements} ->
                                [format_function(ModuleName, FuncElements) | Acc];
                            _ -> Acc
                        end
                    end, [], Funcs);
                _ -> [] % no function was found (e.g. erlang_stub.xml)
            end,
            [{string:to_lower(ModuleName), ModuleName, lists:reverse(FormattedFunctionsR)}];
        _ ->
            []  % not an erl_ref file.
    end.

format_function(ModuleName, FuncElements) ->
    FunctionNames = lists:foldl(fun(FuncElement, Acc) ->
        case FuncElement of
            #xmlElement{name = name, content = FuncNameElements} ->
                FuncName = lists:append(lists:map(fun(FuncNameElement) ->
                    case FuncNameElement of
                        #xmlText{value = FuncNameTextValue} -> FuncNameTextValue;
                        #xmlElement{name = c, content = [#xmlText{value = FuncNameTextValue}]} -> FuncNameTextValue
                    end
                end, FuncNameElements)),
                [parse_function_name(ModuleName, FuncName) | Acc];
            _ -> Acc
        end
    end, [], FuncElements),
    {_ResultType, Result} = lists:foldl(fun(ParsedFunctionName, {PreviousReturnType, Acc}) ->
        case ParsedFunctionName of
            {cannot_process, FuncName} ->
                {PreviousReturnType, [io_lib:format("<!-- Cannot process ~s -->\n", [xml_escape(FuncName)]) | Acc]};
            {Name, Args, ReturnType} ->
                FormattedFunctionName = format_function_name(Name, Args, ReturnType),
                {ReturnType, [FormattedFunctionName | Acc]};
            {Name, Args} ->
                FormattedFunctionName = format_function_name(Name, Args, PreviousReturnType),
                {PreviousReturnType, [FormattedFunctionName | Acc]}
        end
    end, {unknown_return_type, []}, FunctionNames),
    Result.

parse_function_name("erlang", FuncName) ->
    parse_function_name0(iolist_to_binary(FuncName));
parse_function_name(Module, FuncName) ->
    parse_function_name0(iolist_to_binary([Module, ":", FuncName])).

parse_function_name0(FuncName0) ->
    % Some initial cleanup.
    FuncName1 = re:replace(FuncName0, <<"\n">>, <<>>, [global, {return, binary}]),
    FuncName2 = re:replace(FuncName1, <<",\\s*">>, <<", ">>, [global, {return, binary}]),
    FuncName = re:replace(FuncName2, <<"\\s*\\|\\s*">>, <<" | ">>, [global, {return, binary}]),
    % Parse FuncName to generate three lines:
    % <string>alarm_handler:clear_alarm</string>
    % <string>alarm_handler:clear_alarm(&lt;#AlarmId#&gt;)</string>
    % <string>alarm_handler:clear_alarm(AlarmId) -&gt; void()</string>
    case re:run(FuncName, <<"^(.*?)\\(([^-]*)\\)(?:\s*(?:->|=)\s*(.*))?$">>, [unicode, {capture, all_but_first, binary}]) of
        nomatch -> {cannot_process, FuncName};
        {match, [Name, Args]} -> {Name, Args};
        {match, [Name, Args, <<>>]} -> {Name, Args};
        {match, [Name, Args, ReturnType]} -> {Name, Args, ReturnType}
    end.

format_function_name(Name, Args, unknown_return_type) ->
    ArgsWithPlaceHolders = put_placeholders(Args),
    [
        io_lib:format("<string>~s</string>\n", [xml_escape(Name)]),
        io_lib:format("<string>~s(~s)</string>\n", [xml_escape(Name), xml_escape(ArgsWithPlaceHolders)]),
        io_lib:format("<string>~s(~s)</string>\n", [xml_escape(Name), xml_escape(Args)])
    ];
format_function_name(Name, Args, ReturnType) ->
    ArgsWithPlaceHolders = put_placeholders(Args),
    [
        io_lib:format("<string>~s</string>\n", [xml_escape(Name)]),
        io_lib:format("<string>~s(~s)</string>\n", [xml_escape(Name), xml_escape(ArgsWithPlaceHolders)]),
        io_lib:format("<string>~s(~s) -&gt; ~s</string>\n", [xml_escape(Name), xml_escape(Args), xml_escape(ReturnType)])
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
