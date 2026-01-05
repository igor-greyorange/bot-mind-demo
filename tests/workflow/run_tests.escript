#!/usr/bin/env escript
%%! -pa _build/default/lib/bot_mind/ebin

%%% Comprehensive Test Runner
%%% Purpose: Run all workflow tests in sequence with proper server management

main(Args) ->
    io:format("BOT SYSTEM COMPREHENSIVE TEST SUITE~n"),
    io:format("=====================================~n"),
    
    %% Parse arguments
    TestsToRun = case Args of
        [] -> ["all"];
        _ -> Args
    end,
    
    %% Check if server is running
    check_server_status(),
    
    %% Run selected tests
    run_test_suite(TestsToRun).

%% Check if server is accessible
check_server_status() ->
    io:format("🔍 Checking server status...~n"),
    case gen_tcp:connect("localhost", 5555, [binary, {packet, line}, {active, false}]) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            io:format("Server is running and accessible~n");
        {error, _Reason} ->
            io:format("Server is not running!~n"),
            io:format("Please start the server first:~n"),
            io:format("   cd /Users/ravi.mishra/dev/botsys/bot_mind~n"),
            io:format("   make run~n"),
            io:format("   (or in another terminal: erl -pa _build/default/lib/bot_mind/ebin -s application start bot_mind)~n"),
            exit(1)
    end.

%% Run the test suite
run_test_suite(["all"]) ->
    run_test_suite(["1", "2", "3"]);

run_test_suite(TestNumbers) ->
    TestFiles = [
        {"1", "test_01_server_connection.escript", "Server Connection & Ping Test"},
        {"2", "test_02_bot_management.escript", "Bot Registration & Management"},
        {"3", "test_03_complete_workflow.escript", "Complete Bot Workflow"}
    ],
    
    Results = lists:map(fun(TestNum) ->
        case lists:keyfind(TestNum, 1, TestFiles) of
            {_, Filename, Description} ->
                run_single_test(Filename, Description);
            false ->
                io:format("Unknown test number: ~s~n", [TestNum]),
                {error, unknown_test}
        end
    end, TestNumbers),
    
    %% Print summary
    print_test_summary(Results).

%% Run a single test file
run_single_test(Filename, Description) ->
    io:format("~nRunning: ~s~n", [Description]),
    io:format("File: ~s~n", [Filename]),
    io:format("  Starting at: ~s~n", [format_timestamp()]),
    
    TestPath = lists:flatten(io_lib:format("tests/workflow/~s", [Filename])),
    
    %% Execute the test
    case os:cmd(lists:flatten(io_lib:format("escript ~s", [TestPath]))) of
        Output when is_list(Output) ->
            io:format("~s~n", [Output]),
            case string:find(Output, "PASSED") of
                nomatch -> 
                    case string:find(Output, "FAILED") of
                        nomatch -> {unknown, Description};
                        _ -> {failed, Description}
                    end;
                _ -> {passed, Description}
            end;
        Error ->
            io:format("Test execution error: ~p~n", [Error]),
            {error, Description}
    end.

%% Print comprehensive test summary
print_test_summary(Results) ->
    io:format("~n📊 TEST SUITE SUMMARY~n"),
    io:format("=====================~n"),
    
    Passed = length([1 || {passed, _} <- Results]),
    Failed = length([1 || {failed, _} <- Results]),
    Errors = length([1 || {error, _} <- Results]),
    Unknown = length([1 || {unknown, _} <- Results]),
    Total = length(Results),
    
    io:format("Total Tests: ~p~n", [Total]),
    io:format("Passed: ~p~n", [Passed]),
    io:format(" Failed: ~p~n", [Failed]),
    io:format(" Errors: ~p~n", [Errors]),
    io:format(" Unknown: ~p~n", [Unknown]),
    
    %% Print individual results
    lists:foreach(fun({Status, Description}) ->
        Icon = case Status of
            passed -> "passed";
            failed -> "failed";
            error -> "error";
            unknown -> "unknown"
        end,
        io:format("~s ~s~n", [Icon, Description])
    end, Results),
    
    %% Overall result
    case Failed + Errors of
        0 -> 
            io:format("~n ALL TESTS PASSED! Bot system is working correctly!~n");
        FailCount ->
            io:format("~n  ~p TESTS HAD ISSUES. Please check the output above.~n", [FailCount])
    end.

%% Format current timestamp
format_timestamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:local_time(),
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", 
                                [Year, Month, Day, Hour, Min, Sec])).