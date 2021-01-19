-module(test_mailfilter_eunit).

-export([test_all/0]).

-include_lib("eunit/include/eunit.hrl").

test_all() ->
    eunit:test( [
                  test_start()
                , test_add_mail()
                , test_get_config()
                , test_default()
                , test_add_filter()
                , test_chained()
                , test_stop()
                , test_enough()
                , test_capacity()
                , test_group()
                , test_timelimit()
                ], [verbose]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% start %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_start() -> [ start_success_int()
                , start_success_inf()
                , start_fail()
                ].

start_success_int() ->
    {"start a mailfilter server with positive capacity",
    fun () ->
        {Res, MS} =  mailfilter:start(1),
        ?assertEqual(ok, Res),
        mailfilter:stop(MS)
    end }.

start_success_inf() ->
    {"start a mailfilter server with infinite capacity",
    fun () ->
        {Res, MS} =  mailfilter:start(infinite),
        ?assertEqual(ok, Res),
        mailfilter:stop(MS)
    end }.

start_fail() ->
    {"fail at starting a mailfilter server",
    fun () ->
        ?assertMatch({error, _}, mailfilter:start(0))
    end }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% add_mail %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_add_mail() -> [add_mail_succ(), add_mail_conf(), add_mail_def()].

add_mail_succ() ->
    {"Add mail and check it returns correctly",
    fun () ->
        {ok, MS} = mailfilter:start(infinite),
        ?assertMatch({ok, {MS, _MR}}, mailfilter:add_mail(MS, "Hola,
                                                          AP-team")),
        mailfilter:stop(MS)
    end}.

add_mail_conf() ->
    {"Add mail and check it has been added/has a configuration",
    fun () ->
        {ok, MS} = mailfilter:start(infinite),
        {ok, MR} = mailfilter:add_mail(MS, "Howdy, AP-team"),
        ?assertEqual({ok, []}, mailfilter:get_config(MR)),
        mailfilter:stop(MS)
    end}.

add_mail_def() ->
    {"Add mail when there is default filter, check the configuration",
    fun () ->
        {ok, MS} = mailfilter:start(infinite),
        mailfilter:default(MS, theAnswer, {simple, fun funny/2}, 0),
        {ok, MR} = mailfilter:add_mail(MS, "Howdy, AP-team"),
        timer:sleep(5),
        ?assertEqual({ok, [{theAnswer, {done, 42}}]},
                     mailfilter:get_config(MR)),
        mailfilter:stop(MS)
    end}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% get_config %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This is testing that get_config/1 fails correctly (negative tests).
% Positive test during test of other functionality, e.g. test of add_mail/2
test_get_config() -> [get_config1(), get_config2()].

get_config1() ->
    {"Test get_config returns error when wrong argument",
    fun () ->
        {ok, MS} = mailfilter:start(infinite),
        ?assertMatch({error, _Reason}, mailfilter:get_config(MS)),
        mailfilter:stop(MS)
    end}.

get_config2() ->
    {"Test get_config returns error when mail is not stored in server",
    fun () ->
        {ok, MS} = mailfilter:start(infinite),
        ?assertMatch({error, _Reason}, mailfilter:get_config({MS, rnd_ref})),
        mailfilter:stop(MS)
    end}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% default %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_default() -> [add_defaults(), add_defaults2()].

funny(Mail, Data) ->
    case is_list(Mail) of
        true  -> {just, Data + 42};
        false -> unchanged % result is Data
    end.

add_defaults() ->
    {"add 2 simpel filters as default",
    fun() ->
        {ok, MS} = mailfilter:start(infinite),
        mailfilter:default(MS, theAnswer, {simple, fun funny/2}, 0),
        mailfilter:default(MS, "lAbEl", {simple, fun funny/2}, 8),
        {ok, MR} = mailfilter:add_mail(MS, "a string:O"),
        timer:sleep(5),
        ?assertEqual({ok, [{"lAbEl", {done, 50}}, {theAnswer, {done, 42}}]},
                     mailfilter:get_config(MR)),
        mailfilter:stop(MS)
    end}.

add_defaults2() ->
    {"Add default, add mail, add one more default which transforms new mails",
    fun () ->
        {ok, MS} = mailfilter:start(infinite),
        mailfilter:default(MS, toogfyrre,
                           {simple, fun (M, D) ->
                                        case M of
                                            "Wut" -> {just, D+42};
                                            "lol HAHA!" -> {just, 1000}
                                        end end}, 0),
        {ok, MR} = mailfilter:add_mail(MS, "Wut"),
        timer:sleep(5),
        ?assertEqual({ok, [{toogfyrre, {done, 42}}]},
                     mailfilter:get_config(MR)),
        mailfilter:default(MS, transform,
                           {simple, fun (M, D) ->
                                        case is_integer(D) of
                                            true -> {transformed, M++" HAHA!"};
                                            false -> unchanged
                                        end end}, 0),
        {ok, MR2} = mailfilter:add_mail(MS, "lol"),
        timer:sleep(5),
        ?assertMatch({ok, [{transform, {done, 0}},
                           {toogfyrre, {done, 1000}}]},
                     mailfilter:get_config(MR2)),
        mailfilter:stop(MS)
    end}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% add_filter %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_add_filter() -> [add_filter1()]. %, add_filter2()].

add_filter1() ->
    {"Add a filter to a mail",
    fun () ->
        {ok, MS} = mailfilter:start(infinite),
        {ok, MR} = mailfilter:add_mail(MS, omg_have_you_seen_this__INCREDIBLE),
        mailfilter:add_filter(MR, spam_or_not, {simple, fun funny/2},
                              naahh_is_not_spam),
        timer:sleep(5),
        ?assertEqual({ok, [{spam_or_not, {done, naahh_is_not_spam}}]},
                     mailfilter:get_config(MR)),
        mailfilter:stop(MS)
    end}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Chained filters %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_chained() -> [chain(), chain2()].

twoChain() -> {chain, [{simple, fun funny/2}, {simple, fun funny/2}]}.

chain() ->
    {"Add a default filter, which is a chain of two simple filters",
    fun () ->
        {ok, MS} = mailfilter:start(infinite),
        mailfilter:default(MS, chain_me_up, twoChain(), 0),
        {ok, MR} = mailfilter:add_mail(MS, "Chains are FUN."),
        timer:sleep(5),
        ?assertEqual({ok, [{chain_me_up, {done, 84}}]},
                     mailfilter:get_config(MR)),
        mailfilter:stop(MS)
    end}.

twoChainChain() -> {chain, [{chain, [{simple, fun funny/2},
                                     {simple, fun funny/2}]},
                            {simple, fun funny/2}]}.
chain2() ->
    {"Add a default filter, which is a chain of a chain and a simple filter",
    fun () ->
        {ok, MS} = mailfilter:start(infinite),
        mailfilter:default(MS, chain_me_up, twoChainChain(), 0),
        {ok, MR} = mailfilter:add_mail(MS, "Chains are FUN."),
        timer:sleep(5),
        ?assertEqual({ok, [{chain_me_up, {done, 126}}]},
                     mailfilter:get_config(MR)),
        mailfilter:stop(MS)
    end}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% stop %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_stop() -> [stop1(), stop2(), stop3()].

stop1() ->
    {"Start a server and stop it immediately.",
    fun() ->
        {ok, MS} = mailfilter:start(infinite),
        ?assertEqual([], mailfilter:stop(MS)),
        ?assertNot(is_process_alive(MS))
    end}.

stop2() ->
    {"Start server, add default filter and mail. Test stop server.",
    fun() ->
        {ok, MS} = mailfilter:start(infinite),
        mailfilter:default(MS, theAnswer, {simple, fun funny/2}, 0),
        {ok, _MR} = mailfilter:add_mail(MS, jibii),
        timer:sleep(5),
        ?assertEqual([{jibii, [{theAnswer, {done, 0}}]}], mailfilter:stop(MS)),
        ?assertNot(is_process_alive(MS))
    end}.

slow(_X, _Y) -> timer:sleep(10000), unchanged.

stop3() ->
    {"Start server, add slow default filter and mail. Test stop server.",
    fun() ->
        {ok, MS} = mailfilter:start(infinite),
        mailfilter:default(MS, theAnswer, {simple, fun slow/2}, 0),
        {ok, _MR} = mailfilter:add_mail(MS, jibii),
        timer:sleep(5),
        ?assertEqual([{jibii, [{theAnswer, inprogress}]}],
                     mailfilter:stop(MS)),
        ?assertNot(is_process_alive(MS))
    end}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% enough %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_enough() -> [enough1()].

enough1() ->
    {"Stop analysis of mail w/ slooow filter",
    fun() ->
        {ok, MS} = mailfilter:start(infinite),
         mailfilter:default(MS, theAnswer, {simple, fun slow/2}, 0),
         {ok, MR} = mailfilter:add_mail(MS, jibii),
         mailfilter:enough(MR),
         ?assertMatch({error, _Reason}, mailfilter:get_config(MR)),
         mailfilter:stop(MS)
    end}.


%enough2() -> {"", fun() -> end}.
%funny(Mail, Data) ->
%    case is_list(Mail) of
%        true  -> {just, Data + 42};
%        false -> unchanged % result is Data
%    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% capacity %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_capacity() -> [cap1(), cap2()].

cap1() ->
    {"Test server only runs upto capacity (1) number of filters",
    fun () ->
        {ok, MS} = mailfilter:start(1),
        mailfilter:default(MS, the_answer, {simple, fun slow/2}, 0),
        mailfilter:default(MS, come_on, {simple, fun funny/2}, 0),
        {ok, MR} = mailfilter:add_mail(MS, jibii),
        ?assertEqual({ok, [{come_on,    inprogress},
                           {the_answer, inprogress}]},
                     mailfilter:get_config(MR)),
         mailfilter:stop(MS)
    end}.

slowChain() -> {chain, [{simple, fun funny/2}, {simple, fun slow/2}]}.
cap2() ->
    {"Test server only runs upto capacity (2) number of filters",
    fun () ->
        {ok, MS} = mailfilter:start(2),
        mailfilter:default(MS, come_on, {simple, fun funny/2}, 0),
        mailfilter:default(MS, the_answer, slowChain(), 0),
        {ok, MR} = mailfilter:add_mail(MS, jibii),
        timer:sleep(10),
        ?assertEqual({ok,
                      [
                       {the_answer, inprogress},
                       {come_on,    {done, 0}}
                      ]},
                     mailfilter:get_config(MR)),
        mailfilter:stop(MS)
    end}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% group %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_group() -> [group1(), group2()].

merge_fun(FiltResults) ->
    case lists:member(inprogress, FiltResults) of
        true  -> continue;
        false -> {just, b@ng}
    end.

group1() ->
    {"Test a grouping of filter functions",
    fun () ->
        {ok, MS} = mailfilter:start(infinite),
        mailfilter:default(MS, gang, { group
                                         , [ {simple, fun funny/2}
                                           , {simple, fun funny/2}
                                           , {simple, fun funny/2}
                                           ]
                                         , fun merge_fun/1}, 0),
        {ok, MR} = mailfilter:add_mail(MS, "can i join the gang"),
        timer:sleep(10),
        ?assertEqual({ok, [{gang, {done, b@ng}}]}, mailfilter:get_config(MR)),
        mailfilter:stop(MS)
    end}.

group2() ->
    {"Test a grouping of filter functions where everyone cannot run at same
     time",
    fun () ->
        {ok, MS} = mailfilter:start(1),
        mailfilter:default(MS, gang, { group
                                         , [ {simple, fun funny/2}
                                           , {simple, fun funny/2}
                                           , twoChain()
                                           , {simple, fun funny/2}
                                           ]
                                         , fun merge_fun/1}, 0),
        {ok, MR} = mailfilter:add_mail(MS, "can i join the gang"),
        timer:sleep(100),
        ?assertEqual({ok, [{gang, {done, b@ng}}]}, mailfilter:get_config(MR)),
        mailfilter:stop(MS)
    end}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% timelimit %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_timelimit() -> [time1(), time2(), time3()].

slow2(_X, Y) -> timer:sleep(10000), {just, Y}.

time1() ->
    {"test running filter with a timeout, that times out",
    fun () ->
        {ok, MS} = mailfilter:start(2),
        mailfilter:default(MS, slowpoke,
                           {timelimit, 50, {simple, fun slow2/2}}, 0),
        {ok, MR} = mailfilter:add_mail(MS, "gotta catch em all"),
        timer:sleep(80),
        ?assertEqual({ok, [{slowpoke, {done, 0}}]}, mailfilter:get_config(MR))
    end}.

time2() ->
    {"test running filter with a timeout, that does not time out",
    fun () ->
        {ok, MS} = mailfilter:start(infinite),
        mailfilter:default(MS, furtpoke,
                           {timelimit, 50, {simple, fun funny/2}}, 0),
        {ok, MR} = mailfilter:add_mail(MS, "gotta catch em all"),
        timer:sleep(20),
        ?assertEqual({ok, [{furtpoke, {done, 42}}]}, mailfilter:get_config(MR))
    end}.

time3() ->
    {"test running filter with a timeout, that does time out",
    fun () ->
        {ok, MS} = mailfilter:start(infinite),
        mailfilter:default(MS, furtpoke,
                           {chain, [{simple, fun funny/2},
                                    {simple, fun funny/2},
                                    {timelimit, 10, {simple, fun slow2/2}}]},
                           pika),
        {ok, MR} = mailfilter:add_mail(MS, "gotta catch em all, boi"),
        timer:sleep(30),
        ?assertEqual({ok, [{furtpoke, {done, pika}}]}, mailfilter:get_config(MR))
    end}.
%funny(Mail, Data) ->
%    case is_list(Mail) of
%        true  -> {just, Data + 42};
%        false -> unchanged % result is Data
%    end.

%slow(_X, _Y) -> timer:sleep(10000), unchanged.

