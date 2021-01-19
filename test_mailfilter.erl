-module(test_mailfilter).
-include_lib("eqc/include/eqc.hrl").

-export([test_all/0, test_everything/0]).

-export([ wellbehaved_filter_fun/0
        , filter/1
        , prop_consistency/0
        , prop_mail_is_sacred/0
        , prop_labres_regfil/0
        ]).

test_all() ->
  test_mailfilter_eunit:test_all(),
  quickcheck(prop_mail_is_sacred()),
  quickcheck(prop_labres_regfil()),
  quickcheck(prop_consistency()).

test_everything() ->
  test_all().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% QuickCheck %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


gens() -> oneof([binary(), real(), bool(), int(), char(), nat()]).

gen_filter_result() ->
    ?LET(Data, gens(),
        oneof([ {just, Data}
              , {transformed, Data}
              , unchanged
              , {both, Data, Data}
              ])).

% generates a sweet, kind and wellbehaved filter function.
wellbehaved_filter_fun() ->
    ?LET(Res, gen_filter_result(),
        fun (_Mail, _Data) -> Res end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% generates a filter_result() or the atom continue
gen_merge_res() ->
    ?LET(FR, gen_filter_result(), oneof([FR, continue])).

% generates a merge function for group
gen_merge_fun() ->
    ?LET(Res, gen_merge_res(),
        fun (_Arg) -> Res end).

% generates integer between 0 and 2.
gen_length() ->
    ?LET(Len, choose(0,2), Len).

% generates Time for timed filters().
gen_time() -> frequency([{1, infinite},
                         {4, choose(5, 2000)}]).

% generates filters()
filter(FunGen) ->
    ?LAZY(oneof([
                 ?LET(F, FunGen, {simple, F})

               , ?LET(Len, gen_length(),
                      ?LET(Lst, vector(Len, filter(FunGen)),
                           {chain, Lst}))

               , ?LET({Len, Merge}, {gen_length(), gen_merge_fun()},
                      ?LET(Lst, vector(Len, filter(FunGen)),
                           {group, Lst, Merge}))

               , ?LET({Time, Filt}, {gen_time(), filter(FunGen)},
                           {timelimit, Time, Filt})
               ])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_nr_of_calls() ->
    ?SUCHTHAT({N1, N2}, {choose(3,7), choose(0,4)}, N1 >= N2).

% adds mails to MS
% returns list of Mail references
call_add_mail(_MS, 0, MRs) -> MRs;
call_add_mail(MS, N, MRs) ->
    {ok, MR} = mailfilter:add_mail(MS, "spam"), % nom nom nom!
    call_add_mail(MS, N - 1, [MR | MRs]).

% calls 'enough' on the mail references in the list.
call_enough(0, _MRs) -> [];
call_enough(N, [MR | MRs]) ->
    mailfilter:enough(MR), % enough with your nom!
    call_enough(N - 1, MRs).

% Checks that #add_mail/2 calls - #enough/1 calls  == Length of stop/1 output.
prop_mail_is_sacred() ->
    ?FORALL({MailCalls, EnoughCalls}, gen_nr_of_calls(),
                 begin
                     {ok, MS} = mailfilter:start(infinite),
                     MRs      = call_add_mail(MS, MailCalls, []),
                     call_enough(EnoughCalls, MRs),
                     State    = mailfilter:stop(MS),
                     StateLen = length(State),
                     StateLen == (MailCalls - EnoughCalls)
                 end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% If both filters are done when stop is called =>
%    mail + its config corresponds to applying F1 then F2, or F2 then F1.
prop_consistency() ->
    ?FORALL({[{Mail1, [{Lab1a,_}, {Lab1b,_}]}],
             [{Mail2, [{Lab2a,_}, {Lab2b,_}]}]},
            get_s1s2(t1, t2, l1, l2),
             begin
                (((Mail1 == t1) or (Mail1 == t2))
                 and (case Lab1a of
                        l1 -> Lab1b == l2;
                        l2 -> Lab1b == l1
                      end))
                and
                (((Mail2 == t1) or (Mail2 == t2))
                 and (case Lab2a of
                        l1 -> Lab2b == l2;
                        l2 -> Lab2b == l1
                      end))
             end).

% generate states where both filters are done running.
% this is slow.
get_s1s2(FArg1, FArg2, L1, L2) ->
    ?SUCHTHAT({S1, S2}, gen_filter_mail_transformed(FArg1, FArg2, L1, L2),
                (filter_is_done(S1, FArg1, FArg2) == true)
                and
                (filter_is_done(S2, FArg1, FArg2) == true)).

% true if filter is done running
filter_is_done(S, A1, A2) ->
    case S of
        [{A1, [{_,{done,_}}, {_,{done,_}}]}] -> true;
        [{A2, [{_,{done,_}}, {_,{done,_}}]}] -> true;
        _ -> false
    end.

% filterfun that always transform the mail to A
gen_filter_fun_transformer(A) ->
    fun(_Mail, _Data) -> {transformed, A} end.

% generate two filter()'s and use them to build the states
gen_filter_mail_transformed(FArg1, FArg2, L1, L2) ->
    ?LET(Filt, {filter(gen_filter_fun_transformer(FArg1)),
                filter(gen_filter_fun_transformer(FArg2))},
        begin
            {F1, F2} = Filt,
            S1 = get_S(F1, F2, L1, L2),
            S2 = get_S(F2, F1, L2, L1),
            {S1, S2}
        end).

get_S(F1, F2, L1, L2) ->
    {ok, MS} = mailfilter:start(infinite),
    {ok, MR} = mailfilter:add_mail(MS, "yeehaw"),
    mailfilter:add_filter(MR, L1, F1, 0),
    timer:sleep(5),
    mailfilter:add_filter(MR, L2, F2, 2),
    timer:sleep(5),
    mailfilter:stop(MS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_defaults(_MS, [], _) -> [];
add_defaults(MS, [F | FVec], [L | LVec]) ->
    mailfilter:default(MS, L, F, 0),
    add_defaults(MS, FVec, LVec).

get_config_length(FVec, LabVec) ->
    {ok, MS} = mailfilter:start(infinite),
    add_defaults(MS, FVec, LabVec),
    timer:sleep(5),
    mailfilter:add_mail(MS, "my mail man"),
    timer:sleep(20),
    [{_Mail, Config}] = mailfilter:stop(MS),
    length(Config).

gen_filter_vec() ->
    ?LET(N, choose(3,7),
        ?LET(FVec, vector(N, filter(wellbehaved_filter_fun())),
             begin
                 LabVec = lists:seq(1,N), % labels from 1 to N
                 {N, FVec, LabVec}
             end)).

% Nr of labelled results in result of stop/1 is equal to number of registrered
% filters.
prop_labres_regfil() ->
    ?FORALL({ExpLen, FVec, LabVec}, gen_filter_vec(),
            begin
                LenConf = get_config_length(FVec, LabVec),
                LenConf == ExpLen
            end
           ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%get_ongoing(N, FVec, LabVec) ->
%    {ok, MS} = mailfilter:start(N),
%    add_defaults(MS, FVec, LabVec),
%    timer:sleep(5),
%    mailfilter:add_mail(MS, "my mail man"),
%    timer:sleep(5),
%    % exploit handle_info to get list of ongoing/executing filters
%    MS ! {self(), ongoing},
%    receive S ->
%                io:format("ONGOING, N ~p, ~p~n", [N, S]),
%                mailfilter:stop(MS),
%                length(S)
%    end.
%
%% I did not have the right idea for how to explore this property.
%% %
%% no more than N filters are executing at the same time
%prop_respect_capacity() ->
%    ?FORALL({N, FVec, LabVec}, gen_filter_vec(),
%                begin
%                    get_ongoing(N, FVec, LabVec) =< N
%                end).
