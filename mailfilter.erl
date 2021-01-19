-module(mailfilter).
-behaviour(gen_server).

% export API
-export(
  [ start/1
  , stop/1
  , default/4
  , add_mail/2
  , get_config/1
  , enough/1
  , add_filter/4
  ]).

% export callback functions
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        ]).

% define types
-type mail() :: any().
-type data() :: any().
-type label() :: any().
-type result() :: {done, data()} | inprogress.
-type labelled_result() :: {label(), result()}.
-type configuration() :: list(labelled_result()).
-type state() :: list({mail(), configuration()}).
-type filter_result() :: {just, data()}
                       | {transformed, mail()}
                       | unchanged
                       | {both, mail(), data()}.
-type filter_fun() :: fun( (mail(), data()) -> filter_result() ).
-type mr() :: {pid(), reference()}. % mail reference {server pid, make_ref()}
-type merge_fun() :: fun( (list(filter_result() | inprogress)) ->
                                filter_result() | continue ).
-type filter() :: {simple, filter_fun()}
                | {chain, list(filter())}
                | {group, list(filter()), merge_fun()}
                | {timelimit, timeout(), filter()}.

% internal state of server represented w/ record
-record(state, { capacity =   0 :: integer() | infinite
               , defaults =  [] :: list(label())
               , filters  = #{} :: #{label() => {data(), filter()}}
               , mails    = #{} :: #{reference() => {mail(), configuration()}}
               , ongoing  =  [] :: list({reference(), pid()})
               , queue    =  [] :: list(pid())
               , timer    =  [] :: list(pid())
               }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% start a mail filter server
-spec start(Cap :: integer() | infinite) -> {ok, pid()} | {error, string()}.
start(Cap) when (is_integer(Cap) andalso Cap > 0) or (Cap == infinite) ->
    gen_server:start(?MODULE, Cap, []);
start(_) ->
    {error, "Capacity must be positive number or infinite"}.

% stop the server and return configs for all mails
-spec stop(MS :: pid()) -> {ok, state()}.
stop(MS) ->
  gen_server:call(MS, stop).

% add a mail to the server, all default filters will be run on it
-spec add_mail(MS :: pid(), Mail :: mail()) -> {ok, mr()} | {error, string()}.
add_mail(MS, Mail) ->
  gen_server:call(MS, {add_mail, Mail}).

% set a default filter for all incoming mails on server
-spec default(MS :: pid(), Label :: label(), Filt :: filter(), Data :: data())
       -> no_return().
default(MS, Label, Filt, Data) ->
  gen_server:cast(MS, {add_default, Label, Filt, Data}).

% get the configuration for a specific mail
-spec get_config(MR :: mr()) -> {ok, configuration()} |{error, Reason :: any()}.
get_config({MS, Ref}) ->
    gen_server:call(MS, {get_config, Ref});
get_config(_) -> {error, "Mail reference is not well formed."}.

% stop filter analysis of MR and remove mail from server
-spec enough(MR :: mr()) -> no_return().
enough(MR) ->
  {MS, Ref} = MR,
  gen_server:cast(MS, {delete, Ref}).

% add a filter function to a specific mail
-spec add_filter(MR :: mr(), Label :: label(), Filt :: filter(), Data :: data())
    -> no_return().
add_filter(MR, Label, Filt, Data) ->
  {MS, Ref} = MR,
  gen_server:cast(MS, {add_filter, Ref, Label, Filt, Data}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%% gen_server callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Cap) ->
   State = #state{capacity = Cap},
   {ok, State}.

-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
            {reply, Reply :: term(), NewState :: term()} |
            {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
            {reply, Reply :: term(), NewState :: term(), hibernate} |
            {noreply, NewState :: term()} |
            {noreply, NewState :: term(), Timeout :: timeout()} |
            {noreply, NewState :: term(), hibernate} |
            {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
            {stop, Reason :: term(), NewState :: term()}.
handle_call({add_mail, Mail}, _From, State) ->
    try
        MS       = self(),
        Ref      = make_ref(),
        MR       = {MS, Ref}, % mail reference is tuple of {server pid, ref}
        Config   = [ {Label, inprogress} || Label <- State#state.defaults],
        Mails    = State#state.mails,
        NewMails = Mails#{Ref => {Mail, Config}},
        NewState = State#state{mails = NewMails},
        % Run default filters on mail
        run_filters(Mail, Ref, State#state.defaults, State#state.filters),
        {reply, {ok, MR}, NewState}
    catch
        % some fault occured
        _ -> {error, "Could not add mail to our server."}
    end;

handle_call({get_config, MailRef}, _From, State) ->
    Mails = State#state.mails,
    Res =
        case maps:get(MailRef, Mails, not_ref) of
            {_, Config} -> {ok, Config};
            not_ref -> {error, "Server holds no data on given mail reference."}
        end,
    {reply, Res, State};

handle_call(stop, _From, State) ->
    {_Refs, Ongoing} = lists:unzip(State#state.ongoing),
    % kill all ongoing filters. DIE!
    lists:foreach(fun (Pid) -> exit(Pid, kill) end, Ongoing),
    % build the State to return
    Mails       = State#state.mails,
    ReturnState = maps:values(Mails),
    % exit normally and return ReturnState :: list({mail(), configuration()})
    {stop, normal, ReturnState, State};

% handling simple filters asking for permission to use capacity
handle_call({can_i_use_capacity, FilterPid}, _From, State) ->
    Capacity = State#state.capacity,
    if
        Capacity == infinite ->
            case lists:keysearch(FilterPid, 2, State#state.timer) of
                false -> {reply, okay, State};
                {value, {SendTo,_}} -> {reply, {okay_set_timer, SendTo}, State}
            end;
        Capacity > 0 ->
            NewCapacity = State#state.capacity - 1,
            NewState    = State#state{capacity = NewCapacity},

            case lists:keysearch(FilterPid, 2, State#state.timer) of
                false ->
                    {reply, okay, NewState};
                {value, {SendTo,_}} ->
                    {reply, {okay_set_timer, SendTo}, NewState}
            end;
        % ELSE: enqueue filter. let it wait until capacity is released.
        true ->
            NewQueue = State#state.queue ++ [FilterPid],
            NewState = State#state{queue = NewQueue},
            {reply, wait_please, NewState}
    end;

handle_call(_Request, _From, State) -> {noreply, State}.


-spec handle_cast(Request :: term(), State :: term()) ->
                  {noreply, NewState :: term()} |
                  {noreply, NewState :: term(), Timeout :: timeout()} |
                  {noreply, NewState :: term(), hibernate} |
                  {stop, Reason :: term(), NewState :: term()}.

% add a default filter to be run on new incoming mails
handle_cast({add_default, Label, Filt, Data}, State) ->
    Filters = State#state.filters,
    case maps:is_key(Label, Filters) of
        true  -> {noreply, State}; %throw Filt away. Already exist Filt w/ Label
        false ->
            case is_filter_well_formed(Filt) of
                true ->
                    NewFilters  = Filters#{Label => {Data, Filt}},
                    NewDefaults = [Label | State#state.defaults],
                    NewState    = State#state{defaults = NewDefaults,
                                              filters  = NewFilters},
                    {noreply, NewState};
                % was not well formed. Keep State as is
                false -> {noreply, State}
            end
    end;

% handling deleting a mail from server and stop the analysis of it
handle_cast({delete, MailRef}, State) ->
    NewMails = maps:remove(MailRef, State#state.mails),
    % partition the Ongoing filters into the ones to delete and to keep.
    {Deletes, NewOngoing} =
        lists:partition(fun ({OnRef, _OnPid}) ->
                           if OnRef == MailRef -> true;
                              true -> false
                           end
                        end, State#state.ongoing),
    % stop all analysis of the mail
    lists:foreach(fun ({_OnRef, OnPid}) -> exit(OnPid, kill) end, Deletes),
    Capacity = State#state.capacity,
    NewCapacity =
        case Capacity of
           infinite -> infinite;
           _        -> Capacity - length(Deletes)
        end,
    NewState = State#state{mails    = NewMails,
                           ongoing  = NewOngoing,
                           capacity = NewCapacity},
    {noreply, NewState};

% handle adding a filter to specific mail
handle_cast({add_filter, Ref, Label, Filt, Data}, State) ->
    Filters = State#state.filters,
    case maps:is_key(Label, Filters) of
        true  -> {noreply, State}; %Throw Filt away. Already exist Filt w/ Label
        false ->
            case is_filter_well_formed(Filt) of
                true  ->
                    Mails      = State#state.mails,
                    {Mail, Config} = maps:get(Ref, Mails),
                    NewConfig  = [{Label, inprogress} | Config],
                    NewMails   = Mails#{Ref => {Mail, NewConfig}},
                    NewFilters = Filters#{Label => {Data, Filt}},
                    NewState   = State#state{filters = NewFilters,
                                             mails   = NewMails},
                    % run the added filter
                    run_filters(Mail, Ref, [Label], NewFilters),
                    {noreply, NewState};
                false -> {noreply, State} % was not well formed. Keep State.
            end
    end;

% handle updating a mail's configuration and possibly itself
handle_cast({Update, Ref, Label, Init, {done, Data}}, State) ->
    Mails = State#state.mails,
    {Mail, Config} = maps:get(Ref, Mails),
    NewState =
        case Update of
           update_data ->
              NewConfig = lists:keyreplace(Label, 1, Config,
                                           {Label, {done, Data}}),
              NewMails = Mails#{Ref := {Mail, NewConfig}},
              State#state{mails = NewMails};
           update_mail ->
              NewConfig = lists:keyreplace(Label, 1, Config,
                                           {Label, {done, Init}}),
              NewMails = Mails#{Ref := {Data, NewConfig}},
              % Mail is transformed. Run other filters on the transformed mail
              {Labels,_} = lists:unzip(Config),
              FiltsToRun = lists:delete(Label, Labels),
              run_filters(Data, Ref, FiltsToRun, State#state.filters),
              State#state{mails = NewMails};
           update_data_mail ->
              {NewData, NewMail} = Data,
              NewConfig = lists:keyreplace(Label, 1, Config,
                                           {Label, {done, NewData}}),
              NewMails = Mails#{Ref := {NewMail, NewConfig}},
              % Mail is transformed. Run other filters on the transformed mail
              {Labels,_} = lists:unzip(Config),
              FiltsToRun = lists:delete(Label, Labels),
              run_filters(NewMail, Ref, FiltsToRun, State#state.filters),
              State#state{mails = NewMails}
        end,

    % incrementing capacity (if possible) because a filter fun is done running
    if State#state.capacity == infinite ->
           {noreply, NewState};
       true ->
           gen_server:cast(self(), increment_capacity),
           {noreply, NewState}
    end;

% handling the decrementing of the current capacity
%handle_cast(decrement_capacity, State) ->
%    NewCapacity = State#state.capacity - 1,
%    NewState    = State#state{capacity = NewCapacity},
%    {noreply, NewState};

% handling the incrementing of the current capacity
handle_cast(increment_capacity, State) ->
    case State#state.queue of
        % if there is no filter fun waiting in queue then increment capacity
        [] ->
            NewCapacity =
                case State#state.capacity of
                    infinite -> infinite;
                    Capacity -> Capacity + 1
                end,
            NewState = State#state{capacity = NewCapacity},
            {noreply, NewState};
        % dequeing and letting next filter fun run
        % does nothing to current capacity. lets it stay the same.
        [H | NewQueue] ->
            case lists:keysearch(H, 2, State#state.timer) of
                % Am in handle_cast so cannot use gen_server:reply {>_<}
                false -> H ! okay;
                {value, {SendTo,_}} -> H ! {okay_set_timer, SendTo}
            end,
            NewState = State#state{queue = NewQueue},
            {noreply, NewState}
    end;

% add {MailRef, pid} to `ongoing` list,
% where the pid is the pid of the process running a filter fun on MailRef's mail
handle_cast({ongoing_add, RefPid}, State) ->
    NewOngoing = [RefPid | State#state.ongoing],
    NewState   = State#state{ongoing = NewOngoing},
    {noreply, NewState};

% delete a process from ongoing. it's done running.
handle_cast({ongoing_del, RefPid}, State) ->
    NewOngoing = lists:delete(RefPid, State#state.ongoing),
    NewState   = State#state{ongoing = NewOngoing},
    {noreply, NewState};

handle_cast({set_timelimit, Pids}, State) ->
    NewTimer   = [Pids | State#state.timer],
    NewState   = State#state{timer = NewTimer},
    {noreply, NewState};
% delete a process from ongoing. it's done running.
handle_cast({delete_timelimit, Pids}, State) ->
    NewTimer   = lists:delete(Pids, State#state.timer),
    NewState   = State#state{timer = NewTimer},
    {noreply, NewState};

handle_cast(_Request, State) -> {noreply, State}.


-spec handle_info(Info :: term() | timeout, State :: term()) ->
                 {noreply, NewState :: term()}
               | {noreply, NewState :: term(), Timeout :: timeout()}
               | {noreply, NewState :: term(), hibernate}
               | {stop, Reason :: term(), NewState :: term()}.
handle_info({From, ongoing}, State) ->
    From ! State#state.ongoing, % used for a test using qc.
    {noreply, State};
handle_info(Msg, State) ->
    Msg ! State, % used for debugging.
    {noreply, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% internal functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec is_filter_well_formed(Filt :: filter()) -> boolean().
is_filter_well_formed({simple, _})       -> true;
is_filter_well_formed({chain, _})        -> true;
is_filter_well_formed({group, _, _})     -> true;
is_filter_well_formed({timelimit, _, _}) -> true;
is_filter_well_formed(_)                 -> false.

-spec run_filters(Mail     :: any(),
                  RefLabel :: reference(),
                  Defaults :: list(label()),
                  Filters  :: #{label() => {data(), filter()}}) -> no_return().
run_filters(Mail, Ref, FilterLabels, AllFilters) ->
    Server = self(),
    % Run all filters listen in FilterLabels
    lists:foreach(
             fun (Label) ->
                %Pid = spawn(fun() ->
                spawn(fun() ->
                        {Init, Filter} = maps:get(Label, AllFilters),
                        % execute a filter with initial data Init
                        Res = execute_filter(Server, Ref, Mail, {Init, Filter}),
                        % Remove filter process from ongoing list
                        gen_server:cast(Server, {ongoing_del, {Ref, self()}}),
                        % update the server accordingly with Res
                        update_server(Server, Res, Ref, Label, Init)
                      end)
             end,
             FilterLabels).

% tell the server what to update with depending on a filter fun's result (Res)
update_server(Server, Res, Ref, Label, InitData) ->
    case Res of
        {just, NewData} ->
            gen_server:cast(Server, {update_data, Ref, Label, InitData,
                                    {done, NewData}});
        {transformed, NewMail} ->
            gen_server:cast(Server, {update_mail, Ref, Label, InitData,
                                    {done, NewMail}});
        {both, NewMail, NewData} ->
            gen_server:cast(Server, {update_data_mail, Ref, Label, InitData,
                                    {done, NewData, NewMail}});
        % everything else is equal to returning unchanged
        _ ->
            gen_server:cast(Server, {update_data, Ref, Label, InitData,
                                    {done, InitData}})
    end.


%%%%%% Handling simple, chain, group, timelimit

% >> simple << is the basecase for the filter() type.
execute_filter(Server, Ref, Mail, {InitData, {simple, Filterfun}}) ->
    try
        % ask if there is capacity, wait if asked to
        Ans = gen_server:call(Server, {can_i_use_capacity, self()}),
        case Ans of
            okay ->
                % Add spawned filter pid and mail ref to ongoing filters list
                gen_server:cast(Server, {ongoing_add, {Ref, self()}}),
                Filterfun(Mail, InitData);
            {okay_set_timer, SendTo} ->
                gen_server:cast(Server, {ongoing_add, {Ref, self()}}),
                SendTo ! start_timer,
                Res = Filterfun(Mail, InitData),
                SendTo ! Res;
            wait_please ->
                receive
                    okay ->
                        gen_server:cast(Server, {ongoing_add, {Ref, self()}}),
                        Filterfun(Mail, InitData);
                    {okay_set_timer, SendTo} ->
                        SendTo ! start_timer,
                        gen_server:cast(Server, {ongoing_add, {Ref, self()}}),
                        Res = Filterfun(Mail, InitData),
                        SendTo ! Res
                end
        end
    catch
        % catch everything.
        _:_ -> unchanged
    end;


% sequential execution of filter functions
execute_filter(Server, Ref, Mail, {InitData, {chain, Filters}}) ->
    case Filters of
        [] -> unchanged;
        _  -> chained(Server, Ref, Mail, InitData, Filters)
    end;

% there is a timelimit for running Filter
execute_filter(Server, Ref, Mail, {InitData, {timelimit, Time, Filter}}) ->
    % spawn a new process as to be able to track when it starts executing
    Pid = spawn(fun () -> execute_filter(Server, Ref, Mail, {InitData, Filter})
                end),
    % log {self(), Pid} so Pid can send start_timer msg to self() when it
    % starts running a filter fun.
    gen_server:cast(Server, {set_timelimit, {self(), Pid}}),
    receive
        start_timer ->
            receive
                Res -> gen_server:cast(Server, {delete_timelimit, self()}),
                       Res
            after
                Time -> unchanged
            end
    end;

execute_filter(Server, Ref, Mail, {InitData, {group, FilterFuns, Merge}}) ->
    grouped(Server, Ref, Mail, InitData, FilterFuns, Merge).


% execute one filter() at a time, propagate data and mail in chain.
% the final result is the result of the last filter() in the list.
chained(MS, Ref, Mail, Init, [H]) -> execute_filter(MS, Ref, Mail, {Init, H});
chained(MS, Ref, Mail, Init, [H | T]) ->
    case execute_filter(MS, Ref, Mail, {Init,H}) of
         {just, NewInit}          -> chained(MS, Ref, Mail, NewInit, T);
         {transformed, NewMail}   -> chained(MS, Ref, NewMail, Init, T);
         {both, NewMail, NewInit} -> chained(MS, Ref, NewMail, NewInit, T);
         % every other return values resembles 'unchanged'
         _ -> chained(MS, Ref, Mail, Init, T)
    end.

grouped(MS, Ref, Mail, Init, Filters, Merge) ->
    % make a list of same length as FilterFuns containing 'inprogress'
    Length  = length(Filters),
    Results = lists:duplicate(Length, inprogress),
    % make a list of tuples w/ indices and Filters
    Indices = lists:seq(0, Length - 1),
    IndFilt = lists:zip(Indices, Filters),
    Box      = self(),
    % Broker to handle updating results and running Merge fun
    Broker  = spawn(fun() -> broker_loop(MS, Box, Results, Merge) end),
    % spawn process for each Filter. They send result to Broker when done.
    lists:foreach(
        fun ({Index, Filt}) ->
            spawn(fun () ->
                     Broker ! {Index, execute_filter(MS, Ref, Mail,
                                                     {Init, Filt})}
                  end)
        end, IndFilt),
    % waiting for Broker to send the final result
    receive
        FinalRes ->
            FinalRes
    end.

% The broker waits for filters to send their results and feeds it
% to the merge function. loops if merge returns `continue`, otherwise send
% final result back as msg.
broker_loop(MS, SendTo, Results, Merge) ->
    receive
        {Index, Res} ->
            gen_server:cast(MS, increment_capacity),
            % Update Results list with incoming Res for a filter at index Index
            {L1, [_E | L2]} = lists:split(Index, Results),
            NewResults     = L1 ++ [Res | L2],
            case Merge(NewResults) of
                continue ->
                    broker_loop(MS, SendTo, NewResults, Merge);
                FinalResult ->
                    SendTo ! FinalResult
            end
    end.

