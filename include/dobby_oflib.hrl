%%%=============================================================================
%%% @copyright (C) 2015, Erlang Solutions Ltd
%%% @author Szymon Mentel <szymon.mentel@erlang-solutions.com>
%%%=============================================================================
%% Add includes and records

-type of_version() :: 4 | 5.

-type flow_mod() :: {Matches :: [term()],
                     Instructions :: [term()],
                     Opts :: [term()]}.

-type datapath_flow_mod() :: {dby_identifier(), of_version(), flow_mod()}.

