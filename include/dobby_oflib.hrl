%%%=============================================================================
%%% @copyright (C) 2015, Erlang Solutions Ltd
%%% @author Szymon Mentel <szymon.mentel@erlang-solutions.com>
%%%=============================================================================
%% Add includes and records

-type flow_path() :: [#{DatapathId :: binary() =>
                                      list({OFVersion :: 4 | 5, [flow_mod()]})
                       }].

-type of_version() :: 4 | 5.

-type flow_mod() :: {Matches :: [term()],
                     Instructions :: [term()],
                     Opts :: [term()]}.

