-module(erlsom_tests).

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

gexf_test_() ->
    {module, erlsom_gexf_tests}.
