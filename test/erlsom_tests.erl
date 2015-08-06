-module(erlsom_tests).

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

gexf_test_() ->
    {module, erlsom_gexf_tests}.

all_test_() ->
    {"Test XSD with the xs:all tag.", {module, erlsom_all_tests}}.

extension_test_() ->
    {"Test XSD type extensions.", {module, erlsom_extension_tests}}.
