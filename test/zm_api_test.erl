-module(zm_api_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, zm_api).


data_includes_field_test() ->
  Name = <<"foo">>,
  Spec = #{<<"name">> => #{type => string}},
  Input = #{<<"name">> => Name},

  {Data, Errors} = zm_api:validate_spec_to_data(Spec, Input),

  ?assertEqual(Name, maps:get(<<"name">>, Data)),
  ?assertEqual([], Errors).

data_doesnt_include_field_test() ->
  Name = <<"foo">>,
  Spec = #{<<"name">> => #{type => string}},
  Input = #{<<"not-name">> => Name},

  {Data, Errors} = zm_api:validate_spec_to_data(Spec, Input),

  ?assertEqual(map_fail, maps:get(<<"name">>, Data, map_fail)),
  ?assertEqual([{field_missing, <<"name">>}], Errors).
