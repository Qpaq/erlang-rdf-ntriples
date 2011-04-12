#!/usr/bin/env escript

main([]) ->
  code:add_path("ebin")
, code:add_path("../pardec/ebin")
, {ok, Triples} = file:consult("test/w3.erl")
, {ok, Data} = file:read_file("test/w3.nt")
, compare(Triples, rdf_ntriples:decode(Data), 0)
  .

compare([], [], N) ->
  io:format("~p triples ok~n", [N]);
compare([Triple | A], [Triple | B], N) ->
  compare(A, B, N + 1);
compare([Triple | A], [Term | B], N) ->
  io:format("expecting: ~p~n", [Triple]),
  io:format("not equal: ~p~n", [Term]),
  compare(A, B, N).
