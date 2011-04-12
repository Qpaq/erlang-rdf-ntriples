-module(rdf_ntriples).

-export([decode/1, decode/2]).

decode(Input) ->
  decode(Input, []).

decode(<<>>, Accumulator) when is_list(Accumulator) ->
  lists:reverse(Accumulator);
decode(<<>>, _) ->
  ok;
decode(Input, Accumulator) ->
  case readline(Input) of
    [Line, Lines] ->
      decode(Line, Lines, Accumulator);
    [Line] ->
      decode(Line, <<>>, Accumulator)
  end.

decode(Line, Lines, Accumulator) ->
  case pardec:parse(Line, line, pardec_rules()) of
    {Triple={_, _, _}, <<>>} ->
      decode(Lines, accumulate(Triple, Accumulator));
    _ ->
      decode(Lines, Accumulator)
  end.

readline(Input) ->
  binary:split(Input, [<<"\r\n">>, <<"\r">>, <<"\n">>]).

accumulate(Triple, Accumulator) when is_list(Accumulator) ->
  [Triple | Accumulator];
accumulate(Triple, Accumulator={M, F}) ->
  erlang:apply(M, F, [Triple]),
  Accumulator;
accumulate(Triple, Accumulator) ->
  Accumulator(Triple),
  Accumulator.

pardec_rules() ->
  [ {line, {unseq, [{skip, {'*', ws}}, {optional, {either, comment, triple}}]}}
  , {comment, {tag, comment, {re, "^#\\s*(.*)$", capture, 1}}}
  , {triple, {tuple, [
      subject
    , {skip, {'1*', ws}}
    , predicate
    , {skip, {'1*', ws}}
    , object
    , {skip, {'*', ws}}
    , {skip, $.}
    , {skip, {'*', ws}}
    ]}}
  , {subject, {either, uriref, nodeid}}
  , {predicate, uriref}
  , {object, {either, [uriref, nodeid, literal]}}
  , {uriref, {tag, uriref, {re, "^<([^>]+)>", capture, 1}}}
  , {nodeid, {tag, nodeid, {unseq, [{skip, "_:"}, {re, "^[A-Za-z][A-Za-z0-9]*"}]}}}
  , {literal, {either, lang_string, datatype_string}}
  , {lang_string, {tag, plain_literal, {either, lang_string_with_language, string}}}
  , {lang_string_with_language, {tuple, [string, {skip, $@}, {re, "^([a-z]+(\\-[a-z0-9]+)*)"}]}}
  , {datatype_string, {tag, typed_literal, {tuple, [string, {skip, "^^"}, uriref]}}}
  , {ws, {either, 'SP', 'HTAB'}}
  , {string, {unseq, [{skip, $"}, {'*', {either, escape_sequence, character}, until, $"}, {skip, $"}]}}
  , {character, {char, {16#20, 16#7E}}}
  , {escape_sequence, {either, [
      {map, {re, "^\\\\u([0-9A-F]{4})", capture, 1}, {erlang, list_to_integer, [16]}}
    , {const, "\\t", $\t}
    , {const, "\\n", $\n}
    , {const, "\\r", $\r}
    , {const, "\\\"", $"}
    , {const, "\\\\", $\\}
    ]}}
  ].
