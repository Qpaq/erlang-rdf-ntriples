-module(rdf_ntriples).

-export([decode/1]).

decode(Input) ->
  decode(Input, []).

decode(<<>>, Accumulator) when is_list(Accumulator) ->
  lists:reverse(Accumulator);
decode(<<>>, _) ->
  done;
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
  , {lang_string, {tag, plain_literal, {either, lang_string_with_language, lang_string_without_language}}}
  , {lang_string_with_language, {tuple, [
      {skip, $"}
    , {re, "^[^\\\"]*"}
    , {skip, "\\\"@"}
    , {re, "^([a-z]+(\\-[-a-z0-9]+)*)"}
    ]}}
  , {lang_string_without_language, {re, "^\\\"([^\\\"]*)\\\"", capture, 1}}
  , {datatype_string, {tag, typed_literal, {tuple, [
      {re, "^\\\"([^\\\"]*)\\\"", capture, 1}
    , {skip, "^^"}
    , uriref
    ]}}}
  , {ws, {either, 'SP', 'HTAB'}}
  ].
