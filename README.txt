An Erlang RDF N-Triples reader.

Quick start (reading n-triples from a file):

  $ make
  ...
  $ erl -pa ebin -pa path/to/pardec/ebin
  ...
  1> {ok, Data} = file:read_file("path/to/data.nt").
  ...
  2> rdf_ntriples:decode(Data).
  ...


Triples are currently represented as follows:

  Triple :: {Subject, Predicate, Object}

  Subject :: URI || Node

  Predicate :: URI

  Object :: URI || Node || Literal

  Literal :: PlainLiteral || TypedLiteral

  PlainLiteral :: {'plain_literal', {string(), string()}} | {'plain_literal', string()}

  TypedLiteral :: {'typed_literal', {string(), URI}}

  URI :: {'uriref', string()}

  Node :: {'nodeid', string()}


The function rdf_ntriples:decode/1 will return a list of triples. To process
triples as they are decoded use rdf_ntriples:decode/2, passing either a fun
or a {ModAtom, FunAtom} tuple as the second argument.

Depends on pardec (http://github.com/tim/pardec).
