-module(svg).
-export([line/5, line/4, box/5, box/4, createSVG/1, link2/4, link2/5]).

% line(X1,Y1,X2,Y2,Option) -> Draw a line between (X1,Y1) and (X2,Y2).
% X1,Y1 : Integer, position of the starting point.
% X2,Y2 : Integer, position of the ending point of the line.
% Option: Tuple, [{optionsName, "option's value"}].
line(X1, Y1, X2, Y2) ->
	{path, [{d, "M "
		++erlang:integer_to_list(X1)++" "
		++erlang:integer_to_list(Y1)++" L "
		++erlang:integer_to_list(X2)++" "
		++erlang:integer_to_list(Y2)}], []}.

line(X1,Y1,X2,Y2,{}) ->
	line(X1,Y1,X2,Y2);

line(X1, Y1, X2, Y2, Option) ->
	{path, [{d, "M "
		++erlang:integer_to_list(X1)++" "
		++erlang:integer_to_list(Y1)++" L "
		++erlang:integer_to_list(X2)++" "
		++erlang:integer_to_list(Y2)}]++Option, []}.


% box(X,Y,W,H,Option) -> Draw a box at the position (X,Y), with the size (W,H).
% X,Y : Integer, position of the box.
% W,H : Integer, size of the box.
% Option: Tuple, [{optionsName, "option's value"}].
box(X,Y,W,H) ->
	{rect, [{x, X}, {y, Y}, {width, W}, {height, H}], []}.

box(X,Y,W,H,{}) ->
	box(X,Y,W,H);
	
box(X,Y,W,H, Option) ->
	{rect, [{x, X}, {y, Y}, {width, W}, {height, H}]++Option, []}.

% link2(X,Y, A,B) -> Create a squared link between (X,Y) and (A,B).
link2(X,Y, A,B)  ->
	[line(X,Y, A,Y),
	line(A,Y, A,B)].
link2(X,Y, A,B, Option) ->
	[line(X,Y, A,Y, Option),
	line(A,Y, A,B, Option)].

% createSVG(Object) -> Create a valid SVG file with the specified object in argument.
% Object -> a Array of tuple. Simply said, the best way to do something
% with this is to do something like :
% 	svg:createSVG( [ svg:line(args), svg:box(args, ... ] ).
createSVG(Objects) ->
	Data = {svg, [{xmlns, "http://www.w3.org/2000/svg"}, {version, "1.1"}], Objects},
	xmerl:export_simple([Data], xmerl_xml).
