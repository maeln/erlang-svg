-module(svg).
-export([line/5, line/4, box/5, box/4, createSVG/1]).

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

%
createSVG(Objects) ->
	Data = {svg, [{xmlns, "http://www.w3.org/2000/svg"}, {version, "1.1"}], Objects},
	xmerl:export_simple([Data], xmerl_xml).
