-module(svg).
-export([line/5, line/4, box/5]).

% line(X1,Y1,X2,Y2,Option) -> Draw a line between (X1,Y1) and (X2,Y2).
% X1,Y1 : Integer, position of the starting point.
% X2,Y2 : Integer, position of the ending point of the line.
% Option: Tuple, {optionsName, "option's value"}.
line(X1, Y1, X2, Y2) ->
	Line = {path, [{d, "M "
		++erlang:integer_to_list(X1)++" "
		++erlang:integer_to_list(Y1)++" L "
		++erlang:integer_to_list(X2)++" "
		++erlang:integer_to_list(Y2)}], []},
	xmerl:export_simple_content([Line], xmerl_xml).

line(X1,Y1,X2,Y2,{}) ->
	line(X1,Y1,X2,Y2);

line(X1, Y1, X2, Y2, Option) ->
	Line = {path, [{d, "M "
		++erlang:integer_to_list(X1)++" "
		++erlang:integer_to_list(Y1)++" L "
		++erlang:integer_to_list(X2)++" "
		++erlang:integer_to_list(Y2)}, Option], []},
	xmerl:export_simple_content([Line], xmerl_xml).


box(X,Y,W,H, Option) ->
	Rect = {rect, [{x, X}, {y, Y}, {width, W}, {height, H}, Option], []},
	xmerl:export_simple_content([Rect], xmerl_xml).
	
