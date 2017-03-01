-module(index).
-export([get_file_contents/1,show_file_contents/1,get_words_with_line_number/1]).


% index:get_words_with_line_number(Content).
get_words_with_line_number([]) -> [];
get_words_with_line_number(Lines) ->
  group_by_word(
    sort_by_word(
      get_words_with_line_number(Lines, 1))).

get_words_with_line_number([], _LineNumber) -> [];
get_words_with_line_number([Line|Lines], LineNumber) ->
  lists:append(get_words_with_line_number(Lines, LineNumber + 1),
              get_words_from_line(Line, LineNumber)).

get_words_from_line([], _LineNumber) -> [];
get_words_from_line(Line, LineNumber) ->
  add_line_number(re:split(Line," ",[{return,list}]), LineNumber).

add_line_number([], _LineNumber) -> [];
add_line_number([Word|Words], LineNumber) ->
  [{string:to_lower(Word), [LineNumber]}] ++ add_line_number(Words, LineNumber).

sort_by_word(Words) ->
  lists:sort(
    fun({Word1, _LineNumber1}, {Word2, _LineNumber2}) -> Word1 < Word2 end,
    Words).

group_by_word([]) -> [];
group_by_word(WordsWithLineNumber) ->
  group_by_word(WordsWithLineNumber, []).

group_by_word([], WordsWithLineNumbers) -> WordsWithLineNumbers;
group_by_word([{Word, LineNumber}|WordsWithLineNumber], []) ->
  group_by_word(WordsWithLineNumber, [{Word, LineNumber}]);
group_by_word([{Word, LineNumbers}|WordsWithLineNumber], [{LastWord, LastLineNumbers}|WordsWithLineNumbers]) ->
  case Word == LastWord of
    true -> group_by_word(WordsWithLineNumber,[{Word, LastLineNumbers ++ LineNumbers}|WordsWithLineNumbers]);
    false -> group_by_word(WordsWithLineNumber,[{Word, LineNumbers},{LastWord, LastLineNumbers}|WordsWithLineNumbers])
  end.

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)


% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.
% Content = index:get_file_contents("gettysburg-address.txt").
get_file_contents(Name) ->
  {ok,File} = file:open(Name,[read]),
  Rev = get_all_lines(File,[]),
  lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.
get_all_lines(File,Partial) ->
  case io:get_line(File,"") of
    eof -> file:close(File),
      Partial;
    Line ->
      {Strip,_} = lists:split(length(Line)-1,Line),
      get_all_lines(File,[Strip|Partial])
    end.

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.
show_file_contents([L|Ls]) ->
  io:format("~s~n",[L]),
  show_file_contents(Ls);
show_file_contents([]) -> ok.
