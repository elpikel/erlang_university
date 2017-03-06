-module(index).
-export([get_file_contents/1,get_words_with_line_number/1]).

% Gets list of entries consisting of a word and a list of the ranges of lines
% on which it occurs.
%
% Sample usage:
% Content = index1:get_file_contents("gettysburg-address.txt").
% index1:get_words_with_line_number(Content).
get_words_with_line_number([]) -> [];
get_words_with_line_number(Lines) ->
  sort_by_word(
    format_word_lines(
      group_by_word(
          get_words_with_line_number(Lines, 1)))).

% Transforms list of words into list of words associated with line number
% on which it occurs.
get_words_with_line_number([], _LineNumber) -> [];
get_words_with_line_number([Line|Lines], LineNumber) ->
  sort_by_word(
    lists:append(
      get_words_with_line_number(Lines, LineNumber + 1),
      get_words_from_line(Line, LineNumber))).

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

format_word_lines([]) -> [];
format_word_lines([{Word,LineNumbers}|WordsWithLineNumbers]) ->
  [{Word, group_lines(lists:usort(LineNumbers))}|format_word_lines(WordsWithLineNumbers)].

group_lines([]) -> [];
group_lines([LineNumber|LineNumbers]) ->
  group_lines([{LineNumber, LineNumber}], LineNumbers).

group_lines(GroupedLineNumbers, []) -> sort_by_line_number(GroupedLineNumbers);
group_lines([{LineNumber1, LineNumber2}|GroupedLineNumbers], [LineNumberToCheck|LineNumbers]) ->
  case LineNumber2 + 1 == LineNumberToCheck of
    true -> group_lines([{LineNumber1, LineNumberToCheck}|GroupedLineNumbers], LineNumbers);
    false -> group_lines([{LineNumberToCheck, LineNumberToCheck},{LineNumber1, LineNumber2}|GroupedLineNumbers], LineNumbers)
  end.

sort_by_line_number(GroupedLineNumbers) ->
  lists:sort(
    fun({LineNumber1, _LineNumber2}, {LineNumber3, _LineNumber4}) -> LineNumber1 < LineNumber3 end,
    GroupedLineNumbers).

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)

% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.
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
