:- module(catalog, [level_files/1, level_file/2]).

level_files(Files) :-
    levels_dir(Dir),
    directory_files(Dir, Entries),
    include([E]>>file_name_extension(_, pl, E), Entries, Names),
    msort(Names, Sorted),
    maplist(directory_file_path(Dir), Sorted, Files).

level_file(Number, File) :-
    level_files(Files),
    nth1(Number, Files, File).

levels_dir(Dir) :-
    module_property(catalog, file(File)),
    file_directory_name(File, LevelDir),
    directory_file_path(LevelDir, '../../../levels', Relative),
    absolute_file_name(Relative, Dir, [file_type(directory)]).
