:-module(make_tex_configuration, [directories/2
                                 ,tex_files_dir/2
                                 ,doc_file/1
                                 ,doc_title/1
                                 ]).

/** <module> Documentation for make_tex module.
*/


%!      directories(?What,?Where) is semidet.
%
%       Directories to include or exclude from latex documentation.
%
%       What is one of [included,excluded]. Where is a list of atomic
%       paths to directories with Prolog module files whose structured
%       documentation is to be included or excluded from latex
%       documentation, according to What.
%
directories(included,['./src'
                     ,'./lib'
                     ,'./data/examples'
                     ,'./data/scripts'
                     ]).
directories(excluded,['./lib/metasmith'
                      ,'./lib/metasmith/datasets'
                      ,'./lib/program_reduction/data'
                     ]).


%!      tex_files_dir(?Path,?Leaf) is semidet.
%
%       Relative path to directory for latex documentation files.
%
%       Leaf is the name of the directory where the files are saved,
%       i.e. the leaf of Path.
%
tex_files_dir('./doc/latex/texfiles','texfiles').


%!      doc_file(?Path) is semidet.
%
%       Relative path to main latex documentation file.
%
doc_file('./doc/latex/documentation.tex').


%!      doc_title(?Title) is semidet.
%
%       Title for the latex documentation file.
%
doc_title('Louise source documentation').
