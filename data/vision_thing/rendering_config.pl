:-module(rendering_config, [colour_symbol/2
                           ]).

/** <module> Configuration options for rendering.pl.
*/

%!      colour_symbol(?Colour:atom, ?Symbol:atom) is semidet.
%
%       Mapping between ARC dataset colours and pretty-printer Symbols.
%
%       An additional -1 symbol is added to portray the shape of
%       background objects, i.e. the complement of the shapes of objects
%       extracted from an image with objects/2.
%
colour_symbol(0, '.').
colour_symbol(1, b).
colour_symbol(2, r).
colour_symbol(3, g).
colour_symbol(4, y).
colour_symbol(5, e).
colour_symbol(6, f).
colour_symbol(7, o).
colour_symbol(8, t).
colour_symbol(9, w).
colour_symbol(-1, #).
