:-module(heroes_configuration, [dataset_filename/1
			       ,hero_attributes/1]).


%!	dataset_filename(?Path) is semidet.
%
%	Path to the generated heroes dataset.
%
dataset_filename('data/noise/heroes/heroes.pl').


%!	hero_attributes(?Attributes) is semidet.
%
%	Attributes that should be associated with heroes.
%
hero_attributes([class,honour,faith,kills]).
