:-module(mtg_fragment, [background_knowledge/2
		       ,metarules/2
		       ,positive_example/2
		       ,negative_example/2

		       ,destroy_verb/2
		       ,exile_verb/2
		       ,return_verb/2

		       ,target_permanent/2

		       ,target/2

		       ,target_artifact_type/2
		       ,target_creature_type/2
		       ,target_enchantment_type/2
		       ,target_land_type/2
		       ,target_basic_land_type/2
		       ,target_planeswalker_type/2

		       ,all_permanents_of_type/2

		       ,artifact_type/2
		       ,creature_type/2
		       ,enchantment_type/2
		       ,land_type/2
		       ,basic_land_type/2
		       ,planeswalker_type/2

		       ,all_of_artifact_type/2
		       ,all_of_creature_type/2
		       ,all_of_enchantment_type/2
		       ,all_of_land_type/2
		       ,all_of_basic_land_type/2
		       ,all_of_planeswalker_type/2

		       ,all/2

		       ,artifact_types/2
		       ,creature_types/2
		       ,enchantment_types/2
		       ,land_types/2
		       ,basic_land_types/2
		       ,planeswalker_types/2

		       ,a_permanent_type/2
		       ,permanent_type/2
		       ,permanent_types/2

		       ,target_from_battlefield_to_hand/2
		       ,target_from_graveyard_to_hand/2
		       ,target_from_graveyard_to_battlefield/2

		       ,from_battlefield_to_hand/2
		       ,from_graveyard_to_hand/2
		       ,from_graveyard_to_battlefield/2

		       ,all_from_battlefield_to_hand/2
		       ,all_from_graveyard_to_hand/2
		       ,all_from_graveyard_to_battlefield/2

		       ,permanents/2

		       ,all_of_type_from_battlefield_to_hand/2
		       ,all_of_type_from_graveyard_to_hand/2
		       ,all_of_type_from_graveyard_to_battlefield/2

		       ,to_owners_hand/2
		       ,to/2
		       ,its/2
		       ,owners_gen/2
		       ,zone_hand/2
		       ,to_owners_hands/2
		       ,their/2
		       ,owners_gen_plural/2
		       ,hands/2

		       ,from_graveyard/2
		       ,from/2
		       ,your/2
		       ,a/2
		       ,zone_graveyard/2

		       ,to_battlefield/2
		       ,the/2
		       ,zone_battlefield/2
		       ]).

/** <module> Learning a fragment of the Magic: the Gathering grammar.

Background predicates defined in this experiment file are non- and
pre-terminals of a grammar of a subset of the M:tG language. The
start symbol of the grammar is ability//0 and it is used to collect
examples for the learning problem.

__1. Conriguration__

Make sure your configuration matches the one listed below. Important
options are marked with an asterisk (*):

==
?- list_config.
* clause_limit(0)
example_clauses(call)
* experiment_file(data/examples/mtg_fragment.pl,mtg_fragment)
fold_recursive(false)
generalise_learned_metarules(false)
learner(louise)
listing_limit(10)
* max_error(0,0)
* max_invented(0)
metarule_formatting(quantified)
metarule_learning_limits(none)
minimal_program_size(2,inf)
recursive_reduction(false)
reduce_learned_metarules(false)
* reduction(plotkins)
* resolutions(5000)
theorem_prover(resolution)
unfold_invented(false)
true.
==


__2. List the MIL problem__

There are more than three thousand examples of ability//0 and 64
background predicates. In such cases it's better to use
list_problem_statistics/1 instead of list_mil_problem/1, to avoid
cluttering your screen with thousands of lines:

==
?- list_problem_statistics(ability/2).
Positive examples:    1356
Negative examples:    0
Background knowledge: 64 [destroy_verb/2,exile_verb/2,return_verb/2,target_permanent/2,target/2,target_artifact_type/2,target_creature_type/2,target_enchantment_type/2,target_land_type/2,target_basic_land_type/2,target_planeswalker_type/2,all_permanents_of_type/2,artifact_type/2,creature_type/2,enchantment_type/2,land_type/2,basic_land_type/2,planeswalker_type/2,all_of_artifact_type/2,all_of_creature_type/2,all_of_enchantment_type/2,all_of_land_type/2,all_of_basic_land_type/2,all_of_planeswalker_type/2,all/2,artifact_types/2,creature_types/2,enchantment_types/2,land_types/2,basic_land_types/2,planeswalker_types/2,a_permanent_type/2,permanent_type/2,permanent_types/2,target_from_battlefield_to_hand/2,target_from_graveyard_to_hand/2,target_from_graveyard_to_battlefield/2,from_battlefield_to_hand/2,from_graveyard_to_hand/2,from_graveyard_to_battlefield/2,all_from_battlefield_to_hand/2,all_from_graveyard_to_hand/2,all_from_graveyard_to_battlefield/2,permanents/2,all_of_type_from_battlefield_to_hand/2,all_of_type_from_graveyard_to_hand/2,all_of_type_from_graveyard_to_battlefield/2,to_owners_hand/2,to/2,its/2,owners_gen/2,zone_hand/2,to_owners_hands/2,their/2,owners_gen_plural/2,hands/2,from_graveyard/2,from/2,your/2,a/2,zone_graveyard/2,to_battlefield/2,the/2,zone_battlefield/2]
Metarules:            1 [chain]
true.
==

__3. Learning query__

Make a learning attempt. The result should look like this:

==
?- time(learn(ability/2,_Ps)), length(_Ps, N), print_clauses(_Ps).
% 2,179,513 inferences, 2.109 CPU in 2.562 seconds (82% CPU, 1033251 Lips)
ability(A,B):-destroy_verb(A,C),all_of_artifact_type(C,B).
ability(A,B):-destroy_verb(A,C),all_of_basic_land_type(C,B).
ability(A,B):-destroy_verb(A,C),all_of_creature_type(C,B).
ability(A,B):-destroy_verb(A,C),all_of_enchantment_type(C,B).
ability(A,B):-destroy_verb(A,C),all_of_land_type(C,B).
ability(A,B):-destroy_verb(A,C),all_of_planeswalker_type(C,B).
ability(A,B):-destroy_verb(A,C),all_permanents_of_type(C,B).
ability(A,B):-destroy_verb(A,C),target_artifact_type(C,B).
ability(A,B):-destroy_verb(A,C),target_basic_land_type(C,B).
ability(A,B):-destroy_verb(A,C),target_creature_type(C,B).
ability(A,B):-destroy_verb(A,C),target_enchantment_type(C,B).
ability(A,B):-destroy_verb(A,C),target_land_type(C,B).
ability(A,B):-destroy_verb(A,C),target_permanent(C,B).
ability(A,B):-destroy_verb(A,C),target_planeswalker_type(C,B).
ability(A,B):-exile_verb(A,C),all_of_artifact_type(C,B).
ability(A,B):-exile_verb(A,C),all_of_basic_land_type(C,B).
ability(A,B):-exile_verb(A,C),all_of_creature_type(C,B).
ability(A,B):-exile_verb(A,C),all_of_enchantment_type(C,B).
ability(A,B):-exile_verb(A,C),all_of_land_type(C,B).
ability(A,B):-exile_verb(A,C),all_of_planeswalker_type(C,B).
ability(A,B):-exile_verb(A,C),all_permanents_of_type(C,B).
ability(A,B):-exile_verb(A,C),target_artifact_type(C,B).
ability(A,B):-exile_verb(A,C),target_basic_land_type(C,B).
ability(A,B):-exile_verb(A,C),target_creature_type(C,B).
ability(A,B):-exile_verb(A,C),target_enchantment_type(C,B).
ability(A,B):-exile_verb(A,C),target_land_type(C,B).
ability(A,B):-exile_verb(A,C),target_permanent(C,B).
ability(A,B):-exile_verb(A,C),target_planeswalker_type(C,B).
ability(A,B):-return_verb(A,C),all_from_battlefield_to_hand(C,B).
ability(A,B):-return_verb(A,C),all_from_graveyard_to_battlefield(C,B).
ability(A,B):-return_verb(A,C),all_from_graveyard_to_hand(C,B).
ability(A,B):-return_verb(A,C),all_of_type_from_battlefield_to_hand(C,B).
ability(A,B):-return_verb(A,C),all_of_type_from_graveyard_to_battlefield(C,B).
ability(A,B):-return_verb(A,C),all_of_type_from_graveyard_to_hand(C,B).
ability(A,B):-return_verb(A,C),from_battlefield_to_hand(C,B).
ability(A,B):-return_verb(A,C),from_graveyard_to_battlefield(C,B).
ability(A,B):-return_verb(A,C),from_graveyard_to_hand(C,B).
ability(A,B):-return_verb(A,C),target_from_battlefield_to_hand(C,B).
ability(A,B):-return_verb(A,C),target_from_graveyard_to_battlefield(C,B).
ability(A,B):-return_verb(A,C),target_from_graveyard_to_hand(C,B).
N = 40.
==

*/

background_knowledge(ability/2, [destroy_verb/2
				,exile_verb/2
				,return_verb/2

				,target_permanent/2

				,target/2

				,target_artifact_type/2
				,target_creature_type/2
				,target_enchantment_type/2
				,target_land_type/2
				,target_basic_land_type/2
				,target_planeswalker_type/2

				,all_permanents_of_type/2

				,artifact_type/2
				,creature_type/2
				,enchantment_type/2
				,land_type/2
				,basic_land_type/2
				,planeswalker_type/2

				,all_of_artifact_type/2
				,all_of_creature_type/2
				,all_of_enchantment_type/2
				,all_of_land_type/2
				,all_of_basic_land_type/2
				,all_of_planeswalker_type/2

				,all/2

				,artifact_types/2
				,creature_types/2
				,enchantment_types/2
				,land_types/2
				,basic_land_types/2
				,planeswalker_types/2

				,a_permanent_type/2
				,permanent_type/2
				,permanent_types/2

				,target_from_battlefield_to_hand/2
				,target_from_graveyard_to_hand/2
				,target_from_graveyard_to_battlefield/2

				,from_battlefield_to_hand/2
				,from_graveyard_to_hand/2
				,from_graveyard_to_battlefield/2

				,all_from_battlefield_to_hand/2
				,all_from_graveyard_to_hand/2
				,all_from_graveyard_to_battlefield/2

				,permanents/2

				,all_of_type_from_battlefield_to_hand/2
				,all_of_type_from_graveyard_to_hand/2
				,all_of_type_from_graveyard_to_battlefield/2

				,to_owners_hand/2
				,to/2
				,its/2
				,owners_gen/2
				,zone_hand/2
				,to_owners_hands/2
				,their/2
				,owners_gen_plural/2
				,hands/2

				,from_graveyard/2
				,from/2
				,your/2
				,a/2
				,zone_graveyard/2

				,to_battlefield/2
				,the/2
				,zone_battlefield/2
				]).

/* Small fragment of the Magic: the Gathering ability text language grammar.
Derived primarily from examples on published cards.

*/


metarules(ability/2,[chain]).

positive_example(ability/2,ability(E, [])):-
	phrase(ability, E).

negative_example(ability/2,_):-
	fail.


% Target theory.
ability --> destroy_verb, target_permanent.
ability --> exile_verb, target_permanent.

ability --> destroy_verb, all_permanents_of_type.
ability --> exile_verb, all_permanents_of_type.

ability --> destroy_verb, target_artifact_type.
ability --> destroy_verb, target_creature_type.
ability --> destroy_verb, target_enchantment_type.
ability --> destroy_verb, target_land_type.
ability --> destroy_verb, target_basic_land_type.
ability --> destroy_verb, target_planeswalker_type.

ability --> exile_verb, target_artifact_type.
ability --> exile_verb, target_creature_type.
ability --> exile_verb, target_enchantment_type.
ability --> exile_verb, target_land_type.
ability --> exile_verb, target_basic_land_type.
ability --> exile_verb, target_planeswalker_type.

ability --> destroy_verb, all_of_artifact_type.
ability --> destroy_verb, all_of_creature_type.
ability --> destroy_verb, all_of_enchantment_type.
ability --> destroy_verb, all_of_land_type.
ability --> destroy_verb, all_of_basic_land_type.
ability --> destroy_verb, all_of_planeswalker_type.

ability --> exile_verb, all_of_artifact_type.
ability --> exile_verb, all_of_creature_type.
ability --> exile_verb, all_of_enchantment_type.
ability --> exile_verb, all_of_land_type.
ability --> exile_verb, all_of_basic_land_type.
ability --> exile_verb, all_of_planeswalker_type.

ability --> return_verb, target_from_battlefield_to_hand.
ability --> return_verb, target_from_graveyard_to_hand.
ability --> return_verb, target_from_graveyard_to_battlefield.

ability --> return_verb, from_battlefield_to_hand.
ability --> return_verb, from_graveyard_to_hand.
ability --> return_verb, from_graveyard_to_battlefield.

ability --> return_verb, all_of_type_from_battlefield_to_hand.
ability --> return_verb, all_of_type_from_graveyard_to_hand.
ability --> return_verb, all_of_type_from_graveyard_to_battlefield.

ability --> return_verb, all_from_battlefield_to_hand.
ability --> return_verb, all_from_graveyard_to_hand.
ability --> return_verb, all_from_graveyard_to_battlefield.

destroy_verb --> [destroy].
exile_verb --> [exile].
return_verb --> [return].

target_permanent --> target, permanent_type.

target --> [target].

all_permanents_of_type --> all, permanent_types.

target_artifact_type --> target, artifact_type.
target_creature_type --> target, creature_type.
target_enchantment_type --> target, enchantment_type.
target_land_type --> target, land_type.
target_basic_land_type --> target, basic_land_type.
target_planeswalker_type --> target, planeswalker_type.

all_of_artifact_type --> all, artifact_types.
all_of_creature_type --> all, creature_types.
all_of_enchantment_type --> all, enchantment_types.
all_of_land_type --> all, land_types.
all_of_basic_land_type --> all, basic_land_types.
all_of_planeswalker_type --> all, planeswalker_types.

all --> [all].

a_permanent_type --> [an,artifact].
a_permanent_type --> [a,creature].
a_permanent_type --> [an,enchantment].
a_permanent_type --> [a,land].
a_permanent_type --> [a,planeswalker].

permanent_type --> [artifact].
permanent_type --> [creature].
permanent_type --> [enchantment].
permanent_type --> [land].
permanent_type --> [planeswalker].

permanent_types --> [artifacts].
permanent_types --> [creatures].
permanent_types --> [enchantments].
permanent_types --> [lands].
permanent_types --> [planeswalkers].

target_from_battlefield_to_hand --> target_permanent, to_owners_hand.
target_from_graveyard_to_hand --> target_permanent,from_graveyard,to_owners_hand.
target_from_graveyard_to_battlefield --> target_permanent, from_graveyard, to_battlefield.

from_battlefield_to_hand --> a_permanent_type, to_owners_hand.
from_graveyard_to_hand --> a_permanent_type,from_graveyard,to_owners_hand.
from_graveyard_to_battlefield --> a_permanent_type, from_graveyard, to_battlefield.

all_from_battlefield_to_hand --> all,permanents,to_owners_hands.
all_from_graveyard_to_hand --> all,permanents, from_graveyard,to_owners_hands.
all_from_graveyard_to_battlefield --> all,permanents, from_graveyard, to_battlefield.

permanents --> [permanents].

all_of_type_from_battlefield_to_hand --> all, permanent_types, to_owners_hands.
all_of_type_from_graveyard_to_hand --> all, permanent_types, from_graveyard, to_owners_hands.
all_of_type_from_graveyard_to_battlefield --> all, permanent_types, from_graveyard, to_battlefield.

to_owners_hand --> to,its,owners_gen,zone_hand.

to --> [to].

its --> ['its\''].

owners_gen --> ['owner\'s'].

zone_hand --> [hand].

to_owners_hands --> to,their,owners_gen_plural,hands.

their --> [their].

owners_gen_plural --> ['owners\''].

hands --> [hands].

from_graveyard --> from,your,zone_graveyard.
from_graveyard --> from,a,zone_graveyard.
from --> [from].
your --> [your].
a --> [a].
zone_graveyard --> [graveyard].

to_battlefield --> to,the,zone_battlefield.
the --> [the].
zone_battlefield --> [battlefield].


/* Following rules are extracted from the Comprehensive Rules document effective
as of August 2018, online here:

https://media.wizards.com/2018/downloads/MagicCompRules%2020180810.txt
*/

% See rule 205.3g
artifact_type --> ['Clue'].
artifact_type --> ['Contraption'].
artifact_type --> ['Equipment'].
artifact_type --> ['Fortification'].
artifact_type --> ['Treasure'].
artifact_type --> ['Vehicle'].

% Plural forms by me.
artifact_types --> ['Clues'].
artifact_types --> ['Contraptions'].
artifact_types --> ['Equipments'].
artifact_types --> ['Fortifications'].
artifact_types --> ['Treasures'].
artifact_types --> ['Vehicles'].

% Creature subtypes are shared by tribal spells.
% See rule 205.3m
creature_type --> ['Advisor'].
creature_type --> ['Aetherborn'].
creature_type --> ['Ally'].
creature_type --> ['Angel'].
creature_type --> ['Antelope'].
creature_type --> ['Ape'].
creature_type --> ['Archer'].
creature_type --> ['Archon'].
creature_type --> ['Artificer'].
creature_type --> ['Assassin'].
creature_type --> ['Assembly-Worker'].
creature_type --> ['Atog'].
creature_type --> ['Aurochs'].
creature_type --> ['Avatar'].
creature_type --> ['Azra'].
creature_type --> ['Badger'].
creature_type --> ['Barbarian'].
creature_type --> ['Basilisk'].
creature_type --> ['Bat'].
creature_type --> ['Bear'].
creature_type --> ['Beast'].
creature_type --> ['Beeble'].
creature_type --> ['Berserker'].
creature_type --> ['Bird'].
creature_type --> ['Blinkmoth'].
creature_type --> ['Boar'].
creature_type --> ['Bringer'].
creature_type --> ['Brushwagg'].
creature_type --> ['Camarid'].
creature_type --> ['Camel'].
creature_type --> ['Caribou'].
creature_type --> ['Carrier'].
creature_type --> ['Cat'].
creature_type --> ['Centaur'].
creature_type --> ['Cephalid'].
creature_type --> ['Chimera'].
creature_type --> ['Citizen'].
creature_type --> ['Cleric'].
creature_type --> ['Cockatrice'].
creature_type --> ['Construct'].
creature_type --> ['Coward'].
creature_type --> ['Crab'].
creature_type --> ['Crocodile'].
creature_type --> ['Cyclops'].
creature_type --> ['Dauthi'].
creature_type --> ['Demon'].
creature_type --> ['Deserter'].
creature_type --> ['Devil'].
creature_type --> ['Dinosaur'].
creature_type --> ['Djinn'].
creature_type --> ['Dragon'].
creature_type --> ['Drake'].
creature_type --> ['Dreadnought'].
creature_type --> ['Drone'].
creature_type --> ['Druid'].
creature_type --> ['Dryad'].
creature_type --> ['Dwarf'].
creature_type --> ['Efreet'].
creature_type --> ['Egg'].
creature_type --> ['Elder'].
creature_type --> ['Eldrazi'].
creature_type --> ['Elemental'].
creature_type --> ['Elephant'].
creature_type --> ['Elf'].
creature_type --> ['Elk'].
creature_type --> ['Eye'].
creature_type --> ['Faerie'].
creature_type --> ['Ferret'].
creature_type --> ['Fish'].
creature_type --> ['Flagbearer'].
creature_type --> ['Fox'].
creature_type --> ['Frog'].
creature_type --> ['Fungus'].
creature_type --> ['Gargoyle'].
creature_type --> ['Germ'].
creature_type --> ['Giant'].
creature_type --> ['Gnome'].
creature_type --> ['Goat'].
creature_type --> ['Goblin'].
creature_type --> ['God'].
creature_type --> ['Golem'].
creature_type --> ['Gorgon'].
creature_type --> ['Graveborn'].
creature_type --> ['Gremlin'].
creature_type --> ['Griffin'].
creature_type --> ['Hag'].
creature_type --> ['Harpy'].
creature_type --> ['Hellion'].
creature_type --> ['Hippo'].
creature_type --> ['Hippogriff'].
creature_type --> ['Homarid'].
creature_type --> ['Homunculus'].
creature_type --> ['Horror'].
creature_type --> ['Horse'].
creature_type --> ['Hound'].
creature_type --> ['Human'].
creature_type --> ['Hydra'].
creature_type --> ['Hyena'].
creature_type --> ['Illusion'].
creature_type --> ['Imp'].
creature_type --> ['Incarnation'].
creature_type --> ['Insect'].
creature_type --> ['Jackal'].
creature_type --> ['Jellyfish'].
creature_type --> ['Juggernaut'].
creature_type --> ['Kavu'].
creature_type --> ['Kirin'].
creature_type --> ['Kithkin'].
creature_type --> ['Knight'].
creature_type --> ['Kobold'].
creature_type --> ['Kor'].
creature_type --> ['Kraken'].
creature_type --> ['Lamia'].
creature_type --> ['Lammasu'].
creature_type --> ['Leech'].
creature_type --> ['Leviathan'].
creature_type --> ['Lhurgoyf'].
creature_type --> ['Licid'].
creature_type --> ['Lizard'].
creature_type --> ['Manticore'].
creature_type --> ['Masticore'].
creature_type --> ['Mercenary'].
creature_type --> ['Merfolk'].
creature_type --> ['Metathran'].
creature_type --> ['Minion'].
creature_type --> ['Minotaur'].
creature_type --> ['Mole'].
creature_type --> ['Monger'].
creature_type --> ['Mongoose'].
creature_type --> ['Monk'].
creature_type --> ['Monkey'].
creature_type --> ['Moonfolk'].
creature_type --> ['Mutant'].
creature_type --> ['Myr'].
creature_type --> ['Mystic'].
creature_type --> ['Naga'].
creature_type --> ['Nautilus'].
creature_type --> ['Nephilim'].
creature_type --> ['Nightmare'].
creature_type --> ['Nightstalker'].
creature_type --> ['Ninja'].
creature_type --> ['Noggle'].
creature_type --> ['Nomad'].
creature_type --> ['Nymph'].
creature_type --> ['Octopus'].
creature_type --> ['Ogre'].
creature_type --> ['Ooze'].
creature_type --> ['Orb'].
creature_type --> ['Orc'].
creature_type --> ['Orgg'].
creature_type --> ['Ouphe'].
creature_type --> ['Ox'].
creature_type --> ['Oyster'].
creature_type --> ['Pangolin'].
creature_type --> ['Pegasus'].
creature_type --> ['Pentavite'].
creature_type --> ['Pest'].
creature_type --> ['Phelddagrif'].
creature_type --> ['Phoenix'].
creature_type --> ['Pilot'].
creature_type --> ['Pincher'].
creature_type --> ['Pirate'].
creature_type --> ['Plant'].
creature_type --> ['Praetor'].
creature_type --> ['Prism'].
creature_type --> ['Processor'].
creature_type --> ['Rabbit'].
creature_type --> ['Rat'].
creature_type --> ['Rebel'].
creature_type --> ['Reflection'].
creature_type --> ['Rhino'].
creature_type --> ['Rigger'].
creature_type --> ['Rogue'].
creature_type --> ['Sable'].
creature_type --> ['Salamander'].
creature_type --> ['Samurai'].
creature_type --> ['Sand'].
creature_type --> ['Saproling'].
creature_type --> ['Satyr'].
creature_type --> ['Scarecrow'].
creature_type --> ['Scion'].
creature_type --> ['Scorpion'].
creature_type --> ['Scout'].
creature_type --> ['Serf'].
creature_type --> ['Serpent'].
creature_type --> ['Servo'].
creature_type --> ['Shade'].
creature_type --> ['Shaman'].
creature_type --> ['Shapeshifter'].
creature_type --> ['Sheep'].
creature_type --> ['Siren'].
creature_type --> ['Skeleton'].
creature_type --> ['Slith'].
creature_type --> ['Sliver'].
creature_type --> ['Slug'].
creature_type --> ['Snake'].
creature_type --> ['Soldier'].
creature_type --> ['Soltari'].
creature_type --> ['Spawn'].
creature_type --> ['Specter'].
creature_type --> ['Spellshaper'].
creature_type --> ['Sphinx'].
creature_type --> ['Spider'].
creature_type --> ['Spike'].
creature_type --> ['Spirit'].
creature_type --> ['Splinter'].
creature_type --> ['Sponge'].
creature_type --> ['Squid'].
creature_type --> ['Squirrel'].
creature_type --> ['Starfish'].
creature_type --> ['Surrakar'].
creature_type --> ['Survivor'].
creature_type --> ['Tetravite'].
creature_type --> ['Thalakos'].
creature_type --> ['Thopter'].
creature_type --> ['Thrull'].
creature_type --> ['Treefolk'].
creature_type --> ['Trilobite'].
creature_type --> ['Triskelavite'].
creature_type --> ['Troll'].
creature_type --> ['Turtle'].
creature_type --> ['Unicorn'].
creature_type --> ['Vampire'].
creature_type --> ['Vedalken'].
creature_type --> ['Viashino'].
creature_type --> ['Volver'].
creature_type --> ['Wall'].
creature_type --> ['Warrior'].
creature_type --> ['Weird'].
creature_type --> ['Werewolf'].
creature_type --> ['Whale'].
creature_type --> ['Wizard'].
creature_type --> ['Wolf'].
creature_type --> ['Wolverine'].
creature_type --> ['Wombat'].
creature_type --> ['Worm'].
creature_type --> ['Wraith'].
creature_type --> ['Wurm'].
creature_type --> ['Yeti'].
creature_type --> ['Zombie'].
creature_type --> ['Zubera'].

% Don't lose this. Can't always add "s" to end of words to make a plural
% form so after an initial search-and-replace operation this needed
% some hand-editing.
creature_types --> ['Advisors'].
creature_types --> ['Aetherborns'].
creature_types --> ['Alles'].
creature_types --> ['Angels'].
creature_types --> ['Antelopes'].
creature_types --> ['Apes'].
creature_types --> ['Archers'].
creature_types --> ['Archons'].
creature_types --> ['Artificers'].
creature_types --> ['Assassins'].
creature_types --> ['Assembly-Workers'].
creature_types --> ['Atogs'].
creature_types --> ['Aurochs'].
creature_types --> ['Avatars'].
creature_types --> ['Azras'].
creature_types --> ['Badgers'].
creature_types --> ['Barbarians'].
creature_types --> ['Basilisks'].
creature_types --> ['Bats'].
creature_types --> ['Bears'].
creature_types --> ['Beasts'].
creature_types --> ['Beebles'].
creature_types --> ['Berserkers'].
creature_types --> ['Birds'].
creature_types --> ['Blinkmoths'].
creature_types --> ['Boars'].
creature_types --> ['Bringers'].
creature_types --> ['Brushwaggs'].
creature_types --> ['Camarids'].
creature_types --> ['Camels'].
creature_types --> ['Caribou'].
creature_types --> ['Carriers'].
creature_types --> ['Cats'].
creature_types --> ['Centaurs'].
creature_types --> ['Cephalids'].
creature_types --> ['Chimeras'].
creature_types --> ['Citizens'].
creature_types --> ['Clerics'].
creature_types --> ['Cockatrices'].
creature_types --> ['Constructs'].
creature_types --> ['Cowards'].
creature_types --> ['Crabs'].
creature_types --> ['Crocodiles'].
creature_types --> ['Cyclopss'].
creature_types --> ['Dauthis'].
creature_types --> ['Demons'].
creature_types --> ['Deserters'].
creature_types --> ['Devils'].
creature_types --> ['Dinosaurs'].
creature_types --> ['Djinns'].
creature_types --> ['Dragons'].
creature_types --> ['Drakes'].
creature_types --> ['Dreadnoughts'].
creature_types --> ['Drones'].
creature_types --> ['Druids'].
creature_types --> ['Dryads'].
creature_types --> ['Dwarfs'].
creature_types --> ['Efreets'].
creature_types --> ['Eggs'].
creature_types --> ['Elders'].
creature_types --> ['Eldrazis'].
creature_types --> ['Elementals'].
creature_types --> ['Elephants'].
creature_types --> ['Elves'].
creature_types --> ['Elks'].
creature_types --> ['Eyes'].
creature_types --> ['Faeries'].
creature_types --> ['Ferrets'].
creature_types --> ['Fish'].
creature_types --> ['Flagbearers'].
creature_types --> ['Foxes'].
creature_types --> ['Frogs'].
creature_types --> ['Fungi'].
creature_types --> ['Gargoyles'].
creature_types --> ['Germs'].
creature_types --> ['Giants'].
creature_types --> ['Gnomes'].
creature_types --> ['Goats'].
creature_types --> ['Goblins'].
creature_types --> ['Gods'].
creature_types --> ['Golems'].
creature_types --> ['Gorgons'].
creature_types --> ['Graveborns'].
creature_types --> ['Gremlins'].
creature_types --> ['Griffins'].
creature_types --> ['Hags'].
creature_types --> ['Harpies'].
creature_types --> ['Hellions'].
creature_types --> ['Hippos'].
creature_types --> ['Hippogriffs'].
creature_types --> ['Homarids'].
creature_types --> ['Homunculuss'].
creature_types --> ['Horrors'].
creature_types --> ['Horses'].
creature_types --> ['Hounds'].
creature_types --> ['Humans'].
creature_types --> ['Hydras'].
creature_types --> ['Hyenas'].
creature_types --> ['Illusions'].
creature_types --> ['Imps'].
creature_types --> ['Incarnations'].
creature_types --> ['Insects'].
creature_types --> ['Jackals'].
creature_types --> ['Jellyfishs'].
creature_types --> ['Juggernauts'].
creature_types --> ['Kavu'].
creature_types --> ['Kirins'].
creature_types --> ['Kithkins'].
creature_types --> ['Knights'].
creature_types --> ['Kobolds'].
creature_types --> ['Kors'].
creature_types --> ['Krakens'].
creature_types --> ['Lamias'].
creature_types --> ['Lammasu'].
creature_types --> ['Leeches'].
creature_types --> ['Leviathans'].
creature_types --> ['Lhurgoyfs'].
creature_types --> ['Licids'].
creature_types --> ['Lizards'].
creature_types --> ['Manticores'].
creature_types --> ['Masticores'].
creature_types --> ['Mercenaries'].
creature_types --> ['Merfolks'].
creature_types --> ['Metathran'].
creature_types --> ['Minions'].
creature_types --> ['Minotaurs'].
creature_types --> ['Moles'].
creature_types --> ['Mongers'].
creature_types --> ['Mongeese'].
creature_types --> ['Monks'].
creature_types --> ['Monkeys'].
creature_types --> ['Moonfolks'].
creature_types --> ['Mutants'].
creature_types --> ['Myr'].
creature_types --> ['Mystics'].
creature_types --> ['Naga'].
creature_types --> ['Nautili'].
creature_types --> ['Nephilim'].
creature_types --> ['Nightmares'].
creature_types --> ['Nightstalkers'].
creature_types --> ['Ninjas'].
creature_types --> ['Noggles'].
creature_types --> ['Nomads'].
creature_types --> ['Nymphs'].
creature_types --> ['Octopi'].
creature_types --> ['Ogres'].
creature_types --> ['Oozes'].
creature_types --> ['Orbs'].
creature_types --> ['Orcs'].
creature_types --> ['Orggs'].
creature_types --> ['Ouphes'].
creature_types --> ['Oxen'].
creature_types --> ['Oysters'].
creature_types --> ['Pangolins'].
creature_types --> ['Pegasi'].
creature_types --> ['Pentavites'].
creature_types --> ['Pests'].
creature_types --> ['Phelddagrifs'].
creature_types --> ['Phoenixes'].
creature_types --> ['Pilots'].
creature_types --> ['Pinchers'].
creature_types --> ['Pirates'].
creature_types --> ['Plants'].
creature_types --> ['Praetors'].
creature_types --> ['Prisms'].
creature_types --> ['Processors'].
creature_types --> ['Rabbits'].
creature_types --> ['Rats'].
creature_types --> ['Rebels'].
creature_types --> ['Reflections'].
creature_types --> ['Rhinos'].
creature_types --> ['Riggers'].
creature_types --> ['Rogues'].
creature_types --> ['Sables'].
creature_types --> ['Salamanders'].
creature_types --> ['Samurais'].
creature_types --> ['Sands'].
creature_types --> ['Saprolings'].
creature_types --> ['Satyrs'].
creature_types --> ['Scarecrows'].
creature_types --> ['Scions'].
creature_types --> ['Scorpions'].
creature_types --> ['Scouts'].
creature_types --> ['Serfs'].
creature_types --> ['Serpents'].
creature_types --> ['Servos'].
creature_types --> ['Shades'].
creature_types --> ['Shamans'].
creature_types --> ['Shapeshifters'].
creature_types --> ['Sheeps'].
creature_types --> ['Sirens'].
creature_types --> ['Skeletons'].
creature_types --> ['Sliths'].
creature_types --> ['Slivers'].
creature_types --> ['Slugs'].
creature_types --> ['Snakes'].
creature_types --> ['Soldiers'].
creature_types --> ['Soltaris'].
creature_types --> ['Spawns'].
creature_types --> ['Specters'].
creature_types --> ['Spellshapers'].
creature_types --> ['Sphinxes'].
creature_types --> ['Spiders'].
creature_types --> ['Spikes'].
creature_types --> ['Spirits'].
creature_types --> ['Splinters'].
creature_types --> ['Sponges'].
creature_types --> ['Squids'].
creature_types --> ['Squirrels'].
creature_types --> ['Starfish'].
creature_types --> ['Surrakars'].
creature_types --> ['Survivors'].
creature_types --> ['Tetravites'].
creature_types --> ['Thalakoi'].
creature_types --> ['Thopters'].
creature_types --> ['Thrulls'].
creature_types --> ['Treefolks'].
creature_types --> ['Trilobites'].
creature_types --> ['Triskelavites'].
creature_types --> ['Trolls'].
creature_types --> ['Turtles'].
creature_types --> ['Unicorns'].
creature_types --> ['Vampires'].
creature_types --> ['Vedalkens'].
creature_types --> ['Viashinos'].
creature_types --> ['Volvers'].
creature_types --> ['Walls'].
creature_types --> ['Warriors'].
creature_types --> ['Weirds'].
creature_types --> ['Werewolves'].
creature_types --> ['Whales'].
creature_types --> ['Wizards'].
creature_types --> ['Wolves'].
creature_types --> ['Wolverines'].
creature_types --> ['Wombats'].
creature_types --> ['Worms'].
creature_types --> ['Wraiths'].
creature_types --> ['Wurms'].
creature_types --> ['Yetis'].
creature_types --> ['Zombies'].
creature_types --> ['Zuberas'].

% See rule 205.3h
enchantment_type --> ['Aura'].
enchantment_type --> ['Cartouche'].
enchantment_type --> ['Curse'].
enchantment_type --> ['Saga'].
enchantment_type --> ['Shrine'].

% My plurals.
enchantment_types --> ['Auras'].
enchantment_types --> ['Cartouches'].
enchantment_types --> ['Curses'].
enchantment_types --> ['Sagas'].
enchantment_types --> ['Shrines'].

% See rule 205.3i
land_type --> ['Desert'].
land_type --> ['Forest'].
land_type --> ['Gate'].
land_type --> ['Island'].
land_type --> ['Lair'].
land_type --> ['Locus'].
land_type --> ['Mine'].
land_type --> ['Mountain'].
land_type --> ['Plains'].
land_type --> ['Power-Plant'].
land_type --> ['Swamp'].
land_type --> ['Tower'].
land_type --> ['Urza�s'].

% My plurals
land_types --> ['Deserts'].
land_types --> ['Forests'].
land_types --> ['Gates'].
land_types --> ['Islands'].
land_types --> ['Lairs'].
land_types --> ['Loci'].
land_types --> ['Mines'].
land_types --> ['Mountains'].
land_types --> ['Plains'].
land_types --> ['Power-Plants'].
land_types --> ['Swamps'].
land_types --> ['Towers'].
land_types --> ['Urza�s'].

% See rule 205.3i
basic_land_type --> ['Forest'].
basic_land_type --> ['Island'].
basic_land_type --> ['Mountain'].
basic_land_type --> ['Plains'].
basic_land_type --> ['Swamp'].

% My plurals
basic_land_types --> ['Forest'].
basic_land_types --> ['Island'].
basic_land_types --> ['Mountain'].
basic_land_types --> ['Plains'].
basic_land_types --> ['Swamp'].

% See rule 205.3j
planeswalker_type --> ['Ajani'].
planeswalker_type --> ['Aminatou'].
planeswalker_type --> ['Angrath'].
planeswalker_type --> ['Arlinn'].
planeswalker_type --> ['Ashiok'].
planeswalker_type --> ['Bolas'].
planeswalker_type --> ['Chandra'].
planeswalker_type --> ['Dack'].
planeswalker_type --> ['Daretti'].
planeswalker_type --> ['Domri'].
planeswalker_type --> ['Dovin'].
planeswalker_type --> ['Elspeth'].
planeswalker_type --> ['Estrid'].
planeswalker_type --> ['Freyalise'].
planeswalker_type --> ['Garruk'].
planeswalker_type --> ['Gideon'].
planeswalker_type --> ['Huatli'].
planeswalker_type --> ['Jace'].
planeswalker_type --> ['Jaya'].
planeswalker_type --> ['Karn'].
planeswalker_type --> ['Kaya'].
planeswalker_type --> ['Kiora'].
planeswalker_type --> ['Koth'].
planeswalker_type --> ['Liliana'].
planeswalker_type --> ['Nahiri'].
planeswalker_type --> ['Narset'].
planeswalker_type --> ['Nissa'].
planeswalker_type --> ['Nixilis'].
planeswalker_type --> ['Ral'].
planeswalker_type --> ['Rowan'].
planeswalker_type --> ['Saheeli'].
planeswalker_type --> ['Samut'].
planeswalker_type --> ['Sarkhan'].
planeswalker_type --> ['Sorin'].
planeswalker_type --> ['Tamiyo'].
planeswalker_type --> ['Teferi'].
planeswalker_type --> ['Tezzeret'].
planeswalker_type --> ['Tibalt'].
planeswalker_type --> ['Ugin'].
planeswalker_type --> ['Venser'].
planeswalker_type --> ['Vivien'].
planeswalker_type --> ['Vraska'].
planeswalker_type --> ['Will'].
planeswalker_type --> ['Windgrace'].
planeswalker_type --> ['Xenagos'].
planeswalker_type --> ['Yanggu'].
planeswalker_type --> ['Yanling'].

% No idea whether these can be made plural
planeswalker_types --> ['Ajani'].
planeswalker_types --> ['Aminatou'].
planeswalker_types --> ['Angrath'].
planeswalker_types --> ['Arlinn'].
planeswalker_types --> ['Ashiok'].
planeswalker_types --> ['Bolas'].
planeswalker_types --> ['Chandra'].
planeswalker_types --> ['Dack'].
planeswalker_types --> ['Daretti'].
planeswalker_types --> ['Domri'].
planeswalker_types --> ['Dovin'].
planeswalker_types --> ['Elspeth'].
planeswalker_types --> ['Estrid'].
planeswalker_types --> ['Freyalise'].
planeswalker_types --> ['Garruk'].
planeswalker_types --> ['Gideon'].
planeswalker_types --> ['Huatli'].
planeswalker_types --> ['Jace'].
planeswalker_types --> ['Jaya'].
planeswalker_types --> ['Karn'].
planeswalker_types --> ['Kaya'].
planeswalker_types --> ['Kiora'].
planeswalker_types --> ['Koth'].
planeswalker_types --> ['Liliana'].
planeswalker_types --> ['Nahiri'].
planeswalker_types --> ['Narset'].
planeswalker_types --> ['Nissa'].
planeswalker_types --> ['Nixilis'].
planeswalker_types --> ['Ral'].
planeswalker_types --> ['Rowan'].
planeswalker_types --> ['Saheeli'].
planeswalker_types --> ['Samut'].
planeswalker_types --> ['Sarkhan'].
planeswalker_types --> ['Sorin'].
planeswalker_types --> ['Tamiyo'].
planeswalker_types --> ['Teferi'].
planeswalker_types --> ['Tezzeret'].
planeswalker_types --> ['Tibalt'].
planeswalker_types --> ['Ugin'].
planeswalker_types --> ['Venser'].
planeswalker_types --> ['Vivien'].
planeswalker_types --> ['Vraska'].
planeswalker_types --> ['Will'].
planeswalker_types --> ['Windgrace'].
planeswalker_types --> ['Xenagos'].
planeswalker_types --> ['Yanggu'].
planeswalker_types --> ['Yanling'].
