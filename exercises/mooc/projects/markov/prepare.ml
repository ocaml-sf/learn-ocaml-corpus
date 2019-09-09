let graded_selection : int list option ref =
  ref None

let grade_only l =
  graded_selection := Some l

let display_quote words =
  let buf = Buffer.create 100 in
  let ppf = Format.formatter_of_buffer buf in
  let rec print ppf = function
    | [] -> ()
    | ("\"" | "(" as o) :: rest ->
        Format.fprintf ppf "%s%a" o print rest
    | w :: ("!" | "?" | "." | ";" | ":" | "\"" | ")" | "," as t) :: rest ->
        Format.fprintf ppf "%s%s %a" w t print rest
    | w :: rest ->
        Format.fprintf ppf "%s %a" w print rest in
  Format.fprintf ppf
    "<div style='\
     white-space:normal;\
     text-align:justify;\
     font-style:italic;\
     margin:30px;\
     padding:25px;\
     font-size:16px;\
     line-height:18px;\
     font-family:serif;\
     '>\
     <span style='\
     display:inline;\
     font-size:64px;\
     margin:0px 0px 0px -16px;\
     vertical-align: -2500%%;\
     line-height:1px;'>“</span>\
     %a\
     <span style='\
     display:inline;\
     font-size:64px;\
     vertical-align: -4000%%;\
     line-height: 1px;\
     '>”</span>\
     </div>@."
    print words ;
  print_html (Buffer.contents buf)

let grimms_travelling_musicians = {|
An honest farmer had once an ass that had been a faithful servant to him
a great many years, but was now growing old and every day more and more
unfit for work. His master therefore was tired of keeping him and
began to think of putting an end to him; but the ass, who saw that some
mischief was in the wind, took himself slyly off, and began his journey
towards the great city, 'For there,' thought he, 'I may turn musician.'

After he had travelled a little way, he spied a dog lying by the
roadside and panting as if he were tired. 'What makes you pant so, my
friend?' said the ass. 'Alas!' said the dog, 'my master was going to
knock me on the head, because I am old and weak, and can no longer make
myself useful to him in hunting; so I ran away; but what can I do to
earn my livelihood?' 'Hark ye!' said the ass, 'I am going to the great
city to turn musician: suppose you go with me, and try what you can
do in the same way?' The dog said he was willing, and they jogged on
together.

They had not gone far before they saw a cat sitting in the middle of the
road and making a most rueful face. 'Pray, my good lady,' said the ass,
'what's the matter with you? You look quite out of spirits!' 'Ah, me!'
said the cat, 'how can one be in good spirits when one's life is in
danger? Because I am beginning to grow old, and had rather lie at my
ease by the fire than run about the house after the mice, my mistress
laid hold of me, and was going to drown me; and though I have been lucky
enough to get away from her, I do not know what I am to live upon.'
'Oh,' said the ass, 'by all means go with us to the great city; you are
a good night singer, and may make your fortune as a musician.' The cat
was pleased with the thought, and joined the party.

Soon afterwards, as they were passing by a farmyard, they saw a cock
perched upon a gate, and screaming out with all his might and main.
'Bravo!' said the ass; 'upon my word, you make a famous noise; pray what
is all this about?' 'Why,' said the cock, 'I was just now saying that
we should have fine weather for our washing-day, and yet my mistress and
the cook don't thank me for my pains, but threaten to cut off my
head tomorrow, and make broth of me for the guests that are coming
on Sunday!' 'Heaven forbid!' said the ass, 'come with us Master
Chanticleer; it will be better, at any rate, than staying here to have
your head cut off! Besides, who knows? If we care to sing in tune, we
may get up some kind of a concert; so come along with us.' 'With all my
heart,' said the cock: so they all four went on jollily together.

They could not, however, reach the great city the first day; so when
night came on, they went into a wood to sleep. The ass and the dog laid
themselves down under a great tree, and the cat climbed up into the
branches; while the cock, thinking that the higher he sat the safer he
should be, flew up to the very top of the tree, and then, according to
his custom, before he went to sleep, looked out on all sides of him to
see that everything was well. In doing this, he saw afar off something
bright and shining and calling to his companions said, 'There must be a
house no great way off, for I see a light.' 'If that be the case,' said
the ass, 'we had better change our quarters, for our lodging is not the
best in the world!' 'Besides,' added the dog, 'I should not be the
worse for a bone or two, or a bit of meat.' So they walked off together
towards the spot where Chanticleer had seen the light, and as they drew
near it became larger and brighter, till they at last came close to a
house in which a gang of robbers lived.

The ass, being the tallest of the company, marched up to the window and
peeped in. 'Well, Donkey,' said Chanticleer, 'what do you see?' 'What
do I see?' replied the ass. 'Why, I see a table spread with all kinds of
good things, and robbers sitting round it making merry.' 'That would
be a noble lodging for us,' said the cock. 'Yes,' said the ass, 'if we
could only get in'; so they consulted together how they should contrive
to get the robbers out; and at last they hit upon a plan. The ass placed
himself upright on his hind legs, with his forefeet resting against the
window; the dog got upon his back; the cat scrambled up to the dog's
shoulders, and the cock flew up and sat upon the cat's head. When
all was ready a signal was given, and they began their music. The ass
brayed, the dog barked, the cat mewed, and the cock screamed; and then
they all broke through the window at once, and came tumbling into
the room, amongst the broken glass, with a most hideous clatter! The
robbers, who had been not a little frightened by the opening concert,
had now no doubt that some frightful hobgoblin had broken in upon them,
and scampered away as fast as they could.

The coast once clear, our travellers soon sat down and dispatched what
the robbers had left, with as much eagerness as if they had not expected
to eat again for a month. As soon as they had satisfied themselves, they
put out the lights, and each once more sought out a resting-place to
his own liking. The donkey laid himself down upon a heap of straw in
the yard, the dog stretched himself upon a mat behind the door, the
cat rolled herself up on the hearth before the warm ashes, and the
cock perched upon a beam on the top of the house; and, as they were all
rather tired with their journey, they soon fell asleep.

But about midnight, when the robbers saw from afar that the lights were
out and that all seemed quiet, they began to think that they had been in
too great a hurry to run away; and one of them, who was bolder than
the rest, went to see what was going on. Finding everything still, he
marched into the kitchen, and groped about till he found a match in
order to light a candle; and then, espying the glittering fiery eyes of
the cat, he mistook them for live coals, and held the match to them to
light it. But the cat, not understanding this joke, sprang at his face,
and spat, and scratched at him. This frightened him dreadfully, and away
he ran to the back door; but there the dog jumped up and bit him in the
leg; and as he was crossing over the yard the ass kicked him; and the
cock, who had been awakened by the noise, crowed with all his might. At
this the robber ran back as fast as he could to his comrades, and told
the captain how a horrid witch had got into the house, and had spat at
him and scratched his face with her long bony fingers; how a man with a
knife in his hand had hidden himself behind the door, and stabbed him
in the leg; how a black monster stood in the yard and struck him with a
club, and how the devil had sat upon the top of the house and cried out,
'Throw the rascal up here!' After this the robbers never dared to go
back to the house; but the musicians were so pleased with their quarters
that they took up their abode there; and there they are, I dare say, at
this very day.
|}

let grimms_cat_and_mouse_in_partnership = {|
A certain cat had made the acquaintance of a mouse, and had said so much
to her about the great love and friendship she felt for her, that at
length the mouse agreed that they should live and keep house together.
'But we must make a provision for winter, or else we shall suffer
from hunger,' said the cat; 'and you, little mouse, cannot venture
everywhere, or you will be caught in a trap some day.' The good advice
was followed, and a pot of fat was bought, but they did not know where
to put it. At length, after much consideration, the cat said: 'I know no
place where it will be better stored up than in the church, for no one
dares take anything away from there. We will set it beneath the altar,
and not touch it until we are really in need of it.' So the pot was
placed in safety, but it was not long before the cat had a great
yearning for it, and said to the mouse: 'I want to tell you something,
little mouse; my cousin has brought a little son into the world, and has
asked me to be godmother; he is white with brown spots, and I am to hold
him over the font at the christening. Let me go out today, and you look
after the house by yourself.' 'Yes, yes,' answered the mouse, 'by all
means go, and if you get anything very good to eat, think of me. I
should like a drop of sweet red christening wine myself.' All this,
however, was untrue; the cat had no cousin, and had not been asked to
be godmother. She went straight to the church, stole to the pot of fat,
began to lick at it, and licked the top of the fat off. Then she took a
walk upon the roofs of the town, looked out for opportunities, and then
stretched herself in the sun, and licked her lips whenever she thought
of the pot of fat, and not until it was evening did she return home.
'Well, here you are again,' said the mouse, 'no doubt you have had a
merry day.' 'All went off well,' answered the cat. 'What name did they
give the child?' 'Top off!' said the cat quite coolly. 'Top off!' cried
the mouse, 'that is a very odd and uncommon name, is it a usual one in
your family?' 'What does that matter,' said the cat, 'it is no worse
than Crumb-stealer, as your godchildren are called.'

Before long the cat was seized by another fit of yearning. She said to
the mouse: 'You must do me a favour, and once more manage the house for
a day alone. I am again asked to be godmother, and, as the child has a
white ring round its neck, I cannot refuse.' The good mouse consented,
but the cat crept behind the town walls to the church, and devoured
half the pot of fat. 'Nothing ever seems so good as what one keeps to
oneself,' said she, and was quite satisfied with her day's work. When
she went home the mouse inquired: 'And what was the child christened?'
'Half-done,' answered the cat. 'Half-done! What are you saying? I
never heard the name in my life, I'll wager anything it is not in the
calendar!'

The cat's mouth soon began to water for some more licking. 'All good
things go in threes,' said she, 'I am asked to stand godmother again.
The child is quite black, only it has white paws, but with that
exception, it has not a single white hair on its whole body; this only
happens once every few years, you will let me go, won't you?' 'Top-off!
Half-done!' answered the mouse, 'they are such odd names, they make me
very thoughtful.' 'You sit at home,' said the cat, 'in your dark-grey
fur coat and long tail, and are filled with fancies, that's because
you do not go out in the daytime.' During the cat's absence the mouse
cleaned the house, and put it in order, but the greedy cat entirely
emptied the pot of fat. 'When everything is eaten up one has some
peace,' said she to herself, and well filled and fat she did not return
home till night. The mouse at once asked what name had been given to
the third child. 'It will not please you more than the others,' said the
cat. 'He is called All-gone.' 'All-gone,' cried the mouse 'that is the
most suspicious name of all! I have never seen it in print. All-gone;
what can that mean?' and she shook her head, curled herself up, and lay
down to sleep.

From this time forth no one invited the cat to be godmother, but
when the winter had come and there was no longer anything to be found
outside, the mouse thought of their provision, and said: 'Come, cat,
we will go to our pot of fat which we have stored up for ourselves--we
shall enjoy that.' 'Yes,' answered the cat, 'you will enjoy it as much
as you would enjoy sticking that dainty tongue of yours out of the
window.' They set out on their way, but when they arrived, the pot of
fat certainly was still in its place, but it was empty. 'Alas!' said the
mouse, 'now I see what has happened, now it comes to light! You are a true
friend! You have devoured all when you were standing godmother. First
top off, then half-done, then--' 'Will you hold your tongue,' cried the
cat, 'one word more, and I will eat you too.' 'All-gone' was already on
the poor mouse's lips; scarcely had she spoken it before the cat sprang
on her, seized her, and swallowed her down. Verily, that is the way of
the world.
|}

let the_war_of_the_worlds_chapter_one = {|
No one would have believed in the last years of the nineteenth
century that this world was being watched keenly and closely by
intelligences greater than man's and yet as mortal as his own; that as
men busied themselves about their various concerns they were
scrutinised and studied, perhaps almost as narrowly as a man with a
microscope might scrutinise the transient creatures that swarm and
multiply in a drop of water.  With infinite complacency men went to
and fro over this globe about their little affairs, serene in their
assurance of their empire over matter.  It is possible that the
infusoria under the microscope do the same.  No one gave a thought to
the older worlds of space as sources of human danger, or thought of
them only to dismiss the idea of life upon them as impossible or
improbable.  It is curious to recall some of the mental habits of
those departed days.  At most terrestrial men fancied there might be
other men upon Mars, perhaps inferior to themselves and ready to
welcome a missionary enterprise.  Yet across the gulf of space, minds
that are to our minds as ours are to those of the beasts that perish,
intellects vast and cool and unsympathetic, regarded this earth with
envious eyes, and slowly and surely drew their plans against us.  And
early in the twentieth century came the great disillusionment.

The planet Mars, I scarcely need remind the reader, revolves about the
sun at a mean distance of 140,000,000 miles, and the light and heat it
receives from the sun is barely half of that received by this world.
It must be, if the nebular hypothesis has any truth, older than our
world; and long before this earth ceased to be molten, life upon its
surface must have begun its course.  The fact that it is scarcely one
seventh of the volume of the earth must have accelerated its cooling
to the temperature at which life could begin.  It has air and water
and all that is necessary for the support of animated existence.

Yet so vain is man, and so blinded by his vanity, that no writer,
up to the very end of the nineteenth century, expressed any idea that
intelligent life might have developed there far, or indeed at all,
beyond its earthly level.  Nor was it generally understood that since
Mars is older than our earth, with scarcely a quarter of the
superficial area and remoter from the sun, it necessarily follows that
it is not only more distant from time's beginning but nearer its end.

The secular cooling that must someday overtake our planet has
already gone far indeed with our neighbour.  Its physical condition is
still largely a mystery, but we know now that even in its equatorial
region the midday temperature barely approaches that of our coldest
winter.  Its air is much more attenuated than ours, its oceans have
shrunk until they cover but a third of its surface, and as its slow
seasons change huge snowcaps gather and melt about either pole and
periodically inundate its temperate zones.  That last stage of
exhaustion, which to us is still incredibly remote, has become a
present-day problem for the inhabitants of Mars.  The immediate
pressure of necessity has brightened their intellects, enlarged their
powers, and hardened their hearts.  And looking across space with
instruments, and intelligences such as we have scarcely dreamed of,
they see, at its nearest distance only 35,000,000 of miles sunward of
them, a morning star of hope, our own warmer planet, green with
vegetation and grey with water, with a cloudy atmosphere eloquent of
fertility, with glimpses through its drifting cloud wisps of broad
stretches of populous country and narrow, navy-crowded seas.

And we men, the creatures who inhabit this earth, must be to them
at least as alien and lowly as are the monkeys and lemurs to us.  The
intellectual side of man already admits that life is an incessant
struggle for existence, and it would seem that this too is the belief
of the minds upon Mars.  Their world is far gone in its cooling and
this world is still crowded with life, but crowded only with what they
regard as inferior animals.  To carry warfare sunward is, indeed,
their only escape from the destruction that, generation after
generation, creeps upon them.

And before we judge of them too harshly we must remember what
ruthless and utter destruction our own species has wrought, not only
upon animals, such as the vanished bison and the dodo, but upon its
inferior races.  The Tasmanians, in spite of their human likeness,
were entirely swept out of existence in a war of extermination waged
by European immigrants, in the space of fifty years.  Are we such
apostles of mercy as to complain if the Martians warred in the same
spirit?

The Martians seem to have calculated their descent with amazing
subtlety--their mathematical learning is evidently far in excess of
ours--and to have carried out their preparations with a well-nigh
perfect unanimity.  Had our instruments permitted it, we might have
seen the gathering trouble far back in the nineteenth century.  Men
like Schiaparelli watched the red planet--it is odd, by-the-bye, that
for countless centuries Mars has been the star of war--but failed to
interpret the fluctuating appearances of the markings they mapped so
well.  All that time the Martians must have been getting ready.

During the opposition of 1894 a great light was seen on the
illuminated part of the disk, first at the Lick Observatory, then by
Perrotin of Nice, and then by other observers.  English readers heard
of it first in the issue of _Nature_ dated August 2.  I am inclined to
think that this blaze may have been the casting of the huge gun, in
the vast pit sunk into their planet, from which their shots were fired
at us.  Peculiar markings, as yet unexplained, were seen near the site
of that outbreak during the next two oppositions.

The storm burst upon us six years ago now.  As Mars approached
opposition, Lavelle of Java set the wires of the astronomical exchange
palpitating with the amazing intelligence of a huge outbreak of
incandescent gas upon the planet.  It had occurred towards midnight of
the twelfth; and the spectroscope, to which he had at once resorted,
indicated a mass of flaming gas, chiefly hydrogen, moving with an
enormous velocity towards this earth.  This jet of fire had become
invisible about a quarter past twelve.  He compared it to a colossal
puff of flame suddenly and violently squirted out of the planet, "as
flaming gases rushed out of a gun."

A singularly appropriate phrase it proved.  Yet the next day there
was nothing of this in the papers except a little note in the _Daily
Telegraph_, and the world went in ignorance of one of the gravest
dangers that ever threatened the human race. I might not have heard of
the eruption at all had I not met Ogilvy, the well-known astronomer,
at Ottershaw.  He was immensely excited at the news, and in the excess
of his feelings invited me up to take a turn with him that night in a
scrutiny of the red planet.

In spite of all that has happened since, I still remember that
vigil very distinctly: the black and silent observatory, the shadowed
lantern throwing a feeble glow upon the floor in the corner, the
steady ticking of the clockwork of the telescope, the little slit in
the roof--an oblong profundity with the stardust streaked across it.
Ogilvy moved about, invisible but audible.  Looking through the
telescope, one saw a circle of deep blue and the little round planet
swimming in the field.  It seemed such a little thing, so bright and
small and still, faintly marked with transverse stripes, and slightly
flattened from the perfect round.  But so little it was, so silvery
warm--a pin's-head of light! It was as if it quivered, but really this
was the telescope vibrating with the activity of the clockwork that
kept the planet in view.

As I watched, the planet seemed to grow larger and smaller and to
advance and recede, but that was simply that my eye was tired.  Forty
millions of miles it was from us--more than forty millions of miles of
void.  Few people realise the immensity of vacancy in which the dust
of the material universe swims.

Near it in the field, I remember, were three faint points of light,
three telescopic stars infinitely remote, and all around it was the
unfathomable darkness of empty space.  You know how that blackness
looks on a frosty starlight night.  In a telescope it seems far
profounder.  And invisible to me because it was so remote and small,
flying swiftly and steadily towards me across that incredible
distance, drawing nearer every minute by so many thousands of miles,
came the Thing they were sending us, the Thing that was to bring so
much struggle and calamity and death to the earth.  I never dreamed of
it then as I watched; no one on earth dreamed of that unerring
missile.

That night, too, there was another jetting out of gas from the
distant planet.  I saw it.  A reddish flash at the edge, the slightest
projection of the outline just as the chronometer struck midnight; and
at that I told Ogilvy and he took my place.  The night was warm and I
was thirsty, and I went stretching my legs clumsily and feeling my way
in the darkness, to the little table where the siphon stood, while
Ogilvy exclaimed at the streamer of gas that came out towards us.

That night another invisible missile started on its way to the
earth from Mars, just a second or so under twenty-four hours after the
first one.  I remember how I sat on the table there in the blackness,
with patches of green and crimson swimming before my eyes.  I wished I
had a light to smoke by, little suspecting the meaning of the minute
gleam I had seen and all that it would presently bring me.  Ogilvy
watched till one, and then gave it up; and we lit the lantern and
walked over to his house.  Down below in the darkness were Ottershaw
and Chertsey and all their hundreds of people, sleeping in peace.

He was full of speculation that night about the condition of Mars,
and scoffed at the vulgar idea of its having inhabitants who were
signalling us.  His idea was that meteorites might be falling in a
heavy shower upon the planet, or that a huge volcanic explosion was in
progress.  He pointed out to me how unlikely it was that organic
evolution had taken the same direction in the two adjacent planets.

"The chances against anything manlike on Mars are a million to
one," he said.

Hundreds of observers saw the flame that night and the night after
about midnight, and again the night after; and so for ten nights, a
flame each night.  Why the shots ceased after the tenth no one on
earth has attempted to explain.  It may be the gases of the firing
caused the Martians inconvenience.  Dense clouds of smoke or dust,
visible through a powerful telescope on earth as little grey,
fluctuating patches, spread through the clearness of the planet's
atmosphere and obscured its more familiar features.

Even the daily papers woke up to the disturbances at last, and
popular notes appeared here, there, and everywhere concerning the
volcanoes upon Mars.  The seriocomic periodical _Punch_, I remember,
made a happy use of it in the political cartoon.  And, all
unsuspected, those missiles the Martians had fired at us drew
earthward, rushing now at a pace of many miles a second through the
empty gulf of space, hour by hour and day by day, nearer and nearer.
It seems to me now almost incredibly wonderful that, with that swift
fate hanging over us, men could go about their petty concerns as they
did.  I remember how jubilant Markham was at securing a new photograph
of the planet for the illustrated paper he edited in those days.
People in these latter times scarcely realise the abundance and
enterprise of our nineteenth-century papers.  For my own part, I was
much occupied in learning to ride the bicycle, and busy upon a series
of papers discussing the probable developments of moral ideas as
civilisation progressed.

One night (the first missile then could scarcely have been
10,000,000 miles away) I went for a walk with my wife.  It was
starlight and I explained the Signs of the Zodiac to her, and pointed
out Mars, a bright dot of light creeping zenithward, towards which so
many telescopes were pointed.  It was a warm night.  Coming home, a
party of excursionists from Chertsey or Isleworth passed us singing
and playing music.  There were lights in the upper windows of the
houses as the people went to bed.  From the railway station in the
distance came the sound of shunting trains, ringing and rumbling,
softened almost into melody by the distance.  My wife pointed out to
me the brightness of the red, green, and yellow signal lights hanging
in a framework against the sky.  It seemed so safe and tranquil.
|}

let some_cookbook_sauce_chapter = {|
Wine Chaudeau: Into a lined saucepan put ½ bottle Rhine wine,
4 tablespoonfuls sugar, 1 teaspoonful cornstarch, the peel of ½
lemon and the yolks of 6 eggs; place the saucepan over a medium hot
fire and beat the contents with an egg beater until just at boiling
point; then instantly remove from the fire, beat a minute longer,
pour into a sauce bowl and serve with boiled or baked pudding.

White Wine Sauce: Over the fire place a saucepan containing 2
cups white wine, 4 tablespoonfuls sugar, 3 whole eggs, the yolks of
4 eggs and the peel and juice of 1 lemon; beat the contents of
saucepan with an egg beater until nearly boiling; then instantly
remove and serve.

Wine Cream Sauce: ½ bottle white wine, ½ teaspoonful
cornstarch, 3 eggs (yolks and whites beaten separately), 4
tablespoonfuls sugar and the peel and juice of ½ lemon; put all the
ingredients except the whites of eggs in saucepan; beat with an egg
beater until just about to boil; then remove from fire; have the
whites beaten to a stiff froth; add them to the sauce, beat for a
minute longer and then serve.

Claret Sauce: Over the fire place a lined saucepan containing
½ bottle claret, 3 or 4 tablespoonfuls sugar, 1 lemon cut into
slices and freed of the pits, a piece of cinnamon and 1 small
tablespoonful cornstarch mixed with water or wine; stir constantly
until it comes to a boil; then strain and serve. Or boil 1
tablespoonful cornstarch in 1½ cups water, with piece of cinnamon
and a few slices of lemon, for a few minutes; then remove from the
fire; add ½ pint claret and sugar to taste.

Bishop Sauce: Boil 2 ounces of sago in 2 cups water, with 1
tablespoonful fine minced or ground bitter almonds, a piece of
cinnamon and the peel of 1 lemon; when sago is done strain it
through a sieve, add 1½ cups claret, ¼ pound sugar and 1 teaspoonful
of bishop essence.

Madeira Sauce, No. 1: Set a small saucepan on the stove with
the yolks of 3 eggs, 1 cup Madeira and 2 tablespoonfuls sugar; stir
until it comes to a boil; then remove from fire and add by degrees 4
tablespoonfuls sweet cream, stirring constantly, and serve.

Madeira Sauce, No. 2: Mix 1 tablespoonful flour with 1½
spoonfuls butter; add 1½ cups boiling water; boil 3 minutes,
stirring constantly; remove from the fire, add ½ cup Madeira and 3
tablespoonfuls sugar.

Butter Sauce: In a small saucepan mix 1 tablespoonful flour
with a little cold water; add by degrees 1 cup of boiling water,
stirring constantly; set the saucepan over the fire, add 1 heaping
tablespoonful butter in small pieces; continue stirring and boil for
a few minutes.

Sherry Wine Sauce, No. 1: Add to the Butter Sauce ½ cup sugar
and ½ pint sherry wine.

Sherry Wine Sauce, No. 2: 1 cup sherry wine, ½ cup water,
the yolks of 3 eggs, 2 tablespoonfuls sugar and the grated rind of ½
lemon; put all the ingredients in a small saucepan over the fire and
keep stirring until the sauce begins to thicken; then take it off;
if allowed to boil it will be spoiled, as it will immediately
curdle; beat the whites to a stiff froth, stir them into the sauce
and serve.

Sherry Wine Sauce, No. 3: Melt in a small saucepan 1
tablespoonful butter; add 1 teaspoonful flour; when well mixed add 1
cup sherry wine, 2 tablespoonfuls sugar and the yolks of 4 eggs;
stir briskly until the sauce is on the point of boiling; then
instantly remove and serve with plum or bread pudding.

Wine or Brandy Sauce: Prepare 1 cup Butter Sauce, sweeten it
with sugar, add 1 glass brandy, port or sherry wine, a little lemon
juice and nutmeg.

Arrack Sauce (Allemande): Mix 2 tablespoonfuls flour with
some white wine; add in small pieces 2 tablespoonfuls butter, peel
and juice of ½ lemon and 2 cups white wine; place a saucepan
containing the ingredients over the fire and stir until it comes to
a boil; remove from the fire, add 1 cup arrack and 1 cup sugar.

Arrack Sauce (English): Put in a small saucepan 1
tablespoonful flour mixed with a little cold water, the yolks of 3
eggs, 1 tablespoonful butter, a piece of cinnamon, a little lemon
peel, 2 tablespoonfuls sugar and 1½ cups water; set saucepan over
the fire, stir constantly until it commences to boil; then instantly
remove from the stove, add a little lemon juice and ½ cup arrack.
This sauce can be made with any kind of wine or brandy.

Brandy Sauce (with Milk, “English Style”): Put in a small
saucepan 1 cup milk, the yolks of 2 eggs, 1 tablespoonful sugar and
a little grated lemon peel; stir over the fire till the sauce is at
boiling point; instantly remove and add 3 tablespoonfuls brandy;
serve with plum pudding.

Brandy Sauce (American), No. 1: Stir 4 tablespoonfuls
powdered sugar with 1½ spoonfuls butter to a cream; add by degrees
the yolks of 2 eggs, ½ cup boiling water and ½ cup brandy; put all
the ingredients in a tin cup and set it in a saucepan of hot water;
stir until the sauce is boiling hot; flavor with nutmeg and vanilla.
This sauce may be made of wine in the same manner.

Brandy Sauce, No. 2: Beat 1 tablespoonful butter with 6
tablespoonfuls powdered sugar to a cream; add by degrees 1
wine-glassful of brandy, 3 tablespoonfuls boiling water and a little
nutmeg; put the sauce into a tin cup, set in saucepan of boiling
water and stir until the sauce is hot; but do not allow it to boil.

Punch Sauce: Place a small vessel on the stove with 1 cup of
rum, 2 tablespoonfuls powdered sugar, the grated rind of ½ an orange
and 1 teaspoonful vanilla essence; let it remain over the fire until
the liquor catches a light flame; put on the lid for 1 minute; then
remove it from the fire, add the juice of 1 orange and serve hot.
This sauce is usually poured over the pudding.

Rum Sauce: Mix ½ tablespoonful flour with a piece of butter
the size of an egg; add 1 cup boiling water; when well mixed
together add ½ cup Rhine wine, the peel and juice of ½ lemon, 4
tablespoonfuls sugar, a piece of cinnamon and the yolks of 3 eggs;
place in a saucepan over the fire and beat with an egg beater till
the sauce comes to a boil; instantly remove and add ½ cup rum. In
place of rum, brandy may be used. NOTE.--The eggs may be omitted and
1 tablespoonful flour used instead of ½.

Sauce à la Diaz: Place a tin pan over the fire with 1 cup
rum, ½ cup Marella wine, 3 tablespoonfuls sugar, the grated rind of
1 orange and 1 teaspoonful vanilla; leave the pan on the stove until
the liquor takes fire; then cover quickly; boil 1 minute; draw it
from the fire to the side of the stove; let it stand a few minutes;
then strain into a bowl; cover tightly and when cold pour it over
the pudding.

Wine Chaudeau (with Rum): Place a saucepan on the stove with
1 teaspoonful cornstarch mixed with a little cold water; add 2 whole
eggs, the yolks of 2 eggs, 4 tablespoonfuls sugar, a little lemon
juice, some grated orange peel, ½ bottle Rhine wine and 2 glasses of
rum; stir with an egg beater until just about to boil; then
instantly remove from the fire, stir for a few minutes longer and
serve. Any other kind of liquor may be used instead of rum.

Wine Sauce (with Almonds and Raisins): Put a small vessel
over the fire with ½ bottle claret, 3 tablespoonfuls ground almonds,
3 tablespoonfuls raisins, a piece of cinnamon, 4 tablespoonfuls
sugar and the peel of 1 lemon; stir until it boils; then remove from
the fire, take out cinnamon and lemon peel and serve.

Hard Sauce: Stir ¼ pound butter with 8 tablespoonfuls
powdered sugar to a cream until it looks white; add by degrees 1
small glass of brandy (and, if liked, a little nutmeg); the yolks of
2 eggs may also be beaten through the sauce.

Hard Sauce (with Cherries): Make a hard sauce with the yolks
of 2 eggs and put some nice, ripe cherries (without the pits) into
it; stir the whole well together and serve with suet pudding or
dumplings. Blackberries, peaches or plums may be used instead of
cherries.

Strawberry Sauce: Boil in a saucepan 2 teaspoonfuls
cornstarch in 1½ cups water with the rind of 1 lemon; take it from
the fire, add 1 cup strawberry juice, a little Rhine wine or claret
and sweeten with sugar.

Sauce of Apricots: Boil 3 tablespoonfuls apricot marmalade
with 1 tablespoonful butter and ½ cup water 5 minutes; add 2
tablespoonfuls brandy and serve with boiled suet, batter pudding or
apple dumplings.

Sauce of Cherries, No. 1: Place in a saucepan 1 cup sugar,
1 cup water and ½ cup claret; when this boils add 1 pint of ripe
cherries (without the pits); boil them 10 minutes; then take out the
cherries and mix 1 teaspoonful cornstarch with a little water; add
it to the sauce, boil a minute, strain and put cherries back into
the sauce; serve cold.

Sauce of Cherries, No. 2: Remove the pits from ½ pound ripe
cherries; put the stones into a mortar and pound them fine; put
them, with the cherries, 1 pint water and a piece of cinnamon, in a
saucepan; add ¾ cup sugar and boil slowly ½ hour; strain and thicken
the sauce with 2 teaspoonfuls cornstarch; boil a minute, add ½ cup
claret and serve.

Strawberry Hard Sauce: Stir 2 tablespoonfuls butter to a
cream with 1 cup powdered sugar; add the yolks of 2 eggs; beat until
very light and stir 1 cup nice, ripe strawberries through it; put
the sauce in a glass dish, cover with the beaten whites of 2 eggs
and put some nice strawberries on top of the sauce. Any other kind
of fruit may be used instead of strawberries. Or stir ½ cup butter
with 1 cup powdered sugar to a cream; add the beaten white of 1 egg
and 1 cup thoroughly mashed strawberries.

Raspberry Sauce, No. 1: Put in a small saucepan the peel of 1
lemon, a little piece of cinnamon, 1 cup water and 1 spoonful sugar;
boil 5 minutes; mix 2 teaspoonfuls cornstarch with some cold water;
add it to the contents of saucepan; boil a minute; add 1 cup
raspberry juice or syrup and serve either hot or cold.

Raspberry Sauce, No. 2: Set a saucepan on the stove with 1½
cups raspberry juice, ½ cup water, the juice and peel of 1 lemon,
sugar to taste, 1 teaspoonful cornstarch and the yolks of 3 eggs;
beat constantly with an egg beater until it comes to a boil; quickly
remove it from the fire; beat for a few minutes longer; beat the
whites of the 3 eggs to a stiff froth and stir them into the sauce.

Huckleberry Sauce: Put the huckleberries with a little water
in a saucepan over the fire; boil slowly for ½ hour; then strain
through a sieve, sweeten with sugar and thicken with a little
cornstarch; add a few tablespoonfuls port wine or a little lemon
juice and claret; serve cold.

Sauce of Dried Cherries: Wash 1 pound dried cherries; put
them into a mortar and pound fine; place them in a saucepan with 3
or 4 cups water over the fire; add a few zwiebacks, a piece of
cinnamon and boil 1 hour; strain through a sieve, add a little
claret and lemon juice and sweeten with sugar.

Nut Sauce: Stir 1 tablespoonful butter with 5 tablespoonfuls
powdered sugar to a cream; add the yolks of 2 eggs and a few
spoonfuls of water; put it in a tin pail; set in a vessel of hot
water; stir until hot; remove the sauce from the fire, add ½ cup
fine, minced almonds and flavor with vanilla. Fine, chopped, stoned
raisins may be used instead of almonds.

Hard Sauce (with Nuts): Prepare a hard sauce of 1
tablespoonful butter and 5 tablespoonfuls powdered sugar; beat this
until white; add by degrees the yolks of 2 eggs; beat the whites of
2 eggs to a stiff froth; add the sauce gradually to the whites; beat
constantly with an egg beater; and lastly add 1 cup pounded or
ground nuts, almonds, walnuts, hazel or hickory nuts. The nuts may
be finely chopped if more convenient. This sauce may be prepared in
the same manner with peaches, apricots (peeled and cut into pieces)
or preserved pineapple.

Strawberry Custard Sauce: Place a small saucepan on the stove
with 1 pint milk, the yolks of 2 eggs and 2 tablespoonfuls sugar;
stir constantly until it comes to a boil; instantly remove from the
fire, flavor with vanilla and set it away to cool; then stir 1 cup
strawberries into it; beat the whites of the 2 eggs to a stiff froth
and put it on top of the sauce. This sauce is excellent with
strawberry shortcake. NOTE.--Any kind of fruit may be substituted
for strawberries.

Fruit Sauce (not boiled): Stir 1 cup raspberry juice and 1 of
currants with 8 tablespoonfuls sugar for 20 minutes; serve with cold
puddings. Or boil 2 teaspoonfuls cornstarch in water for a few
minutes; sweeten with sugar; thin it with raspberry, currant or
cherry juice; add a little Rhine wine and serve with cold pudding.
This sauce is exceedingly nice when made of strawberries with the
addition of the juice of 1 orange and a little grated skin.

Peach Sauce, No. 1: To be served cold. Pare and cut in halves
½ dozen peaches; stew them in sugar syrup; press them through a
sieve; thicken them with a little arrowroot or cornstarch; boil a
minute, add a little white wine and serve. Or boil the peaches
(after they are peeled and free from the stones) in sugar syrup
until tender; then take them out, put in a dish, cut each half into
4 pieces and pour the liquor over them; then serve with tapioca
pudding.

Peach Sauce, No. 2: Beat 1 tablespoonful butter with 4
tablespoonfuls powdered sugar to a cream; add the yolks of 2 eggs;
beat until very light and creamy; then beat the whites of the 2 eggs
to a stiff froth; add the sauce to them by degrees; keep on beating
with an egg beater until all is well mixed together and stir 1 cup
of fine, cut peaches through it; serve with boiled pudding.

Sauce of Currants and Raspberries: Wash ½ pound red currants
and raspberries; sprinkle with sugar and let them stand ½ hour;
prepare a sauce the same as for Peach Sauce and stir the fruit
through it.

Cream Sauce (with Jelly), No. 1: Stir 1 cup currant jelly
until smooth; add 1 cup rich, sweet cream and beat with an egg
beater to a froth; add a little arrack rum or Cognac and serve with
cold pudding.

Cream Sauce (with Jelly), No. 2: Beat ½ cup fruit jelly and
the whites of 2 eggs to a stiff froth and serve with cold pudding.

Lemon Sauce, No. 1: Stir 1 tablespoonful butter with 4
tablespoonfuls powdered sugar to a cream; add by degrees 1 beaten
egg, the juice and grated rind of ½ lemon, a little nutmeg and 4
tablespoonfuls boiling water; beat the sauce thoroughly for 5
minutes; put in a tin pail and set in saucepan of hot water; stir
constantly until very hot, but do not allow it to boil.

Lemon Custard Sauce: Place a saucepan with 1 pint milk,
3 whole eggs and 3 tablespoonfuls sugar over the fire and stir until
it just comes to the boiling point; quickly remove, pour sauce into
a dish, flavor with lemon essence and serve cold with cold pudding.

Lemon Sauce (with Liquor): Melt in a saucepan 1 tablespoonful
butter; add ½ tablespoonful flour; when well mixed pour in 1 cup
boiling water; boil 2 minutes; remove from the fire, pour sauce into
a bowl; add the juice of ½ lemon, a little nutmeg and a glass of
brandy; sweeten with sugar and serve hot. Very nice with rolly-poly
pudding or apple dumplings. Sherry or Madeira wine may be used
instead of brandy.

Sauce à l’Orange: Stir the yolks of 4 eggs with 2
tablespoonfuls powdered sugar to a cream; add by degrees 1 cup sweet
cream and stir constantly; add the grated rind of 1 orange; put the
whole in a tin cup or pail, set in a vessel of hot water and stir
all the time until it is on the point of boiling; then instantly
remove from the fire, strain through a sieve over the pudding and
serve hot.

Sauce au Kirsch: Boil 1 teaspoonful cornstarch in 1 cup
water; sweeten with 2 tablespoonfuls sugar; add 2 tablespoonfuls
kirsch and serve.

Lemon Sauce, No. 2: Mix 2 teaspoonfuls flour with a little
cold water; put it in a saucepan; add 1 pint boiling water, 1
tablespoonful butter and ½ cup sugar; stir until the sauce boils;
then remove from the fire, add the juice of 1 lemon and a little of
the grated rind and nutmeg.

Lemon Cream Sauce: Put in a tin pail or cup 1½ cups milk, the
yolks of 2 eggs and 2 tablespoonfuls sugar; set in a vessel of hot
water; beat with an egg beater until the sauce comes to a boil;
remove from the fire; add ½ teaspoonful lemon essence; beat the
whites to a stiff froth and stir them into the sauce.

Almond Sauce: Remove the brown skin of 2 ounces of almonds,
ground or chopped fine; put them in a saucepan with 2 cups milk,
2 tablespoonfuls sugar, the yolks of 3 eggs and 1 teaspoonful of
arrowroot; put the saucepan in a vessel of hot water; keep stirring
until the sauce comes to a boil. Instead of almonds almond essence
may be used; a little brandy may also be added if liked.

Chocolate Sauce: Boil ¼ pound grated chocolate with 1 cup
water and 3 tablespoonfuls sugar for 5 minutes; beat up the yolks of
3 eggs with 1½ cups cold milk; add it to the chocolate; keep
stirring until the sauce comes to a boil; instantly take it from the
fire, beat for a few minutes longer and pour it into a sauce bowl;
serve cold with cold pudding.

Chocolate Cream Sauce: Boil ¼ pound grated chocolate with 1
cup water for 5 minutes; add sugar to taste; beat up the yolks of 3
eggs with 1½ cups sweet cream; add it to the chocolate; keep
stirring until nearly boiling; remove from fire, add some vanilla
essence and the beaten whites of the 3 eggs.

Vanilla Cream Sauce: Put in a saucepan 2 cups sweet cream, 2
or 3 tablespoonfuls sugar, 2 whole eggs and the yolks of 2 eggs; set
the saucepan in a vessel of hot water; beat with an egg beater till
the sauce just comes to the boiling point; then instantly remove
from the fire; do not allow the sauce to boil; flavor with vanilla
extract and serve cold.

Vanilla Sauce: Put in a tin cup or pail 2 cups milk and 1
teaspoonful cornstarch; add the yolks of 3 eggs and 2 tablespoonfuls
sugar; place the cup in a vessel of hot water; beat with an egg
beater until it comes to a boil; instantly remove; pour the sauce
into a saucière; flavor with 1 teaspoonful vanilla and serve cold.
Do not allow the sauce to boil or it will curdle.

Sauce à la Cream (sweet): Put in a tin pail 2 cups milk, the
yolks of 2 eggs, 2 tablespoonfuls sugar and 1 teaspoonful
cornstarch; set in a vessel of hot water; stir constantly until it
comes to a boil; instantly remove; flavor with vanilla; beat the
whites of the eggs to a stiff froth; pour the sauce into a glass
dish, spread the beaten whites over it and dust some powdered sugar
over all.

White Sauce: Boil 2 teaspoonfuls arrowroot in 1 pint milk;
add 2 tablespoonfuls sugar and 1 teaspoonful lemon essence; beat the
white of 1 egg to a froth and stir it through the sauce when cold.

Cream Sauce (plain): Stir ½ tablespoonful butter with 4
tablespoonfuls powdered sugar to a cream; boil 1 tablespoonful flour
in 1 cup of water; pour it slowly into the creamed butter; keep on
beating until the whole is well mixed; flavor with 1 teaspoonful
lemon essence and serve hot.

Vanilla Sauce (plain): Put in a saucepan 1 pint milk, 1½
teaspoonfuls cornstarch, sugar to taste and stir over the fire until
it boils; flavor with 1 teaspoonful vanilla essence and serve when
cold.

Vanilla Sauce (with Cognac): Stir 2 tablespoonfuls butter
with 6 tablespoonfuls powdered sugar to a cream; add by degrees 3
tablespoonfuls Cognac, sherry or Madeira wine and ½ cup boiling
water; keep beating all the time; put this in a tin pail and set in
a vessel of hot water; keep stirring until hot, but do not allow it
to boil; remove from the fire and add 2 teaspoonfuls vanilla
essence.

Caramel Sauce: Put 2 tablespoonfuls sugar in a saucepan over
the fire; let it get light brown; add a little water; boil for a
minute or two; then pour it into a small saucepan; add 1½ cups of
milk or cream and the yolks of 2 eggs; set the saucepan in a vessel
of hot water; stir until it comes to a boil; remove from the fire
and flavor with 1 teaspoonful vanilla.

Coffee Cream Sauce: Pour 2 cups boiling hot cream over 2
tablespoonfuls freshly ground coffee; cover tightly and let it stand
10 minutes; then strain the cream through a fine sieve; put the
cream in a small saucepan; add the yolks of 2 eggs, 1 teaspoonful
cornstarch and 2 tablespoonfuls sugar; put this over a moderate fire
and stir until it comes to a boil; remove from the stove, pour it
into a sauce bowl and stir the beaten whites of the eggs through it;
serve cold.

Nutmeg Sauce: Mix 1 tablespoonful butter with 1 tablespoonful
flour; add 2 cups boiling water and boil 5 minutes; sweeten with
sugar and flavor with grated nutmeg.

Orange Cream Sauce: Stir the yolks of 4 eggs with 1½
tablespoonfuls sugar to a cream; add 1 teaspoonful butter, a little
grated orange peel and ½ pint sweet cream or milk; put the
ingredients in a small saucepan over the fire and stir till boiling
hot; when cold mix it with a few spoonfuls whipped cream. Lemon
Sauce is made in the same manner. This sauce may also be flavored
with vanilla or lemon extract.

Sabayon Sauce: Put the yolks of 4 eggs and 1 whole egg in a
lined saucepan and beat them with an egg beater to a froth; add 4
tablespoonfuls sugar, a small piece of lemon peel, the juice of 1
lemon and ½ bottle of Rhine wine; 5 minutes before serving put the
saucepan over the fire and beat constantly till boiling hot; but do
not allow it to boil; serve at once. Sabayon of Madeira or Malaga
wine without lemon juice is made the same way. If rum is added in
place of wine it is then called Rum Sabayon Sauce.

Strawberry Chaudeau Sauce: Put 1 cup strawberry juice or
syrup in a saucepan; sweeten to taste; add ½ cup white wine and the
yolks of 2 eggs; beat this over the fire with an egg beater till it
foams and rises up; remove from the fire and mix it with the beaten
whites of 2 eggs; serve with vanilla koch or souflée.

Pineapple Chaudeau Sauce: Put 1 cup pineapple juice or syrup
in a saucepan; sweeten to taste; add ½ cup white wine and the yolks
of 2 eggs; beat this over the fire with an egg beater till it foams
and rises up; remove from the fire and mix it with the beaten whites
of 2 eggs; serve with vanilla koch or souflée.

67. +Raspberry Chaudeau Sauce+ is made the same as Strawberry
Chaudeau Sauce.

Cocoanut Snow Sauce: Beat the whites of 3 eggs to a stiff
froth and boil 1 cup sugar with ½ cup water till it forms a thread
between 2 fingers; then gradually pour it into the beaten whites,
stirring constantly; next add 1 cup freshly grated cocoanut.

Cocoanut Sauce (another way): Stir 2 tablespoonfuls butter
with 1 cup powdered sugar to a cream; add by degrees the yolks of 2
eggs; then beat the whites to a stiff froth; mix them with the
sauce; add ¾ cup freshly grated cocoanut and serve with boiled
pudding.

Snow Sauce (with orange flavor): Beat the whites of 2 eggs to
a stiff froth; boil a small cup of sugar with ½ cup water till it
forms a thread between two fingers; remove it from the fire; add the
juice of 1 orange and gradually pour it while hot into the beaten
whites, stirring constantly; add last a little grated rind of orange
and serve. Snow Sauce with lemon flavor is made the same way.

Pistachio Sauce: Stir the yolks of 4 eggs with 1 pint sweet
cream and 2 tablespoonfuls sugar over the fire till nearly boiling;
remove from fire; add 2 ounces finely pounded pistachio nuts; serve
when ice cold with frozen pudding.

Cold Pineapple Sauce: Pare and grate a small, ripe pineapple;
press it through a sieve; add 1 cup sugar and a glass of Rhine wine;
let it stand on ice for 1 hour and serve with frozen pudding.
|}

let history_of_ocaml = {|
“Caml” was originally an acronym for Categorical Abstract Machine Language. It was a pun on CAM, the Categorical Abstract Machine, and ML, the family of programming languages to which Caml belongs. The name Caml has remained throughout the evolution of the language, even though the present implementation has no relation with the CAM.

Caml was first designed and implemented by INRIA's Formel team, headed by Gérard Huet. Its development now continues within the Cristal team.
The Origin

The Formel team became interested in the ML language in 1980-81. ML was the meta-language of the Edinburgh version of the LCF proof assistant, both designed by Robin Milner. It was implemented by a kind of interpreter written in Lisp by Mike Gordon, Robin Milner and Christopher Wadsworth. LCF itself was written partly in ML and partly in Lisp. In order to be able to use the LCF proof assistant on the various systems in use at Formel at that time (Multics, Berkeley Unix on Vax, Symbolics), Gérard Huet decided to make the ML implementation compatible with various Lisp compilers (MacLisp, FranzLisp, LeLisp, ZetaLisp). This work involved Guy Cousineau and Larry Paulson. The performance of the ML implementation was improved by the addition of a compiler. Guy Cousineau also added algebraic data types and pattern-matching, following ideas from Robin Milner, which he in turn had borrowed from Hope, a programming language designed by Rod Burstall and Dave McQueen. At some point, this implementation was called Le_ML, a name that did not survive. It was used by Larry Paulson to develop Cambridge LCF and by Mike Gordon for the first version of HOL, as recalled in Gordon's short history of HOL.

Around 1984, three events motivated us to get even more involved in the development of ML:

    In Edinburgh, Luca Cardelli developed a much faster implementation of ML using his Functional Abstract Machine (FAM). He also designed his own version of the language, known at that time as Cardelli's ML.
    Robin Milner thought it was a good moment to propose a new definition of ML in order to avoid divergence between various implementations. He defined the core Standard ML language, which was later complemented by a module system designed by Dave McQueen.
    At the same time, Pierre-Louis Curien developed a calculus of categorical combinators, as well as a correspondence between lambda-calculus and categorical combinators, which, as noticed by Guy Cousineau, could be seen as a compilation technique for ML. Indeed, it was quite close to the original implementation technique of Edinburgh ML, but could be described, proved correct, and optimized in a simple way. This led to the definition of the Categorical Abstract Machine (CAM). 

This urged Guy Cousineau to develop a new implementation of ML, based on the CAM. However, the language that we ended up implementing was not Standard ML, but... Caml. Why? Our main reason for developing Caml was to use it for sofware development inside Formel. Indeed, it was used for developing the Coq system, which, following Thierry Coquand's thesis in 1985, became the team's main aim. We were reluctant to adopt a standard that could later prevent us from adapting the language to our programming needs. In particular, Philippe Le Chenadec and Michel Mauny developed syntax manipulation tools that appeared useful and were incorporated into Caml. Synchronizing with the Standard ML team before adopting the language modifications that seemed useful to us would have introduced too much delay in our work. Furthermore, our philosophy was in conflict with that of a “standard” language, which is not supposed to evolve too quickly. We did incorporate into Caml most of the improvements brought by Standard ML over Edinburgh ML.
The First Implementation

The first implementation of Caml appeared in 1987 and was further developed until 1992. It was created mainly by Ascander Suarez. After Ascander left in 1988, Pierre Weis and Michel Mauny, carried on with the development and maintenance of the system. This implementation compiled Caml down to LLM3, the virtual machine of the Le_Lisp system.

Guy Cousineau modestly recalls: “I must admit that when the Caml development started, my experience with programming language implementation was very limited. Relying on the LLM3 abstract machine and on the Le_Lisp memory allocation and garbage collection system saved a lot of work but could not lead to high efficiency. The CAM model led to fast closure construction and good environment sharing but was poor at environment access and made optimizations difficult. It also potentially introduced memory leaks, since useless values were kept inside closures. Also, I had not realized that it was more important to have good performance on non-functional programs than on very functional ones. Above all, I had overlooked the importance of portability and openness. In spite of these inadequacies, for which I am initially responsible, Ascander, Pierre and Michel did quite a nice piece of work.”
Caml Light

In 1990 and 1991, Xavier Leroy designed a completely new implementation of Caml, based on a bytecode interpreter written in C. Damien Doligez provided an excellent memory management system. This new implementation, known as Caml Light, was highly portable and easily ran on small desktop machines such as Macs and PCs. It replaced the old Caml implementation and highly helped promote the use of Caml in education and in research teams. Its support for data streams and its parsing facilities, due to Michel Mauny, were issued from a continued effort of the Formel team to promote syntax manipulation tools.
Caml Special Light

In 1995, Xavier Leroy released Caml Special Light, which improved over Caml Light in several ways. First, an optimizing native-code compiler was added to the bytecode compiler. This native-code compiler matched or exceeded the performances of the best existing compilers for functional languages, and enabled Caml to be more competitive performance-wise with mainstream imperative programming languages such as C++. Second, Caml Special Light offered a high-level module system, designed by Xavier Leroy and inspired by the module system of Standard ML. This module system provides powerful abstraction and parameterization facilities for programming in the large.
Objective Caml

Type systems and type inference for object-oriented programming has been a hot area of research since the early 1990's. Didier Rémy, later joined by Jérôme Vouillon, designed an elegant and highly expressive type system for objects and classes. This design was integrated and implemented within Caml Special Light, leading to the Objective Caml language and implementation, first released in 1996 and renamed to OCaml in 2011. Objective Caml was the first language to combine the full power of object-oriented programming with ML-style static typing and type inference. It supports many advanced OO programming idioms (type-parametric classes, binary methods, mytype specialization) in a statically type-safe way, while these idioms cause unsoundness or require run-time type checks in other OO languages such as C++ and Java.

In 2000, Jacques Garrigue extended Objective Caml with several new features, which he had been experimenting with for a few years in the Objective Label dialect of Objective Caml. Among these features were polymorphic methods, labeled and optional function arguments, and polymorphic variants.
The rise of OCaml

Since the late 1990's, OCaml has been steadily gaining in popularity and attracted a significant user base. In addition to impressive programs developed in OCaml, the user community also contributed many high-quality libraries, frameworks and tools in areas ranging from graphical user interfaces and database bindings to Web and network programming, cross-language interoperability and static program analysis. In parallel, the core OCaml development team actively maintains the base system, improving the quality of the implementation and porting it to the latest architectures and systems.
Some Close Relatives

In addition to these mainstream versions of Caml, one should mention many related compilers. Michel Mauny and Daniel de Rauglaudre designed Chamau, which offers unique syntax manipulation facilities which are now offered in the Camlp4 pre-processor for OCaml. Manuel Serrano and Pierre Weis created BIGLOO. Régis Cridlig made Camlot. Jean Goubault-Larrecq wrote HimML, which features implicit hash-consing and efficient operations on sets and maps. Emmanuel Chailloux published CeML. In the Para team, Francis Dupont implemented Caml for parallel machines, while Luc Maranget built Gaml, a compiler for a lazy functional programming language.
Final Quote

In 1996, Guy Cousineau wrote: “Certainly, the history of Caml could have been more linear. However, through trial and error, a capacity for producing high performance, portable, and flexible functional programming language implementations has emerged in France.”
|}
