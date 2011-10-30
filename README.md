TODO: Figure out the General Use... thing.  
TODO: So python doesn't like the `p` or the `.` in its hex numbers. Make sure you know what kind of syntax is going to be keen with this system.  
TODO: Double check the comment system, and then add in that block quote stuff.

## General Use
So, I need to sit down and figure out how this is going to be used...
I know I want to be able to access values through `Section.value`.  
Maybe `FileName.Section.value`, or `pifInstance.Section.value`. Whatever. Figure that out.

## Syntax
### Name/Value Pairs
Sorta the biggest deal here. Name/Value associations can be set up in either the Python(`:`) or the INI(`=`) ways. Spaces are entirely optional. **Names need to follow python naming conventions**, so they can't have spaces in. Before and after the `[=|:]`, though, witespace is ignored (if present).

 + `name = [value]`
 + `name: [value]`

##### Example

	name: 'TheOnly_Rew'
	number: 1`

Notes: The values, btw, are -- as you might have guessed -- a string and an int.

### Comments
There are three types of single-line comment markers, all of which can be used to comment on a line of code. (I'm not sure this is right. You should check that and make sure you're not lying. People don't love liars, Drew.)

 + `;`
 + `#`
 + `//`

##### Example

	name: 'TheOnly_Rew' #This is my name.
	number: 1 ;I'd like to say that this is where I stand in the charts.
	//Might be a bit disingenuous though...

I would like to add a block-quote marker using the good ol' `/*..*/`, but that can be added later.

### Sections
So I like the idea of sections a goodly bit. So you can sub-divide your PIF files into sections and sub-sections as you ought to be able to in INI files. Sections are accessed e.g. `pif.Section`, `pif.Section.Sub`, etc..

 + `[Section]`
 + `[Section.Sub]`

Are you able to grab references to sections? I think you can.... Hm.

##### Example

    sect_name = 'base?' #Accessed via `pif.sect_name`. Yields 'base?'
    [SectionA]
    sect_name = 'Section the First' #`pif.SectionA.sect_name` => 'Section the First'
    [SectionB]
    sect_name = 'Second Section' #`pif.SectionB.sect_name` => 'Second Section'
    [SectionA.Sub]
    sect_name = 'A SubSection!' #`pif.SectionA.Sub` => 'A SubSection!'

### Numbers
As is seeming to be the case, these are pretty self-explanatory. There's no way to do arbitrary-base numbers in here (I think... Again with the lying, man.), but functionality is built in for binary and hexadecimal (using scientific notation).

##### Example
    int: 4
    float: 0.2 #The syntax `.2` works fine, but `4.` doesn't.
    scientific: 2.3e9
    binary: 0b11011010
    hexadecimal: 0x12AF
    hex_sci: 0x1.f2p-6 #Hmm.. This isn't keen py syntax... Interesting.

### Strings
Strings are kinda funky, atm. Not sure I really like 'em.  
As it stands, we have all kinds of python strings, single-tick/quote as well as their triple counterparts. You can include ticks in quoted strings, and quotes in ticked string. We also have implicit strings for compatibility with INI files (and because... well that's the point, ain't it?). There's so much weirdness that comes out of these implicit strings... This needs to be cleared up a bit, methinks.

##### Examples

    implicit_string: implicit strings will be evaluated into a single strings
    explicit_string: 'These use Pythonic conventions and give us much more freedom.'
    quoted_string: "Both ticks(') and quotes work."
    newline_string: """Triple-ticks and triple-quotes are accepted too.
                       You can even have newlines and tabs in 'em."""

### Math
Math mostly acts like does in Python. This is one of the things I know needs a little work. Being able to figure out parentheticals (and throwing sane error messages?) is on the to-do list.

##### Examples

    add: 4 + 2    #6
    multiply: 6 * 2.4    #14.4
    floor_divide: 9 // 4    #2
    continuation: 4 * 
                      2 + 6    #14. Notice that the order of operations is maintained.

### Lists
Uhm.. They're lists. ¬_¬

##### Examples

    list: [anything, """can""", "go", 'here', 42]
    nested: [lists, [can, be, [nested,]]]
    semicolons: [elements; can; be; separated; with; semicolons]
    empty: []

### Tuples
So right now, I think this implementation of singleton tuples is why I'm having trouble with parenthetic maths. I need to reevaluate how expressions break down when in or surrounded by parenthesis.

##### Examples

    tuple: (anything, """can""", "go", 'here', 4 + 2)
    nested: (lists, (can, be, (nested)))
    semicolons: (again; with; the; semis;)
    empty: ( )

### Dicts
Yeah. I got a lot less talkative, didn't I?

##### Examples

    dict: {'key': value, 4= 'more values'}
    empty: { }
