'''Python Ini File (pif) management system.
'''
__copyright__ = "(C)2011 asz, All Rights Reserved"
__license__ = "asz source license"

#Logging Boilerplate
import logging
log = logging.getLogger(__name__)

log.info("Setting %s module logging level to DEBUG." % __name__)
log.setLevel(logging.DEBUG)

#Standard Library Imports
import re
from collections import OrderedDict
from ply import lex, yacc

#Project Imports

#Global Variables
_COMMENT_MARKERS = [';', '#', '//']
_TRUE  = ['on', 'yes', 'true']
_FALSE = ['off', 'no', 'false']

_SECTION_RE = re.compile(r'\[(?P<header>[a-zA-Z_][a-zA-Z_.]*)\]')
_OPTION_RE  = re.compile(
              r'(?P<option>[a-zA-Z_][a-zA-Z0-9_ ]*)' #Pythonic naming scheme
              r'\s*(?:[:=]\s*'                      #Space, :|=, then more space
              r'(?P<value>(?!%s).*))?' % '|'.join(_COMMENT_MARKERS)
              #Now ain't this fun? %s'ing into a regex's negative lookahead?
              # The (?!...|...) makes sure that <value> won't be a match with
              # anything following any of the _COMMENT_MARKERS.
              )

##############################
# Private Section Superclass #
##############################
###### SEE BELOW FOR PUBLIC PifManager CLASS ######
class _Section(object):
    def __init__(self, name, parent, root):
        self._sub_sects = OrderedDict()
        self._options = OrderedDict()
        self._name = name
        self._par_sect = parent
        self._root = root

    def sub_sections(self):
        '''Returns a list of Sub-Sections contained in this Section.'''
        return self._sub_sects.keys()

    def has_section(self, section_name):
        '''Indicates whether this Section contains a Sub-Section named
        section_name.
        '''
        return section_name in self._sub_sects

    def add_section(self, section_name):
        '''Adds a Sub-Section named section_name to this Section.'''
        if section_name in self._sub_sects:
            #TODO: Should the be an error, or a pass?
            raise NotImplementedError()
        
        new_sect = _Section('%s.%s' % (self._name, section_name),
                            self, self._root)
        self._sub_sects[section_name] = new_sect
        self.__setattr__(section_name, new_sect)

    def get_section(self, section_name):
        '''Returns the Sub-Section named section_name.'''
        return self._sub_sects[section_name]

    def remove_section(self, section_name):
        '''Deletes the given Option from this Section.

        If the Sub-Section existed and was removed, return True. Otherwise
        return False.
        '''
        existed = section_name in self._sub_sects
        if existed:
            del self._sub_sects[section_name]
        return existed
        
    def option_names(self):
        '''Returns a list of the names of all of the Options contained in this
        Section.
        '''
        return self._options.keys()

    def options(self, interpolate=True):
        '''Return a list of tuples with (name, value) for each Option in this
        Section.

        All %()x interpolations are expanded for the returned values unless the
        optional interpolate argument is set to False. This may trigger parsing
        if anything other than strings are being interpolated.
        '''
        ret = []
        for name, val in self._options.items():
            if interpolate:
                val = self._interpolate(val)
            ret += [(name, val),]
        return ret

    def has_option(self, option_name):
        '''Indicates whether this Section contains an Option named option_name.
        '''
        return option_name in self._options

    def set_option(self, option_name, value):
        '''Sets the named Option to the specified value.'''
        self._options[option_name] = value

    def remove_option(self, option_name):
        '''Removes the named Option from the Section.

        If the Option existed and was removed, return True. Otherwise
        return False.
        '''
        existed = option_name in self._options
        if existed:
            del self._options[option_name]
        return existed

    def get_option(self, option_name):
        '''Returns the value of option_name. value A KeyError is raised if the
        named Option does not exist'''
        value = self._options[option_name]
        if value and isinstance(value, basestring):
            value = self._interpolate(value)
            if self._root._eval:
                parsed = self._root._val_parser.parse(value)
                if parsed:
                    value = parsed
        return value

    def _interpolate(self, value):
        '''Leverages python's built-in string replace-y whatever magic to
        interpolate values based on Option names in the same section, and
        returns the interpolated value as a string.
        '''
        depth = self._root._interp_depth
        while depth:
            depth -= 1
            if '%(' in value:
                value = value % self._options
                #TODO: If this throws a key-error, a more descriptive error should be raised
            else:
                break
        return value

    def __getattr__(self, name):
        '''Convenience method that checks for name in _options before throwing
        an AttributeError.
        '''
        try:
            return self.get_option(name)
        except KeyError:
            raise AttributeError("'%s' object has no attribute '%s'" % (self, name))
    
    def __setattr__(self, name, value):
        '''Checks to see if name exists in _options (and changes the value
        contained there) before assigning value to __dict__.
        '''
        if '_options' in self.__dict__ and name in self._options:
            self._options[name] = value
        # elif '_sub_sects' in self.__dict__ and name in self._sub_sects:
        #     raise AttributeError("'%s' object cannot directly overwrite "\
        #                          "sub-section '%s'" % (self, name))
        else:
            self.__dict__[name] = value

    def write(self, file_object):
        '''Write this Section (and all Sub-Sections, depth-first) to the given
        file object.
        '''
        file_object.write('[%s]\n' % self._name)
        for (option, value) in self.options(False):
            file_object.write("%s: %s\n" % (option,
                                             str(value).replace('\n', '\n\t')))
        for sect in self._sub_sects.values():
            file_object.write('\n')
            sect.write(file_object)

    def __str__(self):
        return 'Section "%s"' % self._name

    def __repr__(self):
        return "Section %s(Sub-Sections%s\n\t Options%s" % \
                        (self._name, self.sub_sections(), self.options(False))


###########################
# PifManager Public class #
###########################
class PifManager(_Section):
    def __init__(self, evaluate = True, interpolate_depth = -1):
        '''Disabling evaluate will not evaluate the value of each option

        interpolate_depth caps the maximum recursion level of self-referencing
        interpolated option values.
        '''
        #Section.__init__(self, name, parent, root):
        _Section.__init__(self, None, self, self)
        self._eval = evaluate
        self._interp_depth = interpolate_depth
        self._val_parser = _PifValParser()

    # def read(self, target, overwrite=None):
    #     '''Parse the target stream. Can be either a file name, a list of file
    #     names, an opened file object, or a properly formated string. If the
    #     optional overwrite argument is not set, read will overwrite repeated
    #     options in files and it will ignore repeated options in strings.

    #     If list is passed to read, a recursive call will be made to read.
    #     If a string is passed to read, it will first be checked as a potential
    #     file name, and then passed into read_string.
    #     '''
    #     if isinstance(target, basestring):
    #         try:
    #             file_obj = open(target, 'Ur')
    #             if not overwrite:
    #                 overwrite = True
    #             self.read_file(target, overwrite)
    #             file_obj.close()
    #         except IOError:
    #             if not overwrite:
    #                 overwrite = False
    #             self.read_string(target, overwrite)
    #     elif isinstance(target, file):
    #         if not overwrite:
    #             overwrite = True
    #         self.read_file(target, overwrite)
    #     else: #Must be an iterable
    #         for elem in target:
    #             self.read(elem)

    def read_file(self, target_file, overwrite=True):#, filename):
        '''Parse the given file or file-like object. The value of overwrite
        determines whether repeated option names overwrite old values, or if
        they are ignored.'''
        if isinstance(target_file, basestring):
            #TODO: Add a try-except for IOErrors, and raise a better error.
            file_object = open(target_file, 'Ur')
            self._parse_stream(file_object, overwrite)
            file_object.close()
        self._parse_stream(target_file, overwrite)

    def read_string(self, string, overwrite=False):
        '''Parse the given string. The value of overwrite determines whether
        repeated option names overwrite old values, or if they are ignored.'''
        self._parse_stream(string.splitlines(), overwrite)

    def _parse_stream(self, stream, overwrite):
        '''Parse a given input stream. The value of overwrite determines
        whether colliding option names that are read in overwrite old values or
        if they are ignored.

        The stream is intended to either be a file object (which will yield an
        iterator returning one line of text at a time) or a list of strings
        derived from a hard-coded defaults string split by line breaks. It can,
        however, be anything that -- when place in a for loop -- yields strings
        that conform to the PifManager syntax.
        '''
        def strip_inline_comments(line):
            '''Convenience function for stripping comments.'''
            finds = []
            for mark in _COMMENT_MARKERS:
                pos = line.find(mark)
                if pos != -1 and line[pos - 1] == ' ':
                    finds.append(pos - 1)
            if finds:
                return line[:min(finds)].strip()
            return line

        current_sec = self
        opt_name = None
        continuations = set()
        for line in stream:
            line_s = line.strip()
            #Is it an empty line or a comment?
            if len(line_s) < 1 or line_s[0] in _COMMENT_MARKERS:
                continue
            #Is it a continuation?
            elif line[0] == ' ' and current_sec and opt_name and len(line_s)>0:
                line_s = strip_inline_comments(line_s)
                #If the value isn't a list, we need it to be so ...
                if not isinstance(current_sec._options[opt_name], list):
                    current_sec._options[opt_name] = \
                                            [current_sec._options[opt_name]]
                #... so we can append the continuation to it.
                current_sec._options[opt_name].append(line_s)
                #Flag the (section, option) for post-processing.
                continuations.add((current_sec, opt_name))
            #So it's not a comment or a continuation...
            else:
                #Is it a section header?
                sec_match = _SECTION_RE.match(line)
                if sec_match:
                    #current_sec here will act as an accumulator, so we are able
                    # to recurse down to the last sub-sub-sub-section.
                    current_sec = self
                    for sec_name in sec_match.group(1).split('.'):
                        if not current_sec.has_section(sec_name):
                            current_sec.add_section(sec_name)
                        current_sec = current_sec._sub_sects[sec_name]
                    #So as to not toss a continuation onto an option that
                    # belongs the previous section.
                    opt_name = None
                #Is it an Option: Value pair?
                else:
                    opt_match = _OPTION_RE.match(line)
                    if opt_match:
                        opt_name, value = opt_match.group('option', 'value')
                        opt_name = opt_name.replace(' ', '_')
                        #Overwrite check    .........       The important bit
                        if current_sec.has_option(opt_name) and not overwrite:
                            opt_name = None
                            continue
                        if value:
                            value = strip_inline_comments(value)
                        if value == '':
                            value = None
                        current_sec._options[opt_name] = value
        #Last chore, join together lists of continuations.
        for sect, opt in continuations:
            sect._options[opt] = '\n'.join(sect._options[opt])

    def add_section(self, section_name):
        '''Adds a sub-section named section_name to the PifManager.'''
        if section_name in self._sub_sects:
            #TODO: Should the be an error, or a pass?
            raise NotImplementedError()
        
        new_sect = _Section(section_name, self, self)
        self._sub_sects[section_name] = new_sect
        self.__setattr__(section_name, new_sect)


    def write(self, file_object):
        '''Write the given PifManager to the give file object.'''
        #I guess all configurations, regardless of their original source
        # should be written out to the single file...
        for (option, value) in self.options(False):
            file_object.write("%s: %s\n" % (option,
                                             str(value).replace('\n', '\n\t')))
        for sect in self._sub_sects.values():
            sect.write(file_object)
            file_object.write('\n')

    def __str__(self):
        return 'PifManager'

    def __repr__(self):
        return "PifManager(Sub-Sections%s\n\t Options%s" % \
                            (self.sub_sections(), self.options(False))


######################################
# Value Parser -- PLY Implementation # 
######################################
class _PifValParser(object):
    '''BE AWARE. There be Dragons here.
    The PifValueParser is implemented using the python implementation of
    lex/yacc, PLY [http://www.dabeaz.com/ply/]. It in and of itself is a DSL
    designed to construct DSLs. If you're not familiar with the syntax and
    workings of PLY -- or at least lex and yacc -- I would encourage you to
    avoid operating on this class.
    '''
    def __init__(self):
        self.quote = None
        self.quote_start = None
        self.lexer = lex.lex(module = self)
        self.parser = yacc.yacc(module = self)

    def test_print(self, string):
        self.lexer.input(string)
        tok = self.lexer.token()
        while tok:
            print tok
            tok = self.lexer.token()
    
    def parse(self, string):
        return self.parser.parse(string, lexer=self.lexer)
    
    ###############################
    # Lexical Tokenizer (ply.lex) #
    ###############################
    states = (('1string', 'exclusive'),
              ('3string', 'exclusive'))
    reserved = dict([[e, 'TRUE']  for e in _TRUE] +
                    [[e, 'FALSE'] for e in _FALSE])
    tokens = (
        'POW', 'FLOOR_DIV', 'EQEQ', 'NEQ', 'LEEQ', 'GREQ',
        'SCIENTIFIC', 'HEX', 'BINARY', 'FLOAT', 'INT',
        'STRING', 'IDENT',
        ) + tuple(set(reserved.values()))
    literals = ':=;,+-/%*^&|(){}[]'
    terminals = r'\s)}\]:=;,\+\-%\*\^&\|' #Used to end certain tokens.

    #This is the fallthough token. Very important
    t_STRING = r'[^\(\[{%s]+' % terminals

    t_ANY_ignore = " \t\n"
    # def t_newline(self, token):
    #     r'\n+'
    #     token.lexer.lineno += len(token.value)
    
    t_POW  = r'\*\*'
    t_FLOOR_DIV = r'//'
    t_EQEQ = r'=='
    t_NEQ  = r'!='
    t_GREQ = r'>='
    t_LEEQ = r'<='
    # t_BACKSLASH = r'\\'

    def t_TRIPLE_QUOTE(self, token):
        r'"""'
        self.quote = '"""'
        self.quote_start = token.lexer.lexpos
        token.lexer.push_state('3string')
    def t_TRIPLE_TICK(self, token):
        r"'''"
        self.quote = "'''"
        self.quote_start = token.lexer.lexpos
        token.lexer.push_state('3string')
    def t_QUOTE(self, token):
        r'"'
        self.quote = '"'
        self.quote_start = token.lexer.lexpos
        token.lexer.push_state('1string')
    def t_TICK(self, token):
        r"'"
        self.quote = "'"
        self.quote_start = token.lexer.lexpos
        token.lexer.push_state('1string')

    def t_1string_STRING(self, token):
        r'''.+?['"\n]'''
        if token.value[-1] == '\n':
            raise NotImplementedError("Found a \n in a quotation. Fix this?")
        elif token.value[-1] != self.quote:
            token.lexer.skip(1)
        else:
            token.value = token.lexer.lexdata[self.quote_start:\
                                                           token.lexer.lexpos-1]
            token.type = 'STRING'
            token.lexer.lineno += token.value.count('\n')
            self.quote = None
            token.lexer.pop_state()
            return token
    def t_3string_STRING(self, token):
        r'''.+?['"]{3}'''
        if token.value[-3:] != self.quote:
            token.lexer.skip(3)
        else:
            token.value = token.lexer.lexdata[self.quote_start:\
                                                           token.lexer.lexpos-3]
            token.type = 'STRING'
            token.lexer.lineno += token.value.count('\n')
            self.quote = None
            token.lexer.pop_state()
            return token

    @lex.TOKEN(r'[0-9]*\.?[0-9]+[eE][+-]?[0-9]+(?=[%s]|$)' % terminals)
    def t_SCIENTIFIC(self, token):
        token.value = float(token.value)
        return token
    @lex.TOKEN(r'0[xX][0-9a-fA-F]*\.?[0-9a-fA-F]+(?:[pP][+-]?[0-9]+)?(?=[%s]|$)' % terminals)
    def t_HEX(self, token):
        if '.' not in token.value:
            token.value = int(token.value, 16)
        else:
            token.value = float.fromhex(token.value)
        return token
    @lex.TOKEN(r'0[bB][01]+(?=[%s]|$)' % terminals)
    def t_BINARY(self, token):
        token.value = int(token.value, 2)
        return token
    @lex.TOKEN(r'[0-9]*\.[0-9]+(?=[%s]|$)' % terminals)
    def t_FLOAT(self, token):
        token.value = float(token.value)
        return token
    @lex.TOKEN(r'[0-9]+(?=[%s]|$)' % terminals)
    def t_INT(self, token):
        token.value = int(token.value)
        return token

    # @lex.TOKEN(r'[A-Z]:\\[^\(\[{%s]+')
    # def t_win_path(self, token):
    #     token.type = 'STRING'
    #     return token

    @lex.TOKEN(r'[a-zA-Z_][a-zA-Z0-9_]*(?=[%s]|$)' % terminals)
    def t_IDENT(self, token):
        token.type = self.reserved.get(token.value.lower(), "IDENT")
        return token

    def t_ANY_error(self, token):
        print "Illegal character '%s'" % token.value[0]
        token.lexer.skip(1)

    ##################################
    # Compiler definition (ply.yacc) #
    ##################################
    precedence = (#('left', 'OR'),
                  #('left', 'AND'),
                  #('left', 'NOT'),
                  ('left', 'COMPARE'),
                  ('left', '|'),
                  ('left', '^'),
                  ('left', '&'),
                  #('left', 'SHIF_L', 'SHIFT_R'),
                  ('left', '+', '-'),
                  ('left', '*', '/', '%', 'FLOOR_DIV'),
                  ('right', 'UNARY'),
                  ('left', 'POW'))
    #Not all pythonic operations are implemented yet, but I wanted to have them
    # listed in their relative position, in case we find use for them.

    def p_expression_reduction(self, p):
        '''expression : number
                      | string
                      | list
                      | comparison
                      | boolean'''
        p[0] = p[1]
    def p_expression_plus(self, p):
        '''expression : expression '+' expression'''
        p1_is_str = isinstance(p[1], basestring)
        p3_is_str = isinstance(p[3], basestring)
        if p1_is_str and p3_is_str:
            p[0] = ' '.join((p[1], p[3]))
        elif p1_is_str:
            p[0] = ' '.join((p[1], str(p[3])))
        elif p3_is_str:
            p[0] = ' '.join((str(p[1]), p[3]))
        else:
            p[0] = p[1] + p[3]
    def p_expression_binaryop(self, p):
        '''expression : expression POW expression
                      | expression FLOOR_DIV expression
                      | expression '/' expression
                      | expression '%' expression
                      | expression '*' expression
                      | expression '-' expression
                      | expression '&' expression
                      | expression '^' expression
                      | expression '|' expression'''
        if   p[2] == '**': p[0] = p[1] ** p[3]
        elif p[2] == '//': p[0] = p[1] // p[3]
        elif p[2] == '/':  p[0] = p[1] / p[3]
        elif p[2] == '%':  p[0] = p[1] % p[3]
        elif p[2] == '*':  p[0] = p[1] * p[3]
        elif p[2] == '-':  p[0] = p[1] - p[3]
        elif p[2] == '&':  p[0] = p[1] & p[3]
        elif p[2] == '^':  p[0] = p[1] ^ p[3]
        elif p[2] == '|':  p[0] = p[1] | p[3]
    def p_expression_join(self, p):
        '''expression : expression expression'''
        p[0] = ' '.join((str(p[1]), str(p[2])))
    def p_expression_unaryop(self, p):
        '''expression : '+' expression %prec UNARY
                      | '-' expression %prec UNARY
                      | '~' expression %prec UNARY'''
        if   p[1] == '+': p[0] =  p[2]
        elif p[1] == '-': p[0] = -p[2]
        elif p[1] == '~': p[0] = ~p[2]

    def p_comparison(self, p):
        '''comparison : expression EQEQ expression %prec COMPARE
                      | expression NEQ  expression %prec COMPARE
                      | expression '<'  expression %prec COMPARE
                      | expression LEEQ expression %prec COMPARE
                      | expression '>'  expression %prec COMPARE
                      | expression GREQ expression %prec COMPARE'''
        if   p[2] == '==': p[0] = p[1] == p[3]
        elif p[2] == '!=': p[0] = p[1] != p[3]
        elif p[2] == '<':  p[0] = p[1] <  p[3]
        elif p[2] == '<=': p[0] = p[1] <= p[3]
        elif p[2] == '>':  p[0] = p[1] >  p[3]
        elif p[2] == '>=': p[0] = p[1] >= p[3]

    def p_list_start(self, p):
        '''list : '[' expression'''
        p[0] = [p[2]]
    def p_list_continue(self, p):
        '''list : list ',' expression
                | list ';' expression'''
        p[0] = p[1] + [p[3]]
    def p_list_end(self, p):
        '''expression : list ']'
                      | list ',' ']'
                      | list ';' ']' '''
        p[0] = p[1]
    def p_list_empty(self, p):
        '''expression : '[' ']' '''
        p[0] = []

    def p_tuple_start(self, p):
        '''tuple : '(' expression'''
        p[0] = [p[2]] #Using a list for mutability's sake.
    def p_tuple_continue(self, p):
        '''tuple : tuple ',' expression
                 | tuple ';' expression'''
        p[0] = p[1] + [p[3]]
    def p_tuple_end(self, p):
        '''expression : tuple ')'
                      | tuple ',' ')'
                      | tuple ';' ')' '''
        p[0] = tuple(p[1]) #Turning that list into a tuple
    def p_tuple_empty(self, p):
        '''expression : '(' ')' '''
        p[0] = ()
    
    def p_dict_start(self, p):
        '''dict : '{' IDENT ':' expression
                | '{' IDENT '=' expression'''
        p[0] = {p[2]: p[4]}
    def p_dict_continue(self, p):
        '''dict : dict ',' IDENT ':' expression
                | dict ',' IDENT '=' expression
                | dict ';' IDENT ':' expression
                | dict ';' IDENT '=' expression'''
        p[1][p[3]] = p[5]
        p[0] = p[1]
    def p_dict_end(self, p):
        '''expression : dict '}'
                      | dict ',' '}'
                      | dict ';' '}' '''
        p[0] = p[1]
    def p_dict_empty(self, p):
        '''expression : '{' '}' '''
        p[0] = {}

    def p_win_path(self, p):
        '''string : string ':' string'''
        p[0] = '%s:%s' % (p[1], p[3])

    def p_string_reduction(self, p):
        '''string : STRING
                  | IDENT'''
        p[0] = p[1]

    def p_boolean_reduction(self, p):
        '''boolean : TRUE
                   | FALSE'''
        p[0] = p[1]

    def p_number_reduction(self, p):
        '''number : INT
                  | FLOAT
                  | HEX
                  | BINARY
                  | SCIENTIFIC'''
        p[0] = p[1]

    def p_error(self, p):
        print("Syntax error! :: %s" % p)