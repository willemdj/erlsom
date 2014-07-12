# Erlsom Reference #

## Function Index
- [compile_xsd/1](#compile_xsd)
- [compile_xsd/2](#compile_xsd_2)
- [compile_xsd_file/1](#compile_xsd_file)
- [compile_xsd_file/2](#compile_xsd_file_2)
- [scan/2](#scan)
- [scan/3](#scan_3)
- [scan_file/2](#scan_file)
- [write/2](#write)
- [write_xsd_hrl_file/3](#write_xsd_hrl_file)
- [parse_sax/4](#parse_sax)
- [simple_form/1](#simple_form)
- [simple_form/2](#simple_form_2)
- [erlsom_lib:toUnicode/1](#toUnicode)
- [erlsom_lib:find_xsd/4](#find_xsd)
- [erlsom_lib:detect_encoding/1](#detect_encoding)
- [erlsom_ucs:from_utf8/1](#from_utf8)
- [erlsom_ucs:from_utf16le](#from_utf16le)
- [erlsom_ucs:from_utf16be](#from_utf16be)

### <a name="compile_xsd">compile_xsd/1</a> ###

```erlang
compile_xsd(XSD) -> {ok, Model}
```

Equivalent to `compile_xsd(XSD, [])`.

### <a name="compile_xsd_2">compile_xsd/2</a> ###
 
```erlang
compile_xsd(XSD, Options) -> {ok, Model}

XSD     = [int()]
Options = [Option]
Option  = {prefix, Prefix} |
          {type_prefix, TypePrefix} |
          {group_prefix, GroupPrefix} |
          {include_fun, Include_fun} |
          {include_dirs, Include_dirs} |
          {include_files, Include_files} |
          {include_any_attribs, boolean()}

Model   = the internal representation of the XSD
```

Compiles an XSD into a structure to be used by erlsom:scan() and erlsom:write(). Returns {ok, Model} or {error, Error}.
 
XSD can be an encoded binary (see section on character encoding) or a decoded list of Unicode code points. 

- `Prefix` is prefixed to the record names in the XSD. It should be a string or 
  'undefined'. If it is 'undefined', no prefix will be applied. The default is 
  'undefined' (no prefix).  The prefix specified with this option is applied to 
  the records that correspond to  types from the target namespace of the 
  specified XSD. Different prefixes can be specified for XSDs that are imported, 
  see the other options below. 
 
  Note that Erlsom:write() uses the prefixes to assign the namespaces. As a consequence, 
  you should use prefixes in case your XML documents use elements from more than one
  namespace (or if they contain a mixture of elements that are namespace qualified and elements that are not).
 
- `TypePrefix` is prefixed to the record names that correspond to type 
  definitions in the XSD. It should be a string.

  Record definitions are created for elements, groups and types. In the 
  XSD there may be groups, elements and types with the same name; this would 
  lead to more than one record with the same name. In order to avoid the problems 
  that this would create, it is possible to specify a prefix that will be put in 
  between the namespace prefix (see above) and the name of the type. 
 
- `GroupPrefix` is prefixed to the record names that correspond to group 
  definitions in the XSD. It should be a string. See the explanation provided 
  above for the TypePrefix option for the background of this option.

- `Include\_fun` is a function that finds the files that are included or 
  imported in the XSD. It should be a function that takes 4 arguments: 

    - Namespace (from the XSD). This is a string or 'undefined'
    - SchemaLocation (from the XSD). This is a string or 'undefined'
    - Include\_files. This is the value of the ‘include_files’ option if this 
      option was passed to compile_xsd(); [] otherwise.
    - Include_dirs. This is the value of the ‘include_dirs’ option if this 
      option was passed to compile_xsd(); 'undefined' otherwise.
 
  Include_fun should return {XSD, Prefix}, where XSD is a XSD = string(), 
  Prefix = string or 'undefined' - if the value is undefined, ‘P’ will be used.
 
  Include_fun defaults to a function that uses the Include_dirs and Include_list options as specified below.
 
    - Include_files  is a list of tuples {Namespace, Prefix, Location}. Default 
      is [].
 
    - Include_dirs is a list of directories (strings). It defaults to ["."].
 
  Behavior for include and import:
 
  If 'include_fun' option was specified, this function will be called. This should
  return both the contents of the file as a string and the prefix (a tuple {Xsd, Prefix}).
 
  Otherwise, if the 'includes_files' option is present, the list provided with this
  option will be searched for a matching namespace. If this is found, the
  specified prefix will be used. If a file is also specified, then this file will
  be used. If no file is specified (value is undefined), then the 'location'
  attribute and the 'include_dirs' option will be used to locate the file.
 
  If the 'include_files' option is not present, or if the namespace is not found, then
  the file will be searched for in the include_dirs (based on the 'location'
  attribute). No prefix will be used.

- If `include_any_attribs` == `true` (this is the default), then the second element of each
  of the records that are created by `erlsom:scan(Xml, Model)` will be a list that contains
  any attributes in the corresponding element of XML that are not explicitly specified by the
  XSD. If `include_any_attribs` == `false`, such an extra element will not be present in the 
  result of `erlsom:scan(Xml, Model)`. If Xml contains attributes that were not explicitly 
  declared in the XSD they will be simply ignored, and they will not be visible in the output.
  

### <a name="compile_xsd_file">compile_xsd_file/1</a> ###
Equivalent to `compile_xsd_file(XSD, [])`.

### <a name="compile_xsd_file_2">compile_xsd_file/2</a> ###
 
```
compile_xsd_file(XSD, Options) -> {ok, Model}
```

As [compile_xsd()](#compile_xsd), but taking its input from a file. 
 
### <a name="add_xsd_file">add_xsd_file/3</a> ###
 
```
add_xsd_file(FileName, Options, Model) -> {ok, Model}
```

Compiles an XSD file (FileName), and adds the elements defined by this XSD to Model. The purpose is to add elements (namespaces) to a model that uses the XML Schema ‘any’  construct. Only elements that are part of the model will be part of the output of ‘parse()’! See the soap example for an example where this is used.
 
See [compile_xsd()](#compile_xsd) for a description of the options.
 
 
### <a name="scan">scan/2</a> ###

```
scan(XML, Model) -> {ok, Struct, Rest}
```

Equivalent to `scan(XML, Model, [])`.


### <a name="scan_3">scan/3</a> ###
 
```
scan(XML, Model, Options) -> {ok, Struct, Rest}

XML     = [int()] or an encoded binary
Model   = the internal representation of the XSD, result of erlsom:compile()
Options = [Option]
Option  =  {continuation_function, Continuation_function,  Continuation_state} |
           {output_encoding, utf8}
Struct  = the translation of the XSD to an Erlang data structure
Rest    = list of characters that follow after the end of the XML document
```
 
Translates an XML document that conforms to the XSD to a structure of records.
 
Returns `{ok, Struct, Rest}` or `{error, Error}`.
 
`Error` has the following structure:
`[{exception, Exception}, {stack, Stack}, {received, Event}]`, where:
 
- `Exception` is the exception that was thrown by the program
- `Stack` is a representation of the 'stack' that is maintained by erlsom. 
- `Event` is the sax event that erlsom was processing when it ran into problems.
 
If specified, the continuation function is called whenever the end of the input XML document is reached before the parsing of the XML has finished. The function should have 1 argument (Continuation_state). It should return a tuple `{NewData, NewState}`, where NewData should be the next block of data (again a list of unicode code points or binary data - but the data type has to be the same for each invocation, and it has to match the data type of XML), and NewState is the information that is passed to the next invocation. Note: if the encoding of the document supports multi-byte characters (UTF8, UTF16) you don’t have to ensure that each block of data contains only complete characters - but in case of UTF16 encoding you do have to ensure that you return an odd number of bytes.

If the ‘output_encoding’ option is used, the text values will be binary encoded - but the values that are specified as integer in the XSD will still be integers.
 
 
### <a name="scan_file">scan_file/2</a> ###
 
```
scan_file(XMLFile, Model) -> {ok, Struct, Rest}
```

As [scan](#scan), but taking its input from a file.
 
 
### <a name="write">write/2</a> ###
 
```
write(Struct, Model) -> {ok, XML}

Struct = a structure that represents an XML document
Model  = the internal representation of the XSD
XML    = [int()].
```
 
Translates a structure of records to an XML document. It is the inverse of erlsom:parse().
 
Note that the output is a list of Unicode code points. If you want to write it to a file, or send it over a wire, you should transform it to binary, and generally you should encode it. You can use erlsom_ucs:to_utf8() to do this.
 
 
### <a name="write_xsd_hrl_file">write_xsd_hrl_file/3</a> ###
 
```
write_xsd_hrl_file(XSD, Output, Options) -> ok

XSD     = the name of the file that contains the XSD
Options = a list of Options, see [compile_xsd()](#compile_xsd).
Output  = the name of the output file
```
            
Produces a set of record definitions for the types defined by the XSD. Note that the options have to be identical to those that are passed to compile_xsd().
  
 
### <a name="parse_sax">parse_sax/4</a> ###

```
parse_sax(XML, Acc0, EventFun, Options) -> {ok, AccOut, Rest}

Xml      = [int()], a list of Unicode code points
Acc0     = a term() that is passed to the EventFun. 
Eventfun = a fun() that is called by the parser whenever it has parsed a bit of the Xml input 
Options  = [Option]
Option   = {continuation_function, CState, CFunction} | {output_encoding, utf8} |
           {expand_entities, true | false} | {max_entity_depth, int() | infinity} |
           {max_entity_size, int() | infinity} | {max_nr_of_entities, int() | infinity} |
           {max_nr_of_entities, int() | infinity} | 
           {max_expanded_entity_size, int() | infinity}

AccOut   = a the result of the last invocation of EventFun. 
Rest     = list of characters that follow after the end of the XML document
```
 
`EventFun` should accept the following arguments:

- `Event`, a tuple that describes the event, see [the section on the Sax parser](../README.md#sax_events)
- `AccIn` , a term() - Acc0 for the first invocation, and the result from the previous invocation for each 
   of the following invocations. 
 
EventFun should return AccOut, a term() that will be passed back to the next invocation of EventFun.

`CFunction` should be a function that takes 2 arguments: Tail and State.

- Tail is the (short) list of characters (or a short binary) that could not yet be parsed 
  because it is (or might be) an incomplete token, or because an encoded character is not
  complete. Since this still has to be parsed, CFunction should include this in front of the 
  next block of data.
- State is information that is passed by the parser to the callback function transparently. 
  This can be used to keep track of the location in the file etc.

CFunction returns `{NewData, NewState}`, where NewData is a list of characters/unicode code points/binary, and NewState the new value for the State. NewData has to be in the same type of encoding as the first part of the document.  

Note: if the encoding of the document supports multi-byte characters (UTF8, UTF16) you don’t have to ensure that each block of data contains only complete characters - but in case of UTF16 you do have to ensure that you return an odd number of bytes.

The ‘output_encoding’ option determines the encoding of the 'character data': element values and attribute values. The only supported encoding at this moment is 'utf8'. The default is string().

There is a number of options to protect against malicious entities, such as the 'billion laughs' attack. An attempt has been made to use defaults that allow most "bona fide" use of entities, but block malicious cases. Depending on the situation it may make sense to select settings that are more or less restrictive.
- expand_entities: if set to 'false', entities will not be expanded. Default: true
- max_entity_depth: limits the level of nesting of entities. The default value
  is 2, which means that an entity can refer to 1 or more other entities, but none of those can contain entity references.
- max_entity_size: limits the size of a single entity definition. Default: 2000
- max_nr_of_entities: limits the number of entities that can be defined. Default: 100
- max_expanded_entity_size: limits the total number of characters that can be introduced in an XML document by expansion of entities. Default: 10.000.000.

### <a name="simple_form">simple_form/1</a> ##

Equivalent to `simple_form(XML, [])`.

### <a name="simple_form_2">simple_form/2</a> ###

```
simple_form(XML, Options) -> {ok, SimpleFormElement, Rest}

XML     = [int()] or an encoded binary
Options = [Option]
Option  =  {nameFun, NameFun} | {output_encoding, utf8}

SimpleFormElement = {Tag, Attributes, Content}, 
Rest    = list of characters that follow after the end of the XML document
```
           
`Tag` is a string (unless otherwise specified through the nameFun option, see below), `Attributes` = `[{AttributeName, Value}]`, and `Content` is a list of SimpleFormElements and/or strings.

`Namefun` is a function with 3 arguments: `Name`, `Namespace`, `Prefix`. It should return a term. It is called for each tag and attribute name. The result will be used in the output. Default is Name if Namespace == undefined, and a string `{Namespace}Name` otherwise.


### <a name="toUnicode">erlsom_lib:toUnicode/1</a> ###
 
```
erlsom_lib:toUnicode(XML) -> DecodedXML

XML        = the XML in binary form.
DecodedXML  = the XML in the form of a list of Unicode code points.
```
 
Decodes the XML, see [the section on character decoding](../README.md#encoding). 
 

### <a name="find_xsd">erlsom_lib:find_xsd/4</a> ###
 
```
erlsom_lib:find_xsd(Namespace, Location, Include_dirs, Include_list) -> {XSD, Prefix}

Namespace = string() | 'undefined' (taken from the XSD)
Location  = string() | 'undefined' (taken from the XSD)
Include_dirs: This is the value of the Include_dirs option if this option was passed to compile_xsd(); 'undefined' otherwise.
Include_list: This is the value of the Include_list option if this option was passed to compile_xsd(); 'undefined' otherwise.
```
 
The function `erlsom_lib:find_xsd` can be passed to compile_xsd as the value for the 'include_fun' option. It will attempt to get imported XSDs from the internet (if the import, include or redefine statement includes a ‘location’ attribute in the form of a URL). 
 
If `find_xsd cannot` find the file on the internet, it will attempt to find the file using the standard function, see the description provided above with the compile_xsd function. 
 

### <a name="detect_encoding">erlsom_lib:detect_encoding/1</a> ###
 
```
erlsom_lib:detect_encoding(Document) -> {Encoding, Binary}

Document = the XML document, either in binary form or as a list
Encoding = the encoding, as an atom
Binary   = the XML document in binary form.
```
 
Tries to detect the encoding. It looks at the first couple of bytes. If these bytes cannot give a definitive answer, it looks into the xml declaration.
 
Possible values for Encoding:

- ucs4be
- ucs4le
- utf16be
- utf16le
- utf8
- iso_8859_1
 
The second return value is identical to the input if the input was in binary form, and the translation to the binary form if the input was a list.
 
(the basis of this function was copied from xmerl_lib, but it was extended to look into the xml declaration).
 
### <a name="from_utf8">erlsom_ucs:from_utf8/1</a> ###
 
``` 
erlsom_ucs:from_utf8(Data) -> {List, Tail}

Data = a block of data, either as a list of bytes or as a binary 
List = the input translated to a list of Unicode code points
Tail = remaining bytes at the end of the input (a list of bytes). 
``` 
 
These functions are based on the corresponding functions in xmerl_ucs, but they have been modified so that they can be used to translate blocks of data. The end of a block can be in the middle of a series of bytes that together correspond to 1 Unicode code point. The remaining bytes are returned, so that they can be put in front of the next block of data.
 
Note on performance: the functions work on lists, not binaries! If the input is a binary, this is translated to a list in a first step, since the functions are faster that way. If you are reading the xml document from a file, it is probably fastest to use pread() in such a way that it returns a list, and not a binary.
 
See the ‘continuation’ example for an example of how this can be used to deal with very large documents (or streams of data).

### <a name="from_utf16le">erlsom_ucs:from_utf16le/1</a> ###

Identical to [erlsom_ucs:from_utf8/1](from_utf8), but for utf16le.

### <a name="from_utf16be">erlsom_ucs:from_utf16be/1</a> ###

Identical to [erlsom_ucs:from_utf8/1](from_utf8), but for utf16be.

### <a name="parse">parse/2</a> ###
This function has been replaced by scan()! Please use [scan()](#scan).
 
### <a name="parse_file">parse_file/2</a> ###
This function has been replaced by scan_file()! Please use [scan_file()](#scan_file).

### <a name="write_hrl">write_hrl/3</a> ###
This function has been replaced by write_xsd_hrl()! Please use [write_xsd_hrl()](#write_xsd_hrl).

### <a name="parseDocument">erlsom_sax:parseDocument/3</a> ###
Obsolete, use [parse_sax()](#parse_sax). 

### erlsom_sax:parseDocument/4 ###
Obsolete, use [parse_sax()](#parse_sax). 

### <a name="compile">compile/3</a> ###
This function has been replaced by compile_xsd()! Please use [compile_xsd()](#compile_xsd).
 
### <a name="compile_file">compile_file/3 </a> ###
This function has been replaced by compile_xsd_file()! Please use [compile_xsd_file()](#compile_xsd_file).

### <a name="add_file">add_file/3</a> ###
This function has been replaced by add_xsd_file()! Please use [add_xsd_file()](#add_xsd_file).

